/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007-2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include <int_route/int_route_client.h>
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>
#include <dev/pl011_uart_dev.h>

#include "serial.h"


struct serial_pl011 {
    struct serial_common m;
    struct pl011_uart_t uart;
};


/* Versatile express UART1. */
#define DEFAULT_IRQ 38
#define DEFAULT_MEMBASE 0x1c0a0000

static void
serial_interrupt(void *arg) {
    struct serial_pl011 * sp = arg;
    /* Read as many characters as possible. */
    while(pl011_uart_FR_rxfe_rdf(&sp->uart) == 0) {
        char c= (char)pl011_uart_DR_rd(&sp->uart);

        serial_input(&sp->m, &c, 1);
    }
}

static errval_t
hw_init(struct serial_pl011 * sp, struct capref mem) {
    // XXX: TODO: figure this out --> kaluga magic?
    errval_t err;
    struct frame_identity id;
    void *vbase;

    err = invoke_frame_identify(mem, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace_map_one_frame_attr failed\n");
    }

    printf("serial: mapping device at PA %"PRIx32"\n", (uint32_t)id.base);

    err = vspace_map_one_frame_attr(&vbase, id.bytes, mem,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL,
                                    NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace_map_one_frame_attr failed\n");
    }
    assert(vbase);

    pl011_uart_initialize(&sp->uart, (mackerel_addr_t) vbase);

    /* Mask all interrupts: set all bits to zero. */
    pl011_uart_IMSC_rawwr(&sp->uart, 0);

    /* Disable the UART before reconfiguring it. */
    pl011_uart_CR_uarten_wrf(&sp->uart, 0);

    /* Clear and enable the receive interrupt. */
    pl011_uart_ICR_rxic_wrf(&sp->uart, 1);
    pl011_uart_IMSC_rxim_wrf(&sp->uart, 1);

    // Configure port to 38400 baud, 8 data, no parity, 1 stop (8-N-1)
    //
    // (This is a mild scam as system is running in QEMU)
    //
    // Note baud rate changes not committed in h/w until lcr_h
    // written.
    pl011_uart_IBRD_divint_wrf(&sp->uart, 0xc); // Assuming UARTCLK is 7.3728MHz
    pl011_uart_FBRD_divfrac_wrf(&sp->uart, 0);

    /* Configure the line control register. */
    pl011_uart_LCR_H_t lcr= (pl011_uart_LCR_H_t)0;
    /* Disable FIFOs.  There's no way to get an interrupt when a single
     * character arrives with FIFOs, so it's useless as a console. */
    lcr= pl011_uart_LCR_H_fen_insert(lcr, 0);
    /* Eight data bits. */
    lcr= pl011_uart_LCR_H_wlen_insert(lcr, pl011_uart_bits8);
    /* No parity. */
    lcr= pl011_uart_LCR_H_pen_insert(lcr, 0);
    /* One stop bit. */
    lcr= pl011_uart_LCR_H_stp2_insert(lcr, 0);
    pl011_uart_LCR_H_wr(&sp->uart, lcr);

    /* Configure the main control register. */
    pl011_uart_CR_t cr = (pl011_uart_CR_t)0;
    /* No flow control. */
    cr= pl011_uart_CR_ctsen_insert(cr, 0);
    cr= pl011_uart_CR_rtsen_insert(cr, 0);
    /* Enable transmit and receive. */
    cr= pl011_uart_CR_txe_insert(cr, 1);
    cr= pl011_uart_CR_rxe_insert(cr, 1);
    /* Enable UART. */
    cr= pl011_uart_CR_uarten_insert(cr, 1);
    pl011_uart_CR_wr(&sp->uart, cr);

    return SYS_ERR_OK;
}

static void
serial_putc(struct serial_pl011 * sp, char c) {
    while(pl011_uart_FR_txff_rdf(&sp->uart) == 1);
    pl011_uart_DR_rawwr(&sp->uart, c);
}

static void
serial_write(void *m, const char *c, size_t len) {
    struct serial_pl011 * sp = m;
    for (int i = 0; i < len; i++) {
        serial_putc(sp, c[i]);
    }
}

static errval_t
init(struct bfdriver_instance* bfi, uint64_t flags, iref_t *dev)
{
    errval_t err;
    struct serial_pl011 *sp = malloc(sizeof(struct serial_pl011));
    init_serial_common(&sp->m);

    bfi->dstate = sp;

    struct capref irq;
    irq.cnode = bfi->argcn;
    irq.slot = PCIARG_SLOT_INT;

    struct capref mem;
    mem.cnode = bfi->argcn;
    mem.slot = PCIARG_SLOT_BAR0;

    sp->m.output = serial_write;
    sp->m.output_arg = sp;

    // initialize hardware
    err = hw_init(sp, mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "serial_init failed\n");
        return -1;
    }

    // register interrupt
    err = int_route_client_route_and_connect(irq, 0, get_default_waitset(),
            serial_interrupt, sp);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "interrupt setup failed.");
    }

    // offer service now we're up
    start_service(&sp->m);
    return SYS_ERR_OK;
}

static errval_t attach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

static errval_t detach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {
    return SYS_ERR_OK;
}

static errval_t destroy(struct bfdriver_instance* bfi) {
    struct serial_pl011 * spc = bfi->dstate;
    free(spc);
    bfi->dstate = NULL;
    // XXX: Tear-down the service
    bfi->device = 0x0;
    return SYS_ERR_OK;
}

static errval_t get_ep(struct bfdriver_instance* bfi, bool lmp, struct capref* ret_cap)
{   
    USER_PANIC("NIY \n");
    return SYS_ERR_OK;
}

DEFINE_MODULE(serial_pl011, init, attach, detach, set_sleep_level, destroy, get_ep);
