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
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>
#include <dev/pl011_uart_dev.h>

#include "serial.h"

static struct pl011_uart_t uart;

/* Versatile express UART1. */
#define DEFAULT_IRQ 38
#define DEFAULT_MEMBASE 0x1c0a0000

static void
serial_interrupt(void *arg) {
    /* Read as many characters as possible. */
    while(pl011_uart_FR_rxfe_rdf(&uart) == 0) {
        char c= (char)pl011_uart_DR_rd(&uart);

        serial_input(&c, 1);
    }
}

errval_t
serial_init(struct serial_params *params) {
    uint32_t membase= DEFAULT_MEMBASE;
    if(params->membase != SERIAL_MEMBASE_INVALID)
        membase= (uint32_t)params->membase;

    uint8_t irq= DEFAULT_IRQ;
    if(params->irq != SERIAL_IRQ_INVALID)
        irq= params->irq;

    // XXX: TODO: figure this out --> kaluga magic?
    errval_t err;
    lvaddr_t base;
    printf("serial: mapping device at PA %"PRIx32"\n", (uint32_t)membase);
    err= map_device_register(membase, BASE_PAGE_SIZE, &base);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "map_device_register failed\n");
        return err;
    }

    pl011_uart_initialize(&uart, (mackerel_addr_t) base);

    /* Mask all interrupts: set all bits to zero. */
    pl011_uart_IMSC_rawwr(&uart, 0);

    /* Register interrupt handler. */
    err = inthandler_setup_arm(serial_interrupt, NULL, irq);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "interrupt setup failed.");
    }

    /* Disable the UART before reconfiguring it. */
    pl011_uart_CR_uarten_wrf(&uart, 0);

    /* Clear and enable the receive interrupt. */
    pl011_uart_ICR_rxic_wrf(&uart, 1);
    pl011_uart_IMSC_rxim_wrf(&uart, 1);

    // Configure port to 38400 baud, 8 data, no parity, 1 stop (8-N-1)
    //
    // (This is a mild scam as system is running in QEMU)
    //
    // Note baud rate changes not committed in h/w until lcr_h
    // written.
    pl011_uart_IBRD_divint_wrf(&uart, 0xc); // Assuming UARTCLK is 7.3728MHz
    pl011_uart_FBRD_divfrac_wrf(&uart, 0);

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
    pl011_uart_LCR_H_wr(&uart, lcr);

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
    pl011_uart_CR_wr(&uart, cr);

    /* Offer service now we're up. */
    start_service();
    return SYS_ERR_OK;
}

static void
serial_putc(char c) {
    while(pl011_uart_FR_txff_rdf(&uart) == 1);
    pl011_uart_DR_rawwr(&uart, c);
}

void
serial_write(const char *c, size_t len) {
    for (int i = 0; i < len; i++) {
        serial_putc(c[i]);
    }
}
