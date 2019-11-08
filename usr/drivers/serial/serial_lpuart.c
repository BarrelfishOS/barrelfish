/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <int_route/int_route_client.h>
#include <pci/pci.h>
#include "serial.h"
#include "serial_debug.h"
#include "pci/pci.h"
#include <dev/lpuart_dev.h>
#include <driverkit/driverkit.h>
#include <barrelfish/deferred.h>
#include <barrelfish/systime.h>

#define DELAY  10000
//#define LPUART_DEBUG_ON

#if defined(LPUART_DEBUG_ON) || defined(GLOBAL_DEBUG)
#    define LPUART_DEBUG(x...) debug_printf(x)
#else
#    define LPUART_DEBUG(x...) ((void)0)
#endif

struct serial_lpuart {
    struct serial_common m;
    struct lpuart_t uart;
};

static void print_regvalues(struct serial_lpuart *spc)
{
    char buffer[10000];
    lpuart_pr(buffer, 10000, &spc->uart);
    LPUART_DEBUG("printing lpuart  %s\n", buffer);
}

static void serial_poll(struct serial_lpuart *spc)
{
    while (lpuart_stat_rdrf_rdf(&spc->uart)) {
        char c = lpuart_rxdata_buf_rdf((&spc->uart));
        if (c)
            LPUART_DEBUG("Read char=%c\n", c);
        else
            LPUART_DEBUG("Read NULL char\n");
        serial_input(&spc->m, &c, 1);
        uint8_t or = lpuart_stat_or_rdf(&spc->uart);
        if (or == 1) {
            LPUART_DEBUG("OR FLAG IS SET\n");
            lpuart_stat_or_wrf(&spc->uart, 1);
        }
    }
}

static void serial_poll_ev(void *arg)
{
    serial_poll((struct serial_lpuart *)arg);

    struct deferred_event *de = malloc(sizeof(struct deferred_event));
    deferred_event_init(de);
    errval_t err = deferred_event_register(de, get_default_waitset(), DELAY,
                                           MKCLOSURE(serial_poll_ev, arg));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "deferred event register failed.");
    }
}


static void hw_init(struct serial_lpuart *spc)
{  // Disable transceiver
    lpuart_ctrl_t ctrl = lpuart_ctrl_rawrd(&spc->uart);
    ctrl = lpuart_ctrl_te_insert(ctrl, 0);
    ctrl = lpuart_ctrl_re_insert(ctrl, 0);
    lpuart_ctrl_rawwr(&spc->uart, ctrl);
    // Set baudrate
    // baudrate = clock rate / (over sampling rate * SBR)
    // TODO: Currently we assume UART clock is set to 8MHz
    lpuart_baud_t baud = lpuart_baud_default;
    baud = lpuart_baud_osr_insert(baud, lpuart_ratio5);
    // OSR of 5 needs bothedge set
    baud = lpuart_baud_bothedge_insert(baud, 1);
    baud = lpuart_baud_sbr_insert(baud, 139);
    lpuart_baud_rawwr(&spc->uart, baud);
    // enable FIFOs
    lpuart_fifo_t fcr = lpuart_fifo_default;
    ctrl = lpuart_ctrl_default;
    ctrl = lpuart_ctrl_te_insert(ctrl, 0);
    ctrl = lpuart_ctrl_re_insert(ctrl, 0);
    lpuart_ctrl_wr(&spc->uart, ctrl);
    // enable fifo
    fcr = lpuart_fifo_rxfe_insert(fcr, 1);
    lpuart_fifo_wr(&spc->uart, fcr);
    fcr = lpuart_fifo_txfe_insert(fcr, 1);
    lpuart_fifo_wr(&spc->uart, fcr);
    lpuart_water_rxwater_wrf(&spc->uart, 0);
    // Enable transceiver
    ctrl = lpuart_ctrl_default;
    ctrl = lpuart_ctrl_te_insert(ctrl, 1);
    ctrl = lpuart_ctrl_re_insert(ctrl, 1);
    lpuart_ctrl_wr(&spc->uart, ctrl);
    print_regvalues(spc);
    // ENABLE INTERRUPTS
    /*
    //transmit interrupt enable
     lpuart_ctrl_tie_wrf(&spc->uart,1);
    //Transmission Complete Interrupt Enable
    lpuart_ctrl_tcie_wrf(&spc->uart,1);
    //Receiver Interrupt Enable
    lpuart_ctrl_rie_wrf(&spc->uart,1);
    //Overrun Interrupt Enable
     lpuart_ctrl_orie_wrf(&spc->uart,1);
     //noise error interrupt enable
     lpuart_ctrl_neie_wrf(&spc->uart,1);
     //framing error interrupt enable
     lpuart_ctrl_feie_wrf(&spc->uart,1);
     //Parity Error Interrupt Enable
     lpuart_ctrl_peie_wrf(&spc->uart,1);
     */
}

static void serial_putc(struct serial_lpuart *spc, char c)
{
    lpuart_t *u = &spc->uart;
    assert(u->base != 0);

    while (lpuart_stat_tdre_rdf(u) == 0)
        ;
    lpuart_txdata_wr(u, c);
}

static void serial_write(void *m, const char *c, size_t len)
{
    struct serial_lpuart *spc = m;
    for (int i = 0; i < len; i++) {
        serial_putc(spc, c[i]);
    }
}

static errval_t serial_lpuart_init(struct serial_lpuart *spc, struct capref irq_src)
{
    debug_printf("Hello world from serial lpuart driver\n");

    // errval_t err;

    // if(capref_is_null(irq_src))
    //     USER_PANIC("serial_kernel requires an irq cap");

    spc->m.output = serial_write;
    spc->m.output_arg = spc;

    // Register interrupt handler
    // err = int_route_client_route_and_connect(irq_src, 0,
    //        get_default_waitset(), serial_interrupt, spc);
    // if (err_is_fail(err)) {
    //    USER_PANIC_ERR(err, "interrupt setup failed.");
    // }

    hw_init(spc);

    // offer service now we're up
    start_service(&spc->m);
    return SYS_ERR_OK;
}

static errval_t init(struct bfdriver_instance *bfi, uint64_t flags, iref_t *dev)
{
    errval_t err;
    struct serial_lpuart *spc = malloc(sizeof(struct serial_lpuart));
    init_serial_common(&spc->m);
    // copy
    lvaddr_t vbase;
    struct capref devframe_cap = { .slot = DRIVERKIT_ARGCN_SLOT_BAR0,
                                   .cnode = bfi->argcn };
    err = map_device_cap(devframe_cap, &vbase);
    debug_printf("vbase = %p\n", vbase);
    lpuart_initialize(&spc->uart, (mackerel_addr_t)vbase);

    /*  struct capref irq_src;
    irq_src.cnode = bfi->argcn;
    irq_src.slot = PCIARG_SLOT_INT; */
    err = serial_lpuart_init(spc, NULL_CAP);
    assert(err_is_ok(err));
    bfi->dstate = spc;

    SERIAL_DEBUG("lpuart Serial driver initialized.\n");
    debug_printf("installing handler, spc=%p\n", spc);
    serial_poll_ev(spc);
    return SYS_ERR_OK;
}

static errval_t attach(struct bfdriver_instance *bfi)
{
    return SYS_ERR_OK;
}

static errval_t detach(struct bfdriver_instance *bfi)
{
    return SYS_ERR_OK;
}

static errval_t set_sleep_level(struct bfdriver_instance *bfi, uint32_t level)
{
    return SYS_ERR_OK;
}

static errval_t destroy(struct bfdriver_instance *bfi)
{
    struct lpuart_t *spc = bfi->dstate;
    free(spc);
    bfi->dstate = NULL;
    // XXX: Tear-down the service
    bfi->device = 0x0;
    return SYS_ERR_OK;
}

static errval_t get_ep(struct bfdriver_instance *bfi, bool lmp, struct capref *ret_cap)
{
    USER_PANIC("NIY \n");
    return SYS_ERR_OK;
}

DEFINE_MODULE(serial_lpuart, init, attach, detach, set_sleep_level, destroy, get_ep);
