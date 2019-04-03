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
#include <dev/pc16550d_dev.h>
#include <driverkit/driverkit.h>

#define DEFAULT_PORTBASE            0x3f8   //< COM1 port
#define DEFAULT_IRQ                 4       //< COM1 IRQ

struct serial_pc16550d {
    struct serial_common m;
    struct pc16550d_t uart;
};

static void serial_poll(struct serial_pc16550d *spc)
{
    // Read as many characters as possible from FIFO
    while(pc16550d_lsr_dr_rdf(&spc->uart)) {
        char c = pc16550d_rbr_rd(&spc->uart);
        serial_input(&spc->m, &c, 1);
    }
}

static void serial_interrupt(void *m)
{
    struct serial_pc16550d *spc = m;
    pc16550d_iir_t iir = pc16550d_iir_rd(&spc->uart);

    // Assert no error happened
    assert(pc16550d_iir_iid_extract(iir) != pc16550d_rls
           && pc16550d_iir_iid_extract(iir) != pc16550d_ms);

    // disable interrupt while polling
    pc16550d_ier_erbfi_wrf(&spc->uart, 0);

    // Read serial port just like with polling
    serial_poll(spc);

    // enable interrupt when done polling
    pc16550d_ier_erbfi_wrf(&spc->uart, 1);
}

static void hw_init(struct serial_pc16550d * spc, uint32_t portbase)
{
    // Initialize Mackerel with base port
    pc16550d_initialize(&spc->uart, portbase);

    // enable interrupt
    pc16550d_ier_t ier = pc16550d_ier_default;
    ier = pc16550d_ier_erbfi_insert(ier, 1);
    pc16550d_ier_wr(&spc->uart, ier);

    // enable FIFOs
    pc16550d_fcr_t fcr = pc16550d_fcr_default;
    fcr = pc16550d_fcr_fifoe_insert(fcr, 1);
    // FIFOs hold 14 bytes
    fcr = pc16550d_fcr_rtrigger_insert(fcr, pc16550d_bytes14);
    pc16550d_fcr_wr(&spc->uart, fcr);

    pc16550d_lcr_t lcr = pc16550d_lcr_default;
    lcr = pc16550d_lcr_wls_insert(lcr, pc16550d_bits8); // 8 data bits
    lcr = pc16550d_lcr_stb_insert(lcr, 1); // 1 stop bit
    lcr = pc16550d_lcr_pen_insert(lcr, 0); // no parity
    pc16550d_lcr_wr(&spc->uart, lcr);

    // set data terminal ready
    pc16550d_mcr_t mcr = pc16550d_mcr_default;
    mcr = pc16550d_mcr_dtr_insert(mcr, 1);
    mcr = pc16550d_mcr_out_insert(mcr, 2);
    pc16550d_mcr_wr(&spc->uart, mcr);

    // Set baudrate (XXX: hard-coded to 115200)
    pc16550d_lcr_dlab_wrf(&spc->uart, 1);
    pc16550d_dl_wr(&spc->uart, pc16550d_baud115200);
    pc16550d_lcr_dlab_wrf(&spc->uart, 0);
}

static void serial_putc(struct serial_pc16550d * spc, char c)
{
    // Wait until FIFO can hold more characters
    while(!pc16550d_lsr_thre_rdf(&spc->uart));
    // Write character
    pc16550d_thr_wr(&spc->uart, c);
}

static void serial_write(void * m, const char *c, size_t len)
{
    struct serial_pc16550d * spc = m;
    for (int i = 0; i < len; i++) {
        serial_putc(spc, c[i]);
    }
}

static errval_t
serial_pc16550d_init(struct serial_pc16550d *spc, struct capref irq_src,
        uint32_t portbase)
{
    errval_t err;

    if(capref_is_null(irq_src))
        USER_PANIC("serial_kernel requires an irq cap");

    spc->m.output = serial_write;
    spc->m.output_arg = spc;

    // Register interrupt handler
    err = int_route_client_route_and_connect(irq_src, 0,
            get_default_waitset(), serial_interrupt, spc);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "interrupt setup failed.");
    }

    printf("Using port base 0x%x.\n", portbase);

    hw_init(spc, portbase);

    // offer service now we're up
    start_service(&spc->m);
    return SYS_ERR_OK;
}


static errval_t
init(struct bfdriver_instance* bfi, uint64_t flags, iref_t *dev)
{
    errval_t err;
    struct serial_pc16550d *spc = malloc(sizeof(struct serial_pc16550d));
    init_serial_common(&spc->m);

    struct capref irq_src;
    irq_src.cnode = bfi->argcn;
    irq_src.slot = PCIARG_SLOT_INT;

    // The mackerel I/O backened expects the io cap to be at the well known
    // location cap_io, so we copy it there
    // TODO: Fix mackerel and pass this on init
    struct capref io_src = {
        .cnode = bfi->argcn,
        .slot = PCIARG_SLOT_IO
    };
    err = cap_copy(cap_io, io_src);
    assert(err_is_ok(err));

    uint32_t portbase = DEFAULT_PORTBASE;

    // Parse args
    for (int i = 1; i < bfi->argc; i++) {
        if (strncmp(bfi->argv[i], "portbase=", sizeof("portbase=") - 1) == 0) {
            uint32_t x=
                strtoul(bfi->argv[i] + sizeof("portbase=") - 1, NULL, 0);
            if (x == 0 || x > 65535) {
                fprintf(stderr, "Error: invalid portbase 0x%"PRIx32"\n", x);
                USER_PANIC("Error: invalid portbase");
            }
            portbase = x;
        } else {
            fprintf(stderr, "Error: unknown option %s\n", bfi->argv[i]);
            USER_PANIC("Error: unknown option\n");
            return SYS_ERR_OK;
        }
    }

    err = serial_pc16550d_init(spc, irq_src, portbase);
    assert(err_is_ok(err));
    bfi->dstate = spc;

    SERIAL_DEBUG("pc16550d Serial driver initialized.\n");
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
    struct pc16550d_t * spc = bfi->dstate;
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

DEFINE_MODULE(serial_pc16550d, init, attach, detach, set_sleep_level, destroy, get_ep);
