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
#include <barrelfish/inthandler.h>
#include <int_route/int_route_client.h>
#include <driverkit/driverkit.h>
#include "serial.h"
#include "serial_debug.h"
#include <pci/pci.h>

struct serial_kernel {
    struct serial_common m;
};

static void
serial_interrupt(void *arg) {
    struct serial_kernel * sk = arg;
    char c;
    errval_t err = sys_getchar(&c);
    assert(err_is_ok(err));
    serial_input(&sk->m, &c, 1);
}

static void
serial_write(void *m, const char *c, size_t len)
{
    sys_print(c, len);
}

static errval_t
serial_kernel_init(struct serial_kernel *sk, struct capref irq_src)
{
    errval_t err;

    sk->m.output = serial_write;
    sk->m.output_arg = sk;

    // Register interrupt handler
    err = int_route_client_route_and_connect(irq_src, 0,
            get_default_waitset(), serial_interrupt, sk);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "interrupt setup failed.");
    }

    // offer service now we're up
    start_service(&sk->m);
    return SYS_ERR_OK;
}

static errval_t
init_kernel(struct bfdriver_instance* bfi, uint64_t flags, iref_t *dev)
{
    errval_t err;
    struct serial_kernel *sk = malloc(sizeof(struct serial_kernel));
    init_serial_common(&sk->m);

    bfi->dstate = sk;

    struct capref irq_src;
    irq_src.cnode = bfi->argcn;
    irq_src.slot = PCIARG_SLOT_INT;


    // Initialize serial driver
    err = serial_kernel_init(sk, irq_src);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "serial_init");
        return err;
    }

    SERIAL_DEBUG("Kernel Serial driver initialized.\n");

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
    struct serial_common * m = bfi->dstate;
    free(m);
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

DEFINE_MODULE(serial_kernel, init_kernel, attach, detach, set_sleep_level, destroy, get_ep);

