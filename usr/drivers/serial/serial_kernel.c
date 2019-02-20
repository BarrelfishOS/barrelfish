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
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/inthandler.h>
#include <int_route/int_route_client.h>
#include <driverkit/driverkit.h>
#include "serial.h"

static void
serial_interrupt(void *arg) {
    struct serial_main * m = arg;
    char c;
    errval_t err = sys_getchar(&c);
    assert(err_is_ok(err));
    serial_input(m, &c, 1);
}

static void
serial_write(struct serial_main *m, const char *c, size_t len)
{
    sys_print(c, len);
}

errval_t serial_kernel_init(struct serial_main *m)
{
    errval_t err;

    if(capref_is_null(m->irq_src))
        USER_PANIC("serial_kernel requires an irq cap");

    m->output = serial_write;

    // Register interrupt handler
    err = int_route_client_route_and_connect(m->irq_src, 0,
            get_default_waitset(), serial_interrupt, m);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "interrupt setup failed.");
    }

    // offer service now we're up
    start_service(m);
    return SYS_ERR_OK;
}


