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
#include <driverkit/driverkit.h>
#include "serial.h"

static void
serial_interrupt(void *arg) {
    char c;
    errval_t err= sys_getchar(&c);
    assert(err_is_ok(err));

    //printf("'%c'\n", c);

    serial_input(&c, 1);
}

errval_t serial_init(struct serial_params *params)
{
    errval_t err;

    if(params->irq == SERIAL_IRQ_INVALID)
        USER_PANIC("serial_kernel requires an irq= parameter");

    /* Register interrupt handler. */
    err = inthandler_setup_arm(serial_interrupt, NULL, params->irq);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "interrupt setup failed.");
    }

    // offer service now we're up
    start_service();
    return SYS_ERR_OK;
}


void serial_write(const char *c, size_t len)
{
    sys_print(c, len);
}
