/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/deferred.h>
#include <devif/queue_interface.h>
#include <devif/loopback_device.h>

static struct endpoint_state* state;

// Prototypes
int main(int argc, char *argv[])
{
    errval_t err;
    struct devq_func_pointer f = {
        .setup = devq_loopback_setup,
        .create = devq_loopback_create,
        .reg = devq_loopback_register,
        .notify = devq_loopback_notify,
        .dereg = devq_loopback_deregister,
        .ctrl = devq_loopback_control,
    };

    struct endpoint_state my_state = {
        .endpoint_type = ENDPOINT_TYPE_NET,
        .device_name = "loopback",
        .q = NULL,
        .features = 0,
        .f = f,
    };
    
    state = &my_state;
    
    err = devq_driver_export(&my_state);
    if (err_is_fail(err)){  
        abort();
    }

    while (true) {
        event_dispatch(get_default_waitset());
    }
}

