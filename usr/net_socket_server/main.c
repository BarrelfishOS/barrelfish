/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <barrelfish/nameservice_client.h>

#include "netss.h"     

/**
 * Instantiate the driver domain.
 *
 * Connect to Kaluga and wait for eventual ddomain requests.
 */
int main(int argc, char** argv)
{
    iref_t kaluga_iref = 0;
    errval_t err = nameservice_blocking_lookup("ddomain_controller", &kaluga_iref);
    assert(err_is_ok(err));
    err = ddomain_communication_init(kaluga_iref, atoi(argv[argc-1]));
    assert(err_is_ok(err));


#ifdef POLLING
#define POLLING_YIELD_IDLE 10
    uint16_t polling_yield = POLLING_YIELD_IDLE;
#endif
    while(1) {
#ifdef POLLING
        err = event_dispatch_non_block(get_default_waitset());
        switch(err_no(err)) {
            case SYS_ERR_OK:
                polling_yield = POLLING_YIELD_IDLE;
            break;
            case LIB_ERR_NO_EVENT:
                polling_yield--;
            break;
            default:
            USER_PANIC_ERR(err, "error happened while dispatching event.");
        }
        err = networking_poll();
        switch(err_no(err)) {
            case SYS_ERR_OK:
                polling_yield = POLLING_YIELD_IDLE;
                break;
            case LIB_ERR_NO_EVENT:
                polling_yield--;
                break;
            default:
                USER_PANIC_ERR(err, "error happened while dispatching event.");
        }

        if (polling_yield == 0) {
            thread_yield();
            polling_yield = POLLING_YIELD_IDLE;
        }
#else
        event_dispatch(get_default_waitset());
#endif
    }

    return 0;
}
