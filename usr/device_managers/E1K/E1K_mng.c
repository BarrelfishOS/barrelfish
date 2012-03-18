/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/waitset.h>
#include <net_device_manager/net_ports_service.h>
#include <barrelfish/nameservice_client.h>
#include "E1K_mng_debug.h"

/*
#include <barrelfish/net_constants.h>
*/

// handle events in infinite loop
static void event_loop(void)
{

    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    } // end while: infinite
} // end function: event_loop


int main(int argc, char **argv)
{
//    char *device = "loopback";
    char *device = "e1000";
    E1KDM_DEBUG("Started the e1k_dev_manager for device [%s]\n", device);
    init_ports_service(device);
    E1KDM_DEBUG("done with most of things for device[%s]\n", device);
    event_loop();
    // network_service_loop
} // end function: main


