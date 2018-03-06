/**
 * \file
 * \brief IOMMU Driver
 */
/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <skb/skb.h>

#include <numa.h>





/**
 * Instantiate the driver domain.
 *
 * Connect to Kaluga and wait for eventual ddomain requests.
 */
int main(int argc, char** argv)
{
    errval_t err;


    /*
     size_t drivers = 0;
    struct bfdriver* cur = NULL;
    driverkit_list(&cur, &drivers);
    for (size_t i=0; i<drivers; i++) {
        printf("%s:%s:%d: Found device driver class = %s\n", __FILE__, __FUNCTION__, __LINE__, cur->name);
        cur += 1;
    }
    for (size_t i=0; i<argc; i++) {
        printf("%s:%s:%d: argv[i] = %s\n", __FILE__, __FUNCTION__, __LINE__, argv[i]);
    }*/

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to connect to the SKB.");
    }

    err = numa_available();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initialize libnuma");
    }

    err = driverkit_iommu_service_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to create the IOMMU service\n");
    }

    iref_t kaluga_iref = 0;
    err = nameservice_blocking_lookup("ddomain_controller", &kaluga_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to connect to ddomain controller");
    }

    err = ddomain_communication_init(kaluga_iref, atoi(argv[2]));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initiate communication with Kaluga");
    }

    while(1) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch for messages_wait_and_handle_next hack");
        }
    }

    return 0;
}
