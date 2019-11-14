/**
 * \file
 * \brief Contains handler functions for server-side octopus interface RPC call.
 */

/*
 * Copyright (c) 2019, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include <int_route/int_route_server.h>


// The main function of this service
int main(void) {
    debug_printf("Hello from int_route standalone service\n");
    errval_t err;

    err = skb_client_connect();
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "starting skb");
    }

    err = skb_execute("[irq_routing_new].");
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "loading irq routing new");
        USER_PANIC("panic");
    }

    err = skb_execute("add_armv8_controllers");
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "add_armv8_controllers");
        USER_PANIC("panic");
    }

    err = int_route_service_init();
    assert(err_is_ok(err));
    while (true) {
        event_dispatch(get_default_waitset());
    }
    return 0;
}
