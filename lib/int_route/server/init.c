/**
 * \file
 * \brief Contains handler functions for server-side octopus interface RPC call.
 */

/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h> // read list
#include <int_route/int_route_server.h>
#include <int_route/int_route_debug.h>

#include <if/int_route_service_defs.h>

static void add_controller_call(struct int_route_service_binding *b, int_route_service_pci_address_t addr,
        int_route_service_controller_type_t type) {
    INT_DEBUG("add_controller_call enter: addr(%d,%d,%d) type: %s\n", addr.bus,
            addr.device, addr.function,
            type == int_route_service_CONTROLLER_MSI ? "MSI" : "MSI-x");

    uint16_t intbase = 0;
    INT_DEBUG("Returning intbase=%"PRIu16"\n",intbase);
    b->rx_vtbl.add_controller_response(b, intbase, SYS_ERR_OK);
}

static void route_call(struct int_route_service_binding *b,
        struct capref intsource, struct capref intdest){
    INT_DEBUG("route_call enter\n");
    b->rx_vtbl.route_response(b, SYS_ERR_OK);
}

static struct int_route_service_rx_vtbl rx_vtbl = {
        .add_controller_call = add_controller_call,
        .route_call = route_call

};

static errval_t rpc_connect_cb(void *st, struct int_route_service_binding *b) {
    INT_DEBUG("rpc_connect_cb");
    b->st = NULL;
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;

}

static void export_cb(void *st, errval_t err, iref_t iref){
    INT_DEBUG("export_cb\n");
    assert(err_is_ok(err));

    err = nameservice_register("int_route_service", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    };
}

errval_t int_route_service_init(void)
{
    int_route_service_export(NULL, export_cb, rpc_connect_cb, get_default_waitset(),
        IDC_EXPORT_FLAGS_DEFAULT);
    return SYS_ERR_OK;
}
