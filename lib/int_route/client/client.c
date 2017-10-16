/**
 * \file
 * \brief Contains helper functions for clients (such as devce drivers) of
 *        the interrupt routing service.
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
#include <int_route/int_route_client.h>
#include <int_route/int_route_debug.h>

#include <if/int_route_service_defs.h>
#include <if/int_route_service_defs.h>

static struct int_route_state {
    bool request_done;
    struct int_route_binding * client;
    struct int_route_service_binding * binding;

} int_route_state_st;


static struct int_route_state * get_int_route_state(void){
    return &int_route_state_st;
}

static void bind_cb(void *st, errval_t binderr, struct int_route_service_binding *b) {
    assert(err_is_ok(binderr));
    int_route_state_st.binding = b;
    int_route_state_st.request_done = true;

    int_route_service_rpc_client_init(b);
}

//errval_t int_route_add_controller(int bus, int dev, int fun,
//        int_route_service_controller_type_t type) {
//    struct int_route_service_binding * b = get_int_route_state()->binding;
//    assert(b != null);
//    // int_route_service_pci_address_t addr, int_route_service_controller_type_t type
//    int_route_service_pci_address_t addr = {.bus = bus, .device = device, .fun = fun}
//    b->rx_vtbl.add_controller_call(b, addr, type);
//}
//
//
//errval_t int_route_route(struct capref intin, struct capref dest){
//    errval_t err;
//    struct int_route_service_binding * b = get_int_route_state()->binding;
//    assert(b != null);
//    return b->tx_vtbl.route_call(b, NULL, intin, dest);
//}

errval_t int_route_client_route(struct capref intsrc, int irq_idx,
        struct capref intdest){
    assert(int_route_state_st.request_done);
    struct int_route_service_binding * cl = int_route_state_st.binding;
    errval_t msgerr, err;
    msgerr = cl->rpc_tx_vtbl.route(cl, intsrc, irq_idx, intdest, &err);
    if(err_is_fail(msgerr)){
        return msgerr;
    }
    return err;
}

errval_t int_route_client_connect(void){
    errval_t err;
    iref_t iref;
    struct int_route_state *state = get_int_route_state();

    /* check if the RPC client already has been initialized */
    if (state->binding != NULL) {
        return SYS_ERR_OK;
    }

    err = nameservice_blocking_lookup("int_route_service", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    state->request_done = false;
    err = int_route_service_bind(iref, bind_cb, NULL, get_default_waitset(),
                   IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    /* XXX: wait for connection to complete */
    while (!state->request_done) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}
