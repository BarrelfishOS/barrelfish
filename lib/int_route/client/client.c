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
#include <if/monitor_blocking_defs.h>

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

static errval_t alloc_dest_irq_cap(struct capref *retcap)
{
    errval_t err, msgerr;

    struct monitor_blocking_binding *r = get_monitor_blocking_binding();
    err = slot_alloc(retcap);
    if (err_is_fail(err)) {
        return err;
    }
    err = r->rpc_tx_vtbl.get_irq_dest_cap(r, retcap, &msgerr);
    if (err_is_fail(err)){
        return err;
    } else {
        return msgerr;
    }
}

struct irq_handler_arg { 
    struct lmp_endpoint *ep;
    struct event_closure uc;
};

static void irq_handler(void *arg)
{
    struct irq_handler_arg *irqarg = (struct irq_handler_arg *)arg;
    struct lmp_recv_buf dummy = { .buflen = 0 };

    /* Consume the endpoint message */
    errval_t err = lmp_endpoint_recv(irqarg->ep, &dummy, NULL);
    assert(err_is_ok(err));

    /* Call user's closure */
    irqarg->uc.handler(irqarg->uc.arg);
}

errval_t int_route_client_route_and_connect(struct capref intsrc, int irq_idx,
        struct waitset * ws, interrupt_handler_fn handler, void *handler_arg)
{
    errval_t err;
    /* allocate irq dest cap */
    struct capref irq_dest_cap;
    err = alloc_dest_irq_cap(&irq_dest_cap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "alloc_dest_irq_cap");
        return err;
    }

    /* create endpoint to handle interrupts */
    struct capref epcap;

    /* use minimum-sized endpoint, because we don't need to buffer >1 interrupt */
    struct lmp_endpoint *idcep;
    err = endpoint_create(LMP_RECV_LENGTH, &epcap, &idcep);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }
    // TODO LH: Don't use persistent, but reregister in irq_handler
    idcep->waitset_state.persistent = true;

    /* Wrap the user callback in a call that receives the lmp message */
    struct irq_handler_arg * ag = malloc(sizeof(struct irq_handler_arg));
    ag->uc = MKCLOSURE(handler, handler_arg); 
    ag->ep = idcep;
    err = lmp_endpoint_register(idcep, ws, MKCLOSURE(irq_handler, ag));

    /* connect irq_dest with EP */
    err = invoke_irqdest_connect(irq_dest_cap, epcap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not connect irq_cap and endpoint");
        return err;
    }

    /* add route from int_src_cap to irq_dest_cap */
    err = int_route_client_route(intsrc, 0, irq_dest_cap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "int_route_client_route");
        return err;
    }

    return SYS_ERR_OK;
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
