/**
 * \file User-level interrupt handler support
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/inthandler.h>
#include <if/monitor_blocking_defs.h>

struct waitset *barrelfish_interrupt_waitset = NULL;

/* allocate inrq */
static errval_t arm_allocirq(struct capref ep, uint32_t irq)
{
    errval_t err, msgerr;

    struct monitor_blocking_binding *r = get_monitor_blocking_binding();
    err = r->rpc_tx_vtbl.arm_irq_handle(r, ep, irq, &msgerr);
    if (err_is_fail(err)){
        return err;
    } else {
        return msgerr;
    }
}


/**
 * Get a new irq destination capability for the current core using the monitor.
 */
errval_t alloc_dest_irq_cap(struct capref *retcap)
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



struct interrupt_handler_state {
    struct lmp_endpoint *idcep;
    interrupt_handler_fn handler;
    void *handler_arg;
    interrupt_handler_fn reloc_handler;
    void *reloc_handler_arg;
};

static void generic_interrupt_handler(void *arg)
{
    struct interrupt_handler_state *state = arg;
    errval_t err;

    // consume message
    struct lmp_recv_msg buf = LMP_RECV_MSG_INIT;
    err = lmp_endpoint_recv(state->idcep, &buf.buf, NULL);
    assert(err_is_ok(err));

    if (buf.buf.msglen == 1 && buf.words[0] == 1) {
        // domain moved notification
        debug_printf("got moved, need to reregister for interrupt\n");
        if (!state->reloc_handler) {
            debug_printf("no relocation handler registered, expect badness!\n");
            return;
        }
        // run relocation handler
        state->reloc_handler(state->reloc_handler_arg);
    } else {
        // run real handler
        //if (init_complete) {
        state->handler(state->handler_arg);
    }
    // re-register
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = arg,
    };
    err = lmp_endpoint_register(state->idcep, barrelfish_interrupt_waitset, cl);
    assert(err_is_ok(err));
}


/**
 * \brief Setup an interrupt handler function to receive device interrupts
 *        on the ARM platform
 *
 * \param handler Handler function
 * \param handler_arg Argument passed to #handler
 * \param irq the IRQ number to activate
 */
errval_t inthandler_setup_arm(interrupt_handler_fn handler, void *handler_arg,
        uint32_t irq)
{
    errval_t err;

    if(barrelfish_interrupt_waitset == NULL) {
        barrelfish_interrupt_waitset = get_default_waitset();
    }

    /* alloc state */
    struct interrupt_handler_state *state;
    state = malloc(sizeof(struct interrupt_handler_state));
    assert(state != NULL);

    state->handler = handler;
    state->handler_arg = handler_arg;

    /* create endpoint to handle interrupts */
    struct capref epcap;

    // use minimum-sized endpoint, because we don't need to buffer >1 interrupt
    err = endpoint_create(LMP_RECV_LENGTH, &epcap, &state->idcep);
    if (err_is_fail(err)) {
        free(state);
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    // allocate a local interrupt vector for this endpoint
    err = arm_allocirq(epcap, irq);
    if (err_is_fail(err)) {
        return err;
    }

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = state,
    };
    err = lmp_endpoint_register(state->idcep, barrelfish_interrupt_waitset, cl);
    if (err_is_fail(err)) {
        lmp_endpoint_free(state->idcep);
        // TODO: release vector
        free(state);
        return err_push(err, LIB_ERR_LMP_ENDPOINT_REGISTER);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Setup an interrupt handler function to receive device interrupts targeted at dest_cap
 *
 * \param dest_cap Capability to an interrupt line that targets the last level controller (such as local APIC)
 * \param handler Handler function
 * \param handler_arg Argument passed to #handler
 */
errval_t inthandler_setup_movable_cap(struct capref dest_cap, interrupt_handler_fn handler, void *handler_arg,
                                  interrupt_handler_fn reloc_handler,
                                  void *reloc_handler_arg)
{
    errval_t err;

    if(barrelfish_interrupt_waitset == NULL) {
        barrelfish_interrupt_waitset = get_default_waitset();
    }

    /* alloc state */
    struct interrupt_handler_state *state;
    state = malloc(sizeof(struct interrupt_handler_state));
    assert(state != NULL);

    state->handler = handler;
    state->handler_arg = handler_arg;
    state->reloc_handler = reloc_handler;
    state->reloc_handler_arg = reloc_handler_arg;

    /* create endpoint to handle interrupts */
    struct capref epcap;

    // use minimum-sized endpoint, because we don't need to buffer >1 interrupt
    err = endpoint_create(LMP_RECV_LENGTH, &epcap, &state->idcep);
    if (err_is_fail(err)) {
        free(state);
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = state,
    };
    err = lmp_endpoint_register(state->idcep, barrelfish_interrupt_waitset, cl);
    if (err_is_fail(err)) {
        lmp_endpoint_free(state->idcep);
        // TODO: release vector
        free(state);
        return err_push(err, LIB_ERR_LMP_ENDPOINT_REGISTER);
    }

    // Connect dest_cap with endpoint
    invoke_irqdest_connect(dest_cap, epcap);


    return SYS_ERR_OK;
}

/**
 * \brief Deprecated. inthandler_setup_moveable_cap Setup an interrupt handler function to receive device interrupts.
 *
 * \param handler Handler function
 * \param handler_arg Argument passed to #handler
 * \param ret_vector On success, returns interrupt vector with which
 *                   handler is associated
 */
errval_t inthandler_setup_movable(interrupt_handler_fn handler, void *handler_arg,
                                  interrupt_handler_fn reloc_handler,
                                  void *reloc_handler_arg,
                                  uint64_t *ret_vector)
{
    errval_t err;

    if(barrelfish_interrupt_waitset == NULL) {
        barrelfish_interrupt_waitset = get_default_waitset();
    }

    /* alloc state */
    struct interrupt_handler_state *state;
    state = malloc(sizeof(struct interrupt_handler_state));
    assert(state != NULL);

    state->handler = handler;
    state->handler_arg = handler_arg;
    state->reloc_handler = reloc_handler;
    state->reloc_handler_arg = reloc_handler_arg;

    // Get irq_dest_cap from monitor
    struct capref irq_dest_cap;
    err = alloc_dest_irq_cap(&irq_dest_cap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "Could not allocate dest irq cap");
        free(state);
        return err;
    }

    // create endpoint to handle interrupts
    struct capref epcap;

    // use minimum-sized endpoint, because we don't need to buffer >1 interrupt
    err = endpoint_create(LMP_RECV_LENGTH, &epcap, &state->idcep);
    if (err_is_fail(err)) {
        free(state);
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    err = invoke_irqdest_connect(irq_dest_cap, epcap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not connect irq_cap and endpoint");
        return err;
    }

    err = invoke_irqdest_get_vector(irq_dest_cap, ret_vector);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not lookup irq vector");
        return err;
    }

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = state,
    };
    err = lmp_endpoint_register(state->idcep, barrelfish_interrupt_waitset, cl);
    if (err_is_fail(err)) {
        lmp_endpoint_free(state->idcep);
        // TODO: release vector
        free(state);
        return err_push(err, LIB_ERR_LMP_ENDPOINT_REGISTER);
    }

    return SYS_ERR_OK;
}

errval_t inthandler_setup(interrupt_handler_fn handler, void *handler_arg,
                          uint64_t *ret_vector)
{

    return inthandler_setup_movable(handler, handler_arg, NULL, NULL, ret_vector);
}
