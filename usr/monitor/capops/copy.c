/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <if/intermon_defs.h>
#include "monitor.h"

/*
 * Error codes TBD {{{1
 */

#define CAP_ERR_FOREIGN 698
#define CAP_ERR_BUSY    699

/*
 * Magic NYI functions {{{1
 */

typedef uint8_t capstate_t;

// get owner core of given cap. used by routing layer only
errval_t cap_get_owner(struct capref, coreid_t*);

// create a cap from the given cap data owned by a given core
// may fail if given owner does not match owner of existing copies
errval_t cap_create_on(struct capability*, coreid_t, struct capref*);

// create a copy of cap other copies exist, otherwise fail
errval_t copy_if_exists(struct capability*, struct capref*);

// get the state of the given cap
errval_t cap_get_state(struct capref, capstate_t*);

// cap state queries
bool cap_state_is_valid(capstate_t);
bool cap_state_is_owner(capstate_t);

/*
 * NYI intermon.if functions {{{1
 */

errval_t intermon_recv_copy_result__tx(struct intermon_binding*, struct event_closure, errval_t, capaddr_t, genvaddr_t);
errval_t intermon_recv_copy__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, genvaddr_t);
errval_t intermon_request_copy__tx(struct intermon_binding*, struct event_closure, coreid_t, intermon_caprep_t, genvaddr_t);

/*
 * Header {{{1
 */

typedef void (*copy_result_handler_t)(errval_t, capaddr_t, void*);

errval_t copy(struct capref capref, coreid_t dest, copy_result_handler_t result_handler, void *st);

/*
 * Routing layer (send only) {{{1
 */

static errval_t
intermon_enqueue_send_target(coreid_t dest, struct msg_queue_elem *queue_elem)
{
    errval_t err;

    // get destination intermon_binding and _state
    struct intermon_binding *dest_b;
    err = intermon_binding_get(dest, &dest_b);
    if (err_is_fail(err)) {
        return err;
    }
    struct intermon_state *inter_st = (struct intermon_state*)dest_b->st;

    // enqueue message
    return intermon_enqueue_send(dest_b, &inter_st->queue, dest_b->waitset, queue_elem);
}

static errval_t
intermon_enqueue_send_owner(struct capref capref, struct msg_queue_elem *queue_elem)
{
    errval_t err;

    // read cap owner
    coreid_t owner;
    err = cap_get_owner(capref, &owner);
    if (err_is_fail(err)) {
        return err;
    }

    // enqueue to owner
    return intermon_enqueue_send_target(owner, queue_elem);
}

__attribute__((unused))
static errval_t
intermon_enqueue_send_one(struct capref capref, struct msg_queue_elem *queue_elem)
{
    USER_PANIC("NYI");
}

__attribute__((unused))
static errval_t
intermon_enqueue_send_all(struct capref capref, struct msg_queue_elem *queue_elem)
{
    USER_PANIC("NYI");
}

/*
 * RPC state {{{1
 */

struct cap_copy_rpc_st {
    // caller/sender st
    genvaddr_t st;
    // sender if acting as intermediary
    coreid_t from;
    // result handler if being called directly
    copy_result_handler_t result_handler;
};

/*
 * Send (enqueue) handlers {{{1
 */

/*
 * Copy result {{{2
 */

// send state struct for recv_copy_result
struct recv_copy_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    capaddr_t capaddr;
    genvaddr_t st;
};

// send queue continuation for recv_copy_result
static void
recv_copy_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct recv_copy_result_msg_st *msg_st = (struct recv_copy_result_msg_st*)e;
    err = intermon_recv_copy_result__tx(b, NOP_CONT, msg_st->status, msg_st->capaddr, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send recv_copy_result");
    }
    free(msg_st);
}

// enqueueing function for recv_copy_result
static errval_t
recv_copy_result(coreid_t dest, errval_t status, capaddr_t capaddr, genvaddr_t st)
{
    errval_t err;

    // create send state
    struct recv_copy_result_msg_st *msg_st = calloc(1, sizeof(struct recv_copy_result_msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = recv_copy_result_send_cont;
    msg_st->status = status;
    msg_st->capaddr = capaddr;
    msg_st->st = st;

    // enqueue message
    err = intermon_enqueue_send_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Copy from owner to dest (possibly as intermediary) {{{2
 */

// send state struct for owner_copy
struct owner_copy_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    intermon_caprep_t caprep;
    genvaddr_t st;
};

// send queue continuation for owner_copy
static void
owner_copy_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    struct owner_copy_msg_st *msg_st = (struct owner_copy_msg_st*)e;
    errval_t err;
    err = intermon_recv_copy__tx(b, NOP_CONT, msg_st->caprep, msg_st->st);
    if (err_is_fail(err)) {
        // if send fails, we try and send the error back to the source
        struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)(msg_st->st);
        assert(rpc_st);
        if (rpc_st->from != my_core_id) {
            // source is another core (we're acting as intermediary)
            assert(!rpc_st->result_handler);
            errval_t send_err = recv_copy_result(rpc_st->from, err, 0, rpc_st->st);
            if (err_is_fail(send_err)) {
                err = err_push(send_err, err);
                USER_PANIC_ERR(err, "failed to send recv_copy_result for recv_copy error");
            }
        }
        else {
            // source is this core, call result handler directly
            if (rpc_st->result_handler) {
                rpc_st->result_handler(err, 0, (void*)rpc_st->st);
            }
        }
        free(rpc_st);
    }
    free(msg_st);
}

// enqueueing function for owner_copy
static errval_t
owner_copy(struct capability *cap, coreid_t from, coreid_t dest, copy_result_handler_t result_handler, genvaddr_t st)
{
    errval_t err;

    // create new rpc state to associate return message
    struct cap_copy_rpc_st *rpc_st = malloc(sizeof(struct cap_copy_rpc_st));
    if (!rpc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    rpc_st->st = st;
    rpc_st->from = from;
    rpc_st->result_handler = result_handler;

    // create send state
    struct owner_copy_msg_st *msg_st = calloc(1, sizeof(struct owner_copy_msg_st));
    if (!msg_st) {
        free(rpc_st);
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = owner_copy_send_cont;
    capability_to_caprep(cap, &msg_st->caprep);
    msg_st->st = (genvaddr_t)rpc_st;

    // enqueue message
    err = intermon_enqueue_send_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        free(rpc_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Copy request from non-owner to owner {{{2
 */

// send state struct for request_copy
struct request_copy_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    intermon_caprep_t caprep;
    coreid_t dest;
    struct cap_copy_rpc_st *st;
};

// send queue continuation for request_copy
static void
request_copy_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct request_copy_msg_st *msg_st = (struct request_copy_msg_st*)e;
    err = intermon_request_copy__tx(b, NOP_CONT, msg_st->dest, msg_st->caprep, (genvaddr_t)msg_st->st);
    if (err_is_fail(err)) {
        assert(msg_st->st);
        struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)msg_st->st;
        if (rpc_st->result_handler) {
            rpc_st->result_handler(err, 0, (void*)rpc_st->st);
        }
        free(rpc_st);
    }
    free(msg_st);
}

// enqueueing function for request_copy
static errval_t
request_copy(struct capref capref, coreid_t dest, copy_result_handler_t result_handler, genvaddr_t st)
{
    errval_t err;
    struct capability cap;
    err = monitor_cap_identify(capref, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    // create new rpc state to associate return message
    struct cap_copy_rpc_st *rpc_st = malloc(sizeof(struct cap_copy_rpc_st));
    if (!rpc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    rpc_st->st = st;
    rpc_st->from = my_core_id;
    rpc_st->result_handler = result_handler;

    // create send state
    struct request_copy_msg_st *msg_st = calloc(1, sizeof(struct request_copy_msg_st));
    if (!msg_st) {
        free(rpc_st);
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = request_copy_send_cont;
    msg_st->dest = dest;
    capability_to_caprep(&cap, &msg_st->caprep);
    msg_st->st = rpc_st;

    // enqueue message
    err = intermon_enqueue_send_owner(capref, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        free(rpc_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Receive handlers {{{1
 */

__attribute__((unused))
static void
recv_copy_result__rx_handler(struct intermon_binding *b, errval_t status, capaddr_t capaddr, genvaddr_t st) {
    assert(st);
    struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)st;

    if (rpc_st->from != my_core_id) {
        // acting as intermediary, forward to origin
        assert(!rpc_st->result_handler);
        recv_copy_result(rpc_st->from, status, capaddr, rpc_st->st);
    }
    else {
        // origin of copy, call result handler
        if (rpc_st->result_handler) {
            rpc_st->result_handler(status, capaddr, (void*)rpc_st->st);
        }
    }
    free(rpc_st);
}

__attribute__((unused))
static void
recv_copy__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st) {
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct capref capref;
    struct capability cap;

    caprep_to_capability(&caprep, &cap);
    // create a cap from the cap data and owner
    // XXX: this should happen in the cap routing layer
    err = cap_create_on(&cap, from, &capref);
    if (err_is_fail(err)) {
        // may fail if given owner does not match owner of existing copies
        recv_copy_result(from, err, 0, st);
    }
    else {
        capaddr_t capaddr = get_cap_addr(capref);
        recv_copy_result(from, SYS_ERR_OK, capaddr, st);
    }
}

__attribute__((unused))
static void
request_copy__rx_handler(struct intermon_binding *b, coreid_t dest, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err, send_err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct capref capref;
    memset(&capref, 0, sizeof(capref));
    struct capability cap;
    caprep_to_capability(&caprep, &cap);
    capstate_t state;

    // find and validate cap
    // NOTE: this function should fail if no copies exist and create a new copy otherwise
    err = copy_if_exists(&cap, &capref);
    if (err_is_fail(err)) {
        goto send_err;
    }
    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        goto send_err;
    }
    if (!cap_state_is_owner(state)) {
        err = CAP_ERR_FOREIGN;
        goto send_err;
    }
    if (!cap_state_is_valid(state)) {
        err = CAP_ERR_BUSY;
        goto send_err;
    }

    if (dest == my_core_id) {
        // tried to send copy to owning core, success!
        capaddr_t result_addr = get_cap_addr(capref);
        recv_copy_result(from, SYS_ERR_OK, result_addr, st);
    }
    else {
        // forward copy to destination core
        err = owner_copy(&cap, from, dest, NULL, st);
        if (err_is_fail(err)) {
            goto send_err;
        }
    }

    goto end;

send_err:
    send_err = recv_copy_result(from, err, 0, st);
    if (err_is_fail(send_err)) {
        err_push(send_err, err);
        USER_PANIC_ERR(err, "failed to send error to request_copy sender");
    }

end:
    // cleanup temporary copy of cap
    cap_destroy(capref);
}

/*
 * Copy operation {{{1
 */

errval_t
copy(struct capref capref, coreid_t dest, copy_result_handler_t result_handler, void *st)
{
    errval_t err;
    struct capability cap;
    capstate_t state;

    // check that cap is valid
    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        return err;
    }
    if (!cap_state_is_valid(state)) {
        return CAP_ERR_BUSY;
    }

    if (dest == my_core_id) {
        // tried to send to self, just create a local copy
        struct capref result_ref;
        err = slot_alloc(&result_ref);
        if (err_is_fail(err)) {
            return err;
        }
        err = cap_copy(capref, result_ref);
        if (err_is_fail(err)) {
            return err;
        }

        capaddr_t result_addr = get_cap_addr(result_ref);
        result_handler(SYS_ERR_OK, result_addr, st);
        return SYS_ERR_OK;
    }

    if (cap_state_is_owner(state)) {
        // sending copy from owner
        err = monitor_cap_identify(capref, &cap);
        if (err_is_fail(err)) {
            return err;
        }
        return owner_copy(&cap, my_core_id, dest, result_handler, (genvaddr_t)st);
    }
    else {
        // sending copy from non-owner, send copy request to owner
        return request_copy(capref, dest, result_handler, (genvaddr_t)st);
    }
}
