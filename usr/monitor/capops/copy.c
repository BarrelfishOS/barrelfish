/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/distcaps.h>
#include <if/intermon_defs.h>
#include "monitor.h"
#include "capops.h"
#include "capsend.h"
#include "magic.h"
#include "capop_handlers.h"

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
 * Send operations {{{1
 */

/*
 * Copy result {{{2
 */

// send state struct for recv_copy_result
struct recv_copy_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    capaddr_t capaddr;
    uint8_t vbits;
    cslot_t slot;
    genvaddr_t st;
};

// send queue continuation for recv_copy_result
static void
recv_copy_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct recv_copy_result_msg_st *msg_st = (struct recv_copy_result_msg_st*)e;
    err = intermon_capops_recv_copy_result__tx(b, NOP_CONT, msg_st->status, msg_st->capaddr, msg_st->vbits, msg_st->slot, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send recv_copy_result");
    }
    free(msg_st);
}

// enqueueing function for recv_copy_result
static errval_t
recv_copy_result(coreid_t dest, errval_t status, capaddr_t capaddr, uint8_t vbits, cslot_t slot, genvaddr_t st)
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
    msg_st->vbits = vbits;
    msg_st->slot = slot;
    msg_st->st = st;

    // enqueue message
    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        return err;
    }

    return SYS_ERR_OK;
}

inline static errval_t
recv_copy_error_result(coreid_t dest, errval_t status, genvaddr_t st)
{
    return recv_copy_result(dest, status, 0, 0, 0, st);
}

/*
 * Copy from owner to dest (possibly as intermediary) {{{2
 */

// send state struct for owner_copy
struct owner_copy_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    struct capref capref;
    intermon_caprep_t caprep;
    genvaddr_t st;
};

// send queue continuation for owner_copy
static void
owner_copy_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    struct owner_copy_msg_st *msg_st = (struct owner_copy_msg_st*)e;
    struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)(msg_st->st);
    assert(rpc_st);
    errval_t err, cleanup_err;

    err = intermon_capops_recv_copy__tx(b, NOP_CONT, msg_st->caprep, msg_st->st);

    if (rpc_st->from != my_core_id) {
        // need to cleanup cap allocated in request_copy__rx_handler
        cleanup_err = cap_destroy(msg_st->capref);
        if (err_is_fail(cleanup_err)) {
            DEBUG_ERR(err, "cleaning up send state");
        }
    }
    free(msg_st);
    msg_st = NULL;

    if (err_is_ok(err)) {
        // skip rpc cleanup if message sent successfully
        return;
    }

    if (rpc_st->from != my_core_id) {
        // source is another core (we're acting as intermediary)
        assert(!rpc_st->result_handler);
        cleanup_err = recv_copy_error_result(rpc_st->from, err, rpc_st->st);
        if (err_is_fail(cleanup_err)) {
            DEBUG_ERR(err, "sending recv_copy from owner");
            USER_PANIC_ERR(cleanup_err, "failed to send recv_copy_result for recv_copy error");
        }
    }
    else {
        // source is this core, call result handler directly
        if (rpc_st->result_handler) {
            rpc_st->result_handler(err, 0, 0, 0, (void*)rpc_st->st);
        }
    }
    free(rpc_st);
}

// enqueueing function for owner_copy
static errval_t
owner_copy(struct capref capref, struct capability *cap, coreid_t from, coreid_t dest, copy_result_handler_t result_handler, genvaddr_t st)
{
    errval_t err;

    bool unused_has_descendants;
    err = monitor_cap_remote(capref, true, &unused_has_descendants);
    if (err_is_fail(err)) {
        return err;
    }

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
    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
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
    err = intermon_capops_request_copy__tx(b, NOP_CONT, msg_st->dest, msg_st->caprep, (genvaddr_t)msg_st->st);
    if (err_is_fail(err)) {
        assert(msg_st->st);
        struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)msg_st->st;
        if (rpc_st->result_handler) {
            rpc_st->result_handler(err, 0, 0, 0, (void*)rpc_st->st);
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

    // cap is foreign so it must be a type that needs "locality" on a particular core
    assert(distcap_needs_locality(cap.type));

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
    err = capsend_owner(get_cap_domref(capref), (struct msg_queue_elem*)msg_st);
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

void
recv_copy_result__rx_handler(struct intermon_binding *b, errval_t status, capaddr_t capaddr, uint8_t vbits, cslot_t slot, genvaddr_t st)
{
    assert(st);
    struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)st;

    if (rpc_st->from != my_core_id) {
        // acting as intermediary, forward to origin
        assert(!rpc_st->result_handler);
        recv_copy_result(rpc_st->from, status, capaddr, vbits, slot, rpc_st->st);
    }
    else {
        // origin of copy, call result handler
        if (rpc_st->result_handler) {
            rpc_st->result_handler(status, capaddr, vbits, slot, (void*)rpc_st->st);
        }
    }
    free(rpc_st);
}

void
recv_copy__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct capref dest = NULL_CAP;
    struct capability cap;

    caprep_to_capability(&caprep, &cap);

    err = slot_alloc(&dest);
    if (err_is_fail(err)) {
        dest = NULL_CAP;
        goto send_result;
    }

    coreid_t owner;
    if (distcap_needs_locality(cap.type)) {
        // if cap needs locality, message source is owner
        owner = from;
    }
    else {
        // otherwise every core is owner
        owner = my_core_id;
    }

    // create a cap from the cap data and owner
    err = monitor_cap_create(dest, &cap, owner);
    if (err_is_fail(err)) {
        // may fail if given owner does not match owner of existing copies
        goto free_slot;
    }

    goto send_result;

free_slot:
    slot_free(dest);
    dest = NULL_CAP;

send_result:
    recv_copy_result(from, err, get_cnode_addr(dest), get_cnode_valid_bits(dest), dest.slot, st);
}

void
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
    distcap_state_t state;

    // copy requests should never happen for types that don't need locality,
    // since every core is owner and can copy directly
    assert(distcap_needs_locality(cap.type));

    // find and validate cap
    // NOTE: this function should fail if no copies exist and create a new copy otherwise
    err = slot_alloc(&capref);
    if (err_is_fail(err)) {
        goto send_err;
    }
    err = monitor_copy_if_exists(&cap, capref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "while finding copy in request_copy");
        goto send_err;
    }
    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        goto send_err;
    }
    if (distcap_state_is_foreign(state)) {
        err = MON_ERR_CAP_FOREIGN;
        goto send_err;
    }
    if (distcap_state_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto send_err;
    }

    if (dest == my_core_id) {
        // tried to send copy to owning core, success!
        recv_copy_result(from, SYS_ERR_OK, capref.cnode.address, capref.cnode.address_bits, capref.slot, st);
    }
    else {
        // mark cap as having remote copies
        bool unused_has_descendants;
        err = monitor_cap_remote(capref, true, &unused_has_descendants);
        if (err_is_fail(err)) {
            goto send_err;
        }

        // forward copy to destination core
        err = owner_copy(capref, &cap, from, dest, NULL, st);
        if (err_is_fail(err)) {
            goto send_err;
        }
    }

    goto end;

send_err:
    send_err = recv_copy_error_result(from, err, st);
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
capops_copy(struct capref capref, coreid_t dest, copy_result_handler_t result_handler, void *st)
{
    errval_t err;
    struct capability cap;
    distcap_state_t state;

    debug_printf("copy\n");

    // check that cap is valid
    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        return err;
    }
    if (distcap_state_is_busy(state)) {
        return MON_ERR_REMOTE_CAP_RETRY;
    }

    if (dest == my_core_id) {
        // tried to send to self, just create a local copy
        struct capref res;
        err = slot_alloc(&res);
        if (err_is_fail(err)) {
            return err;
        }
        err = cap_copy(capref, res);
        if (err_is_fail(err)) {
            return err;
        }

        result_handler(SYS_ERR_OK, res.cnode.address, res.cnode.address_bits, res.slot, st);
        return SYS_ERR_OK;
    }

    if (distcap_state_is_foreign(state)) {
        // sending copy from non-owner, send copy request to owner
        return request_copy(capref, dest, result_handler, (genvaddr_t)st);
    }
    else {
        // sending copy from owner
        err = monitor_cap_identify(capref, &cap);
        if (err_is_fail(err)) {
            return err;
        }
        return owner_copy(capref, &cap, my_core_id, dest, result_handler, (genvaddr_t)st);
    }
}
