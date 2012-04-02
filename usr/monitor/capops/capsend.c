/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "capsend.h"
#include "monitor.h"
#include "magic.h"
#include "capops.h"
#include "capop_handlers.h"

/*
 * Single-cast {{{1
 */

errval_t
capsend_target(coreid_t dest, struct msg_queue_elem *queue_elem)
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

errval_t
capsend_owner(struct domcapref capref, struct msg_queue_elem *queue_elem)
{
    errval_t err;

    // read cap owner
    coreid_t owner;
    err = monitor_get_cap_owner(capref.croot, capref.cptr, capref.bits, &owner);
    if (err_is_fail(err)) {
        return err;
    }

    // enqueue to owner
    return capsend_target(owner, queue_elem);
}

/*
 * Multicast helpers {{{1
 */

struct capsend_mc_msg_st;
struct capsend_mc_st;

typedef errval_t (*capsend_mc_send_cont_t)(struct intermon_binding*, struct capsend_mc_st*);

struct capsend_mc_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    struct capsend_mc_st *mc_st;
    coreid_t dest;
};

static void
capsend_mc_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    struct capsend_mc_msg_st *msg_st = (struct capsend_mc_msg_st*)e;
    struct capsend_mc_st *mc_st = msg_st->mc_st;
    errval_t err = SYS_ERR_OK;

    // if do_send is false, an error occured in the multicast setup, so do not
    // send anything
    if (mc_st->do_send) {
        err = mc_st->send_fn(b, &mc_st->caprep, mc_st);
    }

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = capsend_target(msg_st->dest, (struct msg_queue_elem*)msg_st);
    }

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending dequeued capops message");
    }

    // decrement counter of number of queued messages
    if (!--mc_st->num_queued) {
        // if counter is zero, cleanup outgoing memory
        free(mc_st->msg_st_arr);
        mc_st->msg_st_arr = NULL;
        if (!mc_st->do_send || !mc_st->num_pending) {
            // if the send has been aborted, also cleanup cross-call state
            free(mc_st);
        }
    }
}

static errval_t
capsend_mc_enqueue(struct capsend_mc_st *mc_st, coreid_t dest)
{
    errval_t err;

    // get next msg_st
    struct capsend_mc_msg_st *msg_st = &mc_st->msg_st_arr[mc_st->num_queued];
    msg_st->queue_elem.cont = capsend_mc_send_cont;
    msg_st->mc_st = mc_st;
    msg_st->dest = dest;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_ok(err)) {
        // count successful enqueue
        mc_st->num_queued++;
        if (mc_st->num_pending >= 0) {
            // also track number of pending exchanges if requested
            mc_st->num_pending++;
        }
    }
    return err;
}

static errval_t
capsend_mc_init(struct capsend_mc_st *mc_st, struct capability *cap,
                capsend_send_fn send_fn,
                size_t num_dests, bool track_pending)
{
    mc_st->num_queued = 0;
    mc_st->num_pending = track_pending ? 0 : -1;
    mc_st->do_send = true;
    mc_st->send_fn = send_fn;
    if (cap) {
        capability_to_caprep(cap, &mc_st->caprep);
    }
    mc_st->msg_st_arr = calloc(num_dests, sizeof(*mc_st->msg_st_arr));
    if (!mc_st->msg_st_arr) {
        return LIB_ERR_MALLOC_FAIL;
    }
    return SYS_ERR_OK;
}

bool capsend_handle_mc_reply(genvaddr_t st)
{
    struct capsend_mc_st *mc_st = (struct capsend_mc_st*)st;

    if (!--mc_st->num_pending) {
        return true;
    }
    else {
        return false;
    }
}

/*
 * Broadcast helpers {{{2
 */

static errval_t
capsend_broadcast(struct capsend_mc_st *bc_st, struct capability *cap, capsend_send_fn send_cont)
{
    errval_t err;
    int dest_count = num_monitors;
    err = capsend_mc_init(bc_st, cap, send_cont, dest_count, true);
    if (err_is_fail(err)) {
        free(bc_st);
    }

    for (coreid_t dest = 0; dest < MAX_COREID && bc_st->num_queued < dest_count; dest++)
    {
        err = capsend_mc_enqueue(bc_st, dest);
        if (err_no(err) == MON_ERR_NO_MONITOR_FOR_CORE) {
            // no connection for this core, skip
            continue;
        }
        else if (err_is_fail(err)) {
            // failure, disable broadcast
            bc_st->do_send = false;
            if (!bc_st->num_queued) {
                // only cleanup of no messages have been enqueued
                free(bc_st->msg_st_arr);
                free(bc_st);
            }
            return err;
        }
    }

    if (!bc_st->num_pending) {
        return MON_ERR_NO_MONITOR_FOR_CORE;
    }

    return SYS_ERR_OK;
}

/*
 * Find relations {{{1
 */

/*
 * Find copies broadcast {{{2
 */

struct find_cap_broadcast_msg_st;

struct find_cap_broadcast_st {
    struct capsend_mc_st bc;
    capsend_find_cap_result_fn result_handler;
    bool found;
    void *st;
};

static errval_t
find_cap_broadcast_send_cont(struct intermon_binding *b, intermon_caprep_t *caprep, struct capsend_mc_st *st)
{
    return intermon_capops_find_cap__tx(b, NOP_CONT, *caprep, (genvaddr_t)st);
}

errval_t
capsend_find_cap(struct capability *cap, capsend_find_cap_result_fn result_handler, void *st)
{
    struct find_cap_broadcast_st *bc_st = calloc(1, sizeof(struct find_cap_broadcast_st));
    if (!bc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    bc_st->result_handler = result_handler;
    bc_st->found = false;
    bc_st->st = st;

    return capsend_broadcast((struct capsend_mc_st*)bc_st, cap, find_cap_broadcast_send_cont);
}

/*
 * Find copies result {{{2
 */

struct find_cap_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t result;
    genvaddr_t st;
};

static void
find_cap_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct find_cap_result_msg_st *msg_st = (struct find_cap_result_msg_st*)e;

    err = intermon_capops_find_cap_result__tx(b, NOP_CONT, msg_st->result, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send find_cap_result message");
    }
    free(msg_st);
}

static errval_t
find_cap_result(coreid_t dest, errval_t result, genvaddr_t st)
{
    errval_t err;
    struct find_cap_result_msg_st *msg_st = calloc(1, sizeof(struct find_cap_result_msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = find_cap_result_send_cont;
    msg_st->result = result;
    msg_st->st = st;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
    }

    return err;
}

/*
 * Find copies receive handlers {{{2
 */

void
find_cap__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    struct capability cap;
    caprep_to_capability(&caprep, &cap);
    struct capref capref;

    err = slot_alloc(&capref);
    if (err_is_fail(err)) {
        goto send_err;
    }

    err = monitor_copy_if_exists(&cap, capref);
    if (err_is_fail(err)) {
        goto free_slot;
    }

    err = cap_destroy(capref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to destroy temporary cap");
    }

free_slot:
    slot_free(capref);

send_err:
    err = find_cap_result(from, err, st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send find_cap result");
    }
}

void
find_cap_result__rx_handler(struct intermon_binding *b, errval_t result, genvaddr_t st)
{
    // if we receive a positive result, immediately forward to caller
    struct find_cap_broadcast_st *fc_bc_st = (struct find_cap_broadcast_st*)st;
    if (err_is_ok(result)) {
        if (!fc_bc_st->found) {
            fc_bc_st->found = true;
            struct intermon_state *inter_st = (struct intermon_state*)b->st;
            coreid_t from = inter_st->core_id;
            fc_bc_st->result_handler(SYS_ERR_OK, from, fc_bc_st->st);
        }
    }
    else if (err_no(result) != SYS_ERR_CAP_NOT_FOUND) {
        DEBUG_ERR(result, "ignoring bad find_cap_result");
    }

    // check to see if broadcast is complete
    if (capsend_handle_mc_reply(st)) {
        if (!fc_bc_st->found) {
            // broadcast did not find a core, report notfound to caller
            fc_bc_st->result_handler(SYS_ERR_CAP_NOT_FOUND, 0, fc_bc_st->st);
        }
        free(fc_bc_st);
    }
}

/*
 * Find descendants
 */

struct find_descendants_mc_st {
    struct capsend_mc_st mc_st;
    capsend_result_fn result_fn;
    void *st;
    bool have_result;
};

static errval_t
find_descendants_send_cont(struct intermon_binding *b, intermon_caprep_t *caprep, struct capsend_mc_st *mc_st)
{
    return intermon_capops_find_descendants__tx(b, NOP_CONT, *caprep, (genvaddr_t)mc_st);
}

errval_t
capsend_find_descendants(struct domcapref src, capsend_result_fn result_fn, void *st)
{
    errval_t err;

    struct capability cap;
    err = monitor_domains_cap_identify(src.croot, src.cptr, src.bits, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct find_descendants_mc_st *mc_st;
    mc_st = malloc(sizeof(*mc_st));
    if (!mc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }

    mc_st->result_fn = result_fn;
    mc_st->st = st;
    mc_st->have_result = false;
    return capsend_descendants(&cap, find_descendants_send_cont, (struct capsend_mc_st*)mc_st);
}


struct find_descendants_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    genvaddr_t st;
};

static void
find_descendants_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct find_descendants_result_msg_st *msg_st;
    msg_st = (struct find_descendants_result_msg_st*)e;
    err = intermon_capops_find_descendants_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);
    free(msg_st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not send find_descendants_result");
    }
}

void
find_descendants__rx_fn(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;

    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;

    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    bool has_descendants;
    err = monitor_has_descendants(&cap, &has_descendants);
    assert(err_is_ok(err));

    struct find_descendants_result_msg_st *msg_st;
    msg_st = malloc(sizeof(*msg_st));
    if (!msg_st) {
        err = LIB_ERR_MALLOC_FAIL;
        USER_PANIC_ERR(err, "could not alloc find_descendants_result_msg_st");
    }
    msg_st->queue_elem.cont = find_descendants_result_send_cont;
    msg_st->st = st;

    if (err_is_ok(err)) {
        err = has_descendants ? SYS_ERR_OK : SYS_ERR_CAP_NOT_FOUND;
    }
    msg_st->status = err;

    err = capsend_target(from, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not enqueue find_descendants_result msg");
    }
}

void
find_descendants_result__rx_fn(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    struct find_descendants_mc_st *mc_st = (struct find_descendants_mc_st*)st;

    if (err_is_ok(status)) {
        // found result
        if (!mc_st->have_result) {
            mc_st->have_result = true;
            mc_st->result_fn(SYS_ERR_OK, mc_st->st);
        }
    }
    else if (err_no(status) != SYS_ERR_CAP_NOT_FOUND) {
        DEBUG_ERR(status, "ignoring bad find_descendants result");
    }

    if (capsend_handle_mc_reply(st)) {
        if (!mc_st->have_result) {
            mc_st->result_fn(SYS_ERR_CAP_NOT_FOUND, mc_st->st);
        }
        free(mc_st);
    }
}


/*
 * Ownership update {{{1
 */

/*
 * Update owner broadcast {{{2
 */

struct update_owner_broadcast_st {
    struct capsend_mc_st bc;
    struct event_closure completion_continuation;
};

static errval_t
update_owner_broadcast_send_cont(struct intermon_binding *b, intermon_caprep_t *caprep, struct capsend_mc_st *bc_st)
{
    return intermon_capops_update_owner__tx(b, NOP_CONT, *caprep, (genvaddr_t)bc_st);
}

errval_t
capsend_update_owner(struct capref capref, struct event_closure completion_continuation)
{
    errval_t err;
    struct capability cap;
    err = monitor_cap_identify(capref, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct update_owner_broadcast_st *bc_st = calloc(1, sizeof(struct update_owner_broadcast_st));
    if (!bc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    bc_st->completion_continuation = completion_continuation;

    return capsend_broadcast((struct capsend_mc_st*)bc_st, &cap, update_owner_broadcast_send_cont);
}

/*
 * Owner updated response {{{2
 */

struct owner_updated_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    genvaddr_t st;
};

static void
owner_updated_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct owner_updated_msg_st *msg_st = (struct owner_updated_msg_st*)e;

    err = intermon_capops_owner_updated__tx(b, NOP_CONT, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send owner_updated message");
    }
    free(msg_st);
}

static errval_t
owner_updated(coreid_t owner, genvaddr_t st)
{
    errval_t err;
    struct owner_updated_msg_st *msg_st = calloc(1, sizeof(struct owner_updated_msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = owner_updated_send_cont;
    msg_st->st = st;

    err = capsend_target(owner, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
    }

    return err;
}

/*
 * Receive handlers {{{2
 */

void
owner_updated__rx_handler(struct intermon_binding *b, genvaddr_t st)
{
    if (!capsend_handle_mc_reply(st)) {
        // broadcast is not complete
        return;
    }
    struct update_owner_broadcast_st *uo_bc_st = (struct update_owner_broadcast_st*)st;
    struct event_closure *cl = &uo_bc_st->completion_continuation;
    cl->handler(cl->arg);
    free(uo_bc_st);
}

void
update_owner__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    struct capref capref;
    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    err = slot_alloc(&capref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to allocate slot for owner update");
    }

    err = monitor_copy_if_exists(&cap, capref);
    if (err_is_ok(err)) {
        err = monitor_set_cap_owner(cap_root, get_cap_addr(capref),
                                    get_cap_valid_bits(capref), from);
    }
    if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
        err = SYS_ERR_OK;
    }

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to update cap ownership");
    }

    cap_destroy(capref);
    slot_free(capref);

    err = owner_updated(from, st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send ownership update response");
    }
}

/*
 * Send to all relations of cap {{{1
 */

errval_t
capsend_copies(struct capability *cap,
            capsend_send_fn send_fn,
            struct capsend_mc_st *mc_st)
{
    // this is currently just a broadcast
    return capsend_broadcast(mc_st, cap, send_fn);
}

errval_t
capsend_descendants(struct capability *cap,
            capsend_send_fn send_fn,
            struct capsend_mc_st *mc_st)
{
    // this is currently just a broadcast
    return capsend_broadcast(mc_st, cap, send_fn);
}
