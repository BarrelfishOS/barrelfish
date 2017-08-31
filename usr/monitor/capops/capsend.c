/*
 * Copyright (c) 2012, 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include "capsend.h"
#include "monitor.h"
#include "capops.h"
#include "internal.h"

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
    DEBUG_CAPOPS("capsend_target: ->%d (%p)\n", dest, queue_elem);
    struct intermon_state *inter_st = (struct intermon_state*)dest_b->st;
    if (!inter_st->capops_ready) {
        // XXX: custom error value
        return MON_ERR_CAPOPS_BUSY;
    }

    // enqueue message
    return intermon_enqueue_send(dest_b, &inter_st->queue, dest_b->waitset, queue_elem);
}

errval_t
capsend_owner(struct domcapref capref, struct msg_queue_elem *queue_elem)
{
    errval_t err;

    // read cap owner
    coreid_t owner;
    err = monitor_get_domcap_owner(capref, &owner);
    if (err_is_fail(err)) {
        return err;
    }

    // enqueue to owner
    return capsend_target(owner, queue_elem);
}

/*
 * Multicast helpers {{{2
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

bool capsend_handle_mc_reply(struct capsend_mc_st *st)
{
    // return true iff st->num_pending == 0 after acking one more reply
    return --st->num_pending == 0;
}

/*
 * Broadcast helpers {{{2
 */

static errval_t
capsend_broadcast(struct capsend_mc_st *bc_st, struct capsend_destset *dests,
        struct capability *cap, capsend_send_fn send_cont)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    size_t dest_count;
    bool init_destset = false;
    size_t online_monitors = num_monitors_ready_for_capops();
    // do not count self when calculating #dest cores
    dest_count = online_monitors - 1;
    DEBUG_CAPOPS("%s: dest_count = %zu\n", __FUNCTION__, dest_count);
    DEBUG_CAPOPS("%s: num_queued = %d\n", __FUNCTION__, bc_st->num_queued);
    DEBUG_CAPOPS("%s: num_pending = %d\n", __FUNCTION__, bc_st->num_pending);
    if (dests && dests->set == NULL) {
        dests->set = calloc(dest_count, sizeof(coreid_t));
        dests->capacity = dest_count;
        dests->count = 0;
        init_destset = true;
    } else if (dests) {
        dest_count = dests->count;
    }
    err = capsend_mc_init(bc_st, cap, send_cont, dest_count, true);
    if (err_is_fail(err)) {
        free(bc_st);
    }

    if (init_destset || !dests) {
        for (coreid_t dest = 0; dest < MAX_COREID && bc_st->num_queued < dest_count; dest++)
        {
            if (dest == my_core_id) {
                // do not send to self
                continue;
            }
            err = capsend_mc_enqueue(bc_st, dest);
            if (err_is_ok(err) && dests) {
                // if we're initializing destination set, add destination
                // cores that we were able to enqueue msg for to set.
                dests->set[dests->count++] = dest;
            }
            if (err_no(err) == MON_ERR_NO_MONITOR_FOR_CORE) {
                // no connection for this core, skip
                continue;
            } else if (err_no(err) == MON_ERR_CAPOPS_BUSY) {
                debug_printf("monitor.%d not ready to participate in distops, skipping\n",
                        dest);
            } else if (err_is_fail(err)) {
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
    } else {
        for (int i = 0; i < dest_count; i++) {
            coreid_t dest = dests->set[i];

            err = capsend_mc_enqueue(bc_st, dest);
            if (err_no(err) == MON_ERR_NO_MONITOR_FOR_CORE) {
                // no connection for this core, skip
                continue;
            } else if (err_no(err) == MON_ERR_CAPOPS_BUSY) {
                debug_printf("monitor.%d not ready to participate in distops, skipping\n",
                        dest);
            } else if (err_is_fail(err)) {
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
    }

    if (!bc_st->num_pending && dest_count > 1) {
        // XXX: needs sane error -SG
        return MON_ERR_NO_MONITOR_FOR_CORE;
    }

    return SYS_ERR_OK;
}

/*
 * Find relations {{{1
 */

/*
 * Find copies {{{2
 */

/*
 * Find copies broadcast {{{3
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
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    return intermon_capops_find_cap__tx(b, NOP_CONT, *caprep, (uintptr_t)st);
}

errval_t
capsend_find_cap(struct capability *cap, capsend_find_cap_result_fn result_handler, void *st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    struct find_cap_broadcast_st *bc_st = calloc(1, sizeof(struct find_cap_broadcast_st));
    if (!bc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    bc_st->result_handler = result_handler;
    bc_st->found = false;
    bc_st->st = st;

    return capsend_broadcast((struct capsend_mc_st*)bc_st, NULL, cap, find_cap_broadcast_send_cont);
}

/*
 * Find copies result {{{3
 */

struct find_cap_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t result;
    genvaddr_t st;
};

static void
find_cap_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    struct find_cap_result_msg_st *msg_st = (struct find_cap_result_msg_st*)e;

    err = intermon_capops_find_cap_result__tx(b, NOP_CONT, msg_st->result, msg_st->st);

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        DEBUG_CAPOPS("%s: got FLOUNDER_ERR_TX_BUSY; requeueing msg.\n", __FUNCTION__);
        struct intermon_state *inter_st = (struct intermon_state *)b->st;
        // requeue send request at front and return
        err = intermon_enqueue_send_at_front(b, &inter_st->queue, b->waitset,
                                             (struct msg_queue_elem *)e);
        GOTO_IF_ERR(err, handle_err);
        return;
    }

handle_err:
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send find_cap_result message");
    }
    free(msg_st);
}

static errval_t
find_cap_result(coreid_t dest, errval_t result, genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
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
 * Find copies receive handlers {{{3
 */

void
find_cap__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err, cleanup_err;
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

    cleanup_err = cap_delete(capref);
    if (err_is_fail(cleanup_err)) {
        USER_PANIC_ERR(err, "failed to delete temporary cap");
    }

free_slot:
    cleanup_err = slot_free(capref);
    if (err_is_fail(cleanup_err)) {
        USER_PANIC_ERR(err, "failed to free slot for temporary cap");
    }

send_err:
    cleanup_err = find_cap_result(from, err, st);
    if (err_is_fail(cleanup_err)) {
        USER_PANIC_ERR(err, "failed to send find_cap result");
    }
}

void
find_cap_result__rx_handler(struct intermon_binding *b, errval_t result, genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    // if we receive a positive result, immediately forward to caller
    lvaddr_t lst = (lvaddr_t)st;
    struct find_cap_broadcast_st *fc_bc_st = (struct find_cap_broadcast_st*)lst;
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
    if (capsend_handle_mc_reply(&fc_bc_st->bc)) {
        if (!fc_bc_st->found) {
            // broadcast did not find a core, report notfound to caller
            fc_bc_st->result_handler(SYS_ERR_CAP_NOT_FOUND, 0, fc_bc_st->st);
        }
        free(fc_bc_st);
    }
}

/*
 * Find descendants {{{2
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
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    lvaddr_t lst = (lvaddr_t)mc_st;
    return intermon_capops_find_descendants__tx(b, NOP_CONT, *caprep, (genvaddr_t)lst);
}

errval_t
capsend_find_descendants(struct domcapref src, capsend_result_fn result_fn, void *st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;

    struct capability cap;
    err = monitor_domains_cap_identify(src.croot, src.cptr, src.level, &cap);
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
    DEBUG_CAPOPS("%s: broadcasting find_descendants\n", __FUNCTION__);
    return capsend_relations(&cap, find_descendants_send_cont,
            (struct capsend_mc_st*)mc_st, NULL);
}


struct find_descendants_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    genvaddr_t st;
};

static void
find_descendants_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    struct find_descendants_result_msg_st *msg_st;
    msg_st = (struct find_descendants_result_msg_st*)e;
    err = intermon_capops_find_descendants_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        DEBUG_CAPOPS("%s: got FLOUNDER_ERR_TX_BUSY; requeueing msg.\n", __FUNCTION__);
        struct intermon_state *inter_st = (struct intermon_state *)b->st;
        // requeue send request at front and return
        err = intermon_enqueue_send_at_front(b, &inter_st->queue, b->waitset,
                                             (struct msg_queue_elem *)e);
        GOTO_IF_ERR(err, handle_err);
        return;
    }

handle_err:
    free(msg_st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not send find_descendants_result");
    }
}

void
find_descendants__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
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
find_descendants_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    lvaddr_t lst = (lvaddr_t) st;
    struct find_descendants_mc_st *mc_st = (struct find_descendants_mc_st*)lst;

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

    if (capsend_handle_mc_reply(&mc_st->mc_st)) {
        if (!mc_st->have_result) {
            mc_st->result_fn(SYS_ERR_CAP_NOT_FOUND, mc_st->st);
        }
        free(mc_st);
    }
}


/*
 * Check retypeability {{{1
 */

struct check_retypeable_mc_st {
    struct capsend_mc_st mc_st;
    capsend_result_fn result_fn;
    void *st;
    // msg args
    gensize_t offset;
    gensize_t objsize;
    size_t count;
    bool have_result;
};

static errval_t
check_retypeable_send_cont(struct intermon_binding *b, intermon_caprep_t *caprep, struct capsend_mc_st *mc_st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    lvaddr_t lst = (lvaddr_t)mc_st;
    struct check_retypeable_mc_st *rst = (struct check_retypeable_mc_st *)mc_st;
    return intermon_capops_check_retypeable__tx(b, NOP_CONT, *caprep,
                (genvaddr_t)lst, rst->offset, rst->objsize, rst->count);
}

errval_t
capsend_check_retypeable(struct domcapref src, gensize_t offset, gensize_t objsize,
                         size_t count, capsend_result_fn result_fn, void *st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;

    struct capability cap;
    err = monitor_domains_cap_identify(src.croot, src.cptr, src.level, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct check_retypeable_mc_st *mc_st;
    mc_st = malloc(sizeof(*mc_st));
    if (!mc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }

    // Setup multicast state
    mc_st->result_fn   = result_fn;
    mc_st->st          = st;
    mc_st->offset      = offset;
    mc_st->objsize     = objsize;
    mc_st->count       = count;
    mc_st->have_result = false;

    DEBUG_CAPOPS("%s: broadcasting check_retypeable\n", __FUNCTION__);
    return capsend_relations(&cap, check_retypeable_send_cont,
            (struct capsend_mc_st*)mc_st, NULL);
}


struct check_retypeable_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    genvaddr_t st;
};

static void
check_retypeable_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    struct check_retypeable_result_msg_st *msg_st;
    msg_st = (struct check_retypeable_result_msg_st*)e;
    err = intermon_capops_check_retypeable_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        DEBUG_CAPOPS("%s: got FLOUNDER_ERR_TX_BUSY; requeueing msg.\n", __FUNCTION__);
        struct intermon_state *inter_st = (struct intermon_state *)b->st;
        // requeue send request at front and return
        err = intermon_enqueue_send_at_front(b, &inter_st->queue, b->waitset,
                                             (struct msg_queue_elem *)e);
        GOTO_IF_ERR(err, handle_err);
        return;
    }

handle_err:
    free(msg_st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not send check_retypeable_result");
    }
}

void
check_retypeable__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep,
                             genvaddr_t st, uint64_t offset, uint64_t objsize,
                             uint64_t count)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;

    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;

    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    err = monitor_is_retypeable(&cap, offset, objsize, count);

    DEBUG_CAPOPS("%s: got %s from kernel\n", __FUNCTION__, err_getcode(err));

    struct check_retypeable_result_msg_st *msg_st;
    msg_st = malloc(sizeof(*msg_st));
    if (!msg_st) {
        err = LIB_ERR_MALLOC_FAIL;
        USER_PANIC_ERR(err, "could not alloc check_retypeable_result_msg_st");
    }
    msg_st->queue_elem.cont = check_retypeable_result_send_cont;
    msg_st->st = st;
    msg_st->status = err;

    err = capsend_target(from, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not enqueue check_retypeable_result msg");
    }
}

void
check_retypeable_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    DEBUG_CAPOPS("%s: got %s from %d\n", __FUNCTION__, err_getcode(status),
                 ((struct intermon_state *) b->st)->core_id);
    lvaddr_t lst = (lvaddr_t) st;
    struct check_retypeable_mc_st *mc_st = (struct check_retypeable_mc_st*)lst;

    // Short-circuit when we get SYS_ERR_REVOKE_FIRST
    if (err_no(status) == SYS_ERR_REVOKE_FIRST) {
        if (!mc_st->have_result) {
            DEBUG_CAPOPS("%s: short-circuit with status=%s\n", __FUNCTION__,
                    err_getcode(status));
            mc_st->have_result = true;
            mc_st->result_fn(status, mc_st->st);
        }
    }

    if (capsend_handle_mc_reply(&mc_st->mc_st)) {
        // If we haven't called the callback yet, call it now with the last
        // status value. Calling code needs to figure out what
        // SYS_ERR_CAP_NOT_FOUND means.
        if (!mc_st->have_result) {
            DEBUG_CAPOPS("%s: notifying caller with final status=%s\n", __FUNCTION__,
                    err_getcode(status));
            mc_st->result_fn(status, mc_st->st);
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
    lvaddr_t lst = (lvaddr_t)bc_st;
    return intermon_capops_update_owner__tx(b, NOP_CONT, *caprep, (genvaddr_t)lst);
}

errval_t
capsend_update_owner(struct domcapref capref, struct event_closure completion_continuation)
{
    errval_t err;
    struct capability cap;
    err = monitor_domains_cap_identify(capref.croot, capref.cptr, capref.level,
                                       &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct update_owner_broadcast_st *bc_st = calloc(1, sizeof(struct update_owner_broadcast_st));
    if (!bc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    bc_st->completion_continuation = completion_continuation;

    return capsend_broadcast((struct capsend_mc_st*)bc_st, NULL, &cap, update_owner_broadcast_send_cont);
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

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        DEBUG_CAPOPS("%s: got FLOUNDER_ERR_TX_BUSY; requeueing msg.\n", __FUNCTION__);
        struct intermon_state *inter_st = (struct intermon_state *)b->st;
        // requeue send request at front and return
        err = intermon_enqueue_send_at_front(b, &inter_st->queue, b->waitset,
                                             (struct msg_queue_elem *)e);
        GOTO_IF_ERR(err, handle_err);
        return;
    }

handle_err:
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
    lvaddr_t lst = (lvaddr_t)st;
    struct update_owner_broadcast_st *uo_bc_st = (struct update_owner_broadcast_st*)lst;
    if (!capsend_handle_mc_reply(&uo_bc_st->bc)) {
        // broadcast is not complete
        return;
    }
    struct event_closure *cl = &uo_bc_st->completion_continuation;
    cl->handler(cl->arg);
    free(uo_bc_st);
}

struct delayed_cleanup_st {
    struct deferred_event d;
    struct event_closure ev;
    struct capref capref;
    delayus_t delay;
};

static void defer_free_owner_rx_cap(struct delayed_cleanup_st *st)
{
    errval_t err;
    deferred_event_init(&st->d);
    err = deferred_event_register(&st->d, get_default_waitset(), st->delay, st->ev);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "unable to register deferred event, leaking cap");
        free(st);
    }
}

static void free_owner_rx_cap(void *st_)
{
    errval_t err;
    struct delayed_cleanup_st *st = st_;
    err = cap_destroy(st->capref);
    if (err_no(err) == SYS_ERR_CAP_LOCKED) {
        // exponential backoff
        st->delay *= 2;
        defer_free_owner_rx_cap(st);
        return;
    }
    PANIC_IF_ERR(err, "cap cleanup after update_owner_rx");
    free(st);
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
                                    get_cap_level(capref), from);
    }
    if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
        err = SYS_ERR_OK;
        slot_free(capref);
        goto reply;
    }

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to update cap ownership");
    }

    err = cap_destroy(capref);
    if (err_no(err) == SYS_ERR_CAP_LOCKED) {
        // ownership updates still in flight, delete cap later
        struct delayed_cleanup_st *dst = malloc(sizeof(*dst));
        assert(dst);
        dst->capref = capref;
        dst->ev = MKCLOSURE(free_owner_rx_cap, dst);
        dst->delay = 1000; // 1ms delay
        defer_free_owner_rx_cap(dst);
    }

reply:
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
    DEBUG_CAPOPS("%s: doing broadcast\n", __FUNCTION__);
    // this is currently just a broadcast
    return capsend_broadcast(mc_st, NULL, cap, send_fn);
}

errval_t
capsend_relations(struct capability *cap,
                  capsend_send_fn send_fn,
                  struct capsend_mc_st *mc_st,
                  struct capsend_destset *dests)
{
    DEBUG_CAPOPS("%s: doing broadcast\n", __FUNCTION__);
    // this is currently just a broadcast
    return capsend_broadcast(mc_st, dests, cap, send_fn);
}
