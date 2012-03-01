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
#include "transport.h"
#include "monitor.h"
#include "magic.h"

/*
 * Messaging
 */

errval_t
enqueue_send_target(coreid_t dest, struct msg_queue_elem *queue_elem)
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
enqueue_send_owner(struct capref capref, struct msg_queue_elem *queue_elem)
{
    errval_t err;

    // read cap owner
    coreid_t owner;
    err = cap_get_owner(capref, &owner);
    if (err_is_fail(err)) {
        return err;
    }

    // enqueue to owner
    return enqueue_send_target(owner, queue_elem);
}

/*
 * Broadcast helpers {{{1
 */

struct broadcast_msg_st;
struct broadcast_st;

typedef errval_t (*broadcast_send_cont_t)(struct intermon_binding*, struct broadcast_st*);

struct broadcast_st {
    struct broadcast_msg_st *msg_st_arr;
    int num_pending;
    int num_queued;
    bool do_send;
    broadcast_send_cont_t send_cont;
};

struct broadcast_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    struct broadcast_st *bc_st;
};

static void
broadcast_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct broadcast_msg_st *msg_st = (struct broadcast_msg_st*)e;
    struct broadcast_st *bc_st = msg_st->bc_st;

    // if do_send is false, an error occured in the broadcast setup, so do not
    // send anything
    if (bc_st->do_send) {
        err = bc_st->send_cont(b, bc_st);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to send find core message");
        }
    }

    // decrement counter of number of queued broadcast messages
    if (!--bc_st->num_queued) {
        // if counter is zero, cleanup outgoing memory
        free(bc_st->msg_st_arr);
        bc_st->msg_st_arr = NULL;
        if (!bc_st->do_send) {
            // if the send has been aborted, also cleanup cross-call state
            free(bc_st);
        }
    }
}

static struct broadcast_st*
broadcast_result_rx(genvaddr_t st)
{
    struct broadcast_st *bc_st = (struct broadcast_st*)st;

    if (!--bc_st->num_pending) {
        return bc_st;
    }
    else {
        return NULL;
    }
}

static errval_t
broadcast(struct broadcast_st *bc_st, broadcast_send_cont_t send_cont)
{
    errval_t err;
    bc_st->num_pending = bc_st->num_queued = 0;
    bc_st->do_send = false;
    bc_st->send_cont = send_cont;
    int num_current_monitors = num_monitors;
    bc_st->msg_st_arr = calloc(num_current_monitors, sizeof(*bc_st->msg_st_arr));
    if (!bc_st->msg_st_arr) {
        free(bc_st);
        return LIB_ERR_MALLOC_FAIL;
    }

    for (coreid_t dest = 0; dest < MAX_COREID && bc_st->num_queued < num_current_monitors; dest++)
    {
        // get next msg_st
        struct broadcast_msg_st *msg_st = &bc_st->msg_st_arr[bc_st->num_queued];
        msg_st->queue_elem.cont = broadcast_send_cont;
        msg_st->bc_st = bc_st;

        err = enqueue_send_target(dest, (struct msg_queue_elem*)msg_st);
        if (err_no(err) == MON_ERR_NO_MONITOR_FOR_CORE) {
            // no connection for this core, skip
            continue;
        }
        else if (err_is_fail(err)) {
            // failure, disable broadcast (cleanup is done when all msg_sts have been dequeued)
            return err;
        }
        else {
            // count the number of queued messages and number of pending responses for the broadcast
            bc_st->num_queued++;
            bc_st->num_pending++;
        }
    }
    bc_st->do_send = true;

    return SYS_ERR_OK;
}

/*
 * Find relations {{{1
 */

/*
 * Find relations broadcast {{{2
 */

struct find_core_broadcast_msg_st;

struct find_core_broadcast_st {
    struct broadcast_st bc;
    intermon_caprep_t caprep;
    find_core_result_handler_t result_handler;
    bool found;
    void *st;
};

static errval_t
find_core_broadcast_send_cont(struct intermon_binding *b, struct broadcast_st *bc_st)
{
    struct find_core_broadcast_st *fc_bc_st = (struct find_core_broadcast_st*)bc_st;
    return intermon_find_core__tx(b, NOP_CONT, fc_bc_st->caprep, (genvaddr_t)fc_bc_st);
}

errval_t
find_core_with_cap(struct capability *cap, find_core_result_handler_t result_handler, void *st)
{
    struct find_core_broadcast_st *bc_st = calloc(1, sizeof(struct find_core_broadcast_st));
    if (!bc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    capability_to_caprep(cap, &bc_st->caprep);
    bc_st->result_handler = result_handler;
    bc_st->found = false;
    bc_st->st = st;

    return broadcast((struct broadcast_st*)bc_st, find_core_broadcast_send_cont);
}

/*
 * Find relations result {{{1
 */

struct find_core_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t result;
    genvaddr_t st;
};

static void
find_core_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct find_core_result_msg_st *msg_st = (struct find_core_result_msg_st*)e;

    err = intermon_find_core_result__tx(b, NOP_CONT, msg_st->result, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send find_core_result message");
    }
    free(msg_st);
}

static errval_t
find_core_result(coreid_t dest, errval_t result, genvaddr_t st)
{
    errval_t err;
    struct find_core_result_msg_st *msg_st = calloc(1, sizeof(struct find_core_result_msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = find_core_result_send_cont;
    msg_st->result = result;
    msg_st->st = st;

    err = enqueue_send_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
    }
}

/*
 * Receive handlers {{{2
 */

__attribute__((unused))
static void
find_core__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err, result;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    struct capability cap;
    caprep_to_capability(&caprep, &cap);
    struct capref capref;
    result = copy_if_exists(&cap, &capref);
    if (err_is_ok(result)) {
        err = cap_destroy(capref);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to destroy temporary cap");
        }
    }

    err = find_core_result(from, result, st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send find_core result");
    }
}

__attribute__((unused))
static void
find_core_result__rx_handler(struct intermon_binding *b, errval_t result, genvaddr_t st)
{
    // if we receive a positive result, immediately forward to caller
    struct find_core_broadcast_st *fc_bc_st = (struct find_core_broadcast_st*)st;
    if (err_is_ok(result)) {
        fc_bc_st->found = true;
        struct intermon_state *inter_st = (struct intermon_state*)b->st;
        coreid_t from = inter_st->core_id;
        fc_bc_st->result_handler(SYS_ERR_OK, from, fc_bc_st->st);
    }
    else if (err_no(result) != CAP_ERR_NOTFOUND) {
        printf("ignoring bad find_core_result %"PRIuPTR"\n", result);
    }

    // check to see if broadcast is complete
    struct broadcast_st *bc_st = broadcast_result_rx(st);
    if (bc_st) {
        if (!fc_bc_st->found) {
            // broadcast did not find a core, report notfound to caller
            fc_bc_st->result_handler(CAP_ERR_NOTFOUND, 0, fc_bc_st->st);
        }
        free(fc_bc_st);
    }
}

/*
 * Ownership update {{{1
 */

/*
 * Update owner broadcast {{{2
 */

struct update_owner_broadcast_msg_st;

struct update_owner_broadcast_st {
    struct broadcast_st bc;
    intermon_caprep_t caprep;
    struct event_closure completion_continuation;
};

static errval_t
update_owner_broadcast_send_cont(struct intermon_binding *b, struct broadcast_st *bc_st)
{
    struct update_owner_broadcast_st *uo_bc_st = (struct update_owner_broadcast_st*)bc_st;
    return intermon_update_owner__tx(b, NOP_CONT, uo_bc_st->caprep, (genvaddr_t)uo_bc_st);
}

errval_t
update_owner(struct capref capref, struct event_closure completion_continuation)
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
    capability_to_caprep(&cap, &bc_st->caprep);
    bc_st->completion_continuation = completion_continuation;

    return broadcast((struct broadcast_st*)bc_st, update_owner_broadcast_send_cont);
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

    err = intermon_owner_updated__tx(b, NOP_CONT, msg_st->st);
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

    err = enqueue_send_target(owner, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
    }
}

/*
 * Receive handlers {{{2
 */

__attribute__((unused))
static void
owner_updated__rx_handler(struct intermon_binding *b, genvaddr_t st)
{
    struct broadcast_st *bc_st = broadcast_result_rx(st);
    if (!bc_st) {
        // broadcast is not complete
        return;
    }

    struct update_owner_broadcast_st *uo_bc_st = (struct update_owner_broadcast_st*)bc_st;
    struct event_closure *cl = &uo_bc_st->completion_continuation;
    cl->handler(cl->arg);
    free(uo_bc_st);
}

__attribute__((unused))
static void
update_owner__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    struct capref capref;
    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    err = copy_if_exists(&cap, &capref);
    if (err_is_ok(err)) {
        err = cap_set_owner(capref, from);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to set update cap ownership");
        }
    }
    else if (err_no(err) != CAP_ERR_NOTFOUND) {
        USER_PANIC_ERR(err, "failed to lookup cap for ownership change");
    }
    cap_destroy(capref);

    err = owner_updated(from, st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send ownership update response");
    }
}

