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
#include "ops.h"
#include "capsend.h"
#include "magic.h"
#include "caplock.h"

/*
 * RPC State {{{1
 */

struct cap_move_rpc_st {
    // capref to a copy of the cap in question
    struct capref capref;
    // caller st
    void *st;
    // reuslt handler
    move_result_handler_t result_handler;
};

/*
 * Move request
 */

struct move_request_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    intermon_caprep_t caprep;
    struct cap_move_rpc_st *st;
};

static void
move_request_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct move_request_msg_st *msg_st = (struct move_request_msg_st*)e;
    err = intermon_capops_move_request__tx(b, NOP_CONT, msg_st->caprep, (genvaddr_t)msg_st->st);
    if (err_is_fail(err)) {
        struct cap_move_rpc_st *rpc_st = (struct cap_move_rpc_st*)msg_st->st;
        if (rpc_st->result_handler) {
            rpc_st->result_handler(err, rpc_st->st);
        }
        free(rpc_st);
    }
    free(msg_st);
}

static errval_t
move_request(struct capref capref, coreid_t dest, move_result_handler_t result_handler, void *st)
{
    errval_t err;
    struct capability cap;
    err = monitor_cap_identify(capref, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct cap_move_rpc_st *rpc_st = malloc(sizeof(struct cap_move_rpc_st));
    if (!rpc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    rpc_st->capref = capref;
    rpc_st->st = st;
    rpc_st->result_handler = result_handler;

    struct move_request_msg_st *msg_st = malloc(sizeof(struct move_request_msg_st));
    if (!msg_st) {
        free(rpc_st);
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = move_request_send_cont;
    capability_to_caprep(&cap, &msg_st->caprep);
    msg_st->st = rpc_st;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        free(rpc_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Move result
 */

struct move_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    genvaddr_t st;
};

static void
move_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct move_result_msg_st *msg_st = (struct move_result_msg_st*)e;
    err = intermon_capops_move_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send move_result");
    }
    free(msg_st);
}

static errval_t
move_result(coreid_t dest, errval_t status, genvaddr_t st)
{
    errval_t err;

    struct move_result_msg_st *msg_st = calloc(1, sizeof(struct move_result_msg_st));
    msg_st->queue_elem.cont = move_result_send_cont;
    msg_st->status = status;
    msg_st->st = st;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Receive handlers {{{1
 */

static void
free_owner_recv_cap(void *arg)
{
    struct capref *cap = (struct capref*)arg;
    cap_destroy(*cap);
    free(cap);
}

__attribute__((unused))
static void
move_request__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err, send_err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct capref *capref;
    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    capref = calloc(1, sizeof(*capref));
    if (!capref) {
        err = LIB_ERR_MALLOC_FAIL;
        goto send_err;
    }

    err = copy_if_exists(&cap, capref);
    if (err_is_fail(err)) {
        goto send_err;
    }

    err = monitor_lock_cap(*capref);
    if (err_is_fail(err)) {
        cap_destroy(*capref);
        goto send_err;
    }

    err = monitor_set_cap_owner(*capref, my_core_id);
    if (err_is_fail(err)) {
        cap_destroy(*capref);
        goto send_err;
    }

    err = capsend_update_owner(*capref, MKCONT(free_owner_recv_cap, capref));
    if (err_is_fail(err)) {
        cap_destroy(*capref);
        goto send_err;
    }

    err = SYS_ERR_OK;

send_err:
    send_err = move_result(from, err, st);
    if (err_is_fail(send_err)) {
        USER_PANIC_ERR(send_err, "failed to send error to request_copy sender");
    }

    if (err_is_fail(err)) {
        free(capref);
    }
}

__attribute__((unused))
static void
move_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct cap_move_rpc_st *rpc_st = (struct cap_move_rpc_st*)st;

    caplock_unlock(rpc_st->capref);
    rpc_st->result_handler(status, rpc_st->st);
    free(rpc_st);
}

/*
 * Move operation {{{1
 */

errval_t
move(struct capref capref, coreid_t dest, move_result_handler_t result_handler, void *st)
{
    errval_t err;
    distcap_state_t state;

    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        return err;
    }
    if (distcap_is_busy(state)) {
        return MON_ERR_REMOTE_CAP_RETRY;
    }
    if (distcap_is_foreign(state)) {
        return MON_ERR_CAP_FOREIGN;
    }

    if (dest == my_core_id) {
        // tried to move to self and already owner, no-op
        if (result_handler) {
            result_handler(SYS_ERR_OK, st);
        }
        return SYS_ERR_OK;
    }

    err = monitor_lock_cap(capref);
    if (err_is_fail(err)) {
        return err;
    }

    err = move_request(capref, dest, result_handler, st);
    if (err_is_fail(err)) {
        caplock_unlock(capref);
        return err;
    }

    return SYS_ERR_OK;
}
