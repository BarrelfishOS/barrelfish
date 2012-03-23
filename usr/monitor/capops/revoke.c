/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "monitor.h"
#include "ops.h"
#include "capsend.h"
#include "magic.h"
#include "caplock.h"

struct revoke_st {
    struct domcapref revokecap;
    struct capref delcap;
    struct event_queue_node lock_queue_node;
    revoke_result_handler_t result_handler;
    void *st;
};

static errval_t revoke_local(struct revoke_st *rst);

/*
 * Request revoke from owner {{{1
 */

/*
 * Handle completed revoke request {{{2
 */

static void
request_revoke_move_result(errval_t status, void *st)
{
    errval_t err;
    struct revoke_st *rst = (struct revoke_st*)st;

    if (err_is_ok(status)) {
        // move succeeded, start local revoke
        err = revoke_local(rst);
    }
    else {
        err = status;
    }

    if (err_is_fail(err)) {
        rst->result_handler(err, rst->st);
        free(rst);
    }
}

__attribute__((unused))
static void
revoke_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    request_revoke_move_result(status, (void*)st);
}

/*
 * Handle and reply to revoke request {{{1
 */

struct revoke_request_st {
    struct capref capref;
    coreid_t from;
    genvaddr_t st;
};

/*
 * Revoke request result
 */

struct revoke_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    genvaddr_t st;
};

static void
revoke_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct revoke_result_msg_st *msg_st = (struct revoke_result_msg_st*)e;
    err = intermon_capops_revoke_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send revoke_result message");
    }
    free(msg_st);
}

static errval_t
revoke_result(coreid_t dest, errval_t status, genvaddr_t st)
{
    errval_t err;
    struct revoke_result_msg_st *msg_st;
    msg_st = malloc(sizeof(*msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = revoke_result_send_cont;
    msg_st->status = status;
    msg_st->st = st;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
    }
    return err;
}

/*
 * Move result handler {{{2
 */

static void
request_revoke_move_cont(errval_t status, void *st)
{
    errval_t err;
    struct revoke_request_st *rst = (struct revoke_request_st*)st;
    err = revoke_result(rst->from, status, rst->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not send revoke request result");
    }
    free(st);
}

/*
 * Revoke request receive handler {{{2
 */

__attribute__((unused))
static void
request_revoke__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err, send_err;
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

    distcap_state_t state;
    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        goto destroy_cap;
    }

    if (distcap_state_is_foreign(state)) {
        err = MON_ERR_CAP_FOREIGN;
        goto destroy_cap;
    }

    if (distcap_state_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto destroy_cap;
    }

    struct revoke_request_st *rst;
    rst = malloc(sizeof(*rst));
    if (!rst) {
        err = LIB_ERR_MALLOC_FAIL;
        goto destroy_cap;
    }
    rst->capref = capref;
    rst->from = from;
    rst->st = st;

    err = move(get_cap_domref(capref), from, request_revoke_move_cont, rst);
    if (err_is_fail(err)) {
        goto free_st;
    }

    return;

free_st:
    free(rst);

destroy_cap:
    cap_destroy(capref);

free_slot:
    slot_free(capref);

send_err:
    send_err = revoke_result(from, err, st);
    if (err_is_fail(send_err)) {
        USER_PANIC_ERR(send_err, "could not send revoke error");
    }
}

/*
 * Revoke request {{{2
 */

struct request_revoke_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    intermon_caprep_t caprep;
    struct revoke_st *st;
};

static void
request_revoke_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    struct request_revoke_msg_st *msg_st = (struct request_revoke_msg_st*)e;
    errval_t err;
    err = intermon_capops_request_revoke__tx(b, NOP_CONT, msg_st->caprep, (genvaddr_t)msg_st->st);
    if (err_is_fail(err)) {
        struct revoke_st *rst = msg_st->st;
        rst->result_handler(err, rst->st);
        free(rst);
    }
    free(msg_st);
}

static errval_t
request_revoke(struct revoke_st *st)
{
    errval_t err;
    struct capability cap;
    err = monitor_domains_cap_identify(st->revokecap.croot, st->revokecap.cptr,
                                       st->revokecap.bits, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct request_revoke_msg_st *msg_st = malloc(sizeof(struct request_revoke_msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = request_revoke_send_cont;
    capability_to_caprep(&cap, &msg_st->caprep);
    msg_st->st = st;

    err = capsend_owner(st->revokecap, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        return err;
    }

    return SYS_ERR_OK;
}


/*
 * Local revocation handling {{{1
 */

static void
revoke_delete_result(errval_t status, void *st)
{
    errval_t err;
    struct revoke_st *rst = (struct revoke_st*)st;

    if (err_is_fail(status)) {
        rst->result_handler(status, rst->st);
        free(rst);
    }

    err = revoke_local(rst);
    if (err_is_fail(err)) {
        rst->result_handler(err, rst->st);
    }
}

static void
revoke_unlock_cont(void *arg)
{
    struct revoke_st *rst = (struct revoke_st*)arg;
    errval_t err = revoke_local(rst);
    rst->result_handler(err, rst->st);
}

static errval_t
revoke_local(struct revoke_st *rst)
{
    errval_t err;
    err = monitor_continue_revoke(rst->revokecap.croot, rst->revokecap.cptr,
                                  rst->revokecap.bits, rst->delcap);
    if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
        // kernel encountered a local cap with no copies, explicitly perform a
        // delete in the monitor to deal with possible remote copies
        assert(!capref_is_null(rst->delcap));
        err = delete(get_cap_domref(rst->delcap), revoke_delete_result, rst);
        if (err_is_ok(err)) {
            return err;
        }
    }
    else if (err_no(err) == MON_ERR_REMOTE_CAP_RETRY) {
        // cap is locked, wait in queue for unlock
        assert(!capref_is_null(rst->delcap));
        caplock_wait(get_cap_domref(rst->delcap), &rst->lock_queue_node, MKCLOSURE(revoke_unlock_cont, rst));
        return SYS_ERR_OK;
    }
    else {
        // revoke failed or succeeded locally without any distributed cap
        // interaction
        rst->result_handler(err, rst->st);
    }

    slot_free(rst->delcap);
    free(rst);

    return err;
}

/*
 * Revoke operation {{{1
 */

errval_t
revoke(struct domcapref cap, revoke_result_handler_t result_handler, void *st)
{
    errval_t err;
    distcap_state_t state;

    err = invoke_cnode_get_state(cap.croot, cap.cptr, cap.bits, &state);
    if (err_is_fail(err)) {
        return err;
    }

    if (distcap_state_is_busy(state)) {
        return MON_ERR_REMOTE_CAP_RETRY;
    }

    err = monitor_lock_cap(cap.croot, cap.cptr, cap.bits);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_RCAP_DB_LOCK);
    }

    struct revoke_st *rst = calloc(1, sizeof(struct revoke_st));
    if (!rst) {
        err = LIB_ERR_MALLOC_FAIL;
        goto unlock_cap;
    }
    rst->revokecap = cap;
    rst->result_handler = result_handler;
    rst->st = st;

    if (distcap_state_is_foreign(state)) {
        err = request_revoke(rst);
    }
    else {
        err = slot_alloc(&rst->delcap);
        if (err_is_fail(err)) {
            goto free_st;
        }

        err = revoke_local(rst);
    }

    if (err_is_ok(err)) {
        return err;
    }

free_st:
    free(rst);

unlock_cap:
    caplock_unlock(cap);

    return err;
}
