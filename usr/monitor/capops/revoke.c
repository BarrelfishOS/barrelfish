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

struct revoke_st {
    struct capref capref, delcap;
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
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    struct capref capref;
    err = copy_if_exists(&cap, &capref);
    if (err_is_fail(err)) {
        goto send_err;
    }

    distcap_state_t state;
    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        goto send_err;
    }

    struct revoke_request_st *rst;
    rst = malloc(sizeof(*rst));
    if (!rst) {
        err = LIB_ERR_MALLOC_FAIL;
        goto send_err;
    }
    rst->capref = capref;
    rst->from = from;
    rst->st = st;

    if (distcap_is_foreign(state)) {
        err = MON_ERR_CAP_FOREIGN;
        goto send_err;
    }

    if (distcap_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto send_err;
    }

    err = move(capref, from, request_revoke_move_cont, rst);

send_err:
    if (err_is_fail(err)) {
        errval_t send_err = revoke_result(from, err, st);
        if (err_is_fail(send_err)) {
            USER_PANIC_ERR(send_err, "could not send revoke error");
        }
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
    err = monitor_cap_identify(st->capref, &cap);
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

    err = capsend_owner(st->capref, (struct msg_queue_elem*)msg_st);
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

    err = monitor_revoke(rst->capref, rst->delcap);
    if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
        err = delete(rst->delcap, revoke_delete_result, rst);
        if (err_is_ok(err)) {
            return;
        }
    }
    else {
        slot_free(rst->delcap);
    }

    rst->result_handler(err, rst->st);
    free(rst);
}

static errval_t
revoke_local(struct revoke_st *rst)
{
    errval_t err = slot_alloc(&rst->delcap);
    if (err_is_fail(err)) {
        free(rst);
        return err;
    }

    err = monitor_revoke(rst->capref, rst->delcap);
    if (err_is_ok(err)) {
        // revoke succeeded locally without any distributed cap interaction
        rst->result_handler(err, rst->st);
    }
    else if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
        // kernel encountered a local cap with no copies, explicitly perform a
        // delete in the monitor to deal with possible remote copies
        err = delete(rst->delcap, revoke_delete_result, rst);
        if (err_is_ok(err)) {
            return err;
        }
    }

    slot_free(rst->delcap);
    free(rst);

    return err;
}

/*
 * Revoke operation {{{1
 */

errval_t
revoke(struct capref capref, revoke_result_handler_t result_handler, void *st)
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

    err = cap_set_busy(capref);
    if (err_is_fail(err)) {
        return err;
    }

    struct revoke_st *rst = malloc(sizeof(struct revoke_st));
    if (!rst) {
        err = LIB_ERR_MALLOC_FAIL;
        goto ready_cap;
    }
    rst->capref = capref;
    rst->result_handler = result_handler;
    rst->st = st;

    if (distcap_is_foreign(state)) {
        err = request_revoke(rst);
    }
    else {
        err = revoke_local(rst);
    }

ready_cap:
    if (err_is_fail(err)) {
        errval_t err2 = cap_set_ready(capref);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err, "failed to set cap to ready for cleanup");
        }
    }

    return err;
}
