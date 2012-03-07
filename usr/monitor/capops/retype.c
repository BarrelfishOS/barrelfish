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
#include "transport.h"
#include "magic.h"

/*
 * Check retype multicast
 */

struct check_retype_mc_st {
    struct capsend_mc_st mc_st;
    retype_result_handler_t result_handler;
    void *st;
};

static void
check_retype_send_cont(struct intermon_binding *b, intermon_caprep_t *caprep, struct capsend_mc_st *mc_st)
{
    errval_t err = intermon_find_descendants__tx(b, NOP_CONT, *caprep, (genvaddr_t)mc_st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to send find_descendants message");
    }
}

static errval_t
check_retype(struct capref src, enum objtype type, size_t objbits,
             retype_result_handler_t result_handler, void *st)
{
    errval_t err;

    struct capability cap;
    err = monitor_cap_identify(src, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct check_retype_mc_st *mc_st;
    mc_st = malloc(sizeof(*mc_st));
    if (!mc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }

    mc_st->result_handler = result_handler;
    mc_st->st = st;
    return send_all(&cap, check_retype_send_cont, (struct capsend_mc_st*)mc_st);
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
    err = intermon_find_descendants_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);
    free(msg_st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not send find_descendants_result");
    }
}

__attribute__((unused))
static void
find_descendants__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;

    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;

    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    err = monitor_has_local_descendants(cap);

    struct find_descendants_result_msg_st *msg_st;
    msg_st = malloc(sizeof(*msg_st));
    if (!msg_st) {
        err = LIB_ERR_MALLOC_FAIL;
        USER_PANIC_ERR(err, "could not alloc find_descendants_result_msg_st");
    }
    msg_st->queue_elem.cont = find_descendants_result_send_cont;
    msg_st->status = err;
    msg_st->st = st;

    err = enqueue_send_target(from, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not enqueue find_descendants_result msg");
    }
}

__attribute__((unused))
static void
find_descendants_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    struct check_retype_mc_st *mc_st = (struct check_retype_mc_st*)st;

    if (err_is_ok(status)) {
        // found result
        mc_st->result_handler(SYS_ERR_OK, mc_st->st);
    }
    else if (err_no(status) != CAP_ERR_NOTFOUND) {
        printf("ignoring bad find_descendants result %"PRIuPTR"\n", status);
    }

    if (capsend_result_handler(st)) {
        mc_st->result_handler(CAP_ERR_NOTFOUND, mc_st->st);
        free(mc_st);
    }
}

/*
 *
 */

struct retype_st {
    enum objtype type;
    size_t objbits;
    struct capref dest_cnode;
    cslot_t dest_slot;
    struct capref src;
    retype_result_handler_t result_handler;
    void *st;
};

static void
create_copies_cont(errval_t status, void *st)
{
    errval_t err = status;
    struct retype_st *rtst = (struct retype_st*)st;
    if (err_no(err) == CAP_ERR_NOTFOUND) {
        err = monitor_create_caps(rtst->type, rtst->objbits, rtst->dest_cnode, rtst->dest_slot, rtst->src);
    }
    rtst->result_handler(err, rtst->st);
    free(rtst);
}

struct request_retype_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    struct retype_st *st;
    intermon_caprep_t caprep;
};

static void
request_retype_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    struct request_retype_msg_st *msg_st = (struct request_retype_msg_st*)e;
    struct retype_st *rtst = msg_st->st;
    errval_t err = intermon_request_retype__tx(b, NOP_CONT, msg_st->caprep, rtst->type, rtst->objbits, (genvaddr_t)rtst);
    if (err_is_fail(err)) {
        rtst->result_handler(err, rtst->st);
        free(rtst);
    }
}

static errval_t
request_retype(retype_result_handler_t result_handler, struct retype_st *st)
{
    errval_t err;
    struct capability cap;
    err = monitor_cap_identify(st->src, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct request_retype_msg_st *msg_st;
    msg_st = malloc(sizeof(*msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = request_retype_send_cont;
    capability_to_caprep(&cap, &msg_st->caprep);

    err = enqueue_send_owner(st->src, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
    }
    return err;
}

struct retype_result_st {
    struct intermon_msg_queue_elem queue_elem;
    coreid_t from;
    errval_t status;
    genvaddr_t st;
};

static void
retype_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct retype_result_st *rtst = (struct retype_result_st*)e;
    err = intermon_retype_response__tx(b, NOP_CONT, rtst->status, rtst->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send retype_result message");
    }
    free(rtst);
}

static void
retype_result_cont(errval_t status, void *st)
{
    errval_t err;
    struct retype_result_st *rtst = (struct retype_result_st*)st;
    rtst->status = status;
    rtst->queue_elem.cont = retype_result_send_cont;
    err = enqueue_send_target(rtst->from, (struct msg_queue_elem*)rtst);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to enqueue retype_result");
    }
}

__attribute__((unused))
static void
request_retype__rx_handler(struct intermon_binding *b, intermon_caprep_t srcrep, int desttype, size_t destbits, genvaddr_t st)
{
    errval_t err;

    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;

    struct capability cap;
    caprep_to_capability(&srcrep, &cap);

    struct retype_result_st *rtst;
    rtst = malloc(sizeof(*rtst));
    if (!rtst) {
        USER_PANIC_ERR(LIB_ERR_MALLOC_FAIL, "could not allocate retype request reply state");
    }
    rtst->from = from;
    rtst->st = st;

    struct capref src;
    err = copy_if_exists(&cap, &src);
    if (err_is_fail(err)) {
        goto reply_err;
    }

    capstate_t state;
    err = cap_get_state(src, &state);
    if (err_is_fail(err)) {
        goto reply_err;
    }

    if (!cap_state_is_owner(state)) {
        err = CAP_ERR_FOREIGN;
        goto reply_err;
    }

    if (cap_state_is_busy(state)) {
        err = CAP_ERR_BUSY;
        goto reply_err;
    }

    err = cap_set_busy(src);
    if (err_is_fail(err)) {
        goto reply_err;
    }

    err = check_retype(src, desttype, destbits, retype_result_cont, rtst);

reply_err:
    if (err_is_fail(err)) {
        retype_result_cont(err, rtst);
    }
}

/*
 * Entry
 */

errval_t
retype(enum objtype type, size_t objbits, struct capref dest_cnode,
       cslot_t dest_slot, struct capref src,
       retype_result_handler_t result_handler, void *st)
{
    errval_t err;

    capstate_t src_state;
    err = cap_get_state(src, &src_state);
    if (err_is_fail(err)) {
        return err;
    }

    if (cap_state_is_busy(src_state)) {
        return CAP_ERR_BUSY;
    }

    struct retype_st *rst = malloc(sizeof(struct retype_st));
    rst->type = type;
    rst->objbits = objbits;
    rst->dest_cnode = dest_cnode;
    rst->dest_slot = dest_slot;
    rst->src = src;
    rst->result_handler = result_handler;
    rst->st = st;

    if (cap_state_is_owner(src_state)) {
        err = check_retype(src, type, objbits, create_copies_cont, rst);
    }
    else {
        err = request_retype(create_copies_cont, rst);
    }

    if (err_is_fail(err)) {
        free(rst);
    }

    return err;
}

