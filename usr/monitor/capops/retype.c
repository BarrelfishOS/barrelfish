/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include "monitor.h"
#include "ops.h"
#include "capsend.h"
#include "magic.h"

/*
 *
 */

struct retype_st {
    enum objtype type;
    size_t objbits;
    struct capref dest_start;
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
        // no descendants found
        err = monitor_create_caps(rtst->type, rtst->objbits, rtst->dest_start, rtst->src);
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
    errval_t err = intermon_capops_request_retype__tx(b, NOP_CONT, msg_st->caprep, rtst->type, rtst->objbits, (genvaddr_t)rtst);
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

    err = capsend_owner(st->src, (struct msg_queue_elem*)msg_st);
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
    err = intermon_capops_retype_response__tx(b, NOP_CONT, rtst->status, rtst->st);
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
    err = capsend_target(rtst->from, (struct msg_queue_elem*)rtst);
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

    distcap_state_t state;
    err = cap_get_state(src, &state);
    if (err_is_fail(err)) {
        goto reply_err;
    }

    if (distcap_is_foreign(state)) {
        err = CAP_ERR_FOREIGN;
        goto reply_err;
    }

    if (distcap_is_busy(state)) {
        err = CAP_ERR_BUSY;
        goto reply_err;
    }

    err = cap_set_busy(src);
    if (err_is_fail(err)) {
        goto reply_err;
    }

    err = capsend_find_descendants(src, retype_result_cont, rtst);

reply_err:
    if (err_is_fail(err)) {
        retype_result_cont(err, rtst);
    }
}

/*
 * Entry
 */

errval_t
retype(enum objtype type, size_t objbits, struct capref dest_start,
       struct capref src, retype_result_handler_t result_handler, void *st)
{
    errval_t err;

    distcap_state_t src_state;
    err = cap_get_state(src, &src_state);
    if (err_is_fail(err)) {
        return err;
    }

    if (distcap_is_busy(src_state)) {
        return CAP_ERR_BUSY;
    }

    uint8_t dcn_vbits = get_cnode_valid_bits(dest_start);
    capaddr_t dcn_addr = get_cnode_addr(dest_start);
    capaddr_t scp_addr = get_cap_addr(src);
    err = invoke_cnode_retype(cap_root, scp_addr, type, objbits,
                              dcn_addr, dest_start.slot, dcn_vbits);
    if (err_no(err) != SYS_ERR_RETRY_THROUGH_MONITOR) {
        return err;
    }

    struct retype_st *rst = malloc(sizeof(struct retype_st));
    rst->type = type;
    rst->objbits = objbits;
    rst->dest_start = dest_start;
    rst->src = src;
    rst->result_handler = result_handler;
    rst->st = st;

    if (distcap_is_foreign(src_state)) {
        err = request_retype(create_copies_cont, rst);
    }
    else {
        err = capsend_find_descendants(src, create_copies_cont, rst);
    }

    if (err_is_fail(err)) {
        free(rst);
    }

    return err;
}

