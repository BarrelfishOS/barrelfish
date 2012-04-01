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
#include <monitor.h>
#include <monitor_invocations.h>
#include "capops.h"
#include "capsend.h"
#include "magic.h"
#include "capop_handlers.h"

/*
 *
 */

struct retype_st {
    enum objtype type;
    size_t objbits;
    struct domcapref src;
    struct domcapref destcn;
    cslot_t start_slot;
    retype_result_handler_t result_handler;
    void *st;
};

static void
create_copies_cont(errval_t status, void *st)
{
    errval_t err = status;
    struct retype_st *rtst = (struct retype_st*)st;
    if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
        // no descendants found
        assert(capcmp(rtst->src.croot, rtst->destcn.croot));
        err = monitor_create_caps(rtst->src.croot, rtst->type, rtst->objbits,
                                  rtst->src.cptr, rtst->src.bits,
                                  rtst->destcn.cptr, rtst->destcn.bits,
                                  rtst->start_slot);
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
    assert(rtst);
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
    err = monitor_domains_cap_identify(st->src.croot, st->src.cptr, st->src.bits, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct request_retype_msg_st *msg_st;
    msg_st = malloc(sizeof(*msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = request_retype_send_cont;
    msg_st->st = st;
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

void
request_retype__rx_handler(struct intermon_binding *b, intermon_caprep_t srcrep, int desttype, uint32_t destbits, genvaddr_t st)
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
    err = slot_alloc(&src);
    if (err_is_fail(err)) {
        goto reply_err;
    }
    struct domcapref domsrc = get_cap_domref(src);

    err = monitor_copy_if_exists(&cap, src);
    if (err_is_fail(err)) {
        goto free_slot;
    }

    distcap_state_t state;
    err = cap_get_state(src, &state);
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

    if (desttype == ObjType_EndPoint) {
        // XXX: because of the current "multi-retype" hack for endpoints, a
        // dispatcher->endpoint retype can happen irrespective of the existence
        // of descendents on any core.
        bool unused_has_descendants;
        err = monitor_cap_remote(src, true, &unused_has_descendants);
        goto destroy_cap;
    }

    err = monitor_lock_cap(domsrc.croot, domsrc.cptr, domsrc.bits);
    if (err_is_fail(err)) {
        goto destroy_cap;
    }

    err = capsend_find_descendants(domsrc, retype_result_cont, rtst);
    if (err_is_fail(err)) {
        goto unlock_cap;
    }

    return;

unlock_cap:
    monitor_unlock_cap(domsrc.croot, domsrc.cptr, domsrc.bits);

destroy_cap:
    cap_delete(src);

free_slot:
    slot_free(src);

reply_err:
    retype_result_cont(err, rtst);
}

void
retype_response__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    create_copies_cont(status, (void*)st);
}

/*
 * Entry
 */

errval_t
capops_retype(enum objtype type, size_t objbits, struct capref croot,
              capaddr_t dest_cn, uint8_t dest_bits, cslot_t dest_slot,
              capaddr_t src, uint8_t src_bits,
              retype_result_handler_t result_handler, void *st)
{
    errval_t err;
    debug_printf("retype\n");

    distcap_state_t src_state;
    err = invoke_cnode_get_state(croot, src, src_bits, &src_state);
    if (err_is_fail(err)) {
        return err;
    }

    if (distcap_state_is_busy(src_state)) {
        return MON_ERR_REMOTE_CAP_RETRY;
    }

    err = invoke_cnode_retype(croot, src, type, objbits, dest_cn, dest_slot,
                              dest_bits);
    if (err_no(err) != SYS_ERR_RETRY_THROUGH_MONITOR) {
        return err;
    }

    // if retype invocation failed with "retry through mon", we assume that
    // distcap_needs_locality(cap) would return true.

    struct retype_st *rst = malloc(sizeof(struct retype_st));
    rst->type = type;
    rst->objbits = objbits;
    rst->src = (struct domcapref){ .croot = croot, .cptr = src, .bits = src_bits };
    rst->destcn = (struct domcapref){ .croot = croot, .cptr = dest_cn, .bits = dest_bits };
    rst->start_slot = dest_slot;
    rst->result_handler = result_handler;
    rst->st = st;

    if (distcap_state_is_foreign(src_state)) {
        err = request_retype(create_copies_cont, rst);
    }
    else {
        err = capsend_find_descendants(rst->src, create_copies_cont, rst);
    }

    if (err_is_fail(err)) {
        free(rst);
    }

    return err;
}

