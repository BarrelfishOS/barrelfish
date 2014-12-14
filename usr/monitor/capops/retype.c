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
#include <if/intermon_defs.h>
#include <monitor.h>
#include <monitor_invocations.h>
#include <dom_invocations.h>
#include "capops.h"
#include "capsend.h"
#include "caplock.h"
#include "magic.h"
#include "internal.h"

/*
 * Retype states
 */

struct retype_check_st {
    enum objtype type;
    size_t objbits;
    struct domcapref src;
    struct result_closure cont;
};

struct retype_output_st {
    struct domcapref destcn;
    cslot_t start_slot;
    struct result_closure cont;
};

struct requested_retype_st {
    struct intermon_msg_queue_elem queue_elem;
    struct retype_check_st check;
    struct capref src;
    coreid_t from;
    errval_t status;
    genvaddr_t request_st;
};

struct local_retype_st {
    struct retype_check_st check;
    struct retype_output_st output;
};

struct retype_request_st {
    struct intermon_msg_queue_elem queue_elem;
    intermon_caprep_t caprep;
    struct retype_check_st check;
    struct retype_output_st output;
};

/*
 * Prototypes for static functions so ordering does not matter
 */

static void check_retype__enq(struct retype_check_st *check_st);
static void retype_check__rx(errval_t status, struct retype_check_st* check,
                             struct retype_output_st *output, void *to_free);

/**
 * \brief Intermon is ready to send retype result
 */
static void
retype_result__send(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct requested_retype_st *req_st = (struct requested_retype_st*)e;
    err = intermon_capops_retype_response__tx(b, NOP_CONT, req_st->status,
                                              req_st->request_st);
    PANIC_IF_ERR(err, "sending retype result message");
    free(req_st);
}

/**
 * \brief Enqueue retype result
 */
static void
retype_result__enq(struct requested_retype_st *req_st)
{
    req_st->queue_elem.cont = retype_result__send;
    errval_t err = capsend_target(req_st->from, (struct msg_queue_elem*)req_st);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to enqueue retype result");
        free(req_st);
    }
}

/**
 * \brief Retype temporary cap has been deleted
 */
static void
retype_tmpcap_delete__cont(errval_t status, void *st)
{
    errval_t err;
    struct requested_retype_st *req_st = (struct requested_retype_st*)st;

    if (err_is_fail(status) && err_no(status) != SYS_ERR_CAP_NOT_FOUND) {
        DEBUG_ERR(status, "deleting tmp retype cap, cap will leak");
    }

    err = slot_free(req_st->src);
    DEBUG_IF_ERR(err, "freeing tmp retype slot, slot will leak");
    req_st->src = NULL_CAP;
    memset(&req_st->check.src, 0, sizeof(struct domcapref));

    retype_result__enq(req_st);
}

/**
 * \brief The check for a retype request has completed
 */
static void
retype_request_check__rx(errval_t status, void *st)
{
    struct requested_retype_st *req_st = (struct requested_retype_st*)st;

    if (err_is_ok(status)) {
        status = monitor_remote_relations(req_st->src, RRELS_DESC_BIT,
                                          RRELS_DESC_BIT, NULL);
    }

    req_st->status = status;

    if (!capref_is_null(req_st->src)) {
        capops_delete(req_st->check.src, retype_tmpcap_delete__cont, req_st);
    }
    else {
        retype_result__enq(req_st);
    }
}

void
retype_request__rx(struct intermon_binding *b, intermon_caprep_t srcrep,
                   uint32_t desttype, uint32_t destbits, genvaddr_t st)
{
    errval_t err;

    // allocate and setup state
    struct requested_retype_st *req_st;
    err = calloce(1, sizeof(*req_st), &req_st);
    PANIC_IF_ERR(err, "allocating retype request state");

    req_st->queue_elem.cont = retype_result__send;
    req_st->check.type = desttype;
    req_st->check.objbits = destbits;
    req_st->check.cont = MKRESCONT(retype_request_check__rx, req_st);
    req_st->from = ((struct intermon_state*)b->st)->core_id;
    req_st->request_st = st;

    // get slot and cap
    err = slot_alloc(&req_st->src);
    GOTO_IF_ERR(err, cont_err);
    req_st->check.src = get_cap_domref(req_st->src);

    struct capability cap;
    caprep_to_capability(&srcrep, &cap);
    err = monitor_copy_if_exists(&cap, req_st->src);
    GOTO_IF_ERR(err, cont_err);

    // validate cap state
    distcap_state_t state;
    err = dom_cnode_get_state(req_st->check.src, &state);
    GOTO_IF_ERR(err, cont_err);

    if (distcap_state_is_foreign(state)) {
        err = MON_ERR_CAP_FOREIGN;
        goto cont_err;
    }
    if (distcap_state_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto cont_err;
    }

    // initiate check
    check_retype__enq(&req_st->check);

    return;

cont_err:
    retype_request_check__rx(err, req_st);
}

static void
retype_result__rx(errval_t status, struct retype_request_st *req_st)
{
    retype_check__rx(status, &req_st->check, &req_st->output, req_st);
}

/**
 * \brief Handle the response to a retype request
 */
void
retype_response__rx(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    struct retype_request_st *req_st = (struct retype_request_st*)(lvaddr_t)st;
    retype_result__rx(status, req_st);
}

/**
 * \brief Intermon is ready to send request_retype
 */
static void
retype_request__send(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    struct retype_request_st *req_st = (struct retype_request_st*)e;
    errval_t err;

    err = intermon_capops_request_retype__tx(b, NOP_CONT, req_st->caprep,
                                             req_st->check.type,
                                             req_st->check.objbits,
                                             (lvaddr_t)req_st);

    if (err_is_fail(err)) {
        retype_result__rx(err, req_st);
    }
}

/**
 * \brief Enqueue a retype request
 */
static void
retype_request__enq(struct retype_request_st *req_st)
{
    errval_t err;
    struct capability cap;
    err = monitor_domains_cap_identify(req_st->check.src.croot,
                                       req_st->check.src.cptr,
                                       req_st->check.src.bits, &cap);
    GOTO_IF_ERR(err, err_cont);

    req_st->queue_elem.cont = retype_request__send;
    capability_to_caprep(&cap, &req_st->caprep);

    err = capsend_owner(req_st->check.src, (struct msg_queue_elem*)req_st);
    GOTO_IF_ERR(err, err_cont);

    return;

err_cont:
    retype_result__rx(err, req_st);
}

/**
 * \brief The descendants search has completed
 */
static void
find_descendants__rx(errval_t status, void *st)
{
    struct retype_check_st *check_st = (struct retype_check_st*)st;

    // need to translate error codes:
    // - descendants found -> revoke first
    // - not found -> ok
    // - otherwise -> unchanged
    if (err_is_ok(status)) {
        status = SYS_ERR_REVOKE_FIRST;
    }
    else if (err_no(status) == SYS_ERR_CAP_NOT_FOUND) {
        status = err_push(status, SYS_ERR_OK);
    }

    // unlock cap and procede with check result continuation
    caplock_unlock(check_st->src);
    CALLRESCONT(check_st->cont, status);
}

/**
 * \brief Enqueue a retype check
 */
static void
check_retype__enq(struct retype_check_st *check_st)
{
    errval_t err;

    if (check_st->type == ObjType_EndPoint) {
        // XXX: because of the current "multi-retype" hack for endpoints, a
        // dispatcher->endpoint retype can happen irrespective of the existence
        // of descendents on any core.
        err = monitor_domcap_remote_relations(check_st->src.croot,
                                              check_st->src.cptr,
                                              check_st->src.bits,
                                              RRELS_DESC_BIT,
                                              RRELS_DESC_BIT, NULL);
        goto cont_err;
    }

    err = monitor_lock_cap(check_st->src.croot, check_st->src.cptr,
                           check_st->src.bits);
    GOTO_IF_ERR(err, cont_err);

    err = capsend_find_descendants(check_st->src, find_descendants__rx,
                                   check_st);
    GOTO_IF_ERR(err, unlock_cap);

    return;

unlock_cap:
    caplock_unlock(check_st->src);

cont_err:
    CALLRESCONT(check_st->cont, err);
}

/**
 * \brief Handle a completed retype check
 */
static void
retype_check__rx(errval_t status, struct retype_check_st* check,
                 struct retype_output_st *output, void *to_free)
{
    errval_t err = status;
    if (err_is_ok(err)) {
        // the retype may procede
        struct domcapref *src = &check->src;
        struct domcapref *destcn = &output->destcn;
        assert(capcmp(src->croot, destcn->croot));
        err = monitor_create_caps(src->croot, check->type, check->objbits,
                                  src->cptr, src->bits, destcn->cptr,
                                  destcn->bits, output->start_slot);
    }
    struct result_closure cont = output->cont;
    assert(cont.handler);
    free(to_free);
    CALLRESCONT(cont, err);
}

/**
 * \brief Handle result of a owner-initiated retype check.
 */
static void
local_retype_check__rx(errval_t status, void *st)
{
    struct local_retype_st *rtp_st = (struct local_retype_st*)st;
    retype_check__rx(status, &rtp_st->check, &rtp_st->output, rtp_st);
}

/*
 * Entry
 */

void
capops_retype(enum objtype type, size_t objbits, struct capref croot,
              capaddr_t dest_cn, uint8_t dest_bits, cslot_t dest_slot,
              capaddr_t src, uint8_t src_bits,
              retype_result_handler_t result_handler, void *st)
{
    errval_t err;
    distcap_state_t src_state;
    struct retype_request_st *rtp_req_st;
    struct local_retype_st *rtp_loc_st;

    err = invoke_cnode_get_state(croot, src, src_bits, &src_state);
    GOTO_IF_ERR(err, err_cont);

    if (distcap_state_is_busy(src_state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto err_cont;
    }

    err = invoke_cnode_retype(croot, src, type, objbits, dest_cn, dest_slot,
                              dest_bits);
    if (err_no(err) != SYS_ERR_RETRY_THROUGH_MONITOR) {
        goto err_cont;
    }

    // if retype invocation failed with "retry through mon", we assume that
    // distcap_needs_locality(cap) would return true.

    if (distcap_state_is_foreign(src_state)) {
        // setup retype request
        err = calloce(1, sizeof(*rtp_req_st), &rtp_req_st);
        GOTO_IF_ERR(err, err_cont);

        // fill in parameters
        rtp_req_st->check.type = type;
        rtp_req_st->check.objbits = objbits;
        rtp_req_st->check.src = (struct domcapref){
            .croot = croot,
            .cptr = src,
            .bits = src_bits,
        };
        rtp_req_st->output.destcn = (struct domcapref){
            .croot = croot,
            .cptr = dest_cn,
            .bits = dest_bits,
        };
        rtp_req_st->output.start_slot = dest_slot;
        rtp_req_st->output.cont = MKRESCONT(result_handler, st);

        // enqueue retype request
        retype_request__enq(rtp_req_st);
    }
    else {
        // on owner, setup retype check
        err = calloce(1, sizeof(*rtp_loc_st), &rtp_loc_st);
        GOTO_IF_ERR(err, err_cont);

        // fill in parameters
        rtp_loc_st->check.type = type;
        rtp_loc_st->check.objbits = objbits;
        rtp_loc_st->check.src = (struct domcapref){
            .croot = croot,
            .cptr = src,
            .bits = src_bits,
        };
        rtp_loc_st->output.destcn = (struct domcapref){
            .croot = croot,
            .cptr = dest_cn,
            .bits = dest_bits,
        };
        rtp_loc_st->output.start_slot = dest_slot;
        rtp_loc_st->output.cont = MKRESCONT(result_handler, st);

        // setup handler for retype check result
        rtp_loc_st->check.cont = MKRESCONT(local_retype_check__rx, rtp_loc_st);

        // initiate check
        check_retype__enq(&rtp_loc_st->check);
    }

    return;

err_cont:
    result_handler(err, st);
}

