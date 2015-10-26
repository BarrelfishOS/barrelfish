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
#include "capops.h"
#include "capsend.h"
#include "magic.h"
#include "caplock.h"
#include "internal.h"
#include "dom_invocations.h"
#include "monitor_invocations.h"

struct retrieve_rpc_st {
    struct intermon_msg_queue_elem iqn;
    struct domcapref cap;
    struct capability rawcap;
    move_result_handler_t result_handler;
    void *st;
    coreid_t prev_owner;
};

struct retrieve_response_st {
    struct intermon_msg_queue_elem iqn;
    errval_t status;
    uint8_t relations;
    genvaddr_t st;
    coreid_t from;
};

static void retrieve_owner__enq(struct retrieve_rpc_st *st);
static void retrieve_owner__send(struct intermon_binding *b,
                                 struct intermon_msg_queue_elem *e);
static void retrieve_result__enq(errval_t status,
                                 struct retrieve_response_st *st);
static void retrieve_result__send(struct intermon_binding *b,
                                  struct intermon_msg_queue_elem *e);
static void retrieve_ownership_update__fin(void *st);

void
capops_retrieve(struct domcapref cap,
                move_result_handler_t result_handler,
                void *st)
{
    errval_t err;

    DEBUG_CAPOPS("%s ## start transfer ownership \n", __FUNCTION__);

    distcap_state_t state;
    err = dom_cnode_get_state(cap, &state);
    GOTO_IF_ERR(err, report_error);
    if (distcap_state_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
    }
    GOTO_IF_ERR(err, report_error);

    err = monitor_lock_cap(cap.croot, cap.cptr, cap.bits);
    GOTO_IF_ERR(err, report_error);

    struct retrieve_rpc_st *rst = NULL;
    err = calloce(1, sizeof(*rst), &rst);
    GOTO_IF_ERR(err, unlock_cap);

    rst->cap = cap;
    rst->result_handler = result_handler;
    rst->st = st;

    err = monitor_domains_cap_identify(cap.croot, cap.cptr, cap.bits, &rst->rawcap);
    GOTO_IF_ERR(err, free_st);

    err = monitor_get_domcap_owner(cap, &rst->prev_owner);
    GOTO_IF_ERR(err, free_st);

    if (rst->prev_owner == my_core_id) {
        err = SYS_ERR_OK;
        goto free_st;
    }

    retrieve_owner__enq(rst);

    return;

free_st:
    free(rst);

unlock_cap:
    caplock_unlock(cap);

report_error:
    result_handler(err, st);
}

static void
retrieve_ownership__rx(errval_t status, struct retrieve_rpc_st *st)
{
    DEBUG_CAPOPS("%s ## transfer ownership done. calling %p\n", __FUNCTION__,
                 st->result_handler);

    caplock_unlock(st->cap);
    st->result_handler(status, st->st);
    free(st);
}

static void
retrieve_owner__enq(struct retrieve_rpc_st *st)
{
    errval_t err;

    st->iqn.cont = retrieve_owner__send;
    err = capsend_owner(st->cap, (struct msg_queue_elem*)st);
    if (err_is_fail(err)) {
        retrieve_ownership__rx(err, st);
    }
}

static void
retrieve_owner__send(struct intermon_binding *b,
                     struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct retrieve_rpc_st *st = (struct retrieve_rpc_st*)e;
    intermon_caprep_t caprep;

    err = monitor_set_domcap_owner(st->cap, my_core_id);
    GOTO_IF_ERR(err, report_error);

    capability_to_caprep(&st->rawcap, &caprep);
    err = intermon_capops_retrieve_request__tx(b, NOP_CONT, caprep, (lvaddr_t)st);
    GOTO_IF_ERR(err, report_error);

    return;

report_error:
    DEBUG_CAPOPS("%s failed \n", __FUNCTION__);
    retrieve_ownership__rx(err, st);
}

void
retrieve_request__rx(struct intermon_binding *b,
                     intermon_caprep_t caprep,
                     genvaddr_t st)
{
    errval_t err, err2;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;

    DEBUG_CAPOPS("%s ## transfer ownership request\n", __FUNCTION__);

    struct retrieve_response_st *rst;
    err = calloce(1, sizeof(*rst), &rst);
    PANIC_IF_ERR(err, "allocating retrieve respones state");
    rst->st = st;
    rst->from = inter_st->core_id;

    struct capability rawcap;
    caprep_to_capability(&caprep, &rawcap);

    struct capref cap;
    err = slot_alloc(&cap);
    GOTO_IF_ERR(err, respond_err);

    err = monitor_copy_if_exists(&rawcap, cap);
    GOTO_IF_ERR(err, free_slot);

    distcap_state_t state;
    err = dom_cnode_get_state(get_cap_domref(cap), &state);
    GOTO_IF_ERR(err, delete_cap);

    if (distcap_state_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto delete_cap;
    }
    if (distcap_state_is_foreign(state)) {
        err = MON_ERR_CAP_FOREIGN;
        goto delete_cap;
    }

    uint8_t relations, remote_relations;
    err = monitor_cap_has_relations(cap, 0xFF, &relations);
    GOTO_IF_ERR(err, delete_cap);

    err = monitor_remote_relations(cap, 0, 0, &remote_relations);
    GOTO_IF_ERR(err, delete_cap);

    rst->relations = relations | remote_relations | RRELS_COPY_BIT;

    err = monitor_set_cap_owner(cap_root, get_cap_addr(cap),
                                get_cap_valid_bits(cap),
                                rst->from);

delete_cap:
    err2 = cap_delete(cap);
    DEBUG_IF_ERR(err2, "while deleting temp cap for retrieve");

free_slot:
    err2 = slot_free(cap);
    DEBUG_IF_ERR(err2, "freeing temp cap slot for retrieve");

respond_err:
    retrieve_result__enq(err, rst);
}

static void
retrieve_result__enq(errval_t status, struct retrieve_response_st *st)
{
    errval_t err;
    st->status = status;
    st->iqn.cont = retrieve_result__send;

    err = capsend_target(st->from, (struct msg_queue_elem*)st);
    PANIC_IF_ERR(err, "enqueing retrieve result");
}

static void
retrieve_result__send(struct intermon_binding *b,
                      struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct retrieve_response_st *st = (struct retrieve_response_st*)e;

    err = intermon_capops_retrieve_result__tx(b, NOP_CONT, st->status,
                                              st->relations, st->st);
    PANIC_IF_ERR(err, "sending retrieve result");
    free(st);
}

void
retrieve_result__rx(struct intermon_binding *b, errval_t status,
                    uint8_t relations, genvaddr_t st)
{
    errval_t err;
    struct retrieve_rpc_st *rst = (struct retrieve_rpc_st*)(lvaddr_t)st;

    DEBUG_CAPOPS("%s ## ownership transferred: %s \n", __FUNCTION__,
                 err_getstring(status));

    if (err_is_fail(status)) {
        err = status;
        goto report_error;
    }

    err = monitor_domcap_remote_relations(rst->cap.croot, rst->cap.cptr,
                                          rst->cap.bits, relations, 0xFF,
                                          NULL);
    PANIC_IF_ERR(err, "setting rrels for retrieved cap");

    DEBUG_CAPOPS("%s broadcast updates to other monitors.\n", __FUNCTION__);

    struct event_closure updated_cont
        = MKCONT(retrieve_ownership_update__fin, rst);
    err = capsend_update_owner(rst->cap, updated_cont);
    PANIC_IF_ERR(err, "updating retrieve ownership");

    return;

report_error:
    retrieve_ownership__rx(err, rst);
}

static void
retrieve_ownership_update__fin(void *st)
{
    struct retrieve_rpc_st *rst = (struct retrieve_rpc_st*)st;

    DEBUG_CAPOPS("%s updated in ownership broadcasted.\n", __FUNCTION__);

    retrieve_ownership__rx(SYS_ERR_OK, rst);
}
