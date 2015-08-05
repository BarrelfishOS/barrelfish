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
#include "capops.h"
#include "capsend.h"
#include "caplock.h"
#include "internal.h"
#include "delete_int.h"
#include "dom_invocations.h"
#include "monitor_debug.h"

struct revoke_slave_st *slaves_head = 0, *slaves_tail = 0;

struct revoke_master_st {
    struct delete_queue_node del_qn;
    struct domcapref cap;
    struct capability rawcap;
    struct capsend_mc_st revoke_mc_st;
    struct capsend_destset dests;
    revoke_result_handler_t result_handler;
    void *st;
    bool local_fin, remote_fin;
};

struct revoke_slave_st {
    struct intermon_msg_queue_elem im_qn;
    struct delete_queue_node del_qn;
    struct capability rawcap;
    struct capref cap;
    coreid_t from;
    genvaddr_t st;
    errval_t status;
    struct revoke_slave_st *next;
};

static void revoke_result__rx(errval_t result,
                              struct revoke_master_st *st,
                              bool locked);
static void revoke_retrieve__rx(errval_t result, void *st_);
static void revoke_local(struct revoke_master_st *st);
static void revoke_no_remote(struct revoke_master_st *st);
static errval_t revoke_mark__send(struct intermon_binding *b,
                                  intermon_caprep_t *caprep,
                                  struct capsend_mc_st *mc_st);
static void revoke_ready__send(struct intermon_binding *b,
                               struct intermon_msg_queue_elem *e);
static errval_t revoke_commit__send(struct intermon_binding *b,
                                    intermon_caprep_t *caprep,
                                    struct capsend_mc_st *mc_st);
static void revoke_slave_steps__fin(void *st);
static void revoke_done__send(struct intermon_binding *b,
                              struct intermon_msg_queue_elem *e);
static void revoke_master_steps__fin(void *st);

void
capops_revoke(struct domcapref cap,
              revoke_result_handler_t result_handler,
              void *st)
{
    errval_t err;

    DEBUG_CAPOPS("%s ## start revocation protocol\n", __FUNCTION__);

    distcap_state_t state;
    err = dom_cnode_get_state(cap, &state);
    GOTO_IF_ERR(err, report_error);

    if (distcap_state_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto report_error;
    }

    struct revoke_master_st *rst;
    err = calloce(1, sizeof(*rst), &rst);
    GOTO_IF_ERR(err, report_error);
    rst->cap = cap;
    err = monitor_domains_cap_identify(cap.croot, cap.cptr, cap.bits, &rst->rawcap);
    GOTO_IF_ERR(err, free_st);
    rst->result_handler = result_handler;
    rst->st = st;

    if (distcap_state_is_foreign(state)) {
        // need to retrieve ownership
        DEBUG_CAPOPS("%s getting cap ownership\n", __FUNCTION__);
        capops_retrieve(rst->cap, revoke_retrieve__rx, rst);
    }
    else {
        if (num_monitors_online() == 1) {
            DEBUG_CAPOPS("%s: only one monitor: do simpler revoke\n",
                    __FUNCTION__);
            // no remote monitors exist; do simplified revocation process
            revoke_no_remote(rst);
            // return here
            return;
        }
        // have ownership, initiate revoke
        revoke_local(rst);
    }

    return;

free_st:
    free(rst);

report_error:
    result_handler(err, st);
}

static void
revoke_result__rx(errval_t result,
                  struct revoke_master_st *st,
                  bool locked)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;

    if (locked) {
        caplock_unlock(st->cap);
    }

    if (err_is_ok(result)) {
        // clear the remote copies bit
        err = monitor_domcap_remote_relations(st->cap.croot, st->cap.cptr,
                                              st->cap.bits, 0, RRELS_COPY_BIT,
                                              NULL);
        if (err_is_fail(err) && err_no(err) != SYS_ERR_CAP_NOT_FOUND) {
            DEBUG_ERR(err, "resetting remote copies bit after revoke");
        }
    }

    DEBUG_CAPOPS("%s ## revocation completed, calling %p\n", __FUNCTION__,
                 st->result_handler);

    st->result_handler(result, st->st);
}

static void
revoke_retrieve__rx(errval_t result, void *st_)
{
    struct revoke_master_st *st = (struct revoke_master_st*)st_;

    if (err_is_fail(result)) {
        revoke_result__rx(result, st, false);
    }
    else {

#ifndef NDEBUG
        distcap_state_t state;
        errval_t err = dom_cnode_get_state(st->cap, &state);
        PANIC_IF_ERR(err, "dom_cnode_get_state");
        assert(!distcap_state_is_foreign(state));
#endif
        revoke_local(st);
    }
}

static void
revoke_local(struct revoke_master_st *st)
{
    DEBUG_CAPOPS("%s: called from %p\n", __FUNCTION__,
            __builtin_return_address(0));
    errval_t err;

    delete_steps_pause();

    err = monitor_revoke_mark_target(st->cap.croot,
                                     st->cap.cptr,
                                     st->cap.bits);
    PANIC_IF_ERR(err, "marking revoke");


    DEBUG_CAPOPS("%s ## revocation: mark phase\n", __FUNCTION__);
    // XXX: could check whether remote copies exist here(?), -SG, 2014-11-05
    err = capsend_relations(&st->rawcap, revoke_mark__send,
            &st->revoke_mc_st, &st->dests);
    PANIC_IF_ERR(err, "initiating revoke mark multicast");
}

static void
revoke_no_remote(struct revoke_master_st *st)
{
    assert(num_monitors_online() == 1);

    if (!delete_steps_get_waitset()) {
        delete_steps_init(get_default_waitset());
    }

    errval_t err;
    DEBUG_CAPOPS("%s\n", __FUNCTION__);

    // pause deletion steps
    DEBUG_CAPOPS("%s: delete_steps_pause()\n", __FUNCTION__);
    delete_steps_pause();

    // mark target of revoke
    DEBUG_CAPOPS("%s: mon_revoke_mark_tgt()\n", __FUNCTION__);
    err = monitor_revoke_mark_target(st->cap.croot,
                                     st->cap.cptr,
                                     st->cap.bits);
    PANIC_IF_ERR(err, "marking revoke");


    // resume delete steps
    DEBUG_CAPOPS("%s: delete_steps_resume()\n", __FUNCTION__);
    delete_steps_resume();

    // wait on delete queue, marking that remote cores are done
    st->remote_fin = true;
    DEBUG_CAPOPS("%s: delete_queue_wait()\n", __FUNCTION__);
    struct event_closure steps_fin_cont
        = MKCLOSURE(revoke_master_steps__fin, st);
    delete_queue_wait(&st->del_qn, steps_fin_cont);
}

static errval_t
revoke_mark__send(struct intermon_binding *b,
                  intermon_caprep_t *caprep,
                  struct capsend_mc_st *mc_st)
{
    struct revoke_master_st *st;
    ptrdiff_t off = offsetof(struct revoke_master_st, revoke_mc_st);
    st = (struct revoke_master_st*)((uintptr_t)mc_st - off);
    return intermon_capops_revoke_mark__tx(b, NOP_CONT, *caprep, (lvaddr_t)st);
}

void
revoke_mark__rx(struct intermon_binding *b,
                intermon_caprep_t caprep,
                genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;

    struct revoke_slave_st *rvk_st;
    err = calloce(1, sizeof(*rvk_st), &rvk_st);
    PANIC_IF_ERR(err, "allocating revoke slave state");

    rvk_st->from = inter_st->core_id;
    rvk_st->st = st;
    caprep_to_capability(&caprep, &rvk_st->rawcap);

    if (!slaves_head) {
        assert(!slaves_tail);
        slaves_head = slaves_tail = rvk_st;
    }
    else {
        assert(slaves_tail);
        assert(!slaves_tail->next);
        slaves_tail->next = rvk_st;
        slaves_tail = rvk_st;
    }

    // pause any ongoing "delete stepping" as mark phases on other nodes need
    // to delete all foreign copies before we can delete locally owned caps
    delete_steps_pause();

    // XXX: this invocation could create a scheduling hole that could be
    // problematic in RT systems and should probably be done in a loop.
    err = monitor_revoke_mark_relations(&rvk_st->rawcap);
    if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
        // found no copies or descendants of capability on this core,
        // do nothing. -SG
        DEBUG_CAPOPS("no copies on core %d\n", disp_get_core_id());
    } else if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "marking revoke");
    }

    rvk_st->im_qn.cont = revoke_ready__send;
    err = capsend_target(rvk_st->from, (struct msg_queue_elem*)rvk_st);
    PANIC_IF_ERR(err, "enqueing revoke_ready");
}

static void
revoke_ready__send(struct intermon_binding *b,
                   struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct revoke_slave_st *rvk_st = (struct revoke_slave_st*)e;
    err = intermon_capops_revoke_ready__tx(b, NOP_CONT, rvk_st->st);
    PANIC_IF_ERR(err, "sending revoke_ready");
}

void
revoke_ready__rx(struct intermon_binding *b, genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;

    struct revoke_master_st *rvk_st = (struct revoke_master_st*)(lvaddr_t)st;
    if (!capsend_handle_mc_reply(&rvk_st->revoke_mc_st)) {
        DEBUG_CAPOPS("%s: waiting for remote cores\n", __FUNCTION__);
        // multicast not complete
        return;
    }

    DEBUG_CAPOPS("%s ## revocation: commit phase\n", __FUNCTION__);
    err = capsend_relations(&rvk_st->rawcap, revoke_commit__send,
            &rvk_st->revoke_mc_st, &rvk_st->dests);
    PANIC_IF_ERR(err, "enqueing revoke_commit multicast");

    delete_steps_resume();

    struct event_closure steps_fin_cont
        = MKCLOSURE(revoke_master_steps__fin, rvk_st);
    delete_queue_wait(&rvk_st->del_qn, steps_fin_cont);
}

static errval_t
revoke_commit__send(struct intermon_binding *b,
                    intermon_caprep_t *caprep,
                    struct capsend_mc_st *mc_st)
{
    struct revoke_master_st *st;
    ptrdiff_t off = offsetof(struct revoke_master_st, revoke_mc_st);
    st = (struct revoke_master_st*)((char*)mc_st - off);
    return intermon_capops_revoke_commit__tx(b, NOP_CONT, (lvaddr_t)st);
}

void
revoke_commit__rx(struct intermon_binding *b,
                  genvaddr_t st)
{
    assert(slaves_head);
    assert(slaves_tail);
    assert(!slaves_tail->next);

    struct revoke_slave_st *rvk_st = slaves_head;
    while (rvk_st && rvk_st->st != st) { rvk_st = rvk_st->next; }
    assert(rvk_st);

    delete_steps_resume();

    struct event_closure steps_fin_cont
        = MKCLOSURE(revoke_slave_steps__fin, rvk_st);
    delete_queue_wait(&rvk_st->del_qn, steps_fin_cont);
}

static void
revoke_slave_steps__fin(void *st)
{
    errval_t err;
    struct revoke_slave_st *rvk_st = (struct revoke_slave_st*)st;

    rvk_st->im_qn.cont = revoke_done__send;
    err = capsend_target(rvk_st->from, (struct msg_queue_elem*)rvk_st);
    PANIC_IF_ERR(err, "enqueueing revoke_done");
}

inline static void
remove_slave_from_list(struct revoke_slave_st *rvk_st)
{
    // remove from slave list
    if (slaves_head == slaves_tail) {
        // only one element in list
        if (rvk_st == slaves_head) {
            // we're only, clear list
            slaves_head = slaves_tail = 0;
        } else {
            // we're not the element in list??
            printf("rvk_st: %p; head&tail: %p\n", rvk_st, slaves_head);
        }
    } else {
        // more than one element in list
        if (rvk_st == slaves_head) {
            // we're first, remove from head of list
            slaves_head=slaves_head->next;
        } else {
            // we're non-first
            // find prev
            struct revoke_slave_st *p = slaves_head;
            for (;p&&p->next!=rvk_st;p=p->next);
            // make sure we found prev of us
            assert(p&&p->next==rvk_st);
            // remove us
            p->next = rvk_st->next;
            if (rvk_st == slaves_tail) {
                // we were last, set last to prev
                slaves_tail = p;
            }
        }
    }
}

static void
revoke_done__send(struct intermon_binding *b,
                  struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct revoke_slave_st *rvk_st = (struct revoke_slave_st*)e;
    err = intermon_capops_revoke_done__tx(b, NOP_CONT, rvk_st->st);
    PANIC_IF_ERR(err, "sending revoke_done");
    remove_slave_from_list(rvk_st);
    free(rvk_st);
}

void
revoke_done__rx(struct intermon_binding *b,
                genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);

    struct revoke_master_st *rvk_st = (struct revoke_master_st*)(lvaddr_t)st;

    if (!capsend_handle_mc_reply(&rvk_st->revoke_mc_st)) {
        // multicast not complete
        return;
    }

    DEBUG_CAPOPS("%s ## revocation: fin phase\n", __FUNCTION__);
    rvk_st->remote_fin = true;
    if (rvk_st->local_fin) {
        revoke_result__rx(SYS_ERR_OK, rvk_st, true);
    }
}

static void
revoke_master_steps__fin(void *st)
{
    struct revoke_master_st *rvk_st = (struct revoke_master_st*)st;
    rvk_st->local_fin = true;
    if (rvk_st->remote_fin) {
        revoke_result__rx(SYS_ERR_OK, rvk_st, true);
    }
}
