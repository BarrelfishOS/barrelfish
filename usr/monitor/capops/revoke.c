/*
 * Copyright (c) 2012, 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/cap_predicates.h>
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
    size_t pending_agreements;
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
    size_t pending_agreements;
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

//static errval_t capops_revoke_subscribe()


struct revoke_register_st
{
    struct monitor_client_req req;
    struct revoke_register_st *next, **prev_next;
    struct monitor_binding *subscriber;
    genpaddr_t base;
    genpaddr_t limit;
    bool acked;
    bool notified;
    struct event_closure cont;
};

struct revoke_register_st *revoke_subs = NULL;

static inline void revoke_subs_add(struct revoke_register_st  *rvk_st)
{
    rvk_st->next = revoke_subs;

    if (revoke_subs) {
        revoke_subs->prev_next = &rvk_st->next;
    }
    rvk_st->prev_next = &revoke_subs;
    revoke_subs = rvk_st;
}

static inline void revoke_subs_remove(struct revoke_register_st  *rvk_st)
{
    if (rvk_st->next) {
        rvk_st->next->prev_next = rvk_st->prev_next;
    }

    *rvk_st->prev_next = rvk_st->next;
}

static inline struct revoke_register_st *revoke_subs_lookup_by_id(uintptr_t id)
{
    struct revoke_register_st *rvk_st = revoke_subs;
    while(rvk_st) {
        if (rvk_st->req.reqid == id) {
            return rvk_st;
        }
        rvk_st = rvk_st->next;
    }
}

static struct revoke_register_st *
revoke_subs_remove_by_range(genpaddr_t base, genpaddr_t limit,
                            struct revoke_register_st *curr)
{
    if (curr == NULL) {
        curr = revoke_subs;
    } else {
        curr = curr->next;
    }

    struct revoke_register_st *rvk_st = curr;
    while(rvk_st) {
        if (rvk_st->base <= base && rvk_st->limit >= limit) {
            assert(rvk_st->prev_next);
            *rvk_st->prev_next = rvk_st->next;
            return rvk_st;
        }
        rvk_st = rvk_st->next;
    }
    return NULL;
}


static void cap_revoke_response(struct monitor_binding *sub, uintptr_t id)
{
    struct monitor_state *mst = sub->st;

    if (mst->reqs == NULL) {
        DEBUG_CAPOPS("Received message but no outstanding requests???");
        assert(mst->reqs == NULL);
    }

    struct monitor_client_req *reqs = mst->reqs;
    struct monitor_client_req **prev_next = &mst->reqs;
    while(reqs) {
        if (reqs->reqid == id) {
            *prev_next = reqs->next;
            break;
        }
        prev_next = &reqs->next;
        reqs = reqs->next;
    }

    struct revoke_register_st *rvk_st = (struct revoke_register_st *)reqs;
    rvk_st->cont.handler(rvk_st);
}

errval_t capops_revoke_register_subscribe(struct capability *cap, uintptr_t id,
                                          struct monitor_binding *subscriber)
{
    assert(cap);
    assert(subscriber);

    struct revoke_register_st *rvk_st = calloc(1, sizeof(*rvk_st));
    if (rvk_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    rvk_st->req.reqid = id;
    rvk_st->subscriber = subscriber;

    subscriber->rx_vtbl.cap_revoke_response = cap_revoke_response;

    /* set the ranges */
    rvk_st->base = get_address(cap);
    rvk_st->limit = rvk_st->base + get_size(cap) - 1;

    DEBUG_CAPOPS("%s:%u: cap=[%" PRIxGENPADDR "..%" PRIxGENPADDR "], id=%"
                  PRIuPTR " sub=%p\n", __FUNCTION__, __LINE__,
                 rvk_st->base, rvk_st->limit, id, subscriber);

     /* add it to the subscribers */
    revoke_subs_add(rvk_st);

    return SYS_ERR_OK;
}



#if 0
static void new_monitor_binding_reply_handler(struct monitor_binding *b,
                                              struct monitor_msg_queue_elem *e)
{
    struct new_monitor_binding_reply_state *st =
            (struct new_monitor_binding_reply_state *)e;
    new_monitor_binding_reply_cont(b, st->args.err, st->args.ep, st->args.st);
    free(st);
}
#endif

static void
revoke_agreement_request_cont(struct monitor_binding *b,
                              struct revoke_register_st *st)
{

    errval_t err = b->tx_vtbl.cap_revoke_request(b, NOP_CONT, 0, st->req.reqid);
    assert(err_is_ok(err));
    #if 0
    b->tx_vtbl.new_monitor_binding_reply(b, NOP_CONT, reterr, retcap, st);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct monitor_state *ms = b->st;
            struct new_monitor_binding_reply_state *me =
                    malloc(sizeof(struct new_monitor_binding_reply_state));
            assert(me != NULL);
            me->args.err = reterr;
            me->args.ep = retcap;
            me->args.st = st;
            me->elem.cont = new_monitor_binding_reply_handler;
            err = monitor_enqueue_send(b, &ms->queue,
                                       get_default_waitset(), &me->elem.queue);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor_enqueue_send failed");
            }
            return;
        }

        USER_PANIC_ERR(err, "failed to send new_monitor_binding_reply");
    }
    #endif
}

static void revoke_master_cont(void *arg)
{
    errval_t err;
    struct revoke_register_st *st = arg;
    struct revoke_master_st *rvk_st = st->cont.arg;

    free(arg);

    rvk_st->pending_agreements--;
    if (rvk_st->pending_agreements) {
        return;
    }

    /* continue with the protocol */
    DEBUG_CAPOPS("%s ## revocation: commit phase\n", __FUNCTION__);
    err = capsend_relations(&rvk_st->rawcap, revoke_commit__send,
                            &rvk_st->revoke_mc_st, &rvk_st->dests);
    PANIC_IF_ERR(err, "enqueing revoke_commit multicast");

    delete_steps_resume();

    struct event_closure steps_fin_cont
            = MKCLOSURE(revoke_master_steps__fin, rvk_st);
    delete_queue_wait(&rvk_st->del_qn, steps_fin_cont);

}

static void revoke_slave_cont(void *arg)
{
    errval_t err;
    struct revoke_register_st *st = arg;
    struct revoke_slave_st *rvk_st = st->cont.arg;

    free(arg);

    rvk_st->pending_agreements--;
    if (rvk_st->pending_agreements) {
        return;
    }

    DEBUG_CAPOPS("### %s:%u continue with the protocol\n",
                 __FUNCTION__, __LINE__);

    /* continue with the protocol */
    rvk_st->im_qn.cont = revoke_ready__send;
    err = capsend_target(rvk_st->from, (struct msg_queue_elem*)rvk_st);
    PANIC_IF_ERR(err, "enqueing revoke_ready");
}

/*
 * TODO: the following two functions essentially do the same thing, but with
 *       a different revoke state (slave or master)
 */

static bool capops_revoke_requires_agreement_local(struct revoke_master_st *rvk_st)
{

    if (revoke_subs == NULL) {
        return false;
    }

    genpaddr_t base = get_address(&rvk_st->rawcap);
    gensize_t limit = base + get_size(&rvk_st->rawcap) - 1;

    DEBUG_CAPOPS("%s:%u: cap=[%" PRIxGENPADDR "..%" PRIxGENPADDR "]\n",
                 __FUNCTION__, __LINE__, base, limit);

    struct revoke_register_st *st = revoke_subs_remove_by_range(base, limit, NULL);
    if (st == NULL) {
        DEBUG_CAPOPS("### %s:%u no matching request\n",
                     __FUNCTION__, __LINE__);
        return false;
    }

    while(st != NULL) {
        if (st->notified) {
            /* don't notify two times, shouldn't actually happen */
            st = revoke_subs_remove_by_range(base, limit, st);
            continue;
        }

        struct monitor_binding *b = st->subscriber;
        struct monitor_state *mst = b->st;

        st->req.next = mst->reqs;
        mst->reqs = &st->req;
        st->notified = true;
        st->cont.arg = rvk_st;
        st->cont.handler = revoke_master_cont;
        rvk_st->pending_agreements++;
        revoke_agreement_request_cont(b, st);

        st = revoke_subs_remove_by_range(base, limit, st);
    }

    return true;
}


static bool capops_revoke_requires_agreement_relations(struct revoke_slave_st *rvk_st)
{
    if (revoke_subs == NULL) {
        return false;
    }

    genpaddr_t base = get_address(&rvk_st->rawcap);
    gensize_t limit = base + get_size(&rvk_st->rawcap) - 1;


    DEBUG_CAPOPS("%s:%u: cap=[%" PRIxGENPADDR "..%" PRIxGENPADDR "]\n",
                 __FUNCTION__, __LINE__, base, limit);

    struct revoke_register_st *st = revoke_subs_remove_by_range(base, limit, NULL);
    if (st == NULL) {
        DEBUG_CAPOPS("### %s:%u no matching request\n",
                     __FUNCTION__, __LINE__);
        return false;
    }

    while(st != NULL) {
        if (st->notified) {
            /* don't notify two times, should'nt actually happen */
            st = revoke_subs_remove_by_range(base, limit, st);
            continue;
        }

        struct monitor_binding *b = st->subscriber;
        struct monitor_state *mst = b->st;

        st->req.next = mst->reqs;
        mst->reqs = &st->req;
        st->notified = true;
        st->cont.arg = rvk_st;
        st->cont.handler = revoke_slave_cont;
        rvk_st->pending_agreements++;
        revoke_agreement_request_cont(b, st);

        st = revoke_subs_remove_by_range(base, limit, st);
    }

    return true;
}



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
        DEBUG_CAPOPS("%s MON_ERR_REMOTE_CAP_RETRY\n", __FUNCTION__);
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto report_error;
    }

    struct revoke_master_st *rst;
    err = calloce(1, sizeof(*rst), &rst);
    GOTO_IF_ERR(err, report_error);
    rst->cap = cap;
    err = monitor_domains_cap_identify(cap.croot, cap.cptr, cap.level, &rst->rawcap);
    GOTO_IF_ERR(err, free_st);
    rst->result_handler = result_handler;
    rst->st = st;

    if (distcap_state_is_foreign(state)) {
        // need to retrieve ownership
        DEBUG_CAPOPS("%s getting cap ownership\n", __FUNCTION__);
        capops_retrieve(rst->cap, revoke_retrieve__rx, rst);
    }
    else {
        if (num_monitors_ready_for_capops() == 1) {
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
                                              st->cap.level, 0, RRELS_COPY_BIT,
                                              NULL);
        if (err_is_fail(err) && err_no(err) != SYS_ERR_CAP_NOT_FOUND) {
            DEBUG_ERR(err, "resetting remote copies bit after revoke");
        }
    }

    DEBUG_CAPOPS("%s ## revocation completed, calling %p\n", __FUNCTION__,
                 st->result_handler);

    err = cap_destroy(st->cap.croot);
    PANIC_IF_ERR(err, "deleting monitor's copy of rootcn");
    st->result_handler(result, st->st);
    free(st);
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
                                     st->cap.level);
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
    assert(num_monitors_ready_for_capops() == 1);

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
                                     st->cap.level);
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

    if (capops_revoke_requires_agreement_relations(rvk_st)) {
        return;
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

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        DEBUG_CAPOPS("%s: got FLOUNDER_ERR_TX_BUSY; requeueing msg.\n", __FUNCTION__);
        struct intermon_state *inter_st = (struct intermon_state *)b->st;
        // requeue send request at front and return
        err = intermon_enqueue_send_at_front(b, &inter_st->queue, b->waitset,
                                             (struct msg_queue_elem *)e);
        GOTO_IF_ERR(err, handle_err);
        return;
    }

handle_err:
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

    if (capops_revoke_requires_agreement_local(rvk_st)) {
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

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        DEBUG_CAPOPS("%s: got FLOUNDER_ERR_TX_BUSY; requeueing msg.\n", __FUNCTION__);
        struct intermon_state *inter_st = (struct intermon_state *)b->st;
        // requeue send request at front and return
        err = intermon_enqueue_send_at_front(b, &inter_st->queue, b->waitset,
                                             (struct msg_queue_elem *)e);
        GOTO_IF_ERR(err, handle_err);
        return;
    }

handle_err:
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
