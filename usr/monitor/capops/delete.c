/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>
#include "monitor.h"
#include "capops.h"
#include "capsend.h"
#include "caplock.h"
#include "capqueue.h"
#include "dom_invocations.h"
#include "delete_int.h"
#include "internal.h"
#include "ram_alloc.h"
#include <if/mem_defs.h>

struct delete_remote_mc_st {
    struct capsend_mc_st mc_st;
    struct delete_st *del_st;
    errval_t status;
};

struct delete_remote_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    genvaddr_t st;
};

static void delete_trylock_cont(void *st);

static void
delete_result__rx(errval_t status, struct delete_st *del_st, bool locked)
{
    DEBUG_CAPOPS("%s: status=%s, locked=%d\n", __FUNCTION__, err_getcode(status), locked);
    errval_t err;

    if (locked) {
        caplock_unlock(del_st->capref);
    }

    err = slot_free(del_st->newcap);
    if (err_is_fail(err) && err_no(err) != LIB_ERR_SLOT_UNALLOCATED) {
        DEBUG_ERR(err, "freeing reclamation slot, will leak");
    }

    delete_result_handler_t handler = del_st->result_handler;
    void *st = del_st->st;
    free(del_st);
    handler(status, st);
}

void
send_new_ram_cap(struct capref cap)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err, result;

    struct capability cap_data;
    err = monitor_cap_identify(cap, &cap_data);
    assert(err_is_ok(err));
    assert(cap_data.type == ObjType_RAM);
    struct RAM ram = cap_data.u.ram;

    struct ram_alloc_state *ram_alloc_state = get_ram_alloc_state();
    thread_mutex_lock(&ram_alloc_state->ram_alloc_lock);

    struct mem_binding *b = get_mem_client();
    if (!b) {
        DEBUG_CAPOPS("%s: forwarding to monitor.0\n", __FUNCTION__);
        // we're not on core 0, so forward free_monitor msg to monitor.0
        err = mon_ram_free(&cap_data, ram.base, log2ceil(ram.bytes));
        assert(err_is_ok(err));
    } else {
        DEBUG_CAPOPS("%s: we are monitor.0\n", __FUNCTION__);
        // XXX: This should not be an RPC! It could stall the monitor, but
        // we trust mem_serv for the moment.
        err = b->rpc_tx_vtbl.free_monitor(b, cap, ram.base, log2ceil(ram.bytes), &result);
        assert(err_is_ok(err));
        assert(err_is_ok(result));
    }

    thread_mutex_unlock(&ram_alloc_state->ram_alloc_lock);

    // XXX: this seems to happen during the lmp transfer anyway -SG
    if (!b) {
        DEBUG_CAPOPS("%s: not monitor.0, deleting local copy\n", __FUNCTION__);
        // should we do this if not on core 0? -SG
        err = cap_delete(cap);
        assert(err_is_ok(err));
    }
    DEBUG_CAPOPS("%s: finished\n", __FUNCTION__);
}

static void delete_wait__fin(void *st_)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    struct delete_st *st = (struct delete_st*)st_;
    delete_result__rx(SYS_ERR_OK, st, false);
}

static void delete_last(struct delete_st* del_st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    bool locked = true;

    err = monitor_delete_last(del_st->capref.croot, del_st->capref.cptr,
                              del_st->capref.level, del_st->newcap);
    GOTO_IF_ERR(err, report_error);
    if (err_no(err) == SYS_ERR_RAM_CAP_CREATED) {
        DEBUG_CAPOPS("%s: sending reclaimed RAM to memserv.\n", __FUNCTION__);
        send_new_ram_cap(del_st->newcap);
        err = SYS_ERR_OK;
    }

    DEBUG_CAPOPS("%s: deleted last copy\n", __FUNCTION__);
    // at this point the cap has become "unlocked" because it is either deleted
    // or in a clear/delete queue
    locked = false;

    if (!del_st->wait) {
        goto report_error;
    }

    DEBUG_CAPOPS("%s: waiting on delete queue\n", __FUNCTION__);
    delete_queue_wait(&del_st->qn, MKCLOSURE(delete_wait__fin, del_st));

    return;

report_error:
    DEBUG_CAPOPS("%s: reporting error: %s\n", __FUNCTION__,
            err_getstring(err));
    delete_result__rx(err, del_st, locked);
}

/*
 * Non-moveable cap types: deleting all foreign copies when last owned copy of
 * cap is deleted
 */

static errval_t
delete_remote__send(struct intermon_binding *b, intermon_caprep_t *caprep,
                    struct capsend_mc_st *st)
{
    return intermon_capops_delete_remote__tx(b, NOP_CONT, *caprep,
                                             (lvaddr_t)st);
}

static void
delete_remote__enq(struct capability *cap, struct delete_st *st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    struct delete_remote_mc_st *mc_st;

    err = malloce(sizeof(*mc_st), &mc_st);
    GOTO_IF_ERR(err, report_error);
    mc_st->del_st = st;
    mc_st->status = SYS_ERR_OK;

    err = capsend_copies(cap, delete_remote__send,
                         (struct capsend_mc_st*)mc_st);
    GOTO_IF_ERR(err, report_error);

    return;

report_error:
    delete_result__rx(err, st, true);
}

static void
delete_remote_result__send(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct delete_remote_result_msg_st *msg_st = (struct delete_remote_result_msg_st*)e;
    err = intermon_capops_delete_remote_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);

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
    PANIC_IF_ERR(err, "failed to send delete_remote_result msg");
    free(msg_st);
}

static void
delete_remote_result__enq(coreid_t dest, errval_t status, genvaddr_t st)
{
    DEBUG_CAPOPS("%s: dest=%d, status=%s\n", __FUNCTION__, dest, err_getcode(status));
    errval_t err;

    struct delete_remote_result_msg_st *msg_st;
    err = calloce(1, sizeof(*msg_st), &msg_st);
    PANIC_IF_ERR(err, "allocating delete_remote_result st");

    msg_st->queue_elem.cont = delete_remote_result__send;
    msg_st->status = status;
    msg_st->st = st;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    PANIC_IF_ERR(err, "failed to send delete_remote result");
}

void
delete_remote__rx(struct intermon_binding *b, intermon_caprep_t caprep,
                  genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err, err2;
    struct capability cap;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    caprep_to_capability(&caprep, &cap);
    struct capref capref;

    err = slot_alloc(&capref);
    GOTO_IF_ERR(err, send_err);

    err = monitor_copy_if_exists(&cap, capref);
    if (err_is_fail(err)) {
        DEBUG_CAPOPS("%s: monitor_copy_if_exists: %s\n", __FUNCTION__, err_getcode(err));
        if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
            // not found implies there were no copies, so everything is OK
            err = SYS_ERR_OK;
        }
        goto free_slot;
    }

    err = monitor_delete_foreigns(capref);
    DEBUG_CAPOPS("%s: monitor_delete_foreigns: %s\n", __FUNCTION__, err_getcode(err));
    //err = monitor_delete_copies(capref);
    //err2 = cap_delete(capref);
    //DEBUG_IF_ERR(err2, "deleting temp delete_remote cap");
    //if (err_is_ok(err) && err_is_fail(err2)) {
    //    err = err2;
    //}

free_slot:
    err2 = slot_free(capref);
    DEBUG_IF_ERR(err2, "freeing temp delete_remote cap, will leak");

send_err:
    delete_remote_result__enq(from, err, st);
}

void
delete_remote_result__rx(struct intermon_binding *b, errval_t status,
                         genvaddr_t st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    struct delete_remote_mc_st *mc_st = (struct delete_remote_mc_st*)(lvaddr_t)st;
    struct delete_st *del_st = mc_st->del_st;

    // XXX: do something with received errors?
    if (err_is_fail(status)) {
        mc_st->status = status;
    }
    status = mc_st->status;

    if (!capsend_handle_mc_reply(&mc_st->mc_st)) {
        // multicast not complete
        return;
    }

    // multicast is complete, free state
    free(mc_st);

    // unlock cap so it can be deleted
    caplock_unlock(del_st->capref);

    if (err_is_ok(status)) {
        // remote copies have been deleted, reset corresponding relations bit
        err = monitor_domcap_remote_relations(del_st->capref.croot,
                                              del_st->capref.cptr,
                                              del_st->capref.level,
                                              0, RRELS_COPY_BIT, NULL);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "clearing remote descs bit after remote delete");
        }

        // All remote copies deleted, delete local copy; can be last
        err = dom_cnode_delete(del_st->capref);
        errval_t last_owned = err_push(SYS_ERR_DELETE_LAST_OWNED,
                                       SYS_ERR_RETRY_THROUGH_MONITOR);
        // We got DELETE_LAST_OWNED from cpu driver, do delete_last()
        if (err == last_owned) {
            delete_last(del_st);
            // We just assume that delete_last() succeeds
            err = SYS_ERR_OK;
        }
        else if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
            // this shouldn't really happen either, but isn't a problem
            err = SYS_ERR_OK;
        }
        else if (err_is_fail(err)) {
            // other than DELETE_LAST_OWNED, the simple delete should not fail
            // here.
            USER_PANIC_ERR(err, "this really should not happen");
        }
    }
    else {
        err = status;
    }

    delete_result__rx(err, del_st, false);
}

/*
 * Moveable cap type: try to migrate ownership elsewhere
 */

static void move_result_cont(errval_t status, void *st);

static void
find_core_cont(errval_t status, coreid_t core, void *st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    // called with the result of "find core with cap" when trying to move the
    // last cap
    errval_t err = status;
    struct delete_st *del_st = (struct delete_st*)st;

    // unlock cap so it can be manipulated
    caplock_unlock(del_st->capref);

    if (err_no(status) == SYS_ERR_CAP_NOT_FOUND) {
        // no core with cap exists, delete local cap with cleanup
        err = monitor_domcap_remote_relations(del_st->capref.croot,
                                              del_st->capref.cptr,
                                              del_st->capref.level,
                                              0, RRELS_COPY_BIT, NULL);
        if (err_is_fail(err)) {
            if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
                err = SYS_ERR_OK;
            }
            goto report_error;
        }

        delete_last(del_st);
    }
    else if (err_is_fail(status)) {
        // an error occured
        goto report_error;
    }
    else {
        // core found, attempt move
        err = capops_move(del_st->capref, core, move_result_cont, st);
        GOTO_IF_ERR(err, report_error);
    }

    return;

report_error:
    delete_result__rx(err, del_st, false);
}

static void
move_result_cont(errval_t status, void *st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err = status;
    struct delete_st *del_st = (struct delete_st*)st;
    assert(distcap_is_moveable(del_st->cap.type));

    if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
        // the found remote copy has disappeared, restart move process
        delete_trylock_cont(del_st);
    }
    else if (err_is_fail(err)) {
        delete_result__rx(err, del_st, false);
    }
    else {
        // move succeeded, cap is now foreign
        err = dom_cnode_delete(del_st->capref);
        if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
            err = SYS_ERR_OK;
        }
        delete_result__rx(err, del_st, false);
    }
}

/*
 * Delete operation
 */

static void
delete_trylock_cont(void *st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    errval_t err;
    bool locked = false;
    struct delete_st *del_st = (struct delete_st*)st;

    // try a simple delete
    // NOTE: on the first pass, this is done twice (once in the capops_delete
    // entry), but only this function is executed on every unlock event
    err = dom_cnode_delete(del_st->capref);
    if (err_no(err) != SYS_ERR_RETRY_THROUGH_MONITOR) {
        // If cap is already locked, just enqueue for retry
        if (err_no(err) == SYS_ERR_CAP_LOCKED) {
            DEBUG_CAPOPS("%s: from cnode_delete(): cap already locked, queuing retry\n", __FUNCTION__);
            caplock_wait(del_st->capref, &del_st->lock_qn,
                         MKCLOSURE(delete_trylock_cont, del_st));
            return;
        }
        // If cap not found, it has been deleted elsewhere, return OK
        if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
            DEBUG_CAPOPS("%s: from cnode_delete(): cap not found, got deleted from elsewhere\n", __FUNCTION__);
            err = err_push(SYS_ERR_OK, err);
        }
        goto report_error;
    }

    err = monitor_lock_cap(del_st->capref.croot, del_st->capref.cptr,
                           del_st->capref.level);
    if (err_no(err) == SYS_ERR_CAP_LOCKED) {
        DEBUG_CAPOPS("%s: from lock(): cap already locked, queuing retry\n", __FUNCTION__);
        caplock_wait(del_st->capref, &del_st->lock_qn,
                     MKCLOSURE(delete_trylock_cont, del_st));
        return;
    }
    else if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
        DEBUG_CAPOPS("%s: from lock(): cap not found, got deleted from elsewhere\n", __FUNCTION__);
        // Some other operation (another delete or a revoke) has deleted the
        // target cap. This is OK.
        err = err_push(SYS_ERR_OK, err);
        goto report_error;
    }
    else if (err_is_fail(err)) {
        DEBUG_ERR(err, "locking cap for delete");
        goto report_error;
    }
    else {
        locked = true;
    }

    // check if there could be any remote relations
    uint8_t relations;
    err = monitor_domcap_remote_relations(del_st->capref.croot,
                                          del_st->capref.cptr,
                                          del_st->capref.level,
                                          0, 0, &relations);
    GOTO_IF_ERR(err, report_error);

    if (!(relations & RRELS_COPY_BIT)) {
        // no remote relations, proceed with final delete
        DEBUG_CAPOPS("%s: deleting last copy\n", __FUNCTION__);
        delete_last(del_st);
    }
    else if (distcap_is_moveable(del_st->cap.type)) {
        // if cap is moveable, move ownership so cap can then be deleted
        DEBUG_CAPOPS("%s: move ownership\n", __FUNCTION__);
        err = capsend_find_cap(&del_st->cap, find_core_cont, del_st);
        GOTO_IF_ERR(err, report_error);
    }
    else {
        DEBUG_CAPOPS("%s: cap type %d not moveable, delete all copies\n",
                __FUNCTION__, del_st->cap.type);
        // otherwise delete all remote copies and then delete last copy
        delete_remote__enq(&del_st->cap, del_st);
    }

    return;

report_error:
    DEBUG_CAPOPS("%s: reporting error: %s\n", __FUNCTION__, err_getcode(err));
    delete_result__rx(err, del_st, locked);
}

void
capops_delete_int(struct delete_st *del_st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    delete_trylock_cont(del_st);
}

void
capops_delete(struct domcapref cap,
              delete_result_handler_t result_handler,
              void *st)
{
    errval_t err;
    DEBUG_CAPOPS("%s\n", __FUNCTION__);

    // try a simple delete
    DEBUG_CAPOPS("%s: trying simple delete\n", __FUNCTION__);
    err = dom_cnode_delete(cap);
    // We can also continue here if we get SYS_ERR_CAP_LOCKED, as we're going
    // to handle already locked caps correctly in delete_trylock_cont().
    // -SG, 2017-05-02
    if (err_no(err) != SYS_ERR_RETRY_THROUGH_MONITOR &&
        err_no(err) != SYS_ERR_CAP_LOCKED)
    {
        DEBUG_CAPOPS("%s: err != RETRY && err != LOCKED\n", __FUNCTION__);
        goto err_cont;
    }

    // simple delete was not able to delete cap as:
    // * it was last copy and:
    //    - may have remote copies, need to move or revoke cap
    //    - contains further slots which need to be cleared
    // * currently locked

    struct delete_st *del_st;
    err = calloce(1, sizeof(*del_st), &del_st);
    GOTO_IF_ERR(err, err_cont);

    err = monitor_domains_cap_identify(cap.croot, cap.cptr, cap.level,
                                       &del_st->cap);
    GOTO_IF_ERR(err, free_st);

    err = slot_alloc(&del_st->newcap);
    GOTO_IF_ERR(err, free_st);

    del_st->capref = cap;
    del_st->wait = true;
    del_st->result_handler = result_handler;
    del_st->st = st;

    // after this setup is complete, nothing less than a catastrophic failure
    // should stop the delete
    delete_trylock_cont(del_st);
    return;

free_st:
    free(del_st);

err_cont:
    DEBUG_CAPOPS("%s: calling result handler with err=%"PRIuERRV"\n", __FUNCTION__, err);
    result_handler(err, st);
}
