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

struct delete_st {
    struct capref capref;
    struct capability cap;
    delete_result_handler_t result_handler;
    void *st;
};

/*
 * Delete all copies {{{1
 */

/*
 * Request multicast {{{2
 */

struct delete_remote_mc_st {
    struct capsend_mc_st mc_st;
    struct delete_st *del_st;
};

static void
delete_remote_send_cont(struct intermon_binding *b, intermon_caprep_t *caprep, struct capsend_mc_st *st)
{
    errval_t err;
    err = intermon_capops_delete_remote__tx(b, NOP_CONT, *caprep, (genvaddr_t)st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send delete_remote msg");
    }
}

static errval_t
delete_remote(struct capability *cap, struct delete_st *st)
{
    struct delete_remote_mc_st *mc_st = malloc(sizeof(struct delete_remote_mc_st));
    if (!mc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    mc_st->del_st = st;
    return capsend_copies(cap, delete_remote_send_cont, (struct capsend_mc_st*)mc_st);
}

/*
 * Result reply {{{2
 */

struct delete_remote_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    genvaddr_t st;
};

static void
delete_remote_result_msg_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct delete_remote_result_msg_st *msg_st = (struct delete_remote_result_msg_st*)e;
    err = intermon_capops_delete_remote_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send delete_remote_result msg");
    }
    free(msg_st);
}

static errval_t
delete_remote_result(coreid_t dest, errval_t status, genvaddr_t st)
{
    errval_t err;
    struct delete_remote_result_msg_st *msg_st;
    msg_st = calloc(1, sizeof(*msg_st));
    msg_st->queue_elem.cont = delete_remote_result_msg_cont;
    msg_st->status = status;
    msg_st->st = st;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
    }
    return err;
}

/*
 * Receive handlers {{{2
 */

__attribute__((unused))
static void
delete_remote__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err;
    struct capability cap;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
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

    err = monitor_delete_copies(capref);
    cap_destroy(capref);

free_slot:
    slot_free(capref);

send_err:
    err = delete_remote_result(from, err, st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send delete_remote result");
    }
}

__attribute__((unused))
static void
delete_remote_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    if (!capsend_handle_mc_reply(st)) {
        // multicast not complete
        return;
    }

    struct delete_remote_mc_st *mc_st = (struct delete_remote_mc_st*)st;
    struct delete_st *del_st = mc_st->del_st;
    free(mc_st);

    del_st->result_handler(SYS_ERR_OK, del_st->st);
    free(del_st);
}

/*
 * Move & delete handling {{{1
 */

static void move_result_cont(errval_t status, void *st);

static void
find_core_cont(errval_t status, coreid_t core, void *st)
{
    // called with the result of "find core with cap" when trying to move the
    // last cap
    errval_t err;
    struct delete_st *del_st = (struct delete_st*)st;

    if (err_no(status) == SYS_ERR_CAP_NOT_FOUND) {
        // no core with cap exists, delete local cap with cleanup
        err = monitor_delete_last(del_st->capref);
        del_st->result_handler(err, del_st->st);
        free(del_st);
    }
    else if (err_is_fail(status)) {
        // an error occured
        del_st->result_handler(status, del_st->st);
        free(del_st);
    }
    else {
        // core found, attempt move
        err = move(del_st->capref, core, move_result_cont, st);
        if (err_is_fail(err)) {
            del_st->result_handler(err, del_st->st);
            free(del_st);
        }
    }
}

static void
move_result_cont(errval_t status, void *st)
{
    errval_t err = status;
    struct delete_st *del_st = (struct delete_st*)st;
    assert(distcap_is_moveable(del_st->cap.type));

    if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
        // move failed as dest no longer has cap copy, start from beginning
        err = capsend_find_cap(&del_st->cap, find_core_cont, st);
        if (err_is_ok(err)) {
            return;
        }
    }
    else if (err_is_ok(err)) {
        err = monitor_delete_last(del_st->capref);
    }

    del_st->result_handler(err, del_st->st);
    free(del_st);
}

/*
 * Deleting CNode special case {{{1
 */

struct delete_cnode_st {
    struct capref delcap;
    void *st;
};

static void
delete_cnode_slot_result(errval_t status, void *st)
{
    errval_t err;

    struct delete_cnode_st *dst = (struct delete_cnode_st*)st;
    if (err_is_ok(status) || err_no(status) == SYS_ERR_CAP_NOT_FOUND) {
        if (dst->delcap.slot < (1 << dst->delcap.cnode.size_bits)) {
            dst->delcap.slot++;
            err = delete(get_cap_domref(dst->delcap), delete_cnode_slot_result, dst);
        }
        else {
            err = SYS_ERR_OK;
        }
    }
    if (err_is_fail(err)) {
        USER_PANIC_ERR(status, "deleting cnode slot failed");
    }
}

__attribute__((unused))
static errval_t
delete_cnode(struct capref cap, void *st)
{
    errval_t err;

    err = monitor_set_cap_deleted(cap);
    if (err_is_fail(err)) {
        return err;
    }

    struct capability cnode_cap;
    err = monitor_cap_identify(cap, &cnode_cap);
    if (err_is_fail(err)) {
        return err;
    }
    assert(cnode_cap.type == ObjType_CNode);

    struct cnoderef cnode = build_cnoderef(cap, cnode_cap.u.cnode.bits);
    struct delete_cnode_st *dcst = malloc(sizeof(struct delete_cnode_st));
    dcst->delcap.cnode = cnode;
    dcst->delcap.slot = 0;
    dcst->st = st;

    err = delete(get_cap_domref(dcst->delcap), delete_cnode_slot_result, dcst);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "deleting cnode slot failed");
    }
    return err;
}

/*
 * Delete operation
 */

errval_t
delete(struct domcapref cap, delete_result_handler_t result_handler, void *st)
{
    errval_t err;
    distcap_state_t state;

    err = invoke_cnode_get_state(cap.croot, cap.cptr, cap.bits, &state);
    if (err_is_fail(err)) {
        return err;
    }

    if (distcap_is_busy(state)) {
        return MON_ERR_REMOTE_CAP_RETRY;
    }

    // try a simple delete
    err = invoke_cnode_delete(cap.croot, cap.cptr, cap.bits);
    if (err_no(err) != SYS_ERR_RETRY_THROUGH_MONITOR) {
        return err;
    }

    // simple delete was not able to delete cap as it was last copy and may
    // have remote copies, need to move or revoke cap

    // setup extended delete operation
    err = monitor_lock_cap(cap.croot, cap.cptr, cap.bits);
    if (err_is_fail(err)) {
        return err;
    }

    struct delete_st *del_st = malloc(sizeof(struct delete_st));
    if (!del_st) {
        err = LIB_ERR_MALLOC_FAIL;
        goto cap_set_ready;
    }

    err = monitor_domains_cap_identify(cap.croot, cap.cptr, cap.bits, &del_st->cap);
    if (err_is_fail(err)) {
        goto free_del_st;
    }

    if (distcap_is_moveable(del_st->cap.type)) {
        // if cap is moveable, move ownership so cap can then be deleted
        err = capsend_find_cap(&del_st->cap, find_core_cont, st);
    }
    else {
        // otherwise delete all remote copies and then delete last copy
        err = delete_remote(&del_st->cap, del_st);
    }
    if (err_is_fail(err)) {
        goto free_del_st;
    }

    goto end_cleanup;

free_del_st:
    free(del_st);

cap_set_ready:
    caplock_unlock(cap);

end_cleanup:
    return err;
}
