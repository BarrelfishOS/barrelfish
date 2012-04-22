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
#include "magic.h"
#include "caplock.h"
#include "capqueue.h"
#include "dom_invocations.h"
#include "capop_handlers.h"
#include <if/mem_rpcclient_defs.h>

struct delete_st {
    struct event_queue_node qn;
    struct domcapref capref;
    struct capability cap;
    struct capref newcap;
    delete_result_handler_t result_handler;
    void *st;
};

static errval_t
alloc_delete_st(struct delete_st **out_st, struct domcapref cap, delete_result_handler_t result_handler, void *st)
{
    errval_t err;
    assert(st);

    struct delete_st *del_st;
    del_st = calloc(1, sizeof(*del_st));
    if (!del_st) {
        return LIB_ERR_MALLOC_FAIL;
    }

    err = monitor_domains_cap_identify(cap.croot, cap.cptr, cap.bits, &del_st->cap);
    if (err_is_fail(err)) {
        free(del_st);
        return err;
    }

    err = slot_alloc(&del_st->newcap);
    if (err_is_fail(err)) {
        free(del_st);
        return err;
    }

    del_st->capref = cap;
    del_st->result_handler = result_handler;
    del_st->st = st;

    *out_st = del_st;

    return SYS_ERR_OK;
}

static void
free_delete_st(struct delete_st *del_st)
{
    errval_t err;

    err = slot_free(del_st->newcap);
    assert(err_is_ok(err));

    free(del_st);
}

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

static errval_t
delete_remote_send_cont(struct intermon_binding *b, intermon_caprep_t *caprep, struct capsend_mc_st *st)
{
    return intermon_capops_delete_remote__tx(b, NOP_CONT, *caprep, (genvaddr_t)st);
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

void
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

void
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

static void send_new_ram_cap(struct capref cap)
{
    errval_t err, result;

    struct capability cap_data;
    err = monitor_cap_identify(cap, &cap_data);
    assert(err_is_ok(err));
    assert(cap_data.type == ObjType_RAM);
    struct RAM ram = cap_data.u.ram;

    struct ram_alloc_state *ram_alloc_state = get_ram_alloc_state();
    thread_mutex_lock(&ram_alloc_state->ram_alloc_lock);

    struct mem_rpc_client *b = get_mem_client();
    // XXX: This should not be an RPC! It could stall the monitor, but
    // we trust mem_serv for the moment.
    err = b->vtbl.free_monitor(b, cap, ram.base, ram.bits, &result);
    assert(err_is_ok(err));
    assert(err_is_ok(result));

    thread_mutex_unlock(&ram_alloc_state->ram_alloc_lock);

    err = cap_destroy(cap);
    assert(err_is_ok(err));
}

static void
find_core_cont(errval_t status, coreid_t core, void *st)
{
    // called with the result of "find core with cap" when trying to move the
    // last cap
    errval_t err;
    struct delete_st *del_st = (struct delete_st*)st;

    if (err_no(status) == SYS_ERR_CAP_NOT_FOUND) {
        // no core with cap exists, delete local cap with cleanup
        err = monitor_delete_last(del_st->capref.croot, del_st->capref.cptr, del_st->capref.bits, del_st->newcap);
        if (err_no(err) == SYS_ERR_RAM_CAP_CREATED) {
            send_new_ram_cap(del_st->newcap);
        }
        del_st->result_handler(err, del_st->st);
        free_delete_st(del_st);
    }
    else if (err_is_fail(status)) {
        // an error occured
        del_st->result_handler(status, del_st->st);
        free_delete_st(del_st);
    }
    else {
        // unlock cap for move operation
        caplock_unlock(del_st->capref);

        // core found, attempt move
        err = capops_move(del_st->capref, core, move_result_cont, st);
        if (err_is_fail(err)) {
            del_st->result_handler(err, del_st->st);
            free_delete_st(del_st);
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
        // relock cap
        err = monitor_lock_cap(del_st->capref.croot, del_st->capref.cptr >> (CPTR_BITS-del_st->capref.bits),
                               del_st->capref.bits);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "relocking cap after move");
        }

        // move failed as dest no longer has cap copy, start from beginning
        err = capsend_find_cap(&del_st->cap, find_core_cont, st);
        if (err_is_ok(err)) {
            return;
        }
    }
    else if (err_is_ok(err)) {
        // move succeeded, cap is now foreign
        err = dom_cnode_delete(del_st->capref);
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
            err = capops_delete(get_cap_domref(dst->delcap), delete_cnode_slot_result, dst);
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

    err = capops_delete(get_cap_domref(dcst->delcap), delete_cnode_slot_result, dcst);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "deleting cnode slot failed");
    }
    return err;
}

/*
 * Delete operation
 */

static void
delete_trylock_cont(void *st)
{
    errval_t err;
    struct delete_st *del_st = (struct delete_st*)st;

    // setup extended delete operation
    err = monitor_lock_cap(del_st->capref.croot, del_st->capref.cptr, del_st->capref.bits);
    if (err_no(err) == SYS_ERR_CAP_LOCKED) {
        caplock_wait(del_st->capref, &del_st->qn, MKCLOSURE(delete_trylock_cont, del_st));
        return;
    }
    else if (err_is_fail(err)) {
        DEBUG_ERR(err, "locking cap for delete");
        goto report_error;
    }

    if (distcap_is_moveable(del_st->cap.type)) {
        // if cap is moveable, move ownership so cap can then be deleted
        err = capsend_find_cap(&del_st->cap, find_core_cont, del_st);
    }
    else {
        // otherwise delete all remote copies and then delete last copy
        err = delete_remote(&del_st->cap, del_st);
    }
    if (err_is_fail(err)) {
        goto cap_set_ready;
    }

    return;

cap_set_ready:
    caplock_unlock(del_st->capref);

report_error:
    del_st->result_handler(err, del_st->st);
    free(del_st);

}

errval_t
capops_delete(struct domcapref cap, delete_result_handler_t result_handler, void *st)
{
    errval_t err;

    // try a simple delete
    err = dom_cnode_delete(cap);
    if (err_is_ok(err)) {
        result_handler(err, st);
        return SYS_ERR_OK;
    }
    else if (err_no(err) != SYS_ERR_RETRY_THROUGH_MONITOR) {
        return err;
    }

    // simple delete was not able to delete cap as it was last copy and may
    // have remote copies, need to move or revoke cap

    struct delete_st *del_st = NULL;
    err = alloc_delete_st(&del_st, cap, result_handler, st);
    if (err_is_fail(err)) {
        return err;
    }
    assert(del_st);

    delete_trylock_cont(del_st);
    return SYS_ERR_OK;
}
