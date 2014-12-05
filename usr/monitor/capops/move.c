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

/*
 * RPC State {{{1
 */

struct cap_move_rpc_st {
    // capref to a copy of the cap in question
    struct domcapref capref;
    // caller st
    void *st;
    // reuslt handler
    move_result_handler_t result_handler;
};

/*
 * Move request
 */

struct move_request_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    intermon_caprep_t caprep;
    uint8_t relations;
    struct cap_move_rpc_st *st;
};

static void
move_request_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct move_request_msg_st *msg_st = (struct move_request_msg_st*)e;
    err = intermon_capops_move_request__tx(b, NOP_CONT, msg_st->caprep, msg_st->relations, (lvaddr_t)msg_st->st);
    if (err_is_fail(err)) {
        struct cap_move_rpc_st *rpc_st = (struct cap_move_rpc_st*)msg_st->st;
        if (rpc_st->result_handler) {
            rpc_st->result_handler(err, rpc_st->st);
        }
        free(rpc_st);
    }
    free(msg_st);
}

static errval_t
move_request(struct domcapref capref, struct capability *cap, uint8_t relations, coreid_t dest, move_result_handler_t result_handler, void *st)
{
    errval_t err;

    struct cap_move_rpc_st *rpc_st = malloc(sizeof(struct cap_move_rpc_st));
    if (!rpc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    rpc_st->capref = capref;
    rpc_st->st = st;
    rpc_st->result_handler = result_handler;

    struct move_request_msg_st *msg_st = malloc(sizeof(struct move_request_msg_st));
    if (!msg_st) {
        free(rpc_st);
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = move_request_send_cont;
    capability_to_caprep(cap, &msg_st->caprep);
    msg_st->relations = relations;
    msg_st->st = rpc_st;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        free(rpc_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Move result
 */

struct move_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    genvaddr_t st;
};

static void
move_result_send_cont(struct intermon_binding *b, struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct move_result_msg_st *msg_st = (struct move_result_msg_st*)e;
    err = intermon_capops_move_result__tx(b, NOP_CONT, msg_st->status, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send move_result");
    }
    free(msg_st);
}

static errval_t
move_result(coreid_t dest, errval_t status, genvaddr_t st)
{
    errval_t err;

    struct move_result_msg_st *msg_st = calloc(1, sizeof(struct move_result_msg_st));
    msg_st->queue_elem.cont = move_result_send_cont;
    msg_st->status = status;
    msg_st->st = st;

    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Receive handlers {{{1
 */

static void
free_owner_recv_cap(void *arg)
{
    struct capref *cap = (struct capref*)arg;
    caplock_unlock(get_cap_domref(*cap));
    errval_t err = cap_destroy(*cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "destroying cap receiving ownersihp");
    }
    free(cap);
}

void
move_request__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, uint8_t relations, genvaddr_t st)
{
    errval_t err, send_err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);

    struct capability cap;
    caprep_to_capability(&caprep, &cap);

    struct capref *capref = calloc(1, sizeof(*capref));
    if (!capref) {
        err = LIB_ERR_MALLOC_FAIL;
        goto send_err;
    }

    err = slot_alloc(capref);
    if (err_is_fail(err)) {
        goto free_st;
    }

    struct domcapref domcapref = get_cap_domref(*capref);

    err = monitor_copy_if_exists(&cap, *capref);
    if (err_is_fail(err)) {
        goto free_slot;
    }

    err = monitor_lock_cap(domcapref.croot, domcapref.cptr, domcapref.bits);
    if (err_is_fail(err)) {
        goto destroy_cap;
    }

    err = monitor_set_domcap_owner(domcapref, my_core_id);
    if (err_is_fail(err)) {
        goto unlock_cap;
    }

    err = monitor_domcap_remote_relations(domcapref.croot, domcapref.cptr,
                                          domcapref.bits, relations,
                                          ~(uint8_t)0, NULL);
    if (err_is_fail(err)) {
        goto reset_owner;
    }

    err = capsend_update_owner(domcapref, MKCONT(free_owner_recv_cap, capref));
    if (err_is_fail(err)) {
        goto reset_owner;
    }

    err = SYS_ERR_OK;
    goto send_err;

reset_owner:
    send_err = monitor_set_domcap_owner(domcapref, from);
    if (err_is_fail(send_err)) {
        USER_PANIC_ERR(send_err, "failed to reset owner while handling move failure");
    }

unlock_cap:
    send_err = monitor_unlock_cap(domcapref.croot, domcapref.cptr,
                                  domcapref.bits);
    if (err_is_fail(send_err)) {
        USER_PANIC_ERR(send_err, "failed to unlock cap while handling move failure");
    }

destroy_cap:
    cap_destroy(*capref);

free_slot:
    slot_free(*capref);

free_st:
    free(capref);

send_err:
    send_err = move_result(from, err, st);
    if (err_is_fail(send_err)) {
        USER_PANIC_ERR(send_err, "failed to send error to request_copy sender");
    }
}

void
move_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st)
{
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct cap_move_rpc_st *rpc_st = (struct cap_move_rpc_st*)(lvaddr_t)st;

    caplock_unlock(rpc_st->capref);
    rpc_st->result_handler(status, rpc_st->st);
    free(rpc_st);
}

/*
 * Move operation {{{1
 */

errval_t
capops_move(struct domcapref capref, coreid_t dest, move_result_handler_t result_handler, void *st)
{
    errval_t err;
    distcap_state_t state;

    err = dom_cnode_get_state(capref, &state);
    if (err_is_fail(err)) {
        return err;
    }
    if (distcap_state_is_busy(state)) {
        return MON_ERR_REMOTE_CAP_RETRY;
    }
    if (distcap_state_is_foreign(state)) {
        return MON_ERR_CAP_FOREIGN;
    }

    if (dest == my_core_id) {
        // tried to move to self and already owner, no-op
        if (result_handler) {
            result_handler(SYS_ERR_OK, st);
        }
        return SYS_ERR_OK;
    }

    struct capability cap;
    err = monitor_domains_cap_identify(capref.croot, capref.cptr, capref.bits, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    if (!distcap_needs_locality(cap.type)) {
        // XXX: move doesn't make sense here, but is "OK" result correct?
        return SYS_ERR_OK;
    }
    if (!distcap_is_moveable(cap.type)) {
        return MON_ERR_CAP_MOVE;
    }

    err = monitor_lock_cap(capref.croot, capref.cptr, capref.bits);
    if (err_is_fail(err)) {
        return err;
    }

    uint8_t relations;
    err = monitor_domcap_remote_relations(capref.croot, capref.cptr, capref.bits,
                                          0, 0, &relations);
    if (err_is_fail(err)) {
        caplock_unlock(capref);
        return err;
    }

    err = move_request(capref, &cap, relations, dest, result_handler, st);
    if (err_is_fail(err)) {
        caplock_unlock(capref);
        return err;
    }

    return SYS_ERR_OK;
}
