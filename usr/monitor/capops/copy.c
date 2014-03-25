/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/distcaps.h>
#include <if/intermon_defs.h>
#include "monitor.h"
#include "capops.h"
#include "caplock.h"
#include "capsend.h"
#include "internal.h"

struct cap_copy_rpc_st;

typedef void (*copy_result_recv_fn_t)(errval_t, capaddr_t, uint8_t, cslot_t,
                                      struct cap_copy_rpc_st*);
static void
recv_copy_result__src(errval_t status, capaddr_t capaddr, uint8_t vbits,
                      cslot_t slot, struct cap_copy_rpc_st *rpc_st);
static void
recv_copy_result__fwd(errval_t status, capaddr_t capaddr, uint8_t vbits,
                      cslot_t slot, struct cap_copy_rpc_st *rpc_st);

/*
 * RPC state {{{1
 */

struct cap_copy_rpc_st {
    /// caller/sender st
    genvaddr_t st;
    /// sender if acting as intermediary
    coreid_t from;
    /// cap that is being copied out
    struct capref cap;
    /// copy destination
    coreid_t to;
    /// handler for receiving rpc result
    copy_result_recv_fn_t recv_handler;
    /// result handler if being called directly
    copy_result_handler_t result_handler;
    /// whether the local cap should be deleted when the rpc is complete
    bool delete_after;
    /// cap was last copy on request source (only relevant if delete_after is true)
    bool is_last;
};


/*
 * Copy result {{{1
 */

/**
 * \brief Send state struct for recv_copy_result
 */
struct recv_copy_result_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    errval_t status;
    capaddr_t capaddr;
    uint8_t vbits;
    cslot_t slot;
    genvaddr_t st;
};

/**
 * \brief Intermon is ready to send recv_copy_result
 */
static void
recv_copy_result_send__rdy(struct intermon_binding *b,
                           struct intermon_msg_queue_elem *e)
{
    DEBUG_CAPOPS("recv_copy_result_send__rdy: %p, %p\n", b, e);
    errval_t err;
    struct recv_copy_result_msg_st *msg_st = (struct recv_copy_result_msg_st*)e;
    err = intermon_capops_recv_copy_result__tx(b, NOP_CONT, msg_st->status,
                                               msg_st->capaddr, msg_st->vbits,
                                               msg_st->slot, msg_st->st);
    PANIC_IF_ERR(err, "failed to send recv_copy_result");
    free(msg_st);
}

/**
 * \brief A recv_copy_result needs to be enqueued
 */
static errval_t
recv_copy_result__enq(coreid_t dest, errval_t status, capaddr_t capaddr,
                      uint8_t vbits, cslot_t slot, genvaddr_t st)
{
    errval_t err;
    DEBUG_CAPOPS("recv_copy_result__enq: ->%d, %s\n", dest, err_getstring(status));

    // create send state
    struct recv_copy_result_msg_st *msg_st = calloc(1, sizeof(struct recv_copy_result_msg_st));
    if (!msg_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = recv_copy_result_send__rdy;
    msg_st->status = status;
    msg_st->capaddr = capaddr;
    msg_st->vbits = vbits;
    msg_st->slot = slot;
    msg_st->st = st;

    // enqueue message
    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Copy from owner to dest (possibly as intermediary) {{{1
 */

/**
 * \brief Send state struct for owner_copy
 */
struct owner_copy_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    intermon_caprep_t caprep;
    uint8_t owner_relations;
    genvaddr_t st;
};

/**
 * \brief Intermon is ready to send owner_copy
 */
static void
owner_copy_send__rdy(struct intermon_binding *b,
                     struct intermon_msg_queue_elem *e)
{
    DEBUG_CAPOPS("owner_copy_send__rdy: ->%p, %p\n", b, e);
    struct owner_copy_msg_st *msg_st = (struct owner_copy_msg_st*)e;
    struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)((lvaddr_t)msg_st->st);
    assert(rpc_st);
    errval_t err;

    err = intermon_capops_recv_copy__tx(b, NOP_CONT, msg_st->caprep,
                                        msg_st->owner_relations, msg_st->st);

    if (err_is_fail(err)) {
        // send failed, report result
        rpc_st->recv_handler(err, 0, 0, 0, rpc_st);
    }

    free(msg_st);
    msg_st = NULL;
}

/**
 * \brief An owner_copy is to be enqueued
 */
static void
owner_copy__enq(struct capref capref, struct capability *cap, coreid_t from,
                coreid_t dest, bool give_away,
                copy_result_handler_t result_handler, genvaddr_t st)
{
    DEBUG_CAPOPS("owner_copy__enq: %d->%d, give_away=%d\n", from, dest, give_away);
    errval_t err = SYS_ERR_OK, err2;

    // create new rpc state to associate return message
    struct cap_copy_rpc_st *rpc_st;
    err = malloce(sizeof(struct cap_copy_rpc_st), &rpc_st);
    if (err_is_fail(err)) {
        // rpc_st hasn't been set up so we have to do failure handling manually
        if (from == my_core_id) {
            result_handler(err, 0, 0, 0, (void*)(lvaddr_t)st);
        }
        else {
            err2 = recv_copy_result__enq(from, err, 0, 0, 0, st);
            PANIC_IF_ERR2(err2, "failed to send recv_copy_result",
                          err, "allocating rpc forwarding state on owner");
        }
        return;
    }
    rpc_st->st = st;
    rpc_st->from = from;
    rpc_st->cap = capref;
    rpc_st->to = dest;
    rpc_st->delete_after = give_away;
    rpc_st->is_last = false;
    rpc_st->recv_handler = (from == my_core_id ?
                            recv_copy_result__src :
                            recv_copy_result__fwd);
    rpc_st->result_handler = result_handler;

    uint8_t remote_relations = RRELS_COPY_BIT;

    // XXX: short-circuit out if cap we're sending is null cap
    if (cap->type == ObjType_Null) {
        goto null_shortcircuit;
    }

    // check for special handling of giving away last copy
    if (rpc_st->delete_after) {
        uint8_t relations = 0;
        err = monitor_cap_has_relations(capref, RRELS_COPY_BIT, &relations);
        PANIC_IF_ERR(err, "checking for local copies of give_away cap");
        rpc_st->is_last = !(relations & RRELS_COPY_BIT);
        remote_relations = relations;
    }
    if (rpc_st->is_last) {
        uint8_t relations = 0;
        err = monitor_remote_relations(capref, 0, 0, &relations);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "checking for remote copies of give_away cap");
            rpc_st->is_last = false;
            remote_relations |= RRELS_COPY_BIT;
        }
        else {
            rpc_st->is_last = !(remote_relations & RRELS_COPY_BIT);
            remote_relations |= relations;
        }
    }

    /*
     * Here, we're handling the give_away parameter. If set, the copy operation
     * is also responsible for deleting the input cap. In the case when the
     * input cap has no other copies, this allows the copy operation to
     * optimize transfer across cores by simultaneously passing over ownership
     * (this is called a "true" give away below).
     */

    // unless we're performing a "true" give_away, set the remote relations
    // copy bit
    if (!(rpc_st->delete_after && rpc_st->is_last)) {
        err = monitor_remote_relations(capref, RRELS_COPY_BIT, RRELS_COPY_BIT, NULL);
        GOTO_IF_ERR(err, err_cont);
    }

    // if we're performing a "true" give_away, lock the cap locally as the
    // intermediate state will be inconsistent
    if (rpc_st->delete_after && rpc_st->is_last) {
        struct domcapref domcapref = get_cap_domref(capref);
        err = monitor_lock_cap(domcapref.croot, domcapref.cptr,
                               domcapref.bits);
        // callers of owner_copy should already check cap lock state
        PANIC_IF_ERR(err, "locking cap for true give_away failed");
        assert(!(remote_relations & RRELS_COPY_BIT));
    }

    // create send state
    struct owner_copy_msg_st *msg_st;
null_shortcircuit:
    err = calloce(1, sizeof(struct owner_copy_msg_st), &msg_st);
    GOTO_IF_ERR(err, err_cont);

    msg_st->queue_elem.cont = owner_copy_send__rdy;
    capability_to_caprep(cap, &msg_st->caprep);
    msg_st->owner_relations = remote_relations;
    msg_st->st = (lvaddr_t)rpc_st;

    // enqueue message
    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    GOTO_IF_ERR(err, free_msg_st);

    return;

free_msg_st:
    free(msg_st);

err_cont:
    rpc_st->recv_handler(err, 0, 0, 0, rpc_st);
}

/*
 * Copy request from non-owner to owner {{{1
 */

/**
 * \brief Send state struct for request_copy
 */
struct request_copy_msg_st {
    struct intermon_msg_queue_elem queue_elem;
    intermon_caprep_t caprep;
    coreid_t dest;
    struct cap_copy_rpc_st *st;
};

/**
 * \brief Intermon is ready to send request_copy
 */
static void
request_copy_send__rdy(struct intermon_binding *b,
                       struct intermon_msg_queue_elem *e)
{
    errval_t err;
    struct request_copy_msg_st *msg_st = (struct request_copy_msg_st*)e;
    err = intermon_capops_request_copy__tx(b, NOP_CONT, msg_st->dest,
                                           msg_st->caprep,
                                           (lvaddr_t)msg_st->st);
    if (err_is_fail(err)) {
        assert(msg_st->st);
        struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)msg_st->st;
        rpc_st->recv_handler(err, 0, 0, 0, rpc_st);
    }

    free(msg_st);
}

/**
 * \brief A copy request is to be enqueued
 */
static void
request_copy__enq(struct capref capref, coreid_t dest, bool give_away,
                  copy_result_handler_t result_handler, void *st)
{
    errval_t err = SYS_ERR_OK;
    struct capability cap;
    err = monitor_cap_identify(capref, &cap);
    GOTO_IF_ERR(err, cont);

    // cap is foreign so it must be a type that needs "locality" on a particular core
    assert(distcap_needs_locality(cap.type));

    // create new rpc state to associate return message
    struct cap_copy_rpc_st *rpc_st;
    err = malloce(sizeof(struct cap_copy_rpc_st), &rpc_st);
    if (err_is_fail(err)) {
        result_handler(err, 0, 0, 0, st);
        return;
    }
    rpc_st->st = (lvaddr_t)st;
    rpc_st->from = my_core_id;
    rpc_st->delete_after = give_away;
    rpc_st->is_last = false;
    rpc_st->recv_handler = recv_copy_result__src;
    rpc_st->result_handler = result_handler;

    // create send state
    struct request_copy_msg_st *msg_st;
    err = calloce(1, sizeof(struct request_copy_msg_st), &msg_st);
    GOTO_IF_ERR(err, free_rpc_st);
    msg_st->queue_elem.cont = request_copy_send__rdy;
    msg_st->dest = dest;
    capability_to_caprep(&cap, &msg_st->caprep);
    msg_st->st = rpc_st;

    // enqueue message
    err = capsend_owner(get_cap_domref(capref), (struct msg_queue_elem*)msg_st);
    GOTO_IF_ERR(err, free_msg_st);

    return;

free_msg_st:
    free(msg_st);

free_rpc_st:
    free(rpc_st);

cont:
    result_handler((err), 0, 0, 0, st);
}

/*
 * Receive copy result {{{1
 */

static void
recv_copy_result__fwd(errval_t status, capaddr_t capaddr, uint8_t vbits,
                      cslot_t slot, struct cap_copy_rpc_st *rpc_st)
{
    // acting as intermediary, forward to origin
    errval_t err;

    assert(!rpc_st->result_handler); // must not have a result handler
    assert(rpc_st->delete_after); // also temp cap should be del'd
    assert(!rpc_st->is_last); // and must have other local copies

    err = cap_destroy(rpc_st->cap);
    PANIC_IF_ERR(err, "destroying temp cap for f-to-f copy");
    err = recv_copy_result__enq(rpc_st->from, status, capaddr, vbits, slot,
                                rpc_st->st);
    PANIC_IF_ERR2(err, "failed to send recv_copy_result",
                  status, "sending copy from owner");

    free(rpc_st);
}

static void
recv_copy_result__src(errval_t status, capaddr_t capaddr, uint8_t vbits,
                      cslot_t slot, struct cap_copy_rpc_st *rpc_st)
{
    DEBUG_CAPOPS("recv_copy_result__src: %s\n", err_getstring(status));
    // origin of copy
    errval_t err;

    if (err_is_ok(status) && !capref_is_null(rpc_st->cap)) {
        if (rpc_st->delete_after) {
            DEBUG_CAPOPS("deleting our copy\n");
            if (rpc_st->is_last) {
                DEBUG_CAPOPS("our copy is last\n");
                // a give_away was performed, need to unlock and set new owner
                err = monitor_set_cap_owner(cap_root,
                                            get_cap_addr(rpc_st->cap),
                                            get_cap_valid_bits(rpc_st->cap),
                                            rpc_st->to);
                PANIC_IF_ERR(err, "updating owner after true"
                             " give_away failed");
                caplock_unlock(get_cap_domref(rpc_st->cap));
            }
            // this should always succeed either because there are local
            // copies or because the cap is now foreign
            err = cap_destroy(rpc_st->cap);
            PANIC_IF_ERR(err, "cap_destroy after give_away failed");
        }
    }
    // call result handler
    DEBUG_CAPOPS("result_handler: %p\n", rpc_st->result_handler);
    if (rpc_st->result_handler) {
        rpc_st->result_handler(status, capaddr, vbits, slot, (void*)((lvaddr_t)rpc_st->st));
    }

    free(rpc_st);
}

/**
 * \brief Result from sending copy to dest has been received
 */
void
recv_copy_result__rx(struct intermon_binding *b, errval_t status,
                     capaddr_t capaddr, uint8_t vbits, cslot_t slot,
                     genvaddr_t st)
{
    assert(st);
    DEBUG_CAPOPS("recv_copy_result__rx: %p, %s\n", b, err_getstring(status));
    struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)((lvaddr_t)st);
    rpc_st->recv_handler(status, capaddr, vbits, slot, rpc_st);
}

/*
 * Receive cap copy {{1
 */

/**
 * \brief A cap has been received
 */
void
recv_copy__rx(struct intermon_binding *b, intermon_caprep_t caprep,
              uint8_t owner_relations, genvaddr_t st)
{
    DEBUG_CAPOPS("recv_copy__rx: %p, %"PRIu8"\n", b, owner_relations);
    debug_print_caprep(&caprep);
    errval_t err, err2;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct capref dest = NULL_CAP;
    struct capability cap;

    caprep_to_capability(&caprep, &cap);

    if (cap.type == ObjType_Null) {
        // short-circuit null-cap transfer
        err = SYS_ERR_OK;
        goto zero_slot;
    }

    // get a slot to put the cap
    err = slot_alloc(&dest);
    GOTO_IF_ERR(err, zero_slot);

    coreid_t owner;
    if (distcap_needs_locality(cap.type)) {
        if (owner_relations & RRELS_COPY_BIT) {
            // if cap needs locality and ownership is not being transferred,
            // message source is owner
            owner = from;
        }
        else {
            // if there are no remote copies, ownership is being transferred to
            // this core
            owner = my_core_id;
        }
    }
    else {
        // otherwise every core is owner
        owner = my_core_id;
    }

    // create a cap from the cap data and owner. may fail if given owner does
    // not match owner of existing copies
    err = monitor_cap_create(dest, &cap, owner);
    GOTO_IF_ERR(err, free_slot);

    // if we are now owner we need to set up the relations
    if (owner == my_core_id && owner_relations) {
        err = monitor_remote_relations(dest, owner_relations, ~(uint8_t)0, NULL);
        PANIC_IF_ERR(err, "setting remote rels on copy recv with ownership");
    }

    goto send_result;

free_slot:
    err2 = slot_free(dest);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err2, "slot_free failed, slot will leak");
    }

zero_slot:
    dest = NULL_CAP;

send_result:
    err2 = recv_copy_result__enq(from, err, get_cnode_addr(dest),
                                 get_cnode_valid_bits(dest), dest.slot, st);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "recv_copy_result enque failed, cap will leak");
    }
}

/**
 * \brief A copy request has been received
 */
void
request_copy__rx(struct intermon_binding *b, coreid_t dest,
                 intermon_caprep_t caprep, genvaddr_t st)
{
    errval_t err, err2;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct capref capref = NULL_CAP;
    memset(&capref, 0, sizeof(capref));
    struct capability cap;
    caprep_to_capability(&caprep, &cap);
    distcap_state_t state;

    // copy requests should never happen for types that don't need locality,
    // since every core is owner and can copy directly
    assert(distcap_needs_locality(cap.type));

    // find and validate cap
    err = slot_alloc(&capref);
    GOTO_IF_ERR(err, send_err);
    err = monitor_copy_if_exists(&cap, capref);
    GOTO_IF_ERR(err, free_slot);
    err = cap_get_state(capref, &state);
    GOTO_IF_ERR(err, destroy_cap);
    if (distcap_state_is_foreign(state)) {
        err = MON_ERR_CAP_FOREIGN;
        goto destroy_cap;
    }
    if (distcap_state_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto destroy_cap;
    }

    if (dest == my_core_id) {
        // tried to send copy to owning core, success!
        err = recv_copy_result__enq(from, SYS_ERR_OK, get_cnode_addr(capref),
                                    get_cnode_valid_bits(capref), capref.slot,
                                    st);
        PANIC_IF_ERR(err, "sending result to request_copy sender");
    }
    else {
        // forward copy to destination core
        owner_copy__enq(capref, &cap, from, dest, true, NULL, st);
    }

    return;

destroy_cap:
    err2 = cap_delete(capref);
    PANIC_IF_ERR2(err2, "deleting received cap",
                  err, "handling copy request");

free_slot:
    err2 = slot_free(capref);
    PANIC_IF_ERR2(err2, "freeing slot for cap recv",
                  err, "handling copy request");

send_err:
    err2 = recv_copy_result__enq(from, err, 0, 0, 0, st);
    PANIC_IF_ERR2(err2, "sending error to request_copy sender",
                  err, "handling copy request");
}

/*
 * Copy operation {{{1
 */

void
capops_copy(struct capref capref, coreid_t dest, bool give_away,
            copy_result_handler_t result_handler, void *st)
{
#if defined(DEBUG_MONITOR_CAPOPS)
    char buf[256];
    debug_print_capref(buf, 256, capref);
    DEBUG_CAPOPS("capops_copy: dest=%d, give_away=%d, capref=%s\n", dest, give_away, buf);
#endif
    errval_t err, err2;
    struct capability cap;
    distcap_state_t state;

    // short-circuit out with null capref
    if (capref_is_null(capref)) {
        memset(&cap, sizeof(cap), 0);
        cap.type = ObjType_Null;
        owner_copy__enq(capref, &cap, my_core_id, dest, give_away,
                result_handler, (lvaddr_t)st);
        return;
    }

    // check that cap is valid
    err = cap_get_state(capref, &state);
    GOTO_IF_ERR(err, err_cont);
    if (distcap_state_is_busy(state)) {
        err = MON_ERR_REMOTE_CAP_RETRY;
        goto err_cont;
    }

    // get internal cap representation
    err = monitor_cap_identify(capref, &cap);
    GOTO_IF_ERR(err, err_cont);

    if (dest == my_core_id && !(cap.type == ObjType_Null)) {
        // tried to send to self, just create a local copy
        struct capref res;
        err = slot_alloc(&res);
        GOTO_IF_ERR(err, err_cont);

        err = cap_copy(capref, res);
        if (err_is_fail(err)) {
            err2 = slot_free(res);
            PANIC_IF_ERR(err2, "while freeing slot due to local copy failure");
            goto err_cont;
        }

        if (give_away) {
            err2 = cap_delete(capref);
            DEBUG_IF_ERR(err2, "delete for give away failed, cap will leak");
        }

        result_handler(err, get_cnode_addr(res),
                       get_cnode_valid_bits(res), res.slot, st);
    } else if (distcap_state_is_foreign(state) && distcap_needs_locality(cap.type)) {
        DEBUG_CAPOPS("capops_copy: sending copy from non-owner, forward request to owner\n");

        // sending copy from non-owner, send copy request to owner
        request_copy__enq(capref, dest, give_away, result_handler, st);
    } else {
        DEBUG_CAPOPS("capops_copy: sending copy from here/owner\n");

        // sending copy from here/owner
        owner_copy__enq(capref, &cap, my_core_id, dest, give_away,
                        result_handler, (lvaddr_t)st);
    }

    return;

err_cont:
    result_handler(err, 0, 0, 0, st);
}
