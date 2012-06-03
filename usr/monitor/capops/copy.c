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
#include "magic.h"
#include "internal.h"

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
    errval_t err;
    struct recv_copy_result_msg_st *msg_st = (struct recv_copy_result_msg_st*)e;
    err = intermon_capops_recv_copy_result__tx(b, NOP_CONT, msg_st->status,
                                               msg_st->capaddr, msg_st->vbits,
                                               msg_st->slot, msg_st->st);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send recv_copy_result");
    }
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

/**
 * \brief Shortcut for when a error recv_copy_result needs to be enqueued
 */
inline static errval_t
recv_copy_error_result__enq(coreid_t dest, errval_t status, genvaddr_t st)
{
    return recv_copy_result__enq(dest, status, 0, 0, 0, st);
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
    struct owner_copy_msg_st *msg_st = (struct owner_copy_msg_st*)e;
    struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)(msg_st->st);
    assert(rpc_st);
    errval_t err;

    err = intermon_capops_recv_copy__tx(b, NOP_CONT, msg_st->caprep,
                                        msg_st->owner_relations, msg_st->st);

    // message sent or failed, cleanup message state
    free(msg_st);
    msg_st = NULL;

    if (err_is_ok(err)) {
        // skip rpc cleanup if message sent successfully
        return;
    }
    else {
        // Send failed, report result
        recv_copy_result__rx(b, err, 0, 0, 0, (genvaddr_t)rpc_st);
    }
}

/**
 * \brief An owner_copy is to be enqueued
 */
static errval_t
owner_copy__enq(struct capref capref, struct capability *cap, coreid_t from,
                coreid_t dest, bool give_away,
                copy_result_handler_t result_handler, genvaddr_t st)
{
    errval_t err;

    // create new rpc state to associate return message
    struct cap_copy_rpc_st *rpc_st = malloc(sizeof(struct cap_copy_rpc_st));
    if (!rpc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    rpc_st->st = st;
    rpc_st->from = from;
    rpc_st->cap = capref;
    rpc_st->delete_after = give_away;
    rpc_st->is_last = false;
    rpc_st->result_handler = result_handler;

    // check for special handling of giving away last copy
    uint8_t remote_relations = RRELS_COPY_BIT;
    if (rpc_st->delete_after) {
        uint8_t relations = 0;
        err = monitor_cap_has_relations(capref, RRELS_COPY_BIT, &relations);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "checking for local copies of give_away cap");
        }
        else {
            rpc_st->is_last = !(relations & RRELS_COPY_BIT);
        }
    }
    if (rpc_st->is_last) {
        err = monitor_remote_relations(capref, 0, 0, &remote_relations);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "checking for remote copies of give_away cap");
            rpc_st->is_last = false;
            remote_relations = RRELS_COPY_BIT;
        }
        else {
            rpc_st->is_last = !(remote_relations & RRELS_COPY_BIT);
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
        if (err_is_fail(err)) {
            free(rpc_st);
            return err;
        }
    }

    // if we're performing a "true" give_away, lock the cap locally as the
    // intermediate state will be inconsistent
    if (rpc_st->delete_after && rpc_st->is_last) {
        err = monitor_lock_cap(cap_root, get_cap_addr(capref),
                               get_cap_valid_bits(capref));
        if (err_is_fail(err)) {
            // callers of owner_copy should already check cap lock state
            USER_PANIC_ERR(err, "locking cap for true give_away failed");
        }
        assert(!(remote_relations & RRELS_COPY_BIT));
    }

    // create send state
    struct owner_copy_msg_st *msg_st = calloc(1, sizeof(struct owner_copy_msg_st));
    if (!msg_st) {
        free(rpc_st);
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = owner_copy_send__rdy;
    capability_to_caprep(cap, &msg_st->caprep);
    msg_st->owner_relations = remote_relations;
    msg_st->st = (genvaddr_t)rpc_st;

    // enqueue message
    err = capsend_target(dest, (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        free(rpc_st);
        return err;
    }

    return SYS_ERR_OK;
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
    err = intermon_capops_request_copy__tx(b, NOP_CONT, msg_st->dest, msg_st->caprep, (genvaddr_t)msg_st->st);
    if (err_is_fail(err)) {
        assert(msg_st->st);
        struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)msg_st->st;
        if (rpc_st->result_handler) {
            rpc_st->result_handler(err, 0, 0, 0, (void*)rpc_st->st);
        }
        free(rpc_st);
    }
    free(msg_st);
}

/**
 * \brief A copy request is to be enqueued
 */
static errval_t
request_copy__enq(struct capref capref, coreid_t dest, bool give_away,
                  copy_result_handler_t result_handler, genvaddr_t st)
{
    errval_t err;
    struct capability cap;
    err = monitor_cap_identify(capref, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    // cap is foreign so it must be a type that needs "locality" on a particular core
    assert(distcap_needs_locality(cap.type));

    // create new rpc state to associate return message
    struct cap_copy_rpc_st *rpc_st = malloc(sizeof(struct cap_copy_rpc_st));
    if (!rpc_st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    rpc_st->st = st;
    rpc_st->from = my_core_id;
    rpc_st->delete_after = give_away;
    rpc_st->is_last = false;
    rpc_st->result_handler = result_handler;

    // create send state
    struct request_copy_msg_st *msg_st
        = calloc(1, sizeof(struct request_copy_msg_st));
    if (!msg_st) {
        free(rpc_st);
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->queue_elem.cont = request_copy_send__rdy;
    msg_st->dest = dest;
    capability_to_caprep(&cap, &msg_st->caprep);
    msg_st->st = rpc_st;

    // enqueue message
    err = capsend_owner(get_cap_domref(capref), (struct msg_queue_elem*)msg_st);
    if (err_is_fail(err)) {
        free(msg_st);
        free(rpc_st);
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * Receive copy result {{{1
 */

/**
 * \brief Result from sending copy to dest has been received
 */
void
recv_copy_result__rx(struct intermon_binding *b, errval_t status,
                     capaddr_t capaddr, uint8_t vbits, cslot_t slot,
                     genvaddr_t st)
{
    assert(st);
    errval_t err;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    struct cap_copy_rpc_st *rpc_st = (struct cap_copy_rpc_st*)st;

    if (rpc_st->from != my_core_id) {
        // acting as intermediary, forward to origin
        assert(!rpc_st->result_handler); // must not have a result handler
        assert(rpc_st->delete_after); // also temp cap should be del'd
        assert(!rpc_st->is_last); // and must have other local copies
        err = cap_destroy(rpc_st->cap);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "destroying temp cap for f-to-f copy");
        }
        err = recv_copy_result__enq(rpc_st->from, status, capaddr, vbits, slot,
                                    rpc_st->st);
        if (err_is_fail(err)) {
            DEBUG_ERR(status, "sending copy from owner");
            USER_PANIC_ERR(err, "failed to send recv_copy_result");
        }
    }
    else {
        // origin of copy
        if (rpc_st->delete_after) {
            if (rpc_st->is_last) {
                // a give_away was performed, need to unlock and set new owner
                if (err_is_ok(status)) {
                    // only set new owner if give_away succeeded
                    err = monitor_set_cap_owner(cap_root,
                                                get_cap_addr(rpc_st->cap),
                                                get_cap_valid_bits(rpc_st->cap),
                                                inter_st->core_id);
                    if (err_is_fail(err)) {
                        USER_PANIC_ERR(err, "updating owner after true"
                                       " give_away failed");
                    }
                }
                caplock_unlock(get_cap_domref(rpc_st->cap));
            }
            if (err_is_ok(status)) {
                // this should always succeed either because there are local
                // copies or because the cap is now foreign
                err = cap_destroy(rpc_st->cap);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "cap_destroy after give_away failed");
                }
            }
        }
        // call result handler
        if (rpc_st->result_handler) {
            rpc_st->result_handler(status, capaddr, vbits, slot, (void*)rpc_st->st);
        }
    }
    free(rpc_st);
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
    errval_t err, err2;
    struct intermon_state *inter_st = (struct intermon_state*)b->st;
    coreid_t from = inter_st->core_id;
    assert(from != my_core_id);
    struct capref dest = NULL_CAP;
    struct capability cap;

    caprep_to_capability(&caprep, &cap);

    // get a slot to put the cap
    err = slot_alloc(&dest);
    if (err_is_fail(err)) {
        dest = NULL_CAP;
        goto send_result;
    }

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

    // create a cap from the cap data and owner
    err = monitor_cap_create(dest, &cap, owner);
    if (err_is_fail(err)) {
        // may fail if given owner does not match owner of existing copies
        goto free_slot;
    }

    // if we are now owner we need to set up the relations
    if (owner == my_core_id && owner_relations) {
        err = monitor_remote_relations(dest, owner_relations, ~(uint8_t)0, NULL);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "setting remote rels on copy recv with ownership");
        }
    }

    goto send_result;

free_slot:
    err2 = slot_free(dest);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err2, "slot_free failed, slot will leak");
    }
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
    // NOTE: this function should fail if no copies exist and create a new copy otherwise
    err = slot_alloc(&capref);
    if (err_is_fail(err)) {
        goto send_err;
    }
    err = monitor_copy_if_exists(&cap, capref);
    if (err_is_fail(err)) {
        goto free_slot;
    }
    err = cap_get_state(capref, &state);
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

    if (dest == my_core_id) {
        // tried to send copy to owning core, success!
        err = recv_copy_result__enq(from, SYS_ERR_OK, get_cnode_addr(capref),
                                    get_cnode_valid_bits(capref), capref.slot,
                                    st);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sending result to request_copy sender");
        }
    }
    else {
        // forward copy to destination core
        err = owner_copy__enq(capref, &cap, from, dest, true, NULL, st);
        if (err_is_fail(err)) {
            goto destroy_cap;
        }
    }

    return;

destroy_cap:
    err2 = cap_delete(capref);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err, "handling copy request");
        USER_PANIC_ERR(err2, "deleting received cap");
    }

free_slot:
    err2 = slot_free(capref);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err, "handling copy request");
        USER_PANIC_ERR(err2, "freeing slot for cap recv");
    }

send_err:
    err2 = recv_copy_error_result__enq(from, err, st);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err, "handling copy request");
        USER_PANIC_ERR(err2, "sending error to request_copy sender");
    }
}

/*
 * Copy operation {{{1
 */

errval_t
capops_copy(struct capref capref, coreid_t dest, bool give_away,
            copy_result_handler_t result_handler, void *st)
{
    errval_t err;
    struct capability cap;
    distcap_state_t state;

    // check that cap is valid
    err = cap_get_state(capref, &state);
    if (err_is_fail(err)) {
        return err;
    }
    if (distcap_state_is_busy(state)) {
        return MON_ERR_REMOTE_CAP_RETRY;
    }

    if (dest == my_core_id) {
        // tried to send to self, just create a local copy
        struct capref res;
        err = slot_alloc(&res);
        if (err_is_fail(err)) {
            return err;
        }
        err = cap_copy(capref, res);
        if (err_is_fail(err)) {
            errval_t err2 = slot_free(res);
            if (err_is_fail(err2)) {
                USER_PANIC_ERR(err2, "while freeing slot"
                               " due to local copy failure");
            }
            return err;
        }
        if (give_away) {
            err = cap_delete(capref);
        }

        result_handler(err, get_cnode_addr(res),
                       get_cnode_valid_bits(res), res.slot, st);
        return SYS_ERR_OK;
    }

    if (distcap_state_is_foreign(state)) {
        // sending copy from non-owner, send copy request to owner
        return request_copy__enq(capref, dest, give_away, result_handler,
                                 (genvaddr_t)st);
    }
    else {
        // sending copy from owner
        err = monitor_cap_identify(capref, &cap);
        if (err_is_fail(err)) {
            return err;
        }
        return owner_copy__enq(capref, &cap, my_core_id, dest, give_away,
                               result_handler, (genvaddr_t)st);
    }
}
