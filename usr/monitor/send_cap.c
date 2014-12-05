/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <send_cap.h>
#include <capops.h>
#include <barrelfish/debug.h>
#include <monitor_invocations_arch.h>
#include <string.h>
#include "monitor_debug.h"

static void
captx_prepare_copy_result_cont(errval_t status, capaddr_t cnaddr,
                               uint8_t cnbits, cslot_t slot, void *st_)
{
    struct captx_prepare_state *st = (struct captx_prepare_state*)st_;
    if (err_is_ok(status)) {
        st->captx.cnptr = cnaddr;
        st->captx.cnbits = cnbits;
        st->captx.slot = slot;
    }
    intermon_captx_t *tx = err_is_ok(status) ? &st->captx : NULL;
    DEBUG_CAPOPS("%s: st->send_cont = %p\n", __FUNCTION__, st->send_cont);
    st->send_cont(status, st, tx, st->st);
}

void
captx_prepare_send(struct capref cap, coreid_t dest, bool give_away,
                   struct captx_prepare_state *state, captx_send_cont send_cont,
                   void *st)
{
    assert(state);
    assert(send_cont);
    memset(state, 0, sizeof(*state));
    state->send_cont = send_cont;
    state->st = st;
    capops_copy(cap, dest, give_away, captx_prepare_copy_result_cont, state);
}

static errval_t
captx_get_capref(capaddr_t cnaddr, uint8_t cnbits, cslot_t slot,
                 struct capref *ret)
{
    errval_t err;

    if (cnaddr == 0 && cnbits == 0 && slot == 0) {
        // got a null cap, return null capref
        *ret = NULL_CAP;
        return SYS_ERR_OK;
    }

    struct capability cnode_cap;
    err = invoke_monitor_identify_cap(cnaddr, cnbits, &cnode_cap);
    if (err_is_fail(err)) {
        return err;
    }
    if (cnode_cap.type != ObjType_CNode) {
        return SYS_ERR_CNODE_TYPE;
    }

    *ret = (struct capref) {
        .cnode = {
            .address = cnaddr << (CPTR_BITS-cnbits),
            .address_bits = cnbits,
            .size_bits = cnode_cap.u.cnode.bits,
            .guard_size = cnode_cap.u.cnode.guard_size,
        },
        .slot = slot,
    };

    return SYS_ERR_OK;
}

void
captx_handle_recv(intermon_captx_t *captx, struct captx_recv_state *state,
                  captx_recv_cont recv_cont, void *st)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);
    assert(state);
    assert(recv_cont);
    errval_t err;

    struct capref cap;
    err = captx_get_capref(captx->cnptr, captx->cnbits, captx->slot, &cap);

    recv_cont(err, state, cap, st);
}

static void
captx_abort_delete_cont(errval_t status, void *st_)
{
    struct captx_abort_state *st = (struct captx_abort_state*)st_;

    st->abort_cont(status, st, st->st);
}

void
captx_abort_recv(intermon_captx_t *captx, struct captx_abort_state *state,
                 captx_abort_cont abort_cont, void *st)
{
    assert(state);
    assert(abort_cont);
    errval_t err;

    state->abort_cont = abort_cont;
    state->st = st;

    if (!captx->cnptr && !captx->cnbits) {
        assert(!captx->slot);

        state->abort_cont(SYS_ERR_OK, state, state->st);
    }
    else {
        struct capref cap;
        err = captx_get_capref(captx->cnptr, captx->cnbits, captx->slot, &cap);
        if (err_is_fail(err)) {
            state->abort_cont(err, state, state->st);
        }

        capops_delete(get_cap_domref(cap), captx_abort_delete_cont, state);
    }
}
