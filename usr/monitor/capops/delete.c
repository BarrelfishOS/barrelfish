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
#include "transport.h"
#include "magic.h"

struct delete_st {
    struct capref capref;
    struct capability cap;
    delete_result_handler_t result_handler;
    void *st;
};

static void delete_result_cont(errval_t status, void *st);

static void
find_core_cont(errval_t status, coreid_t core, void *st)
{
    // called with the result of "find core with cap" when trying to move the
    // last cap
    errval_t err;
    struct delete_st *del_st = (struct delete_st*)st;

    if (err_no(status) == CAP_ERR_NOTFOUND) {
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
        err = move(del_st->capref, core, delete_result_cont, st);
        if (err_is_fail(err)) {
            del_st->result_handler(err, del_st->st);
            free(del_st);
        }
    }
}

static void
delete_result_cont(errval_t status, void *st)
{
    errval_t err = status;
    struct delete_st *del_st = (struct delete_st*)st;

    if (cap_is_moveable(&del_st->cap)) {
        // attempted a move
        if (err_no(err) == CAP_ERR_NOTFOUND) {
            // move failed as dest no longer has cap copy, start from beginning
            err = find_core_with_cap(&del_st->cap, find_core_cont, st);
        }
        if (err_is_fail(err)) {
            del_st->result_handler(err, del_st->st);
            free(del_st);
        }
    }
    else {
        // cap non-moveable, performed a revoke
        if (err_is_ok(err)) {
            // revoke succeeded, delete last copy
            err = monitor_delete_last(del_st->capref);
        }
        del_st->result_handler(err, del_st->st);
        free(del_st);
    }
}

errval_t
delete(struct capref cap, delete_result_handler_t result_handler, void *st)
{
    errval_t err;
    capstate_t state;

    err = cap_get_state(cap, &state);
    if (err_is_fail(err)) {
        return err;
    }

    if (!cap_state_is_valid(state)) {
        return CAP_ERR_BUSY;
    }

    if (!cap_state_is_owner(state)) {
        // non-owner cap, just delete
        return cap_delete(cap);
    }
    else {
        // try a simple delete
        err = cap_delete(cap);
        if (err_no(err) != SYS_ERR_RETRY_THROUGH_MONITOR) {
            return err;
        }

        // simple delete was not able to delete cap as it was last copy and may
        // have remote copies, need to move or revoke cap

        // setup extended delete operation
        err = cap_set_busy(cap);
        if (err_is_fail(err)) {
            return err;
        }
        errval_t err2;

        struct delete_st *del_st = malloc(sizeof(struct delete_st));
        if (!del_st) {
            err = LIB_ERR_MALLOC_FAIL;
            goto cap_set_ready;
        }

        err = monitor_cap_identify(cap, &del_st->cap);
        if (err_is_fail(err)) {
            goto free_del_st;
        }

        if (cap_is_moveable(&del_st->cap)) {
            // if cap is moveable, move ownership so cap can then be deleted
            err = find_core_with_cap(&del_st->cap, find_core_cont, st);
        }
        else {
            // otherwise perform revocation and subsequent delete of last copy
            err = revoke(cap, delete_result_cont, del_st);
        }
        if (err_is_fail(err)) {
            goto free_del_st;
        }

        goto end_cleanup;

free_del_st:
        free(del_st);

cap_set_ready:
        err2 = cap_set_ready(cap);
        if (err_is_fail(err2)) {
            USER_PANIC_ERR(err2, "failed to set cap to ready after delete failure");
        }

end_cleanup:
        return err;
    }
}
