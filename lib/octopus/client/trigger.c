/**
 * \file
 * \brief Handler function for asynchronous triggers sent by server.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>

#include <dist2/init.h>
#include <dist2/trigger.h>

#include "handler.h"
#include "common.h"

void trigger_handler(struct dist2_binding* b, dist2_trigger_id_t id,
        uint64_t t, dist2_mode_t mode, char* record, uint64_t st)
{
    assert(t != 0);
    trigger_handler_fn trigger_fn = (trigger_handler_fn) t;
    void* state = (void*) st;

    if (trigger_fn != NULL) {
        trigger_fn(mode, record, state);
    }
    else {
        fprintf(stderr, "Incoming trigger(%lu) for %s with unset handler function.",
                id, record);
        free(record);
    }
}

dist2_trigger_t dist_mktrigger(errval_t in_case, dist2_binding_type_t send_to,
        dist2_mode_t mode, trigger_handler_fn fn, void* state)
{
    return (dist2_trigger_t) {
                .in_case = in_case,
                .m = mode,
                .send_to = send_to,
                // TODO: bad uint64_t here!
                .trigger = (uint64_t) fn,
                .st = (uint64_t) state
            };
}

/**
 * \brief Removes a trigger in the dist2 server.
 *
 * In any case a valid watch id is specified this
 * causes a trigger event to be sent with the
 * DIST_REMOVED flag set. After this event it's safe
 * to clean up any memory associated with the event handler.
 *
 * \param trigger_id ID of trigger we want to remove
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_INVALID_ID
 */
errval_t dist_remove_trigger(dist2_trigger_id_t trigger_id)
{
    errval_t err = SYS_ERR_OK;
    struct dist2_thc_client_binding_t* cl = dist_get_thc_client();
    assert(cl != NULL);

    errval_t error_code;
    err = cl->call_seq.remove_trigger(cl, trigger_id, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    return err;
}
