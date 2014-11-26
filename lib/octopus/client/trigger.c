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
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>

#include <octopus/init.h>
#include <octopus/getset.h>
#include <octopus/trigger.h>
#include <if/octopus_defs.h>
#include <if/octopus_thc.h>

#include "handler.h"
#include "common.h"

void trigger_handler(struct octopus_binding* b, octopus_trigger_id_t id,
        uint64_t t, octopus_mode_t mode, char* record, uint64_t st)
{
    assert(t != 0);

    // XXX: The casting to uintptr_t is for 32-bit archs
    trigger_handler_fn trigger_fn = (trigger_handler_fn) (uintptr_t)t;
    void* state = (void*) (uintptr_t)st;

    if (trigger_fn != NULL) {
        trigger_fn(mode, record, state);
    }
    else {
        fprintf(stderr, "Incoming trigger(%"PRIu64") for %s with unset handler function.",
                id, record);
        free(record);
    }
}

octopus_trigger_t oct_mktrigger(errval_t in_case, octopus_binding_type_t send_to,
        octopus_mode_t mode, trigger_handler_fn fn, void* state)
{
    return (octopus_trigger_t) {
                .in_case = in_case,
                .m = mode,
                .send_to = send_to,
                // TODO: bad uint64_t here!
                .trigger = (uint64_t)(uintptr_t) fn,
                .st = (uint64_t)(uintptr_t) state
            };
}

/**
 * \brief Removes a trigger in the octopus server.
 *
 * In any case a valid watch id is specified this
 * causes a trigger event to be sent with the
 * OCT_REMOVED flag set. After this event it's safe
 * to clean up any memory associated with the event handler.
 *
 * \param trigger_id ID of trigger we want to remove
 *
 * \retval SYS_ERR_OK
 * \retval OCT_INVALID_ID
 */
errval_t oct_remove_trigger(octopus_trigger_id_t trigger_id)
{
    errval_t err = SYS_ERR_OK;
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    assert(cl != NULL);

    errval_t error_code;
    err = cl->call_seq.remove_trigger(cl, trigger_id, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    return err;
}

/**
 * Watches for a query of records and calls the trigger function for
 * all records found and subsequent records registered.
 *
 * \param[in]  query         Records to watch for.
 * \param[in]  event_handler Handler function to call.
 * \param[in]  state         Additional state for handler function
 * \param[out] tid           Trigger id.
 * \retval SYS_ERR_OK        Trigger registered, handler fn called for all
 *                           current records.
 */
errval_t oct_trigger_existing_and_watch(const char* query,
        trigger_handler_fn event_handler, void* state,
        octopus_trigger_id_t* tid)
{
    errval_t error_code;
    char** names = NULL;
    char* output = NULL;
    char* record = NULL; // freed by cpu_change_event
    size_t len = 0;
    octopus_trigger_t t = oct_mktrigger(0, octopus_BINDING_EVENT,
            TRIGGER_ALWAYS, event_handler, state);

    // Get current cores registered in system
    struct octopus_thc_client_binding_t* rpc = oct_get_thc_client();
    errval_t err = rpc->call_seq.get_names(rpc, query,
            t, &output, tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;

    switch(err_no(err)) {
    case SYS_ERR_OK:
        err = oct_parse_names(output, &names, &len);
        if (err_is_fail(err)) {
            goto out;
        }

        for (size_t i=0; i < len; i++) {
            err = oct_get(&record, names[i]);

            switch (err_no(err)) {
            case SYS_ERR_OK:
                event_handler(OCT_ON_SET, record, state);
                break;

            case OCT_ERR_NO_RECORD:
                assert(record == NULL);
                break;

            default:
                DEBUG_ERR(err, "Unable to retrieve core record for %s", names[i]);
                assert(record == NULL);
                break;
            }
        }
        break;
    case OCT_ERR_NO_RECORD:
        err = SYS_ERR_OK; // Overwrite (trigger is set)
        break;

    default:
        // Do nothing (wait for trigger)
        break;
    }

out:
    oct_free_names(names, len);
    free(output);

    return err;
}
