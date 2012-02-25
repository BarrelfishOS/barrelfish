/**
 * \file
 * \brief Semaphore API Implementation
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <dist2/init.h>
#include <dist2/getset.h>
#include <dist2/trigger.h>
#include <dist2/lock.h>
#include <dist2/semaphores.h>

#include "common.h"

static uint32_t get_next_id(void)
{
    uint64_t id = 0;
    char* lock_record = NULL;
    char* record = NULL;

    // Find a valid ID for our next semaphore

    // This lock makes sure that we don't
    // have concurrent access to sem.ids
    errval_t err = dist_lock("sem.lock", &lock_record);
    assert(err_is_ok(err));

    err = dist_get(&record, "sem.ids { current_id: _ }");
    if (err_is_ok(err)) {
        err = dist_read(record, "_ { current_id: %d }", &id);
        assert(err_is_ok(err));
    }
    else if (err_no(err) == DIST2_ERR_NO_RECORD) {
        err = dist_set("sem.ids { current_id: 0 }");
        assert(err_is_ok(err));
    }
    else {
        assert(!"Should not happen.");
    }

    id += 1;

    err = dist_set("sem.ids { current_id: %lu }", id);
    assert(err_is_ok(err));

    err = dist_unlock(lock_record);
    free(lock_record);
    free(record);
    assert(err_is_ok(err));

    return id;
}

errval_t dist_sem_new(uint32_t* id, size_t value)
{
    // Find a valid ID for our next semaphore
    *id = get_next_id();
    debug_printf("dist_sem_new id is: %d\n", *id);

    errval_t err = SYS_ERR_OK;
    for (size_t i=0; i < value; i++) {
        err = dist_mset(SET_SEQUENTIAL, "sem.%d. { sem: %d }", *id, *id);
        if (err_is_fail(err)) {
            return err;
        }
    }

    return err;
}

errval_t dist_sem_post(uint32_t id)
{
    return dist_mset(SET_SEQUENTIAL, "sem.%d. { sem: %d }", id, id);
}

errval_t dist_sem_wait(uint32_t id)
{
    errval_t err = SYS_ERR_OK;
    errval_t error_code;
    char** names = NULL;
    char* result = NULL;
    size_t len = 0;
    dist2_trigger_id_t tid;
    dist2_trigger_t t = dist_mktrigger(DIST2_ERR_NO_RECORD,
            dist2_BINDING_RPC, DIST_ON_SET, NULL, NULL);
    struct dist2_thc_client_binding_t* cl = dist_get_thc_client();
    char name[100];
    snprintf(name, 99, "r'sem.%d.[0-9]+' { sem: %d }", id, id);

    // XXX: The current implementation needs some improvement
    // because it suffers from herd effect (i.e. all waiting parties are
    // woken up if a record is added)
    while (1) {
        err = cl->call_seq.get_names(cl, name, t, &result, &tid, &error_code);
        if (err_is_ok(err)) {
            err = error_code;
        }

        if (err_is_ok(err)) {
            err = dist_parse_names(result, &names, &len);

            // Try to decrease by deleting one record
            for (size_t i=0; i<len; i++) {
                err = dist_del(names[i]);
                if (err_is_ok(err)) {
                    // We are done
                    goto out;
                }
                else if (err_no(err) == DIST2_ERR_NO_RECORD) {
                    // We lost the race, try the next one
                }
                else {
                    // Something strange happened, abort
                    goto out;
                }
            }
            // In case we've come here we cannot decrease
            // because all records have been deleted between our get_names
            // and del calls. This means we have to start over again...
        }
        else if (err_no(err) == DIST2_ERR_NO_RECORD) {
            // Cannot decrease at the moment
            // Wait until a record is added
            uint64_t fn, mode, state;
            char* record = NULL;
            err = cl->recv.trigger(cl, &tid, &fn, &mode, &record, &state);
            free(record);
            if (err_is_fail(err)) {
                goto out;
            }
        }
        else {
            // Some weird error
            goto out;
        }

        dist_free_names(names, len);
        names = NULL;
        len = 0;
    }

out:
    dist_free_names(names, len);
    free(result);
    return err;
}

errval_t dist_sem_trywait(uint32_t id)
{
    errval_t err = SYS_ERR_OK;
    errval_t error_code;
    char** names = NULL;
    char* result = NULL;
    size_t len = 0;
    dist2_trigger_id_t tid;
    struct dist2_thc_client_binding_t* cl = dist_get_thc_client();
    char name[100];
    snprintf(name, 99, "r'sem.%d.[0-9]+' { sem: %d }", id, id);

    err = cl->call_seq.get_names(cl, name, NOP_TRIGGER, &result,
            &tid, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    if (err_is_ok(err)) {
        err = dist_parse_names(result, &names, &len);

        // Try to decrease by deleting one record
        for (size_t i=0; i < len; i++) {
            err = dist_del(names[i]);
            if (err_is_ok(err)) {
                // We are done
                goto out;
            }
            else if (err_no(err) == DIST2_ERR_NO_RECORD) {
                // We lost the race, try the next one
            }
            else {
                // Something strange happened, abort
                goto out;
            }
        }
    }

out:
    dist_free_names(names, len);
    return err;
}
