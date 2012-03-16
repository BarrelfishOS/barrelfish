/**
 * \file
 * \brief Implementation of a synchronous locking API using the dist2 Interface.
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
#include <barrelfish/threads.h>

#include <octopus/init.h>
#include <octopus/lock.h>
#include <octopus/getset.h>
#include <octopus/trigger.h>

#include "common.h"

/**
 * \brief Synchronous locking function.
 *
 * The lock function will create a new record based on the lock name
 * using sequential set mode. This means we create a queue of records
 * with all parties that want to acquire the lock. The lock owner
 * is the one with the lowest sequence appended to its name.
 * Once the lock owner deletes its lock_record the next client in
 * the queue is notified through triggers.
 *
 * \note Once a client holds the lock it can be released using dist_unlock.
 *
 * \param[in] lock_name Name to identify the lock.
 * \param[out] lock_record Your current lock record in the queue.
 * Client needs to free this.
 */
errval_t dist_lock(const char* lock_name, char** lock_record)
{
    assert(lock_name != NULL);

    errval_t err = SYS_ERR_OK;
    errval_t exist_err;
    char** names = NULL;
    char* record = NULL;
    char* name = NULL;
    size_t len = 0;
    size_t i = 0;
    bool found = false;
    uint64_t mode = 0;
    uint64_t state = 0;
    uint64_t fn = 0;
    dist2_trigger_id_t tid;
    dist2_trigger_t t = dist_mktrigger(SYS_ERR_OK, DIST_ON_DEL,
            dist2_BINDING_RPC, NULL, NULL);

    err = dist_set_get(SET_SEQUENTIAL, lock_record, "%s_ { lock: '%s' }",
            lock_name, lock_name);
    if (err_is_fail(err)) {
        goto out;
    }
    err = dist_read(*lock_record, "%s", &name);
    if (err_is_fail(err)) {
        goto out;
    }

    while (true) {
        err = dist_get_names(&names, &len, "_ { lock: '%s' }", lock_name);
        if (err_is_fail(err)) {
            goto out;
        }

        //debug_printf("lock queue:\n");
        found = false;
        for (i=0; i < len; i++) {
            //debug_printf("%s\n", names[i]);
            if (strcmp(names[i], name) == 0) {
                found = true;
                break;
            }
        }
        assert(found);

        if (i == 0) {
            // We are the lock owner
            goto out;
        }
        else {
            // Someone else holds the lock
            struct dist2_thc_client_binding_t* cl = dist_get_thc_client();
            //debug_printf("exists for %s...\n", names[i-1]);
            err = cl->call_seq.exists(cl, names[i-1], t, &tid, &exist_err);
            if (err_is_fail(err)) {
                goto out;
            }

            if (err_is_ok(exist_err)) {
                err = cl->recv.trigger(cl, &tid, &fn, &mode, &record, &state);
                assert(err_is_ok(err));
                free(record);
                assert(mode & DIST_REMOVED);
            }
            else if (err_no(exist_err) != DIST2_ERR_NO_RECORD) {
                err = exist_err;
                goto out;
            }
        }

        // If we've come here our predecessor deleted his record;
        // need to re-check that we are really the lock owner now

        dist_free_names(names, len);
        names = NULL;
        len = 0;
    }


out:
    dist_free_names(names, len);
    free(name);
    return err;
}

/**
 * \brief Synchronous unlock function.
 *
 * Deletes the given lock_record in on the server.
 *
 * \param[in] lock_record Record provided by dist_lock.
 */
errval_t dist_unlock(const char* lock_record)
{
    assert(lock_record != NULL);
    errval_t err = SYS_ERR_OK;
    char* name = NULL;

    err = dist_read(lock_record, "%s", &name);
    if (err_is_ok(err)) {
        err = dist_del(name);
    }
    //debug_printf("id:%d unlocking: %s\n", id, name);

    free(name);
    return err;
}
