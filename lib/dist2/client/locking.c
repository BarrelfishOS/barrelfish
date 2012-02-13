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

#include <dist2/init.h>
#include <dist2/lock.h>
#include <dist2/getset.h>
#include <dist2/trigger.h>

#include "common.h"

static void trigger_lock_deleted(dist2_mode_t m, char* object, void* st)
{
    struct thread_sem* ts = (struct thread_sem*) st;
    debug_printf("object: %s has been deleted, send singal!\n", object);
    thread_sem_post(ts);
    free(object);
}

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
    char* name = NULL;

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
        char** names = NULL;
        size_t len = 0;

        err = dist_get_names(&names, &len, "_ { lock: '%s' }", lock_name);
        if (err_is_fail(err)) {
            dist_free_names(names, len);
            goto out;
        }

        debug_printf("lock queue:\n");
        size_t i = 0;
        bool found = false;
        for (; i < len; i++) {
            debug_printf("%s\n", names[i]);
            if (strcmp(names[i], name) == 0) {
                found = true;
                break;
            }
        }
        assert(found);

        if (i == 0) {
            // We are the lock owner
            dist_free_names(names, len);
            break;
        } else {
            struct dist2_rpc_client* cl = get_dist_rpc_client();

            struct thread_sem ts;
            thread_sem_init(&ts, 0);

            dist2_trigger_t t = dist_mktrigger(SYS_ERR_OK, DIST_ON_DEL,
                    trigger_lock_deleted, &ts);
            errval_t exist_err;
            dist2_trigger_id_t tid;
            DIST_LOCK_BINDING(cl);
            err = cl->vtbl.exists(cl, names[i - 1], t, &tid, &exist_err);
            DIST_UNLOCK_BINDING(cl);
            if (err_is_ok(exist_err)) {
                thread_sem_wait(&ts);
            }
            else if (err_no(err) != DIST2_ERR_NO_RECORD) {
                dist_free_names(names, len);
                goto out;
            }
        }

        dist_free_names(names, len);
    }

out:
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
