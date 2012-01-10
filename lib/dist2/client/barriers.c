/**
 * \file
 * \brief Barrier client API implementation
 *
 * Implementation of a double barrier using the get/set API.
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
#include <dist2/barrier.h>
#include <dist2/getset.h>
#include <dist2/trigger.h>

#include "common.h"

static void barrier_signal_sem(char* record, void* state)
{
    debug_printf("barrier_signal_sem\n");
    struct thread_sem* ts = (struct thread_sem*) state;
    thread_sem_post(ts);

    free(record);
}

/**
 * \brief Client enters a barrier. Blocks until all clients have entered the
 * barrier.
 *
 * Each client creates a (sequential record) based on the provided name.
 * Once a client sees the specified amount (wait_for) of records it
 * creates a record that wakes up all waiting clients.
 *
 * \param[in] name Name of the barrier.
 * \param[out] barrier_record Record created for each client.
 * \param[in] wait_for Number of clients entering the barrier.
 */
errval_t dist_barrier_enter(const char* name, char** barrier_record, size_t wait_for)
{
    errval_t err = dist_set_get(SET_SEQUENTIAL, barrier_record,
            "%s_ { barrier: '%s' }", name, name);

    char** names = NULL;
    size_t current_barriers = 0;
    err = dist_get_names(&names, &current_barriers, "_ { barrier: '%s' }",
            name);
    dist_free_names(names, current_barriers);
    if (err_is_fail(err)) {
        return err;
    }
    debug_printf("current_barriers: %lu wait_for: %lu\n", current_barriers,
            wait_for);

    if (current_barriers != wait_for) {
        struct thread_sem ts;
        thread_sem_init(&ts, 0);

        struct dist2_rpc_client* cl = get_dist_rpc_client();
        dist2_trigger_t t = dist_mktrigger(DIST2_ERR_NO_RECORD, DIST_ON_SET,
                barrier_signal_sem, &ts);
        errval_t exist_err;
        err = cl->vtbl.exists(cl, name, t, &exist_err);
        if (err_is_fail(err)) {
            return err;
        }
        err = exist_err;
        if (err_no(err) == DIST2_ERR_NO_RECORD) {
            debug_printf("waiting for semaphore signal\n");
            thread_sem_wait(&ts);
            err = SYS_ERR_OK;
        }
    }
    else {
        // We are the last to enter the barrier,
        // wake up the others
        err = dist_set(name);
    }

    return err;
}

/**
 * \brief Leave a barrier. Blocks until all involved parties have
 * called dist_barrier_leave().
 *
 * Client deletes its barrier record. In case the client
 * was the last one we delete the special record which
 * wakes up all other clients.
 *
 * \param barrier_record Clients own record as provided by
 * dist_barrier_enter.
 */
errval_t dist_barrier_leave(const char* barrier_record)
{
    char* rec_name = NULL;
    char* barrier_name = NULL;
    debug_printf("leaving: %s\n", barrier_record);
    errval_t err = dist_read(barrier_record, "%s { barrier: %s }", &rec_name,
            &barrier_name);

    if (err_is_ok(err)) {
        err = dist_del(rec_name);
        if (err_is_fail(err)) {
            goto out;
        }

        char** names = NULL;
        size_t remaining_barriers = 0;
        err = dist_get_names(&names, &remaining_barriers, "_ { barrier: '%s' }",
                barrier_name);
        dist_free_names(names, remaining_barriers);

        debug_printf("remaining barriers is: %lu\n", remaining_barriers);

        if (err_is_ok(err)) {
            struct dist2_rpc_client* cl = get_dist_rpc_client();
            struct thread_sem ts;
            thread_sem_init(&ts, 0);

            dist2_trigger_t t = dist_mktrigger(SYS_ERR_OK, DIST_ON_DEL,
                    barrier_signal_sem, &ts);
            errval_t exist_err;
            err = cl->vtbl.exists(cl, barrier_name, t, &exist_err);
            if (err_is_fail(err)) {
                goto out;
            }
            err = exist_err;

            if (err_is_ok(err)) {
                // Wait until everyone has left the barrier
                thread_sem_wait(&ts);
            }
        }
        if (err_no(err) == DIST2_ERR_NO_RECORD) {
            // We are the last one to leave the barrier,
            // wake-up all others
            err = dist_del("%s", barrier_name);
        } else {
            // Don't do anything in case of other errors
        }
    }

out:
    free(rec_name);
    free(barrier_name);
    return err;
}
