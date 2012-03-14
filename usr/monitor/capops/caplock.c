/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "caplock.h"
#include <barrelfish/waitset.h>
#include <barrelfish/event_queue.h>
#include <barrelfish/debug.h>
#include "capqueue.h"
#include "magic.h"

static struct capqueue_queue global_queue;

void
caplock_wait(struct capref cap, struct event_queue_node *qn, struct event_closure cont)
{
    errval_t err;
    err = monitor_lock_cap(cap);
    if (err_is_ok(err)) {
        cont.handler(cont.arg);
    }
    else if (err_no(err) == SYS_ERR_CAP_LOCKED) {
        capqueue_wait(&global_queue, qn, cont);
    }
    else {
        USER_PANIC_ERR(err, "error while locking cap");
    }
}

void
caplock_unlock(struct capref cap)
{
    errval_t err = monitor_unlock_cap(cap);
    assert(err_is_ok(err));
    capqueue_notify(&global_queue);
}

void
caplock_init(struct waitset *ws)
{
    capqueue_init(&global_queue, ws);
}
