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

#include <dist2/trigger.h>

#include "handler.h"

void trigger_handler(struct dist2_binding* b, uint64_t t, uint64_t st,
        char* record)
{
    assert(t != 0);
    trigger_handler_fn trigger_fn = (trigger_handler_fn) t;
    void* state = (void*) st;

    trigger_fn(record, state);
}

dist2_trigger_t dist_mktrigger(errval_t in_case, dist2_mode_t mode,
        trigger_handler_fn fn, void* state)
{
    return (dist2_trigger_t) {
                .in_case = in_case,
                .m = mode,
                // TODO: bad uint64_t here!
                .trigger = (uint64_t) fn,
                .st = (uint64_t) state
            };
}
