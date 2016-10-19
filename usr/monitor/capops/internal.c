/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <monitor.h>
#include "internal.h"

/**
 * \brief Return number of montiors for which we've seen 'capops_ready'
 * \return #monitors that have initialized in the distops protocol
 */
size_t num_monitors_ready_for_capops(void) {

    errval_t err;
    size_t capops_ready_count = 0;

    for (coreid_t c = 0; c < MAX_COREID; c++) {
        struct intermon_binding *b;
        err = intermon_binding_get(c, &b);
        if (err_is_ok(err)) {
            struct intermon_state *st = b->st;
            capops_ready_count += st->capops_ready;
        }

    }

    // Return #<other monitors ready> and us, so that client code can remain
    // unchanged.
    return capops_ready_count + 1;
}
