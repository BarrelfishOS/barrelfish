/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <errno.h>

#include "posixcompat.h"

int posix_memalign(void **memptr, size_t alignment, size_t size)
{
    errval_t err;

    size_t retbytes;
    struct capref memory_cap;

    err = frame_alloc(&memory_cap, size, &retbytes);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not allocate large enough frame.");
    }

    err = vspace_map_one_frame(memptr, retbytes, memory_cap, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not map the frame in the vspace.");
    }

    // Make sure our alignment is actually ok:
    assert(((size_t)*memptr) % alignment == 0);

    return 0;
}
