/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>

#include <string.h>

#include "bulk_sim.h"

void bulk_mapped_setup(void)
{
    // no-op
}

void bulk_mapped_transfer(struct capref dst, struct capref src, size_t len)
{
    errval_t err;

    void *dst_buffer, *src_buffer;
    err = vspace_map_whole_frame(&dst_buffer, dst);
    assert(err_is_ok(err));

    err = vspace_map_whole_frame(&src_buffer, src);
    assert(err_is_ok(err));

    memcpy(dst_buffer, src_buffer, len);

    err = vspace_unmap(dst_buffer);
    assert(err_is_ok(err));

    err = vspace_unmap(src_buffer);
    assert(err_is_ok(err));
}
