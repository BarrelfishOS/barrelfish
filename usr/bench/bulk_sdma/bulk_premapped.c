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

static void **dst_buffer;
static size_t dst_slot_count;

static void **src_buffer;
static size_t src_slot_count;

void bulk_premapped_setup(struct capref *dst_frame, size_t dst_count,
                            struct capref *src_frame, size_t src_count)
{
    errval_t err;

    dst_slot_count = dst_count;
    dst_buffer = malloc(dst_count * sizeof(void*));
    assert(dst_buffer);

    for (size_t i=0; i<dst_slot_count; i++) {
        err = vspace_map_whole_frame(&dst_buffer[i], dst_frame[i]);
        assert(err_is_ok(err));
    }

    src_slot_count = src_count;
    src_buffer = malloc(src_count * sizeof(void*));
    assert(dst_buffer);

    for (size_t i=0; i<src_slot_count; i++) {
        err = vspace_map_whole_frame(&src_buffer[i], src_frame[i]);
        assert(err_is_ok(err));
    }
}

void bulk_premapped_transfer(uint32_t dst_slot, uint32_t src_slot, size_t len)
{
    assert(dst_slot < dst_slot_count);
    assert(src_slot < src_slot_count);

    memcpy(dst_buffer[dst_slot], src_buffer[src_slot], len);
}
