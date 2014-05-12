/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef BULK_SIM_H_
#define BULK_SIM_H_

#include <barrelfish/barrelfish.h>

errval_t vspace_map_whole_frame(void **retbuf, struct capref cap);

void bulk_mapped_setup(void);
void bulk_mapped_transfer(struct capref dst, struct capref src, size_t len);

void bulk_premapped_setup(struct capref *dst_frame, size_t dst_count,
                            struct capref *src_frame, size_t src_count);
void bulk_premapped_transfer(uint32_t dst_slot, uint32_t src_slot, size_t len);

void bulk_sdma_setup(void);
void bulk_sdma_transfer(struct capref dst, struct capref src, size_t len);

#endif // BULK_SIM_H_
