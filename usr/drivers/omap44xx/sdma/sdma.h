/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef SDMA_H_
#define SDMA_H_

#include <if/omap_sdma_defs.h>

#define MIN(a,b) ((a) < (b) ? (a) : (b))

void start_service(void);

errval_t mem_copy(struct capref dst_cap, struct capref src_cap);
errval_t mem_fill(struct capref dst_cap, uint8_t color);

errval_t mem_copy_2d(omap_sdma_addr_2d_t dst, omap_sdma_addr_2d_t src,
                omap_sdma_count_2d_t count, bool transparent, uint32_t color);
errval_t mem_fill_2d(omap_sdma_addr_2d_t dst, omap_sdma_count_2d_t count, uint32_t color);
#endif
