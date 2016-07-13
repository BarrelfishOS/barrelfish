/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef REGION_POOL_H_
#define REGION_POOL_H_ 1


#include <barrelfish/barrelfish.h>

#define INIT_POOL_SIZE 128
struct buffer_pool;

struct region {
    domainid_t domain_id; // TODO needed?
    // ID of the region
    uint32_t region_id;
    // Base address of the region
    lpaddr_t base_addr;
    // Capability of the region
    struct capref* cap;
};

struct region_pool {

    // IDs are "hashed" may have to increase size at some point
    uint16_t region_pool_size;
    // number of regions in pool
    uint16_t num_regions;

    // random offset where regions ids start from
    uint64_t region_offset;

    // TODO structure to store regions
    struct region** pool;
};


errval_t region_pool_init(struct region_pool** pool);

errval_t region_pool_add_region(struct region_pool* pool, 
                                struct region* region,
                                uint32_t* region_id);

errval_t region_pool_remove_region(struct region_pool* pool, 
                                   uint32_t region_id,
                                   struct region** region);


#endif /* REGION_POOL_H_ */
