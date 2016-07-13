/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "region_pool.h"

errval_t region_pool_init(struct region_pool** pool)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

errval_t region_pool_add_region(struct region_pool* pool, 
                                struct region* region,
                                uint32_t* region_id)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

errval_t region_pool_remove_region(struct region_pool* pool, 
                                   uint32_t region_id,
                                   struct region** region)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}


