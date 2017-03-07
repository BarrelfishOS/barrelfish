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
#include "region.h"
#include "dqi_debug.h"

#define INIT_POOL_SIZE 16

struct region_pool {

    // IDs are inserted and may have to increase size at some point
    uint16_t size;
    // number of regions in pool
    uint16_t num_regions;

    // random offset where regions ids start from
    uint64_t region_offset;
    
    // if we have to serach for a slot, need an offset
    uint16_t last_offset;

    // structure to store regions
    struct region** pool;
};

/**
 * @brief initialized a pool of regions
 *
 * @param pool          Return pointer to the region pool
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_pool_init(struct region_pool** pool)
{
    // Allocate pool struct itself including pointers to region
    (*pool) = calloc(1, sizeof(struct region_pool));
    if (*pool == NULL) {
        DQI_DEBUG_REGION("Allocationg inital pool failed \n");
        return LIB_ERR_MALLOC_FAIL;
    }

    (*pool)->num_regions = 0;

    srand(rdtsc());

    // Initialize region id offset
    (*pool)->region_offset = (rand() >> 12) ;
    (*pool)->size = INIT_POOL_SIZE;    

    (*pool)->pool = calloc(INIT_POOL_SIZE, sizeof(struct region*));
    if ((*pool)->pool == NULL) {
        free(*pool);
        DQI_DEBUG_REGION("Allocationg inital pool failed \n");
        return LIB_ERR_MALLOC_FAIL;
    }

    DQI_DEBUG_REGION("Init region pool size=%d addr=%p\n", INIT_POOL_SIZE, *pool);
    return SYS_ERR_OK;
}

/**

 * @brief freeing region pool
 *
 * @param pool          The region pool to free
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_pool_destroy(struct region_pool* pool)
{
    errval_t err;
    struct capref cap;
    // Check if there are any regions left
    if (pool->num_regions == 0) {
        free(pool->pool);
        free(pool);
        return SYS_ERR_OK;
    } else {
        // There are regions left -> remove them
        for (int i = 0; i < pool->size; i++) {
            if ((void*) pool->pool[i] != NULL) {
                err = region_pool_remove_region(pool, pool->pool[i]->id,
                                                &cap);
                if (err_is_fail(err)){
                    printf("Region pool has regions that are still used,"
                           " can not free them \n");
                    return err;
                }
            }
        }
        free(pool->pool);
        free(pool);
    }
   
    return SYS_ERR_OK;
}

/**
 * @brief increase the region pool size by a factor of 2
 *
 * @param pool       the regin pool that has not enough region slots
 *
 * @returns error on failure or SYS_ERR_OK on success
 */


static errval_t region_pool_grow(struct region_pool* pool)
{
    struct region** tmp;

    uint16_t new_size = (pool->size)*2;
    // Allocate new pool twice the size
    tmp = calloc(new_size, sizeof(struct region*));
    if (tmp == NULL) {
        DQI_DEBUG_REGION("Allocationg larger pool failed \n");
        return LIB_ERR_MALLOC_FAIL;
    }

    // Copy all the pointers
    for (int i = 0; i < new_size; i++) {
        tmp[i] = NULL;
    }

    struct region* region;
    for (int i = 0; i < pool->size; i++) {
        region = pool->pool[i];
        uint16_t index =  region->id % new_size;
        tmp[index] = pool->pool[i];
    }

    free(pool->pool);

    pool->pool = tmp;
    pool->size = new_size;
    pool->last_offset = 0;

    return SYS_ERR_OK;
}

/**
 * @brief add a memory region to the region pool
 *
 * @param pool          The pool to add the region to
 * @param cap           The cap of the region
 * @param region_id     Return pointer to the region id 
 *                      that is assigned by the pool
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_pool_add_region(struct region_pool* pool, 
                                struct capref cap,
                                regionid_t* region_id)
{
    errval_t err;
    struct region* region;
    struct frame_identity id;

    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    // for now just loop over all entries
    for (int i = 0; i < pool->size; i++) {
        struct region* tmp;
        tmp = pool->pool[i]; 
   
        if (tmp == NULL) {
            continue;
        }

        // check if region is already registered
        if (tmp->base_addr == id.base) {
            return DEVQ_ERR_INVALID_REGION_ARGS;
        }

        /* if region if entierly before other region or
           entierly after region, otherwise there is an overlap
         */
        if (!((id.base + id.bytes <= tmp->base_addr) ||
            (tmp->base_addr + tmp->len <= id.base))) {
            return DEVQ_ERR_INVALID_REGION_ARGS;
        }

    }

    // Check if pool size is large enough
    if (!(pool->num_regions < pool->size)) {
        DQI_DEBUG_REGION("Increasing pool size to %d \n", pool->size*2);
        err = region_pool_grow(pool);
        if (err_is_fail(err)) {
            DQI_DEBUG_REGION("Increasing pool size failed\n");
            return err;
        }
    }

    pool->num_regions++;
    uint16_t offset = pool->last_offset;
    uint16_t index = 0;

    // find slot
    while (true) {
        index = (pool->region_offset + pool->num_regions + offset) % pool->size;
        DQI_DEBUG_REGION("Trying insert index %d \n", index);
        if (pool->pool[index] == NULL) {
           break;
        } else {
            offset++;
        }
    }

    pool->last_offset = offset;
    err = region_init(&region,
                      pool->region_offset + pool->num_regions + offset,
                      &cap);
    if (err_is_fail(err)) {
        return err;
    }
    // insert into pool
    pool->pool[region->id % pool->size] = region;
    *region_id = region->id;
    DQI_DEBUG_REGION("Inserting region into pool at %d \n", region->id % pool->size);
    return err;
}

/**
 * @brief add a memory region to the region pool using an already
 *        existing id
 *
 * @param pool          The pool to add the region to
 * @param cap           The cap of the region
 * @param region_id     The region id to add to the pool
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_pool_add_region_with_id(struct region_pool* pool, 
                                        struct capref cap,
                                        regionid_t region_id)
{
    errval_t err;
    // Check if pool size is large enough
    if (!(pool->num_regions < pool->size)) {
        DQI_DEBUG_REGION("Increasing pool size to %d \n", pool->size*2);
        err = region_pool_grow(pool);
        if (err_is_fail(err)) {
            DQI_DEBUG_REGION("Increasing pool size failed\n");
            return err;
        }
    }

    struct region* region = pool->pool[region_id % pool->size];
    if (region != NULL) {
        return DEVQ_ERR_INVALID_REGION_ID;
    } else {
        err = region_init(&region, region_id, &cap);
        if (err_is_fail(err)) {
            return err;
        }
        pool->pool[region_id % pool->size] = region;
    }

    pool->num_regions++;
    return SYS_ERR_OK;
}

/**
 * @brief remove a memory region from the region pool
 *
 * @param pool          The pool to remove the region from
 * @param region_id     The id of the region to remove
 * @param cap           Return pointer to the cap of the removed region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_pool_remove_region(struct region_pool* pool, 
                                   regionid_t region_id,
                                   struct capref* cap)
{
    errval_t err;
    struct region* region;
    region = pool->pool[region_id % pool->size]; 
    if (region == NULL) {
        return DEVQ_ERR_INVALID_REGION_ID;
    }

    cap = region->cap;
  
    err = region_destroy(region);
    if (err_is_fail(err)) {
        DQI_DEBUG_REGION("Failed to destroy region, some buffers might still be in use \n");
        return err;
    }

    DQI_DEBUG_REGION("Removing slot %d \n", region_id % pool->size);
    pool->pool[region_id % pool->size] = NULL;

    pool->num_regions--;
    return SYS_ERR_OK;
}


/**
 * @brief get memory region from pool
 *
 * @param pool          The pool to get the region from
 * @param region_id     The id of the region to get
 * @param region        Return pointer to the region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
/*
static errval_t region_pool_get_region(struct region_pool* pool,
                                       regionid_t region_id,
                                       struct region** region)
{
    *region = pool->pool[region_id % pool->size];
    if (*region == NULL) {
        return DEVQ_ERR_INVALID_REGION_ID;
    }

    return SYS_ERR_OK;
}
*/

/**
 * @brief check if buffer is valid
 *
 * @param pool          The pool to get the region from
 * @param region_id     The id of the region
 * @param offset        offset into the region
 * @param length        length of the buffer
 * @param valid_data    offset into the buffer
 * @param valid_length  length of the valid_data
 *
 * @returns true if the buffer is valid otherwise false
 */
bool region_pool_buffer_check_bounds(struct region_pool* pool,
                                     regionid_t region_id,
                                     genoffset_t offset,
                                     genoffset_t length,
                                     genoffset_t valid_data,
                                     genoffset_t valid_length)
{
    struct region* region;
    region = pool->pool[region_id % pool->size];
    if (region == NULL) {
        return false;
    }

    // check validity of buffer within region
    // and check validity of valid data values
    if ((length + offset > region->len) ||
         (valid_data + valid_length > length)) {
        return false;
    }

    return true;
}


