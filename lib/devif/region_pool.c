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

#define INIT_POOL_SIZE 32

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
    (*pool) = calloc(sizeof(struct region_pool), 1);
    if (*pool == NULL) {
        DQI_DEBUG("Allocationg inital pool failed \n");
        return LIB_ERR_MALLOC_FAIL;
    }

    (*pool)->num_regions = 0;

    srand(rdtsc());

    // Initialize region id offset
    (*pool)->region_offset = (rand() >> 12) ;
    (*pool)->size = INIT_POOL_SIZE;    

    (*pool)->pool = calloc(sizeof(struct region*)*INIT_POOL_SIZE, 1);
    if ((*pool)->pool == NULL) {
        free(*pool);
        DQI_DEBUG("Allocationg inital pool failed \n");
        return LIB_ERR_MALLOC_FAIL;
    }

    DQI_DEBUG("Init region pool size=%d addr=%p\n", INIT_POOL_SIZE, *pool);
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
    tmp = calloc(sizeof(struct region*)*new_size, 1);
    if (tmp == NULL) {
        DQI_DEBUG("Allocationg larger pool failed \n");
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
    
    // Check if pool size is large enough
    if (!(pool->num_regions < pool->size)) {
        DQI_DEBUG("Increasing pool size to %d \n", pool->size*2);
        err = region_pool_grow(pool);
        if (err_is_fail(err)) {
            DQI_DEBUG("Increasing pool size failed\n");
            return err;
        }
    }

    pool->num_regions++;
    uint16_t offset = pool->last_offset;
    uint16_t index = 0;

    while (true) {
        index = (pool->region_offset + pool->num_regions + offset) % pool->size;
        DQI_DEBUG("Trying insert index %d \n", index);
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

    // insert into pool
    pool->pool[region->id % pool->size] = region;
    *region_id = region->id;
    DQI_DEBUG("Inserting region into pool at %d \n", region->id % pool->size);
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
        DQI_DEBUG("Failed to destroy region, some buffers might still be in use \n");
        return err;
    }

    DQI_DEBUG("Removing slot %d \n", region_id % pool->size);
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
static errval_t region_pool_get_region(struct region_pool* pool,
                                       regionid_t region_id,
                                       struct region** region)
{
    *region = pool->pool[region_id % pool->size];
    if (region == NULL) {
        return DEVQ_ERR_INVALID_REGION_ID;
    }

    return SYS_ERR_OK;
}


/**
 * @brief get a buffer id from a region
 *
 * @param pool          The pool to get the region from
 * @param region_id     The id of the region to get the buffer from
 * @param addr          The physical address of the buffer
 * @param buffer_id     Return pointer to the buffer id
 *
 * @returns error on failure or SYS_ERR_OK on success
 */

errval_t region_pool_get_buffer_id_from_region(struct region_pool* pool,
                                               regionid_t region_id,
                                               lpaddr_t addr,
                                               bufferid_t* buffer_id)
{
    errval_t err;
    struct region* region;
    err = region_pool_get_region(pool, region_id, &region);
    if (err_is_fail(err)) {
        return err;
    }
    
    err = region_get_buffer_id(region, addr, buffer_id);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * @brief returns the buffer id to the pool of free ids
 *
 * @param pool          The pool to get the region from
 * @param region_id     The id of the region to get the buffer from
 * @param buffer_id     Return pointer to the buffer id
 * @param addr          the physical address of the buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 */

errval_t region_pool_return_buffer_id_to_region(struct region_pool* pool,
                                                regionid_t region_id,
                                                bufferid_t buffer_id)
{
    errval_t err;
    struct region* region;
    err = region_pool_get_region(pool, region_id, &region);
    if (err_is_fail(err)) {
        return err;
    }
    
    err = region_free_buffer_id(region, buffer_id);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * @brief return if a buffer of a region is in use
 *
 * @param pool          The pool to get the region from
 * @param region_id     The id of the region
 * @param buffer_id     The id of the buffer
 *
 * @returns true if the buffer is in use otherwise false
 */
bool region_pool_buffer_id_of_region_in_use(struct region_pool* pool,
                                            regionid_t region_id,
                                            bufferid_t buffer_id)
{
    errval_t err;
    struct region* region;
    err = region_pool_get_region(pool, region_id, &region);
    if (err_is_fail(err)) {
        return false;
    }
    
    return region_buffer_id_in_use(region, buffer_id);
}
