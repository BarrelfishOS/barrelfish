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

struct region_pool;
struct region;
/**
 * @brief initialized a pool of regions
 *
 * @param pool          Return pointer to the region pool
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_pool_init(struct region_pool** pool);


/**
 * @brief freeing region pool
 *
 * @param pool          The region pool to free
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_pool_destroy(struct region_pool* pool);

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
                                uint32_t* region_id);

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
                                   uint32_t region_id,
                                   struct capref* cap);

/**
 * @brief get a page sized buffer from a region of the pool
 *
 * @param pool          The pool to get the region from
 * @param region_id     The id of the region to get the buffer from
 * @param buffer_id     Return pointer to the buffer id
 * @param addr          Return pointer to the physical address of the buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 */

errval_t region_pool_get_buffer_from_region(struct region_pool* pool,
                                            uint32_t region_id,
                                            uint32_t* buffer_id,
                                            lpaddr_t* addr);

/**
 * @brief return a page sized buffer to a region of the pool
 *
 * @param pool          The pool to get the region from
 * @param region_id     The id of the region to return the buffer to
 * @param buffer_id     The id of the buffer to return to the region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */

errval_t region_pool_return_buffer_to_region(struct region_pool* pool,
                                             uint32_t region_id,
                                             uint32_t buffer_id);

/**
 * @brief return if a buffer of a region is in use
 *
 * @param pool          The pool to get the region from
 * @param region_id     The id of the region
 * @param buffer_id     The id of the buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
bool region_pool_buffer_of_region_in_use(struct region_pool* pool,
                                         uint32_t region_id,
                                         uint32_t buffer_id);
#endif /* REGION_POOL_H_ */
