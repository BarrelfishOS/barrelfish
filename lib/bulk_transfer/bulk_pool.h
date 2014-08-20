/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_TRANSFER_POOL_H
#define BULK_TRANSFER_POOL_H

#include <bulk_transfer/bulk_transfer.h>

/**
 * internal representation of a bulk pool
 *
 */
struct bulk_pool_internal {
    struct bulk_pool pool;      ///< pointer to the public interface
    /* internal fields */
    struct vregion *vregion;    ///< pointer to the vregion of the pool
};

/**
 * compares two bulk pool ids
 *
 * @return  -1  if id1 is less than id2
 *           0  if both ids are equal
 *           1  if id1 is bigger than id2
 */
int8_t bulk_pool_cmp_id(struct bulk_pool_id *id1,
                        struct bulk_pool_id *id2);

/**
 * checks if a pool already has been assigned to that channel
 *
 * @param pool      the bulk pool to check for assignment
 * @param channel   the channel to check for assignment
 *
 * @return true:    the pool is assigned to this channel
 *         false:   the pools has not been assigned to the channel
 */
uint8_t bulk_pool_is_assigned(struct bulk_pool      *pool,
                              struct bulk_channel   *channel);


/**
 * Does the mapping of a pool depending on the trust level. If it is not trusted,
 * only the memory range is allocated and the vregions for the buffers created,
 * In the trusted case, the pool is backed with the pool cap and mapped.
 *
 * @param pool  the pool to map
 */
errval_t bulk_pool_map(struct bulk_pool *pool);


/**
 * does the remapping of the pool if the trust level changes from fully trusted
 * to a lower one. This function is called by the backend.
 *
 * @param pool  the pool to remap
 */
errval_t bulk_pool_remap(struct bulk_pool *pool);


/**
 * unmaps the entire pool and frees up the entire memory region of the pool.
 *
 * @param pool  the pool to unmap
 */
errval_t bulk_pool_unmap(struct bulk_pool *pool);


/**
 * initializes the buffers for a pool given the struct pool is allocated and
 * filled with the basic information about the number of buffers in the pool.
 *
 * @param pool  pointer to a pool with the information
 */
errval_t bulk_pool_init_bufs(struct bulk_pool *pool);

#endif // BULK_TRANSFER_POOL_H

