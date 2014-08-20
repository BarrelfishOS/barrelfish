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
    struct capref   cnode_cap;  ///< capability of the buffers cnode
    uint32_t        refcnt;     ///< ref count for pool to channel assignments
    void           *impl_data;  ///< pointer to impl specific data
};


/**
 * returns a pointer to the pool with the given id
 *
 * @param   id  the id of the pool to look up
 *
 * @return  NULL if the pool is not present in the domain
 */
struct bulk_pool *bulk_pool_domain_list_get(struct bulk_pool_id *id);

/**
 * inserts a pool into the domain global bulk pool list
 *
 * @param   pool    the pool to insert
 */
errval_t bulk_pool_domain_list_insert(struct bulk_pool *pool);

errval_t bulk_pool_domain_list_remove(struct bulk_pool *pool);

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
 * adds a pool to a channel's pool list
 *
 * @param pool      the pool to assing to the channel
 * @param channel   the channel to assign the the pool to
 */
errval_t bulk_pool_assign(struct bulk_pool          *pool,
                         struct bulk_channel        *channel);

/**
 * removes the pool from the channel's pool list
 *
 * @param pool      the poo to remove
 * @param channel   the channel to remove the pool from
 */
errval_t bulk_pool_remove(struct bulk_pool          *pool,
                          struct bulk_channel       *channel);


/**
 * gets a pointer to the pool on this channel
 *
 * @param id        the poolid we want the pool
 * @param channel   the channel to look for the pools
 */
struct bulk_pool *bulk_pool_get(struct bulk_pool_id *id,
                                struct bulk_channel *channel);

/**
 * Does the mapping of a pool depending on the trust level. If it is not trusted,
 * only the memory range is allocated and the vregions for the buffers created,
 * In the trusted case, the pool is backed with the pool cap and mapped.
 *
 * @param pool  the pool to map
 */
errval_t bulk_pool_map(struct bulk_pool *pool);


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

/**
 * allocates the data structures for the pool.
 *
 * @param   pool            storage for pointer to newly allocated pool
 * @param   buffer_count    the number of buffers in the pool
 * @param   buffer_size     the size of a single buffer
 * @param   id              pool id
 */
errval_t bulk_pool_alloc_with_id(struct bulk_pool     **pool,
                                 size_t               buffer_count,
                                 size_t               buffer_size,
                                 struct bulk_pool_id id);

/**
 * allocates the data structures for the pool with new id.
 *
 * @param   pool            storage for pointer to newly allocated pool
 * @param   buffer_count    the number of buffers in the pool
 * @param   buffer_size     the size of a single buffer
 */
errval_t bulk_pool_alloc(struct bulk_pool     **pool,
                         size_t               buffer_count,
                         size_t               buffer_size);


/**
 * frees up the resources needed by the pool and does the
 * deletion of the caps (if any)
 *
 * @param pool  the pool to dealloc
 */
errval_t bulk_pool_dealloc(struct bulk_pool *pool);

#endif // BULK_TRANSFER_POOL_H

