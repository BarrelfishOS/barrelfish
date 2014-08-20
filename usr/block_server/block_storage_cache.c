/**
 * \file
 * \brief block_server client process.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include "block_storage_cache.h"

/**
 * \brief inserts a new block into the cache
 *
 * \param blockid   the id of the block to insert
 * \param data      the data of the block XXX: This should be bulk_buf?
 */
errval_t block_cache_insert(size_t blockid, void *data)
{
    assert(!"NYI: block_cache_insert");
    return SYS_ERR_OK;
}

/**
 * \brief invalidates a block in the cache
 *
 * \param blockid   the id of the block to invalidate
 */
errval_t block_cache_invalidate(size_t blockid)
{
    assert(!"NYI: block_cache_invalidate");
    return SYS_ERR_OK;
}

/**
 * \brief looks up a block in the cache and returns it if present
 *
 * \param blockid   the ID of the block to lookup
 * \param ret_data  pointer to the returned data XXX: bulk_buf?
 */
errval_t block_cache_lookup(size_t blockid, void **ret_data)
{
    assert(!"NYI: block_cache_lookup");
    return SYS_ERR_OK;
}
