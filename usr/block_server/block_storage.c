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

#include "block_storage.h"

static struct block_storage blocks;

/**
 * \brief this function allocates an in memory block storage
 *
 * The intended use of this function is to use bulk buffers as a backend
 * for the block store. The used bulk buffers can then be used directly
 * to be sent over the bulk channel.
 */
static errval_t block_storage_alloc(void)
{
    assert(!"NYI: block_storage_alloc");
    return SYS_ERR_OK;
}

/**
 * \brief this function populates the the previously allocated block storage
 *        with sample data
 */
static errval_t block_storage_fill(void)
{
    assert(!"NYI: block_storage_fill");
    return SYS_ERR_OK;
}

/**
 * \brief this function initializes the in-RAM block storage of the block
 *        server.
 *
 * \param num_blocks    the number of blocks to initialize
 * \param block_size    the size of a single block in bytes
 *
 * \return SYS_ERR_OK on success
 *
 */
errval_t block_storage_init(size_t num_blocks, size_t block_size)
{
    blocks.block_size = block_size;
    blocks.num_blocks = num_blocks;

    errval_t err;

    // allocate the block store
    err = block_storage_alloc();

    // initialize the block store with sample data

    err = block_storage_fill();

    return SYS_ERR_OK;
}


/**
 * \brief frees up the in ram block storage
 */
errval_t block_storage_dealloc(void)
{
    assert(!"NYI: block_storage_dealloc");
    return SYS_ERR_OK;
}

