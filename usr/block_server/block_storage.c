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
#include <string.h>

#include <barrelfish/barrelfish.h>

#include "block_server.h"
#include "block_storage.h"

static struct block_storage blocks;

//#define BS_STOR_DEBUG(op, id, data) debug_printf("BS: %s data=[0x%x]  block=[%i]\n", op, data, id);
#define BS_STOR_DEBUG(op, id, data) do{}while(0);

/**
 * \brief this function allocates an in memory block storage
 *
 * The intended use of this function is to use bulk buffers as a backend
 * for the block store. The used bulk buffers can then be used directly
 * to be sent over the bulk channel.
 */
static errval_t block_storage_alloc(void)
{
    BS_BS_DEBUG("allocating %i bytes",
                    (uint32_t )(blocks.block_size * blocks.num_blocks));

    blocks.blocks = malloc(blocks.block_size * sizeof(struct bs_block));
    if (blocks.blocks == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    void *data = malloc(blocks.block_size * blocks.num_blocks);
    assert(data);

    BS_BS_DEBUG("%s", "initializing blocks.");

    struct bs_block *blk = blocks.blocks;
    for (uint32_t i = 0; i < blocks.num_blocks; ++i) {
        blk->data = data;
        blk->id = i;
        blk->size = blocks.block_size;
        blk++;
        data += blocks.block_size;
    }

    BS_BS_DEBUG("%s", "block allocation done.");

    return SYS_ERR_OK;
}

/**
 * \brief this function populates the the previously allocated block storage
 *        with sample data
 */
static errval_t block_storage_fill(void)
{
    for (uint32_t i = 0; i < blocks.num_blocks; ++i) {
        struct bs_block *blk = blocks.blocks + i;
        memset(blk->data, i, blk->size);
    }
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
    BS_BS_DEBUG("%s, nblk=%i, blksz=%i", "initializing block storage",
                    (uint32_t )num_blocks, (uint32_t )block_size);

    if (blocks.ready) {
        debug_printf("NOTICE: block store already initialized\n");
        return SYS_ERR_OK;
    }

    /* just a basic check... */
    /* TODO: formulate better constraints */
    if (block_size & (2048 - 1)) {
        debug_printf("invalid block size: %i\n", (uint32_t) block_size);
        block_size += (2048 - (block_size & (2048 - 1)));
        debug_printf("setting block size to: %i", (uint32_t) block_size);
    }

    blocks.block_size = block_size;
    blocks.num_blocks = num_blocks;

    errval_t err;

    // allocate the block store
    err = block_storage_alloc();
    if (err_is_fail(err)) {
        return err;
    }

    // initialize the block store with sample data
    err = block_storage_fill();
    if (err_is_fail(err)) {
        return err;
    }

    blocks.ready = 1;

    return SYS_ERR_OK;
}

/**
 * \brief frees up the in ram block storage
 */
errval_t block_storage_dealloc(void)
{
    BS_BS_DEBUG("%s", "deallocating block store");

    free(blocks.blocks->data);
    free(blocks.blocks);

    return SYS_ERR_OK;
}

/**
 * \brief a simple function that copies a block into the destination
 *
 * \param bid   the block id to copy
 * \param dst   the destination to copy the contents to
 */
errval_t block_storage_read(size_t bid, void *dst)
{
    if (bid > blocks.num_blocks - 1) {
        return 1;
    }


    struct bs_block *blk = blocks.blocks + bid;

    BS_STOR_DEBUG("READ", (uint32_t)bid, *(uint32_t*)(blk->data));

    memcpy(dst, blk->data, blk->size);

    return SYS_ERR_OK;
}

/**
 * \brief a simpel function that updates the content of a block
 *
 * \param bid   the block id to update
 * \param src   the data to write
 */
errval_t block_storage_write(size_t bid, void *src)
{
    if (bid > blocks.num_blocks - 1) {
        return 1;
    }
    struct bs_block *blk = blocks.blocks + bid;
    BS_STOR_DEBUG("WRITE", (uint32_t)bid, *(uint32_t*)src);
    memcpy(blk->data, src, blk->size);

    return SYS_ERR_OK;
}

/**
 * returns the size of a block
 */
size_t block_storage_get_block_size(void)
{
    return blocks.block_size;
}
