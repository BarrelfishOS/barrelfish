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

#ifndef BLOCK_STORAGE_H
#define BLOCK_STORAGE_H

/// The default size of a single block
#define DEFAULT_BLOCK_SIZE 4096

/// The default number of blocks
#define DEFAULT_BLOCK_COUNT 1024

struct block
{
/* todo: reference to a bulk block */
};

struct block_storage
{
    size_t num_blocks;
    size_t block_size;
    struct block *blocks;
};

errval_t block_storage_init(size_t num_blocks, size_t block_size);

errval_t block_storage_dealloc(void);

errval_t block_storage_get(size_t bid, void **ret_block);

errval_t block_storage_set(size_t bid, struct block *blk);

#endif /* BLOCK_STORAGE_H */
