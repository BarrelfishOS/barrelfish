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



struct bs_block
{
    size_t id;
    size_t size;
    void *data;
};

struct block_storage
{
    uint8_t ready;
    size_t num_blocks;
    size_t block_size;
    struct bs_block *blocks;
};

size_t block_storage_get_block_size(void);

errval_t block_storage_init(size_t num_blocks, size_t block_size);

errval_t block_storage_dealloc(void);

errval_t block_storage_read(size_t bid, void *dst);

errval_t block_storage_write(size_t bid, void *src);

#endif /* BLOCK_STORAGE_H */
