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

#ifndef BLOCK_STORAGE_CACHE_H
#define BLOCK_STORAGE_CACHE_H

errval_t block_cache_insert(size_t blockid, void *data);

errval_t block_cache_invalidate(size_t blockid);

errval_t block_cache_lookup(size_t blockid, void **ret_data);

#endif /* BLOCK_STORAGE_CACHE_H */
