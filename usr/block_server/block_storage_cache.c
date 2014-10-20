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

#include "block_server.h"

#include "block_storage_cache.h"

/**
 * list of buffers received
 *
 * XXX: maybe maintain one for each pool / channel ?
 */

struct buffer_list *bs_bulk_buffers = NULL;

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


struct buffer_list *bl = NULL;

struct bulk_buffer *buffer_stack[BLOCK_NET_BUFFER_COUNT];
uint32_t stack_size = BLOCK_NET_BUFFER_COUNT;
uint32_t stack_top = 0;

struct bulk_buffer *block_server_get_buffer(struct buffer_list **l,
                                            struct bulk_channel *chan)
{
    assert(l);

    if (stack_top == 0) {
        return NULL;
    }

    return buffer_stack[--stack_top];

    struct buffer_list *list = bl;

    if (bl == NULL) {
        debug_printf("LIST == 0\n");
        return NULL;
    }



    struct bulk_buffer *buf;
    /* fast path */
    if (!chan) {
        buf = list->buf;
        bl= list->next;
        free(list);
        return buf;
    }

    struct buffer_list *prev = NULL;
    while(list) {
        if (list->channel == chan) {
            buf = list->buf;
            if (prev) {
                prev->next = list->next;
            } else {
                bl = list->next;
            }
            free(list);
            return buf;
        }
        prev = list;
        list = list->next;
    }

    debug_printf("RETURN NULL");

    return NULL;
}

void block_server_insert_buffer(struct buffer_list **l,
                                struct bulk_buffer *buf,
                                struct bulk_channel *channel)
{
    assert(l);
    assert(stack_top < stack_size);

    buffer_stack[stack_top++] = buf;

    return;

    struct buffer_list *list = malloc(sizeof(struct buffer_list));
    list->next = NULL;
    assert(list);
    if (bl == NULL) {
        bl = list;
    } else {
        list->next = bl;
        bl = list;
    }

    list->buf = buf;
    list->channel = channel;
}
