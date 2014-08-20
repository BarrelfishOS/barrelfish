/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>

#include "bulk_pool.h"
#include "bulk_buffer.h"


/**
 * initializes a new bulk allocator with a pool and allocates memory for it.
 *
 * @param alloc         pointer to an unused allocator handle
 * @param buffer_count  the number of buffers to allocate
 * @param buffer_size   the size of a single buffer
 * @param constraints   memory requirements for this pool or NULL if none
 */
errval_t bulk_alloc_init(struct bulk_allocator *alloc,
                         size_t buffer_count,
                         size_t buffer_size,
                         struct bulk_pool_constraints *constraints)
{
    assert(alloc);

    if (!bulk_buffer_check_size(buffer_size)) {
        return BULK_TRANSFER_ALLOC_BUFFER_SIZE;
    }

    if (buffer_count == 0) {
        return BULK_TRANSFER_ALLOC_BUFFER_COUNT;
    }

    errval_t err;

    size_t pool_size = buffer_count * buffer_size;

    err = bulk_pool_alloc(&alloc->pool, buffer_count, buffer_size);
    if (err_is_fail(err)) {
        return err;
    }

    /* reserve a virtual memory range for the pool */

    /* TODO: how to set the physical alignment constraints ? */

    /* set ram affinity */
    uint64_t minbase, maxlimit;
    if ((constraints != NULL) && (constraints->range_min != 0)
                    && (constraints->range_max + 1) != 0) {
        ram_get_affinity(&minbase, &maxlimit);
        ram_set_affinity(constraints->range_min, constraints->range_max);
    }
    size_t ret_size;
    err = frame_alloc(&alloc->pool->pool_cap, pool_size, &ret_size);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }

    /* restore ram affinity */
    if ((constraints != NULL) && (constraints->range_min != 0)
                    && (constraints->range_max + 1) != 0) {
        ram_set_affinity(minbase, maxlimit);
    }

    if (constraints != NULL){
        alloc->pool->trust = constraints->trust;
    }

    /*
     * in the trusted case, we copy the pool_cap into all buffers and map
     * them with offsets,
     * in the untrusted case, we split the cap into smaller frames and map those
     */
    err = bulk_pool_map(alloc->pool);
    if (err_is_fail(err)) {
        /* TODO: error handling */
        return err;
    }

    alloc->mngs = calloc(buffer_count, sizeof(*alloc->mngs));
    for (int i = 0; i < buffer_count; ++i) {
        struct bulk_buffer *buf = alloc->pool->buffers[i];
        /* setup the management structure for the free list */
        struct bulk_buffer_mng *le = alloc->mngs + i;
        le->buffer = buf;
        le->next = alloc->free_buffers;
        alloc->free_buffers = le;
    }
    alloc->num_free = buffer_count;

    return SYS_ERR_OK;
}

/**
 * creates a new allocator based on the supplied capability. It creates as many
 * buffers as possible of size buffer_size that fit into the capability.
 *
 * @param alloc         an unused allocator handle
 * @param buffer_size   the size of a single buffer
 * @param frame         capability for backing the bulk pool
 */
errval_t bulk_alloc_init_from_cap(struct bulk_allocator *alloc,
                                  size_t buffer_size,
                                  struct capref *frame)
{
    assert(!"NYI: bulk_alloc_init");
    return SYS_ERR_OK;
}

/**
 * Frees up the bulk allocator and it's pool.
 *
 * @param alloc handle to a bulk allocator to be freed
 */
errval_t bulk_alloc_free(struct bulk_allocator *alloc)
{

    assert(!"NYI: bulk_alloc_init");

    /*
     * PERFORMING INTERNAL TESTS
     */
    debug_printf("PERFORMING INTERNAL TESTS...\n");
    assert(alloc->num_free == 1);

    struct bulk_buffer *buf = bulk_alloc_new_buffer(alloc);

    buf = alloc->pool->buffers[4];
    assert(buf);

    errval_t err;

    volatile char *e;

    debug_printf("ABOUT TO UNMAP\n");
    err = bulk_buffer_unmap(buf);
    if (err_is_fail(err)) {
        debug_printf("BUFFER UNMAP FAILED!\n");
    }

    debug_printf("ABOUT TO MAP AGAIN\n");
    err = bulk_buffer_map(buf);
    if (err_is_fail(err)) {
        debug_printf("BUFFER MAP FAILED\n");
    }
    debug_printf("ABOUT CHECKING....\n");
    e = buf->address;
    for (int i = 0; i < 4096; ++i) {
        e[i] = 1;
    }


#if 0
    debug_printf("ABOUT TO UNMAP\n");
    err = bulk_buffer_unmap(buf);
    if (err_is_fail(err)) {
        debug_printf("BUFFER UNMAP FAILED!\n");
    }

    debug_printf("ABOUT TO CRASH....\n");
    e = buf->address;
    for (int i = 0; i < 4096; ++i) {
        e[i] = 1;
    }
#endif
    debug_printf("ABOUT TO CHANGE STATE:\n");
    err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_ONLY);
    if (err_is_fail(err)) {
        debug_printf("change state failed");
    }


    e = buf->address;
    debug_printf("ABOUT TO READ: %p\n", e);
    for (int i = 0; i < 4096; ++i) {
        if (e[i] != 1) {
            debug_printf("ERROR: WRONG VALUE!\n");
            break;
        }
    }
    debug_printf("ABOUT TO WRITE: %p\n", e);
    for (int i = 0; i < 4096; ++i) {
        if (!(i%1024)) {
            debug_printf("writing %p\n", &e[i]);
        }
        e[i] = 2;
    }

    for (int i = 0; i < 4096; ++i) {
            if (e[i] != 2) {
                debug_printf("ERROR: WRONG VALUE!\n");
                break;
            }
        }



    assert(!"NYI: bulk_alloc_init");
    return SYS_ERR_OK;

}

/**
 * Gets a new bulk buffer from the allocator.
 *
 * @param   alloc   the allocator handle to allocate the buffer from
 *
 * @return  pointer to a bulk_buffer on success
 *          NULL if there are no buffer left
 *
 */
struct bulk_buffer *bulk_alloc_new_buffer(struct bulk_allocator *alloc)
{
    assert(alloc);

    if (alloc->num_free == 0) {
        return NULL;
    }

    struct bulk_buffer *buf = alloc->free_buffers->buffer;
    alloc->free_buffers = alloc->free_buffers->next;
    alloc->num_free--;

    /*
     * XXX: do we want to have a special state for being "not allocated"
     *      i.e. maybe set the state to invalid, even though mapped r/w?
     */
    assert(buf->state == BULK_BUFFER_READ_WRITE);

    return buf;
}

/**
 * returns a buffer back to the allocator. The pools must match.
 *
 * @param alloc     the allocator to hand the buffer back
 * @param buffer    the buffer to hand back to the allocator
 */
errval_t bulk_alloc_return_buffer(struct bulk_allocator *alloc,
                                  struct bulk_buffer *buffer)
{
    assert(alloc);

    if (buffer == NULL || (buffer && buffer->state != BULK_BUFFER_READ_WRITE)) {
        /* only read_write i.e. owned buffers can be added back */
        return BULK_TRANSFER_BUFFER_STATE;
    }

    if (buffer->pool != alloc->pool) {
        /* the buffers can only be added to the allocator */
        return BULK_TRANSFER_POOL_INVALD;
    }

    struct bulk_buffer_mng *bm = alloc->mngs + buffer->bufferid;
    bm->buffer = buffer;
    bm->next = alloc->free_buffers;
    alloc->free_buffers = bm;
    alloc->num_free++;

    return SYS_ERR_OK;

}

