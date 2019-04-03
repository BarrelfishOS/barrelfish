/**
 * \file
 * \brief Allocator for managing buffers in a bulk transfer pool.
 *
 * Note using this allocator is optional, an application can do its own buffer
 * management.
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_ALLOCAOR_H
#define BULK_ALLOCAOR_H

#include <bulk_transfer/bulk_transfer.h>

/**
 *
 */
struct bulk_buffer_mng {
    struct bulk_buffer     *buffer;
    struct bulk_buffer_mng *next;
};

/**
 *
 */
struct bulk_allocator {
    struct bulk_pool       *pool;
    struct bulk_buffer_mng *mngs;
    size_t                  num_free;
    struct bulk_buffer_mng *free_buffers;
};


/**
 *
 */
struct bulk_pool_constraints {
    uintptr_t range_min;
    uintptr_t range_max;
    uintptr_t alignment;
    //TRUST_NONE or TRUST_HALF implies seperate capabilities per buffer
    enum bulk_trust_level   trust;
};



/**
 * initializes a new bulk allocator with a pool and allocates memory for it.
 *
 * @param alloc         pointer to an unused allocator handle
 * @param buffer_count  the number of buffers to allocate
 * @param buffer_size   the size of a single buffer
 * @param constraints   memory requirements for this pool or NULL if none
 */
errval_t bulk_alloc_init(struct bulk_allocator         *alloc,
                         size_t                         buffer_count,
                         size_t                         buffer_size,
                         struct bulk_pool_constraints  *constraints);

/**
 * creates a new allocator based on the supplied capability. It creates as many
 * buffers as possible of size buffer_size that fit into the capability.
 *
 * @param alloc         an unused allocator handle
 * @param buffer_size   the size of a single buffer
 * @param frame         capability for backing the bulk pool
 */
errval_t bulk_alloc_init_from_cap(struct bulk_allocator   *alloc,
                                  size_t                   buffer_size,
                                  struct capref            *frame);


/**
 * Frees up the bulk allocator and it's pool.
 *
 * @param alloc handle to a bulk allocator to be freed
 */
errval_t bulk_alloc_free(struct bulk_allocator *alloc);



/**
 * Gets a new bulk buffer from the allocator.
 *
 * @param   alloc   the allocator handle to allocate the buffer from
 *
 * @return  pointer to a bulk_buffer on success
 *          NULL if there are no buffer left
 *
 */
struct bulk_buffer *bulk_alloc_new_buffer(struct bulk_allocator *alloc);


/**
 * returns a buffer back to the allocator. The pools must match.
 *
 * @param alloc     the allocator to hand the buffer back
 * @param buffer    the buffer to hand back to the allocator
 */
errval_t bulk_alloc_return_buffer(struct bulk_allocator *alloc,
                                  struct bulk_buffer    *buffer);


#endif /* BULK_ALLOCAOR_H */

