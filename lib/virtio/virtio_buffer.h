/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_VIRTIO_BUFFER_H
#define VIRTIO_VIRTIO_BUFFER_H

/**
 * library internal representation of a buffer allocator
 */
struct virtio_buffer_allocator
{
    struct virtio_buffer *buffers;  ///< array of virtio_buffers
    uint16_t size;                  ///< number of buffers in this allocator
    uint16_t top;                   ///< pointer to the top slot
    struct capref cap;              ///< frame capability backing this allocator
    struct virtqueue *queue;        ///< the virtqueue this allocator belongs to
};

/**
 * \brief   initializes the buffer allocator and allocates memory for the
 *          buffers
 *
 * \param   alloc   the allocator struct to initialize
 * \param   nbufs   number of buffers to allocate
 * \param   bufsz   size of each buffer to allocate
 *
 * \return  SYS_ERR_OK on success
 */
errval_t virtio_buffer_alloc_init(struct virtio_buffer_allocator *alloc,
                                  size_t nbufs,
                                  size_t bufsz);

/**
 * \brief   destroys a buffer allocator by freeing up all the resources used
 *          by the buffers
 *
 * \param   alloc   the allocator to destroy
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_buffer_alloc_destroy(struct virtio_buffer_allocator *alloc);


struct virtio_buffer *virtio_buffer_alloc(struct virtio_buffer_allocator *alloc);

/**
 * \brief   frees up a unused buffer by returning it to the allocator
 *
 * \param   buf     the buffer to be freed
 */
errval_t virtio_buffer_free(struct virtio_buffer_allocator *alloc,
                            struct virtio_buffer *buf);

/**
 * \brief   returns the backing frame capability of a buffer allocator
 */
errval_t virtio_buffer_alloc_get_cap(struct virtio_buffer_allocator *alloc,
                                     struct capref *ret_cap);


#endif // VIRTIO_VIRTIO_BUFFER_H
