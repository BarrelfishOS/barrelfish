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
    struct virtio_buffer **buffers;  ///< array of virtio_buffers
    uint16_t size;                  ///< number of buffers in this allocator
    uint16_t top;                   ///< pointer to the top slot
    struct capref cap;              ///< frame capability backing this allocator
    struct virtio_device *queue;    ///< the VirtIO device this allocator belongs to
};

/**
 * \brief   assigns a buffer allocator to a virtqueue that it can be used as
 *          buffers over the VirtIO channel.
 *
 * \param   bf   buffer allocator
 * \param   vdev virtqueue the buffer allocator gets added to
 */
errval_t virtio_buffer_alloc_assing(struct virtio_buffer_allocator *bf,
                                    struct virtio_device *vdev);


/**
 * \brief allocated and initializes a new buffer allocator based on the
 *        capability with an already existing mapping
 *
 * \param bf        where to store the buffer allocator pointer
 * \param cap       capability of the buffers
 * \param vaddr     virtual address where they are mapped
 * \param offset    offset where the buffers start
 * \param bufsize   size of a single buffer
 * \param bufcount  number of buffers
 */
errval_t virtio_buffer_alloc_init_vq(struct virtio_buffer_allocator **bf,
                                     struct capref cap,
                                     lvaddr_t vaddr,
                                     lpaddr_t offset,
                                     size_t bufsize,
                                     size_t bufcount);


#endif // VIRTIO_VIRTIO_BUFFER_H
