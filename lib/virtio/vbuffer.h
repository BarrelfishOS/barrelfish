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
    struct virtio_buffer *buffers;
    struct virtio_buffer **buf_stack;   ///< array of virtio_buffers
    uint16_t buf_size;                  ///< size of a buffer
    uint16_t buf_count;                 ///< the number of buffers
    uint16_t top;                       ///< pointer to the top slot
    lpaddr_t offset;                    ///< the offset into the cap
    struct capref cap;                  ///< frame capability backing this allocator
    struct virtio_device *queue;        ///< the VirtIO device this allocator belongs to
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



#endif // VIRTIO_VIRTIO_BUFFER_H
