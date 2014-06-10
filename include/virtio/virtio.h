/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_H
#define VIRTIO_H

#include <barrelfish/barrelfish.h>


/// defines how we map the memory frames
#define VIRTIO_VREGION_FLAGS_DEVICE VREGION_FLAGS_READ_WRITE
#define VIRTIO_VREGION_FLAGS_RING VREGION_FLAGS_READ_WRITE


/*
 * Generic Feature Bits
 */

/// Generate interrupt if queue is completely used
#define VIRTIO_F_NOTIFY_ON_EMPTY (1 << 24)

/// guest should never set this feature. This indicates faulty drivers
#define VIRTIO_F_BAD_FEATURE (1 << 30)

/// range of the transport related feature bits
#define VIRTIO_TRANSPORT_F_START    28
#define VIRTIO_TRANSPORT_F_END      32




/**
 * \brief this struct represents a virtio driver
 *
 * This can be seen as on the guest side of the virtio channel
 */
struct virtio_driver
{
    void *device_config;


};


/**
 * VirtIO Memory segment
 */
struct virtio_buffer
{

};

struct virtio_buffer_allocator
{
    struct virtio_buffer *buffers;
    uint16_t size;
    uint16_t top;
};

errval_t virtio_buffer_alloc_init(struct virtio_buffer_allocator *alloc,
                                  uint16_t nbufs);

errval_t virtio_buffer_alloc(void);
errval_t virtio_buffer_free(void);


errval_t virtio_buffer_list_reset(void);
errval_t virtio_buffer_list_append(void);





#endif // VIRTIO_H
