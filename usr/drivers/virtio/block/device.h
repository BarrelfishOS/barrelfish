/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VBLOCK_DEVICE_H_
#define VBLOCK_DEVICE_H_


#define VBLOCK_DRIVER_BACKEND VIRTIO_DEVICE_BACKEND_MMIO

// forward declaration
struct vblock_req;

/*
 * VirtIO Block Device Flags
 */
/// The block device is read only
#define VBLOCK_FLAGS_READONLY (1 << 1)

enum vblock_device_state {
    VBLOCK_DEVICE_STATE_RESET,
    VBLOCK_DEVICE_STATE_READY,
    VBLOCK_DEVICE_STATE_FAILURE
};

typedef void(*req_callback_t)(struct vblock_req *);

 struct vblock_req {
     struct virtio_block_reqhdr header;
     struct virtio_buffer_list  bl;
     req_callback_t callback;
     void *arg;
     uint8_t ack;
     uint8_t writable;
     /* TODO: flounder binding */
     struct vblock_req *next;
     struct vblock_req_queue *queue;
 };

struct vblock_req_queue
{
     uint32_t length;
     struct vblock_req *head;
     struct vblock_req *tail;
     void (*op)(struct vblock_req *req);
};

#define VBLOCK_FEATURE_
#define VBLOCK_DRIVER_FEATURES 0

struct vblock_device
{
    struct virtio_device_blk blk;
    uint32_t flags;
    enum vblock_device_state state;


    struct vblock_req_queue ready_queue;    ///< queue of executed requests
    struct vblock_req_queue free_queue;     ///< queue of free requests

    struct vblock_req *requests;
    struct virtio_buffer_allocator *alloc;

    struct virtio_buffer_allocator *bf_status;
    struct virtio_buffer_allocator *bf_header;

    /* TODO; pointers to flounder states*/
    iref_t svc_iref;
};

/**
 * \brief handles the VirtIO config change interrupt
 *
 * \param pointer to the virtio device
 *
 */
void vblock_device_config_changed_intr(struct virtio_device *vdev);

/**
 * \brief handles the VirtIO interrupts
 *
 * \param arg the argument passed to the virtqueue when allocating it
 */
void vblock_device_virtqueue_intr(void *arg);



errval_t vblock_device_init(struct vblock_device *blk,
                            void *dev_regs,
                            size_t ret_size);

#endif /* VBLOCK_DEVICE_H_ */
