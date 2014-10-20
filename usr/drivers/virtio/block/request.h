/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VBLOCK_REQUEST_H_
#define VBLOCK_REQUEST_H_

#include "device.h"

/**
 * \brief allocates the request structures based on the number of
 *        descriptors on the virtqueue of the device
 *
 * \param dev   vblock device to allocate the requests for
 *
 * \returns SYS_ERR_OK on sucess
 */
errval_t vblock_request_queue_init(struct vblock_device *dev);

/**
 * \brief frees the allocated requests
 *
 * \param dev   vblock device to free the requests
 *
 * \returns SYS_ERR_OK on success
 */
void vblock_request_queue_destroy(struct vblock_device *dev);

/**
 * \brief Dequeues a request from the request queue
 *
 * \param queue the queue to dequeue a request on
 *
 * \returns vblock_req on success
 *          NULL if the queue was empty
 */
struct vblock_req *vblock_request_dequeue(struct vblock_req_queue *queue);

/**
 * \brief Dequeues a request from the device's free queue
 *
 * \param queue the queue to dequeue a request on
 *
 * \returns vblock_req on success
 *          NULL if the queue was empty
 */
static inline struct vblock_req *vblock_request_alloc(struct vblock_device *dev)
{
    return vblock_request_dequeue(&dev->free_queue);
}

/**
 * \brief enqueues a request into a request queue
 *
 * \param queue the queue to insert the request in
 * \param req   the request to insert
 */
void vblock_request_enqueue(struct vblock_req_queue *queue,
                            struct vblock_req *req);

/**
 * \brief returns a previously allocated reqest by enqueueing it into the
 *        device's free queue
 *
 * \param queue the queue to insert the request in
 * \param req   the request to ins
 */
static inline void vblock_request_free(struct vblock_device *dev,
                                       struct vblock_req *req)
{
    vblock_request_enqueue(&dev->free_queue, req);
}

/**
 * \brief executes a request on the VirtIO block device
 *
 * \param dev   the VirtIO block device
 * \param req   the request to execute
 *
 * \return SYS_ERR_OK on success
 */
errval_t vblock_request_start(struct vblock_device *dev,
                              struct vblock_req *req);

/**
 * \brief executes a request on the VirtIO block device and waits
 *        for its completion by polling the queue
 *
 * \param dev   the VirtIO block device
 * \param req   the request to execute
 *
 * \return SYS_ERR_OK on success
 *
 * NOTE: This is a blocking call
 */
errval_t vblock_request_exec(struct vblock_device *dev,
                             struct vblock_req *req);


/**
 * \brief works through the virtqueue and handling all the completed descriptors
 *
 * \param dev VirtIO block device
 *
 * \return SYS_ERR_OK on success
 */
errval_t vblock_request_finish_completed(struct vblock_device *dev);

/**
 * \brief   issues a get device identifier request on the device
 *
 * \param   dev VirtIO block device to issue the get ID request
 */
errval_t vblock_request_issue_get_id(struct virtio_device_blk *dev);

errval_t vblock_request_issue_write(struct virtio_device_blk *dev);

errval_t vblock_request_issue_read(struct virtio_device_blk *dev);

errval_t vblock_request_issue_flush(struct virtio_device_blk *dev);

errval_t vblock_request_issue_barrier(struct virtio_device_blk *dev);

#endif /* VBLOCK_REQUEST_H_ */
