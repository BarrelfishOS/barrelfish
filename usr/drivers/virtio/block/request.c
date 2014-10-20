/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>

#include <virtio/virtio.h>
#include <virtio/virtqueue.h>
#include <virtio/virtio_device.h>
#include <virtio/devices/virtio_block.h>

#include "device.h"
#include "request.h"
#include "debug.h"

static errval_t request_status(struct vblock_req *req)
{
    switch(req->ack) {
    case VIRTIO_BLOCK_S_OK:
        return SYS_ERR_OK;
    case VIRTIO_BLOCK_S_IOERR:
        return VIRTIO_ERR_BLK_REQ_IOERR;
    case VIRTIO_BLOCK_S_UNSUPP:
        return VIRTIO_ERR_BLK_REQ_UNSUP;
    default:
        return VIRTIO_ERR_BLK_REQ_UNSUP;
    }

    return VIRTIO_ERR_BLK_REQ_UNSUP;
}

static errval_t request_finish(struct vblock_req *req)
{
    struct virtio_buffer *buf;

    buf = virtio_blist_head(&req->bl);
    assert(buf);
    virtio_buffer_free(buf);


    buf = virtio_blist_tail(&req->bl);
    assert(buf);
    virtio_buffer_free(buf);

    return SYS_ERR_OK;
}

/**
 * \brief allocates the request structures based on the number of
 *        descriptors on the virtqueue of the device
 *
 * \param dev   vblock device to allocate the requests for
 *
 * \returns SYS_ERR_OK on sucess
 */
errval_t vblock_request_queue_init(struct vblock_device *dev)
{
    uint16_t ndesc = virtio_virtqueue_get_num_desc(dev->blk.vq);

    VBLOCK_DEBUG_REQ("initialize request queue [%u]\n", ndesc);


    struct vblock_req *req = calloc(ndesc, sizeof(struct vblock_req));
    if (req == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    dev->free_queue.length = ndesc;
    dev->free_queue.head = req;
    dev->free_queue.tail = req+(ndesc-1);

    for (uint32_t i = 0; i < ndesc-1; ++i) {
        req->queue = &dev->free_queue;
        req->next = req+1;
        req++;
    }

    dev->requests = req;

    return SYS_ERR_OK;
}

/**
 * \brief frees the allocated requests
 *
 * \param dev   vblock device to free the requests
 *
 * \returns SYS_ERR_OK on success
 */
void vblock_request_queue_destroy(struct vblock_device *dev)
{
    /*
     * TODO: check that every request is finished.
     */
    free(dev->requests);
}

/**
 * \brief Dequeues a request from the request queue
 *
 * \param queue the queue to dequeue a request on
 *
 * \returns vblock_req on success
 *          NULL if the queue was empty
 */
struct vblock_req *vblock_request_dequeue(struct vblock_req_queue *queue)
{
    struct vblock_req *req;

    if (queue->length == 0) {
        return NULL;
    }

    assert(queue->head);

    req = queue->head;

    if (queue->length == 1) {
        queue->head = NULL;
        queue->tail = NULL;
    } else {
        queue->head = req->next;
    }

    queue->length--;

    req->queue = NULL;
    req->next = NULL;

    return req;
}

/**
 * \brief enqueues a request into a request queue
 *
 * \param queue the queue to insert the request in
 * \param req   the request to insert
 */
void vblock_request_enqueue(struct vblock_req_queue *queue,
                            struct vblock_req *req)
{
    assert(req->queue == NULL);
    assert(req->next == NULL);

    if (queue->length == 0) {
        queue->head = req;
        queue->tail = req;
    } else {
        queue->tail->next = req;
        queue->tail = req;
    }

    queue->length++;

    req->queue = queue;
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
                              struct vblock_req *req)
{
    VBLOCK_DEBUG_REQ("starting request [%016lx]\n", (uintptr_t)req);

    errval_t err;
    uint16_t num_rd = 0;
    uint16_t num_wr = 0;

    struct virtqueue *vq = dev->blk.vq;


    if (req->writable) {
        num_wr = req->bl.length;
    } else {
        num_rd = req->bl.length;
    }


    struct virtio_buffer *buf = virtio_buffer_alloc(dev->bf_header);
    if (!buf) {
        return VIRTIO_ERR_NO_BUFFER;
    }

    assert(buf->length == sizeof(struct virtio_block_reqhdr));

    buf->data_length = sizeof(struct virtio_block_reqhdr);
    buf->state = VIRTIO_BUFFER_S_ALLOCED_READABLE;
    memcpy(buf->buf, &req->header, sizeof(req->header));

    err = virtio_blist_prepend(&req->bl, buf);
    assert(err_is_ok(err));
    num_rd++;


    buf = virtio_buffer_alloc(dev->bf_status);
    if (!buf) {
        return VIRTIO_ERR_NO_BUFFER;
    }

    assert(buf->length == sizeof(uint8_t));

    buf->data_length = sizeof(uint8_t);
    buf->state = VIRTIO_BUFFER_S_ALLOCED_WRITABLE;
    num_wr++;
    err = virtio_blist_append(&req->bl, buf);
    assert(err_is_ok(err));

    return virtio_virtqueue_desc_enqueue(vq, &req->bl, req, num_rd, num_wr);
}

/**
 * \brief executes a request on the VirtIO block device and waits
 *        for its completion by polling the queue
 *
 * \param dev   the VirtIO block device
 * \param req   the request to execute
 *
 * \return SYS_ERR_OK on success
 *
 * NOTE: This is a blocking call. When the function returns, the device request
 *       req has been executed and holds the data.
 */
errval_t vblock_request_exec(struct vblock_device *dev,
                             struct vblock_req *req)
{
    errval_t err;
    struct virtqueue *vq = dev->blk.vq;

    /* when executing a request, there must be no other request in flight */
    if (!virtio_virtqueue_is_empty(vq)) {
        return VIRTIO_ERR_QUEUE_BUSY;
    }

    /* start the requst */
    err = vblock_request_start(dev, req);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failure while starting request\n");
        return err;
    }

    /* notify the host and poll the list */
    virtio_virtqueue_notify_host(vq);

    struct virtio_buffer_list *bl;
    void *ret_req;
    err = virtio_virtqueue_poll(vq, &bl, &ret_req, 1);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failure while polling virtqueue\n");
        return err;
    }

    assert(ret_req == req);

    request_finish(req);

    return request_status(req);
}

/**
 * \brief works through the virtqueue and handling all the completed descriptors
 *
 * \param dev VirtIO block device
 *
 * \return SYS_ERR_OK on success
 */
errval_t vblock_request_finish_completed(struct vblock_device *dev)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}


/**
 * \brief   issues a get device identifier request on the device
 *
 * \param   dev VirtIO block device to issue the get ID request
 */
errval_t vblock_request_issue_get_id(struct virtio_device_blk *dev)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}



