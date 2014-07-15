/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 *
 *
 */

#include <barrelfish/barrelfish.h>

#include <dev/ioat_dma_dev.h>

#include "ioat_dma.h"
#include "ioat_dma_device.h"
#include "ioat_dma_channel.h"
#include "ioat_dma_request.h"
#include "ioat_dma_descriptors.h"

#include "debug.h"



uint64_t req_counter = 1;

static ioat_dma_req_id_t generate_req_id(struct ioat_dma_channel *chan)
{
    ioat_dma_req_id_t id = ioat_dma_channel_get_id(chan);
    return (id << 48) | (req_counter++);
}



static struct ioat_dma_request *req_free_list = NULL;


static struct ioat_dma_request *request_alloc(void)
{
    struct ioat_dma_request *ret;

    if (req_free_list) {
        ret = req_free_list;
        req_free_list = ret->next;
        return ret;
    }
    ret = calloc(1, sizeof(*ret));

    return ret;
}

static void request_free(struct ioat_dma_request *req)
{
    req->next = req_free_list;
    req_free_list = req;
}


static uint32_t req_num_desc_needed(uint32_t max_xfer_size,
                                    size_t bytes)
{
    bytes += (max_xfer_size - 1);
    return (uint32_t) (bytes/max_xfer_size);
}


errval_t ioat_dma_request_memcpy(struct ioat_dma_device *dev,
                                 struct ioat_dma_req_setup *setup)
{
    errval_t err;

    IOREQ_DEBUG("New DMA Memcpy request: [0x%016lx]->[0x%016lx] of %lu bytes\n",
                setup->src, setup->dst, setup->bytes);

    struct ioat_dma_channel *chan = ioat_dma_channel_get(dev);

    uint32_t num_desc = req_num_desc_needed(dev->xfer_size_max, setup->bytes);

    IOREQ_DEBUG("%u descriptors\n",  num_desc);

    /* todo: get request */
    struct ioat_dma_request *req = request_alloc();
    if (req == NULL) {
        return -1;
    }

    struct ioat_dma_desc_alloc *alloc = ioat_dma_channel_get_desc_alloc(chan);

    struct ioat_dma_descriptor *head = ioat_dma_desc_chain_alloc(alloc, num_desc);
    if (head == NULL) {
        request_free(req);
        return -1;
    }

    ioat_dma_desc_ctrl_array_t ctrl = { 0 };
    /* todo: fill ctrl */

    struct ioat_dma_descriptor *desc = head;
    size_t bytes = 0;
    size_t length = setup->bytes;
    for (uint32_t i = 0; i < num_desc; ++i) {
        if (length > dev->xfer_size_max) {
            bytes = dev->xfer_size_max;
        } else {
            bytes = length;
        }
        ioat_dma_desc_init(desc, setup->src, setup->dst, bytes, ctrl);
        ioat_dma_desc_set_request(desc, req);
        length -= bytes;
        desc = ioat_dma_desc_get_next(desc);
    }

    req->desc_head = head;
    req->desc_tail = desc;
    req->done_cb = setup->done_cb;
    req->arg = setup->arg;
    req->id = generate_req_id(chan);

    err = ioat_dma_channel_submit(chan, req);
    if (err_is_fail(err)) {
        request_free(req);
        ioat_dma_desc_chain_free(head);
        return SYS_ERR_OK;
    }

    return SYS_ERR_OK;
}
