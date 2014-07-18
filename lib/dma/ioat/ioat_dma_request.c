/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include<barrelfish/barrelfish.h>

#include <dma_request_internal.h>
#include <ioat/ioat_dma_device_internal.h>
#include <ioat/ioat_dma_channel_internal.h>
#include <ioat/ioat_dma_request_internal.h>
#include <ioat/ioat_dma_descriptors_internal.h>
#include <ioat/ioat_dma_ring_internal.h>

#include <debug.h>

/**
 *
 */
struct ioat_dma_request
{
    struct dma_req_int common;
    struct ioat_dma_descriptor *desc_head;
    struct ioat_dma_descriptor *desc_tail;
    struct ioat_dma_request *next;
};

/*
 * ---------------------------------------------------------------------------
 * Request Management
 * ---------------------------------------------------------------------------
 */

static struct ioat_dma_request *req_free_list = NULL;

static struct ioat_dma_request *request_alloc(void)
{
    struct ioat_dma_request *ret;

    if (req_free_list) {
        ret = req_free_list;
        req_free_list = ret->next;

        IOATREQ_DEBUG("meta: reusing request %p. freelist:%p\n", ret, req_free_list);

        return ret;
    }
    return calloc(1, sizeof(*ret));
}

static void request_free(struct ioat_dma_request *req)
{
    IOATREQ_DEBUG("meta: freeing request %p.\n", req);
    req->next = req_free_list;
    req_free_list = req;
}

/*
 * ---------------------------------------------------------------------------
 * Helper Functions
 * ---------------------------------------------------------------------------
 */

inline static uint32_t req_num_desc_needed(struct ioat_dma_channel *chan,
                                           size_t bytes)
{
    uint32_t max_xfer_size = ioat_dma_channel_get_max_xfer_size(chan);
    bytes += (max_xfer_size - 1);
    return (uint32_t) (bytes / max_xfer_size);
}

/*
 * ===========================================================================
 * Library Internal Interface
 * ===========================================================================
 */

/*
 * ===========================================================================
 * Public Interface
 * ===========================================================================
 */

/**
 * \brief issues a memcpy request to the given channel
 *
 * \param chan  IOAT DMA channel
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_request_memcpy_chan(struct ioat_dma_channel *chan,
                                      struct dma_req_setup *setup)
{
    uint32_t num_desc = req_num_desc_needed(chan, setup->args.memcpy.bytes);

    IOATREQ_DEBUG("DMA Memcpy request: [0x%016lx]->[0x%016lx] of %lu bytes (%u desc)\n",
                setup->args.memcpy.src, setup->args.memcpy.dst, setup->args.memcpy.bytes, num_desc);

    struct ioat_dma_ring *ring = ioat_dma_channel_get_ring(chan);

    if (num_desc > ioat_dma_ring_get_space(ring)) {
        IOATREQ_DEBUG("Too less space in ring: %u / %u\n", num_desc,
                    ioat_dma_ring_get_space(ring));
        return IOAT_ERR_NO_DESCRIPTORS;
    }

    struct ioat_dma_request *req = request_alloc();
    if (req == NULL) {
        IOATREQ_DEBUG("No request descriptors for holding request data\n");
        return IOAT_ERR_NO_REQUESTS;
    }

    ioat_dma_desc_ctrl_array_t ctrl = {
        0
    };

    struct ioat_dma_descriptor *desc;
    size_t length = setup->args.memcpy.bytes;
    lpaddr_t src = setup->args.memcpy.src;
    lpaddr_t dst = setup->args.memcpy.dst;
    size_t bytes, max_xfer_size = ioat_dma_channel_get_max_xfer_size(chan);
    do {
        desc = ioat_ring_get_next_desc(ring);

        if (!req->desc_head) {
            req->desc_head = desc;
        }
        if (length <= max_xfer_size) {
            /* the last one */
            bytes = length;
            req->desc_tail = desc;

            ioat_dma_desc_ctrl_fence_insert(ctrl, setup->args.memcpy.ctrl_fence);
            ioat_dma_desc_ctrl_int_en_insert(ctrl, setup->args.memcpy.ctrl_intr);
            ioat_dma_desc_ctrl_compl_write_insert(ctrl, 0x1);
        } else {
            bytes = max_xfer_size;
        }

        ioat_dma_desc_fill_memcpy(desc, src, dst, bytes, ctrl);
        ioat_desc_set_request(desc, NULL);

        length -= bytes;
        src += bytes;
        dst += bytes;
    } while (length > 0);

    req->common.req.setup = *setup;
    req->common.req.id = generate_req_id(chan);

    /* set the request pointer in the last descriptor */
    ioat_desc_set_request(desc, req);

    assert(req->desc_tail);
    assert(ioat_dma_desc_get_request(req->desc_tail));

    ioat_dma_channel_enq_request(chan, req);

    return SYS_ERR_OK;
}

/**
 * \brief issues a NOP / NULL descriptor request on the given channel
 *
 * \param chan  IOAT DMA channel
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
void ioat_dma_request_nop_chan(struct ioat_dma_channel *chan)
{

    struct ioat_dma_ring *ring = ioat_dma_channel_get_ring(chan);
    assert(ring);

    struct ioat_dma_descriptor *desc = ioat_ring_get_next_desc(ring);

    IOATREQ_DEBUG("New DMA NOP request: descriptor=%p\n", desc);

    ioat_dma_desc_fill_nop(desc);
}

