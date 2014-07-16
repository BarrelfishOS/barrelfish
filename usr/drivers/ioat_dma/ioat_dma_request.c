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
#include "ioat_dma_ring.h"
#include "ioat_dma_descriptors.h"

#include "debug.h"


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
        return ret;
    }
    ret = calloc(1, sizeof(*ret));

    return ret;
}

/*
static void request_free(struct ioat_dma_request *req)
{
    req->next = req_free_list;
    req_free_list = req;
}
*/


/*
 * ---------------------------------------------------------------------------
 * Request ID generation
 * ---------------------------------------------------------------------------
 */

static uint64_t req_counter = 1;

static ioat_dma_req_id_t generate_req_id(struct ioat_dma_channel *chan)
{
    ioat_dma_req_id_t id = ioat_dma_channel_get_id(chan);
    return (id << 48) | (req_counter++);
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
 * Public Interface
 * ===========================================================================
 */

errval_t ioat_dma_request_memcpy_channel(struct ioat_dma_channel *chan,
                                         struct ioat_dma_req_setup *setup)
{
       uint32_t num_desc = req_num_desc_needed(chan, setup->bytes);

    IOREQ_DEBUG("DMA Memcpy request: [0x%016lx]->[0x%016lx] of %lu bytes (%u desc)\n",
                setup->src, setup->dst, setup->bytes, num_desc);

    struct ioat_dma_ring *ring = ioat_dma_channel_get_ring(chan);
    if (num_desc > ioat_dma_ring_get_space(ring)) {
        return IOAT_ERR_NO_DESCRIPTORS;
    }

    struct ioat_dma_request *req = request_alloc();
    if (req == NULL) {
        return IOAT_ERR_NO_REQUESTS;
    }

    ioat_dma_desc_ctrl_array_t ctrl = {
        0
    };

    struct ioat_dma_descriptor *desc;
    size_t length = setup->bytes;
    lpaddr_t src = setup->src;
    lpaddr_t dst = setup->dst;
    size_t bytes, max_xfer_size = ioat_dma_channel_get_max_xfer_size(chan);
    do {
        desc = ioat_dma_ring_get_next_desc(ring);

        if (!req->desc_head) {
            req->desc_head = desc;
        }
        if (length < max_xfer_size) {
            /* the last one */
            bytes = length;
            req->desc_tail = desc;

            ioat_dma_desc_ctrl_fence_insert(ctrl, setup->ctrl_fence);
            ioat_dma_desc_ctrl_int_en_insert(ctrl, setup->ctrl_intr);
            ioat_dma_desc_ctrl_compl_write_insert(ctrl, 0x1);
        } else {
            bytes = max_xfer_size;
        }

        ioat_dma_desc_fill_memcpy(desc, src, dst, bytes, ctrl);
        ioat_dma_desc_set_request(desc, req);

        length -= bytes;
        src += bytes;
        dst += bytes;
    } while(length > 0);

    req->setup = *setup;
    req->id = generate_req_id(chan);

    ioat_dma_channel_enq_request(chan, req);

    return SYS_ERR_OK;
}

inline errval_t ioat_dma_request_memcpy(struct ioat_dma_device *dev,
                                        struct ioat_dma_req_setup *setup)
{
    struct ioat_dma_channel *chan = ioat_dma_channel_get(dev);
    return ioat_dma_request_memcpy_channel(chan, setup);
}

void ioat_dma_request_nop(struct ioat_dma_channel *chan)
{

    struct ioat_dma_ring *ring = ioat_dma_channel_get_ring(chan);
    assert(ring);

    struct ioat_dma_descriptor *desc = ioat_dma_ring_get_next_desc(ring);

    IOREQ_DEBUG("New DMA NOP request: descriptor=%p\n", desc);

    ioat_dma_desc_fill_nop(desc);
}

errval_t ioat_dma_request_process(struct ioat_dma_request *req)
{
    return SYS_ERR_OK;
}
