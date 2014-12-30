/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include<barrelfish/barrelfish.h>


#include <xeon_phi/xeon_phi_dma_internal.h>
#include <xeon_phi/xeon_phi_dma_device_internal.h>
#include <xeon_phi/xeon_phi_dma_channel_internal.h>
#include <xeon_phi/xeon_phi_dma_request_internal.h>
#include <dma_ring_internal.h>
#include <xeon_phi/xeon_phi_dma_descriptors_internal.h>

#include <debug.h>

/**
 * represens the Xeon Phi specific DMA requests
 */
struct xeon_phi_dma_request
{
    struct dma_request common;
    struct dma_descriptor *desc_head;
    struct dma_descriptor *desc_tail;
};

/*
 * ---------------------------------------------------------------------------
 * Request Management
 * ---------------------------------------------------------------------------
 */

/// caches allocated requests which are no longer used
static struct dma_request *req_free_list = NULL;

/**
 * \brief allocates a Xeon Phi DMA request structure
 *
 * \returns Xeon Phi DMA request
 *          NULL on failure
 */
static struct xeon_phi_dma_request *request_alloc(void)
{
    struct xeon_phi_dma_request *ret;

    if (req_free_list) {
        ret = (struct xeon_phi_dma_request *) req_free_list;
        req_free_list = ret->common.next;

        DMAREQ_DEBUG("meta: reusing request %p. freelist:%p\n", ret,
                     req_free_list);

        return ret;
    }
    return calloc(1, sizeof(*ret));
}

/**
 * \brief frees up the used DMA request structure
 *
 * \param req   DMA request to be freed
 */
static void request_free(struct xeon_phi_dma_request *req)
{
    DMAREQ_DEBUG("meta: freeing request %p.\n", req);
    req->common.next = req_free_list;
    req_free_list = &req->common;
}

/*
 * ---------------------------------------------------------------------------
 * Helper Functions
 * ---------------------------------------------------------------------------
 */

inline static uint32_t req_num_desc_needed(struct xeon_phi_dma_channel *chan,
                                           size_t bytes)
{
    struct dma_channel *dma_chan = (struct dma_channel *) chan;
    uint32_t max_xfer_size = dma_channel_get_max_xfer_size(dma_chan);
    bytes += (max_xfer_size - 1);
    return (uint32_t) (bytes / max_xfer_size);
}

/*
 * ===========================================================================
 * Library Internal Interface
 * ===========================================================================
 */

/**
 * \brief handles the processing of completed DMA requests
 *
 * \param req   the DMA request to process
 *
 * \returns SYS_ERR_OK on sucess
 *          errval on failure
 */
errval_t xeon_phi_dma_request_process(struct xeon_phi_dma_request *req)
{
    errval_t err;

    req->common.state = DMA_REQ_ST_DONE;

    err = dma_request_process(&req->common);
    if (err_is_fail(err)) {
        return err;
    }

    request_free(req);

    return SYS_ERR_OK;
}

/*
 * ===========================================================================
 * Public Interface
 * ===========================================================================
 */

/**
 * \brief issues a memcpy request to the given channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_dma_request_memcpy_chan(struct dma_channel *chan,
                                          struct dma_req_setup *setup,
                                          dma_req_id_t *id)
{
    assert(chan->device->type == DMA_DEV_TYPE_XEON_PHI);

    struct xeon_phi_dma_channel *xchan = (struct xeon_phi_dma_channel *) chan;

    uint32_t num_desc = req_num_desc_needed(xchan, setup->args.memcpy.bytes);

    XPHIREQ_DEBUG("DMA Memcpy request: [0x%016lx]->[0x%016lx] of %lu bytes (%u desc)\n",
                  setup->args.memcpy.src, setup->args.memcpy.dst,
                  setup->args.memcpy.bytes, num_desc);

    struct dma_ring *ring = xeon_phi_dma_channel_get_ring(xchan);

    if (num_desc > dma_ring_get_space(ring)) {
        XPHIREQ_DEBUG("Too less space in ring: %u / %u\n", num_desc,
                      dma_ring_get_space(ring));
        return DMA_ERR_NO_DESCRIPTORS;
    }

    struct xeon_phi_dma_request *req = request_alloc();
    if (req == NULL) {
        XPHIREQ_DEBUG("No request descriptors for holding request data\n");
        return DMA_ERR_NO_REQUESTS;
    }

    dma_request_common_init(&req->common, chan, setup->type);

    struct dma_descriptor *desc;
    size_t length = setup->args.memcpy.bytes;
    lpaddr_t src = setup->args.memcpy.src;
    lpaddr_t dst = setup->args.memcpy.dst;
    uint32_t flags = 0;
    size_t bytes, max_xfer_size = dma_channel_get_max_xfer_size(chan);
    do {
        desc = dma_ring_get_next_desc(ring);

        if (!req->desc_head) {
            req->desc_head = desc;
        }
        if (length <= max_xfer_size) {
            /* the last one */
            bytes = length;
            req->desc_tail = desc;
        } else {
            bytes = max_xfer_size;
        }
        xeon_phi_dma_desc_fill_memcpy(desc, src, dst, bytes, flags);
        dma_desc_set_request(desc, NULL);

        length -= bytes;
        src += bytes;
        dst += bytes;
    } while (length > 0);

    req->common.setup = *setup;

    if (id) {
        *id = req->common.id;
    }
    /* set the request pointer in the last descriptor */
    dma_desc_set_request(desc, &req->common);

    assert(req->desc_tail);
    assert(dma_desc_get_request(req->desc_tail));

    //desc = dma_ring_get_next_desc(ring);
    //xeon_phi_dma_desc_fill_status(desc, (512UL * 1024 * 1024 * 1024) + xchan->dstat.paddr, 0xFAFAFAFA, 0);

    return xeon_phi_dma_channel_submit_request(xchan, req);
}

/**
 * \brief issues a memcpy request to a channel of the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_dma_request_memcpy(struct dma_device *dev,
                                     struct dma_req_setup *setup,
                                     dma_req_id_t *id)
{
    struct dma_channel *chan = dma_device_get_channel(dev);
    return xeon_phi_dma_request_memcpy_chan(chan, setup, id);
}

/**
 * \brief issues a NOP / NULL descriptor request on the given channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
void xeon_phi_dma_request_nop_chan(struct dma_channel *chan)
{
    struct xeon_phi_dma_channel *xchan = (struct xeon_phi_dma_channel *)chan;
    struct dma_ring *ring = xeon_phi_dma_channel_get_ring(xchan);
    assert(ring);

    struct dma_descriptor *desc = dma_ring_get_next_desc(ring);
    assert(desc);
    XPHIREQ_DEBUG("New DMA NOP request: descriptor=%p\n", desc);

    xeon_phi_dma_desc_fill_nop(desc);
}

/**
 * \brief issues a NOP / NULL descriptor request on the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
inline void xeon_phi_dma_request_nop(struct dma_device *dev)
{
    struct dma_channel *chan = dma_device_get_channel(dev);
    xeon_phi_dma_request_nop_chan(chan);
}
