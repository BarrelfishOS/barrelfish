/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include<barrelfish/barrelfish.h>

#include <client/dma_client_internal.h>
#include <client/dma_client_device_internal.h>
#include <client/dma_client_channel_internal.h>
#include <client/dma_client_request_internal.h>

#include <debug.h>

/*
 * ---------------------------------------------------------------------------
 * Request Cache /  Helper Functions
 * ---------------------------------------------------------------------------
 */
static struct dma_request *dma_request_cache = NULL;

static struct dma_client_request *request_alloc(void)
{
    if (dma_request_cache) {
        struct dma_request *st = dma_request_cache;
        dma_request_cache = dma_request_cache->next;
        return (struct dma_client_request *) st;
    }
    return calloc(1, sizeof(struct dma_client_request));
}

static inline void request_free(struct dma_client_request *st)
{
    st->common.next = dma_request_cache;
    dma_request_cache = (struct dma_request *) st;
}

/*
 * ============================================================================
 * Library Internal Interface
 * ============================================================================
 */

/**
 * \brief frees up a DMA request (called by the rx_done() function)
 *
 * \param req DMA request to be freed
 */
void dma_client_request_free(struct dma_client_request *req)
{
    request_free(req);
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief registers a memory region with a specified client connection
 *
 * \param chan  DMA client channel
 * \param frame the memory frame to register
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_register_memory_chan(struct dma_channel *chan,
                                         struct capref frame)
{
    errval_t err;
    assert(chan->device->type == DMA_DEV_TYPE_CLIENT);

    CLIENTREQ_DEBUG("registering memory region.\n");

    struct dma_client_request *req = request_alloc();
    if (req == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    req->cap = frame;
    req->common.state = DMA_REQ_ST_PREPARED;
    req->common.type = DMA_REQ_TYPE_MEM_REGISTER;

    err = dma_client_channel_submit_request(chan, &req->common);
    if (err_is_fail(err)) {
        request_free(req);
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief registers a memory region with a specified client connection
 *
 * \param dev   DMA client device
 * \param frame the memory frame to register
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_register_memory(struct dma_device *dev,
                                    struct capref frame)
{
    struct dma_channel *chan = dma_device_get_channel(dev);
    return dma_client_register_memory_chan(chan, frame);
}

/**
 * \brief deregisters a previously registered memory region from the connection
 *
 * \param chan  DMA client channel
 * \param frame the memory frame to deregister
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_deregister_memory_chan(struct dma_channel *chan,
                                           struct capref frame)
{
    errval_t err;
    assert(chan->device->type == DMA_DEV_TYPE_CLIENT);

    CLIENTREQ_DEBUG("removing memory region.\n");

    struct dma_client_request *req = request_alloc();
    if (req == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    req->cap = frame;
    req->common.state = DMA_REQ_ST_PREPARED;
    req->common.type = DMA_REQ_TYPE_MEM_REMOVE;

    err = dma_client_channel_submit_request(chan, &req->common);
    if (err_is_fail(err)) {
        request_free(req);
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief deregisters a previously registered memory region from the connection
 *
 * \param dev   DMA client device
 * \param frame the memory frame to deregister
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_deregister_memory(struct dma_device *dev,
                                      struct capref frame)
{
    struct dma_channel *chan = dma_device_get_channel(dev);
    return dma_client_deregister_memory_chan(chan, frame);
}

/**
 * \brief issues a memcopy request to the service using the connection
 *
 * \param chan  DMA client channel
 * \param setup DMA request setup information
 * \param id    Returns the assigned DMA request ID
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_request_memcpy_chan(struct dma_channel *chan,
                                        struct dma_req_setup *setup,
                                        dma_req_id_t *id)
{
    errval_t err;
    assert(chan->device->type == DMA_DEV_TYPE_CLIENT);

    CLIENTREQ_DEBUG("memcpy request [%016lx]->[%016lx]\n", setup->args.memcpy.src,
                    setup->args.memcpy.dst);

    struct dma_client_request *req = request_alloc();
    if (req == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    if (id) {
        *id = 0;
    }

    req->common.setup = *setup;
    req->common.state = DMA_REQ_ST_PREPARED;
    req->common.type = DMA_REQ_TYPE_MEMCPY;

    err = dma_client_channel_submit_request(chan, &req->common);
    if (err_is_fail(err)) {
        request_free(req);
        return err;
    }

    if (id) {
        *id = req->common.id;
    }

    err = req->common.err;

    switch (req->common.state) {
        case DMA_REQ_ST_ERR:
            request_free(req);
            return err;
        case DMA_REQ_ST_DONE:
            if (setup->done_cb) {
                setup->done_cb(err, req->common.id, setup->cb_arg);
            }
            request_free(req);
            return SYS_ERR_OK;
        default:
            return SYS_ERR_OK;
    }
}

/**
 * \brief issues a memcopy request to the service using the connection
 *
 * \param dev   DMA client device
 * \param setup DMA request setup information
 * \param id    Returns the assigned DMA request ID
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_request_memcpy(struct dma_device *dev,
                                   struct dma_req_setup *setup,
                                   dma_req_id_t *id)
{
    struct dma_channel *chan = dma_device_get_channel(dev);
    return dma_client_request_memcpy_chan(chan, setup, id);
}

