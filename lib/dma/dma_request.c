/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

#include <dma_internal.h>
#include <dma_device_internal.h>
#include <dma_channel_internal.h>
#include <dma_request_internal.h>
#include <debug.h>

/*
 * ---------------------------------------------------------------------------
 * Request ID generation
 * ---------------------------------------------------------------------------
 */

inline dma_req_id_t dma_request_generate_req_id(struct dma_channel *chan)
{
    return dma_request_id_build(chan, dma_channel_incr_req_count(chan));
}

/**
 * \brief initializes the common part of a DMA request
 *
 * \param req   DMA request to initialize
 * \param chan  Channel this request gets executed on
 * \param type  The type of the DMA request
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t dma_request_common_init(struct dma_request *req,
                                 struct dma_channel *chan,
                                 dma_req_st_t type)
{
    req->id = dma_request_id_build(chan, dma_channel_incr_req_count(chan));
    req->type = type;
    req->state = DMA_REQ_ST_INVALID;

#if DMA_BENCH_ENABLED
    req->timer_start = bench_tsc();
#endif
    return SYS_ERR_OK;
}

/**
 * \brief handles the processing of completed DMA requests
 *
 * \param req   the DMA request to process
 *
 * \returns SYS_ERR_OK on sucess
 *          errval on failure
 */
errval_t dma_request_process(struct dma_request *req)
{
    DMAREQ_DEBUG("Processing done request [%016lx]:\n", req->id);

    errval_t err;

    switch (req->state) {
        case DMA_REQ_ST_DONE:
            err = SYS_ERR_OK;
            break;
        default:
            err = DMA_ERR_CHAN_ERROR;  // todo: error code
            break;
    }

#if DMA_BENCH_ENABLED
    cycles_t timer_end = bench_tsc();
    debug_printf("DMA request %016lx took %lu cycles (%lu ms)\n",
                 req->id, timer_end - req->timer_start,
                 bench_tsc_to_ms(timer_end - req->timer_start));
#endif

    if (req->setup.done_cb) {
        req->setup.done_cb(err, req->id, req->setup.cb_arg);
    }

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Getter / Setter Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the state of a DMA request
 *
 * \param req   DMA request
 *
 * \returns DMA request state
 */
inline dma_req_st_t dma_request_get_state(struct dma_request *req)
{
    return req->state;
}

/**
 * \brief returns the ID of a DMA request
 *
 * \param req   DMA request
 *
 * \returns DMA request ID
 */
inline dma_req_id_t dma_request_get_id(struct dma_request *req)
{
    return req->id;
}

/**
 * \brief returns the next DMA request of the given request
 *
 * \param req   DMA request
 *
 * \returns DMA request if there was one
 *          NULL if the request is at the end of the chain
 */
inline struct dma_request *dma_request_get_next(struct dma_request *req)
{
    return req->next;
}

/**
 * \brief sets the next DMA request of the given request
 *
 * \param req   DMA request
 * \param next  DMA request
 */
inline void dma_request_set_next(struct dma_request *req,
                                 struct dma_request *next)
{
    req->next = next;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/*
 * ----------------------------------------------------------------------------
 * Memory Registration
 * ----------------------------------------------------------------------------
 */

/**
 * \brief registers a memory region for future use
 *
 * \param frame the memory frame to register
 *
 * \returns SYS_ERR_OK on succes
 *          errval on error
 */
errval_t dma_register_memory(struct dma_device *dev,
                             struct capref frame)
{
    if (dev->f.register_memory == NULL) {
        return DMA_ERR_REQUEST_UNSUPPORTED;
    }
    return dev->f.register_memory(dev, frame);
}

/**
 * \brief deregisters a previously registered memory region
 *
 * \param frame the memory frame to register
 *
 * \returns SYS_ERR_OK on succes
 *          errval on error
 */
errval_t dma_request_deregister_memory(struct dma_device *dev,
                                       struct capref frame)
{
    if (dev->f.deregister_memory == NULL) {
        return DMA_ERR_REQUEST_UNSUPPORTED;
    }
    return dev->f.deregister_memory(dev, frame);
}

/*
 * ----------------------------------------------------------------------------
 * Request Execution
 * ----------------------------------------------------------------------------
 */

/**
 * \brief issues a new DMA memcpy request based on the setup information
 *
 * \param chan  DMA channel
 * \param setup DMA request setup information
 * \param id    returns the DMA request ID
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_request_memcpy_chan(struct dma_channel *chan,
                                 struct dma_req_setup *setup,
                                 dma_req_id_t *id)
{
    /* check for overlap */
    if (setup->args.memcpy.dst < setup->args.memcpy.src) {
        if ((setup->args.memcpy.dst + setup->args.memcpy.bytes) > setup->args.memcpy.src) {
            return DMA_ERR_MEM_OVERLAP;
        }
    } else if ((setup->args.memcpy.dst > setup->args.memcpy.src)) {
        if ((setup->args.memcpy.src + setup->args.memcpy.bytes) > setup->args.memcpy.dst) {
            return DMA_ERR_MEM_OVERLAP;
        }
    } else {
        /* they are equal so they will overlap */
        return DMA_ERR_MEM_OVERLAP;
    }

    if (chan->f.memcpy == NULL) {
        return DMA_ERR_REQUEST_UNSUPPORTED;
    }

    return chan->f.memcpy(chan, setup, id);
}

/**
 * \brief issues a new DMA memcpy request based on the setup information
 *
 * \param dev   DMA device
 * \param setup DMA request setup information
 * \param id    returns the DMA request ID
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_request_memcpy(struct dma_device *dev,
                            struct dma_req_setup *setup,
                            dma_req_id_t *id)
{
    struct dma_channel *chan = dma_device_get_channel(dev);
    return dma_request_memcpy_chan(chan, setup, id);
}

/**
 * \brief issues a new DMA memcpy request based on the setup information
 *
 * \param chan  DMA channel
 * \param setup DMA request setup information
 * \param id    returns the DMA request ID
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_request_memset_chan(struct dma_channel *chan,
                                 struct dma_req_setup *setup,
                                 dma_req_id_t *id)
{
    if (chan->f.memset == NULL) {
        return DMA_ERR_REQUEST_UNSUPPORTED;
    }

    return chan->f.memset(chan, setup, id);
}

/**
 * \brief issues a new DMA memcpy request based on the setup information
 *
 * \param dev   DMA device
 * \param setup DMA request setup information
 * \param id    returns the DMA request ID
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_request_memset(struct dma_device *dev,
                            struct dma_req_setup *setup,
                            dma_req_id_t *id)
{
    struct dma_channel *chan = dma_device_get_channel(dev);
    return dma_request_memset_chan(chan, setup, id);
}


