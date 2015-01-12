/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <dma_internal.h>
#include <dma_device_internal.h>
#include <dma_channel_internal.h>
#include <dma_request_internal.h>

#include <debug.h>

/*
 * ============================================================================
 * Library Internal Interface
 * ============================================================================
 */

/*
 * ----------------------------------------------------------------------------
 * Request List Management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the first request on the submitted request queue of the channel
 *
 * \param chan  DMA channel
 *
 * \returns pointer to the DMA request
 *          NULL if queue was empty
 */
struct dma_request *dma_channel_deq_request_head(struct dma_channel *chan)
{
    struct dma_request *req = chan->req_list.head;
    if (req == NULL) {
        assert(chan->req_list.count == 0);
        return NULL;
    }

    chan->req_list.count--;

    chan->req_list.head = dma_request_get_next(req);
    if (chan->req_list.head == NULL) {
        chan->req_list.tail = NULL;
        assert(chan->req_list.count == 0);
    }

    DMACHAN_DEBUG("request : deq head [%016lx] count=%u\n", chan->id,
                  dma_request_get_id(req), chan->req_list.count);

    return req;
}

/**
 * \brief returns the request with the given request ID
 *
 * \param chan  DMA channel
 * \param id    DMA request id
 *
 * \returns pointer to the DMA request
 *          NULL if queue was empty
 */
struct dma_request *dma_channel_deq_request_by_id(struct dma_channel *chan,
                                                  dma_req_id_t id)
{
    struct dma_request *req = chan->req_list.head;
    if (req == NULL) {
        assert(chan->req_list.count == 0);
        return NULL;
    }
    while(req) {
        if (req->id == id) {
            if (req->prev != NULL) {
                req->prev->next = req->next;
            }

            if (req->next != NULL) {
                req->next->prev = req->prev;
            }
            chan->req_list.count--;
            if (chan->req_list.count == 0) {
                assert(chan->req_list.head == chan->req_list.tail);
                chan->req_list.head = NULL;
                chan->req_list.tail = NULL;
            }
            req->next = NULL;
            req->prev = NULL;
            return req;
        }
        req = req->next;
    }
    return NULL;
}

/**
 * \brief inserts a request into the channels request list
 *
 * \param chan  DMA channel
 * \param req   DMA request to be inserted
 */
void dma_channel_enq_request_head(struct dma_channel *chan,
                                  struct dma_request *req)
{
    dma_request_set_next(req, chan->req_list.head);

    chan->req_list.count++;

    chan->req_list.head = req;
    if (chan->req_list.tail == NULL) {
        chan->req_list.tail = req;
        assert(chan->req_list.count == 1);
    }

    DMACHAN_DEBUG("request : enq head [%016lx] count=%u\n", chan->id,
                  dma_request_get_id(req), chan->req_list.count);
}

/**
 * \brief inserts a request at the end of the channels request list
 *
 * \param chan  DMA channel
 * \param req   DMA request to be inserted
 */
void dma_channel_enq_request_tail(struct dma_channel *chan,
                                  struct dma_request *req)
{
    dma_request_set_next(req, NULL);

    if (chan->req_list.head == NULL) {
        assert(chan->req_list.count == 0);
        chan->req_list.head = req;
    } else {
        assert(chan->req_list.count > 0);
        dma_request_set_next(chan->req_list.tail, req);
    }

    chan->req_list.tail = req;

    chan->req_list.count++;
    
    DMACHAN_DEBUG("request : enq tail [%016lx] count=%u\n", chan->id,
                  dma_request_get_id(req), chan->req_list.count);
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief polls the DMA channel for completed events
 *
 * \param chan  DMA Channel
 *
 * \returns SYS_ERR_OK if there was something processed
 *          DMA_ERR_CHAN_IDLE if there was no request on the channel
 *          DMA_ERR_REQUEST_UNFINISHED if the request has not been completed yet
 *
 */
inline errval_t dma_channel_poll(struct dma_channel *chan)
{
    if (chan->f.poll) {
        return chan->f.poll(chan);
    }
    return DMA_ERR_DEVICE_UNSUPPORTED;
}

/*
 * ----------------------------------------------------------------------------
 * Getter / Setter Methods
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the ID of the channel
 *
 * \param DMA channel
 *
 * \returns DMA channel ID
 */
inline dma_chan_id_t dma_channel_get_id(struct dma_channel *chan)
{
    return chan->id;
}

/**
 * \brief gets the state of the channel
 *
 * \param DMA channel
 *
 * \returns DMA channel state
 */
inline dma_chan_st_t dma_channel_get_state(struct dma_channel *chan)
{
    return chan->state;
}

/**
 * \brief gets the DMA device this channel belongs to
 *
 * \param DMA channel
 *
 * \returns DMA device
 */
inline struct dma_device *dma_channel_get_device(struct dma_channel *chan)
{
    return chan->device;
}

/**
 * \brief gets the number of unfinished requests on this channel
 *
 * \param DMA channel
 *
 * \returns request count
 */
inline uint32_t dma_channel_get_request_count(struct dma_channel *chan)
{
    return chan->req_list.count;
}

/**
 * \brief gets the address where the MMIO registers are mapped
 *
 * \param DMA channel
 *
 * \returns MMIO register vbase
 */
inline lvaddr_t dma_channel_get_mmio_vbase(struct dma_channel *chan)
{
    return chan->mmio.vaddr;
}

/**
 * \brief gets the maximum transfer size of the channel
 *
 * \param DMA channel
 *
 * \returns maximum transfer size in bytes
 */
inline uint32_t dma_channel_get_max_xfer_size(struct dma_channel *chan)
{
    return chan->max_xfer_size;
}
