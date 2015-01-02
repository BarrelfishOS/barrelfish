/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_CHANNEL_INTERNAL_H
#define DMA_CHANNEL_INTERNAL_H

#include <dma/dma_channel.h>

struct dma_req_setup;

/*
 *
 */
typedef errval_t (*memcpy_fn_t)(struct dma_channel *chan,
                                struct dma_req_setup *setup,
                                dma_req_id_t *id);

typedef errval_t (*memset_fn_t)(struct dma_channel *chan,
                                struct dma_req_setup *setup,
                                dma_req_id_t *id);

typedef errval_t (*chan_poll_fn_t)(struct dma_channel *chan);

/**
 *
 */
struct dma_channel_fn
{
    memcpy_fn_t memcpy;
    memset_fn_t memset;
    chan_poll_fn_t poll;
};

/**
 * Represents the generic part of a DMA channel
 */
struct dma_channel
{
    dma_chan_id_t id;               ///< unique DMA channel id
    dma_chan_st_t state;            ///< channel state
    struct dma_device *device;      ///< DMA device this channel belongs to
    uint32_t max_xfer_size;         ///< maximum number of bytes per transfer

    struct dma_mem mmio;

    struct {
        dma_irq_t type;
        dma_irq_fn_t fn;
        void *arg;
    } irq;                          ///< Device level interrupt

    struct {
        uint32_t count;             ///< number of requests in the list
        struct dma_request *head;   ///< start of the request list
        struct dma_request *tail;   ///< end of the request list
    } req_list;                     ///< list of submitted requests

    uint64_t req_counter;           ///< number of requests issued so far

    struct dma_channel_fn f;        ///< function pointers to channels
};

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
struct dma_request *dma_channel_deq_request_head(struct dma_channel *chan);

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
                                                  dma_req_id_t id);

/**
 * \brief inserts a request into the head of a channel's request list
 *
 * \param chan  DMA channel
 * \param req   DMA request to be inserted
 */
void dma_channel_enq_request_head(struct dma_channel *chan,
                                  struct dma_request *req);

/**
 * \brief inserts a request at the end of the channels request list
 *
 * \param chan  DMA channel
 * \param req   DMA request to be inserted
 */
void dma_channel_enq_request_tail(struct dma_channel *chan,
                                  struct dma_request *req);

/**
 * \brief Submits the pending descriptors to the hardware queue
 *
 * \param chan  IOAT DMA channel
 *
 * \returns number of submitted descriptors
 */
uint16_t dma_channel_submit_pending(struct dma_channel *chan);


/**
 * \brief returns the next DMA request counter value to generate the req id.
 *
 * \param chan  DMA channel
 *
 * \returns request counter value
 */
static inline uint64_t dma_channel_incr_req_count(struct dma_channel *chan)
{
    return ++chan->req_counter;
}


#endif /* DMA_CHANNEL_INTERNAL_H */
