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

#ifndef XEON_PHI_DMA_CHANNEL_H
#define XEON_PHI_DMA_CHANNEL_H

#include <if/xeon_phi_dma_defs.h>

#include "dma_descriptor_ring.h"


#define XEON_PHI_DMA_REQ_SIZE_MAX  (((1U) * 1024 * 1024) >> 1)

struct xdma_req_info
{
    struct xeon_phi_dma_binding *binding;
    errval_t err;           ///< outcome of the request
    xeon_phi_dma_id_t id;   ///< DMA request ID
    uint32_t head :1;      ///< flag indicating this is the first entry
    uint32_t last :1;      ///< flag indicating this is the last entry
    uint32_t done :1;      ///< flag indicating that this transfer is done
    struct xdma_req_info *next;
};

struct xdma_channel
{
    xeon_phi_dma_t *regs;    ///< Mackerel base
    struct xdma_ring ring;   ///< descriptor ring of this channel
    uint16_t tail;           ///< the tail pointer of the ring (cached)
    uint16_t head;           ///< the head pointer of the ring
    uint16_t size;           ///< size of the channel (elements in descriptor ring)
    struct xdma_req *reqs;   ///< stores request information for the descriptors
    uint8_t chanid;          ///< channel id
    uint32_t reqcoutner;     ///< request counter
};

enum xdma_req_type
{
    XDMA_REQ_TYPE_NOP = 0,
    XDMA_REQ_TYPE_MEMCPY,
    XDMA_REQ_TYPE_STATUS,
    XDMA_REQ_TYPE_GENERAL,
    XDMA_REQ_TYPE_KEYNON,
    XDMA_REQ_TYPE_KEY
};

struct xdma_req_setup
{
    enum xdma_req_type type;
    struct xeon_phi_dma_binding *binding;
    union
    {
        struct
        {
            lpaddr_t src;
            lpaddr_t dst;
            size_t   bytes;
        } mem;
        struct
        {

        } status;
        struct
        {

        } general;
        struct {

        }keynon;
        struct {

        }key;
    } info;
};

static inline xeon_phi_dma_id_t xdma_chan_generate_id(struct xdma_channel *chan)
{
    return ((((uint64_t) chan->chanid) << 56) | (((uint64_t) chan->head) << 32)
            | (chan->reqcoutner++));
}

/**
 * \brief initializes a DMA channel
 *
 * \param chan   where to initialize the DMA channel
 * \param ndesc  number of descriptors in the ring
 * \param regs   pointer to the Mackerel information structure
 * \param chanid id of the channel
 */
errval_t xdma_channel_init(struct xdma_channel *chan,
                           uint16_t ndesc,
                           xeon_phi_dma_t *regs,
                           uint8_t chanid);

/**
 * \brief frees up the resources used by the channel
 *
 * \param chan  the DMA channel to be freed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xdma_channel_free(struct xdma_channel *chan);

/**
 * \brief
 *
 * \param chan
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on failure
 *
 */
errval_t xdma_channel_req_memcpy(struct xdma_channel *chan,
                                 struct xdma_req_setup *setup,
                                 xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_status(struct xdma_channel *chan,
                                 struct xdma_req_setup *setup,
                                 xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_general(struct xdma_channel *chan,
                                  struct xdma_req_setup *setup,
                                  xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_keynoncecent(struct xdma_channel *chan,
                                       struct xdma_req_setup *setup,
                                       xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_key(struct xdma_channel *chan,
                              struct xdma_req_setup *setup,
                              xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_stop(struct xdma_channel *chan,
                               xeon_phi_dma_id_t req);

/**
 *
 */
errval_t xdma_channel_poll(struct xdma_channel *chan);

#endif /* XEON_PHI_DMA_DESC_RING_H */
