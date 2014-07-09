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

enum xdma_chan_owner {
    XDMA_CHAN_CARD_OWNED = 0,
    XDMA_CHAN_HOST_OWNED
};

enum xdma_chan_state {
    XDMA_CHAN_STATE_UNINITIALIZED = 0,
    XDMA_CHAN_STATE_DISABLED,
    XDMA_CHAN_STATE_ENABLED
};

#define XDMA_REQ_FLAG_VALID 0xAA000000
#define XDMA_REQ_FLAG_FIRST 0x00000001
#define XDMA_REQ_FLAG_LAST  0x00000002
#define XDMA_REQ_FLAG_DONE  0x00000004

struct xdma_req_info
{
    xdma_done_cb_t cb;
    void *st;
    errval_t err;           ///< outcome of the request
    xeon_phi_dma_id_t id;   ///< DMA request ID
    uint32_t flags;         ///< request flags
};

struct xdma_channel
{
    xeon_phi_dma_t *regs;    ///< Mackerel base
    struct xdma_ring ring;   ///< descriptor ring of this channel
    uint16_t size;           ///< size of the channel (elements in descriptor ring)
    uint16_t tail;           ///< the tail pointer of the ring (cached)
    uint16_t last_processed; ///< index of the last processed element
    uint32_t head;           ///< the head pointer of the ring
    uint32_t write_next;     ///< where to write the next descriptor
    struct xdma_req_info *rinfo;  ///< stores request information for the descriptors
    uint8_t chanid;          ///< channel id
    uint8_t do_poll;         ///< flag setting that we use the polling mode
    enum xdma_chan_owner owner; ///< the owner of the channel
    enum xdma_chan_state state;
    uint32_t reqcoutner;     ///< request counter
    lpaddr_t dstat_wb;
};


static inline xeon_phi_dma_id_t xdma_chan_generate_id(struct xdma_channel *chan)
{

    return ((((uint64_t) chan->chanid+1) << 56) | (((uint64_t) chan->head) << 32)
            | (uint64_t)(chan->reqcoutner++));
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
                           uint8_t chanid,
                           uint8_t do_poll);

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
                                 struct dma_req_setup *setup,
                                 xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_status(struct xdma_channel *chan,
                                 struct dma_req_setup *setup,
                                 xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_general(struct xdma_channel *chan,
                                  struct dma_req_setup *setup,
                                  xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_keynoncecent(struct xdma_channel *chan,
                                       struct dma_req_setup *setup,
                                       xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_key(struct xdma_channel *chan,
                              struct dma_req_setup *setup,
                              xeon_phi_dma_id_t *id);

/**
 *
 */
errval_t xdma_channel_req_stop(struct xdma_channel *chan,
                               xeon_phi_dma_id_t req);

/**
 *
 */
errval_t xdma_channel_intr_handler(struct xdma_channel *chan);

/**
 * \brief polls the channel for completed DMA transfers
 *
 * \param chan the DMA channel to poll
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_IDLE if there were no new completed transfers
 *          XEON_PHI_ERR_DMA_* on failure
 */
errval_t xdma_channel_poll(struct xdma_channel *chan);




#endif /* XEON_PHI_DMA_DESC_RING_H */
