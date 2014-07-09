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
#ifndef XEON_PHI_DMA_H
#define XEON_PHI_DMA_H

#include <if/xeon_phi_dma_defs.h>

#include "dma_benchmark.h"

/// the base name of the exported dma service
#define XEON_PHI_DMA_SERVICE_NAME "xeon_phi_dma_svc"

/// alignment for size and memory addresses
#define XEON_PHI_DMA_ALIGNMENT 64

///
#define XEON_PHI_DMA_ALIGN_SHIFT 6

/// the maximum number of DMA channels on the card
#define XEON_PHI_DMA_CHAN_NUM_MAX 8

/// the number of DMA channels for the host
#define XEON_PHI_DMA_CHAN_NUM_HOST 4

#ifdef __k1om__
#define XEON_PHI_DMA_CHAN_NUM (XEON_PHI_DMA_CHAN_NUM_MAX- XEON_PHI_DMA_CHAN_NUM_HOST)
#define XEON_PHI_DMA_CHAN_OFFSET XEON_PHI_DMA_CHAN_NUM_HOST
#else
#define XEON_PHI_DMA_CHAN_NUM XEON_PHI_DMA_CHAN_NUM_HOST
#define XEON_PHI_DMA_CHAN_OFFSET 0
#endif
#define XEON_PHI_DMA_OWNER_HOST         1
#define XEON_PHI_DMA_OWNER_CARD         2

/// the number of dma descriptors we allocate
#define XEON_PHI_DMA_DESC_NUM  512

/// alignment constraint of the descriptor ring (cache line)
#define XEON_PHI_DMA_DESC_RING_ALIGN 64

/// flag if we use polling or interrupts
#define XEON_PHI_DMA_USE_POLLING 1

///
typedef errval_t (*xdma_done_cb_t)(void *a,
                               errval_t err,
                               xeon_phi_dma_id_t id);

enum dma_req_type
{
    XDMA_REQ_TYPE_NOP = 0,
    XDMA_REQ_TYPE_MEMCPY,
    XDMA_REQ_TYPE_STATUS,
    XDMA_REQ_TYPE_GENERAL,
    XDMA_REQ_TYPE_KEYNON,
    XDMA_REQ_TYPE_KEY
};

struct dma_req_setup
{
    enum dma_req_type type;
    xdma_done_cb_t cb;
    void *st;
    union
    {
        struct
        {
            lpaddr_t src;
            lpaddr_t dst;
            size_t bytes;
            xeon_phi_dma_id_t *dma_id;
        } mem;
        struct
        {

        } status;
        struct
        {

        } general;
        struct
        {

        } keynon;
        struct
        {

        } key;
    } info;
};

/**
 * \brief Initializes the DMA structure for the Xeon Phi
 *
 * \param phi the xeon phi DMA structure
 *
 * \return SYS_ERR_OK on success,
 */
errval_t dma_init(struct xeon_phi *phi);

/**
 * \brief issues a new DMA request
 *
 * \param phi   Xeon Phi to execute the request on
 * \param setup information about the request
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on failure
 */
errval_t dma_do_request(struct xeon_phi *phi,
                        struct dma_req_setup *setup);

/**
 * \brief polls the owned channels for competed transfers
 *
 * \param phi the xeon phi to poll
 *
 * \returns SYS_ERR_OK if there was a transfer completed and executed correctly
 *          XEON_PHI_ERR_DMA_IDLE if there was no transfer completed
 *          errno otherwise
 */
errval_t dma_poll_channels(struct xeon_phi *phi);

errval_t dma_service_init(struct xeon_phi *phi);

errval_t  dma_service_send_done(void *a,
                           errval_t err,
                           xeon_phi_dma_id_t id);

errval_t dma_impl_test(struct xeon_phi *phi);

#endif /* XEON_PHI_DMA_H */
