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

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <dev/xeon_phi/xeon_phi_dma_dev.h>

#include "xeon_phi.h"
#include "dma.h"
#include "dma_channel.h"
#include "dma_descriptor_ring.h"
#include "debug.h"

struct dma_info
{
    struct xdma_channel channels[XEON_PHI_DMA_CHAN_NUM];
    xeon_phi_dma_t dma_dev;
    uint8_t chan_alloc_next;
};

/**
 * \brief Initializes the DMA structure for the Xeon Phi
 *
 * \param phi the xeon phi DMA structure
 *
 * \return SYS_ERR_OK on success,
 */
errval_t dma_init(struct xeon_phi *phi)
{
    errval_t err;

    /* check if already initialized */
    if (phi->dma) {
        return SYS_ERR_OK;
    }

    struct dma_info *info = calloc(1, sizeof(struct dma_info));
    if (info == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    xeon_phi_dma_initialize(&info->dma_dev, XEON_PHI_MMIO_TO_SBOX(phi));

    XDMA_DEBUG("initializing %u channels\n", XEON_PHI_DMA_CHAN_NUM);
    for (uint32_t i = 0; i < XEON_PHI_DMA_CHAN_NUM; ++i) {
        struct xdma_channel *chan = &info->channels[i];
        err = xdma_channel_init(chan,
                                XEON_PHI_DMA_DESC_NUM,
                                &info->dma_dev,
                                i + XEON_PHI_DMA_CHAN_OFFSET);
        if (err_is_fail(err)) {
            free(info);
            return err;
        }
    }

    XDMA_DEBUG("initializing %u channels\n", XEON_PHI_DMA_CHAN_NUM);

    phi->dma = info;

    return SYS_ERR_OK;
}

/**
 * \brief polls the owned channels for competed transfers
 *
 * \param phi the xeon phi to poll
 *
 * \returns SYS_ERR_OK if there was a transfer completed and executed correctly
 *          XEON_PHI_ERR_DMA_IDLE if there was no transfer completed
 *          errno otherwise
 */
errval_t dma_poll_channels(struct xeon_phi *phi)
{
    errval_t err, ret_err = XEON_PHI_ERR_DMA_IDLE;
    for (uint32_t i = 0; i < XEON_PHI_DMA_CHAN_NUM; ++i) {
        err = xdma_channel_poll(&phi->dma->channels[i]);
        if (err_is_fail(err)) {
            if (err_no(err) == XEON_PHI_ERR_DMA_IDLE) {
                continue;
            }
            return err;
        } else {
            ret_err = SYS_ERR_OK;
        }
    }
    return ret_err;
}

