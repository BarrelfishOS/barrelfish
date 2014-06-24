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


struct dma_info {
    struct xdma_channel channels[XEON_PHI_DMA_CHAN_NUM];
    xeon_phi_dma_t dma_dev;
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
    /* check if already initialized */
    if (phi->dma) {
        return SYS_ERR_OK;
    }

    struct dma_info *info = calloc(1, sizeof(struct dma_info));
    if (info == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    XDMA_DEBUG("initializing %u channels\n", XEON_PHI_DMA_CHAN_NUM);
    for (uint32_t i = 0; i < XEON_PHI_DMA_CHAN_NUM; ++i) {
        struct xdma_channel *chan = &info->channels[i];
        chan->chanid = i + XEON_PHI_DMA_CHAN_OFFSET;

    }



    return SYS_ERR_OK;
}

