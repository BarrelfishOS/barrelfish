/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <dma_internal.h>
#include <ioat/ioat_dma_internal.h>
#include <ioat/ioat_dma_dca_internal.h>

/*
 * ===========================================================================
 * Public Interface
 * ===========================================================================
 */

/**
 * \brief initializes the DMA library
 */
errval_t ioat_dma_init(uint8_t dca_enabled)
{
    if (dca_enabled) {
        ioat_set_dca_enabled();
    }
    return SYS_ERR_OK;
}
