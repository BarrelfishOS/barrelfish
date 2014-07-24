/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <ioat/ioat_dma_internal.h>
#include <ioat/ioat_dma_dca_internal.h>

#include <debug.h>

/*
 * ===========================================================================
 * Library Internal Interface
 * ===========================================================================
 */

static uint8_t dca_do_initialization = 0;

/**
 * \brief sets the do initialization flag
 */
void ioat_set_dca_enabled(void)
{
    IOATDCA_DEBUG("enabling DCA initialization\n");
    dca_do_initialization = 1;
}

/*
 * ===========================================================================
 * Public Interface
 * ===========================================================================
 */

/*
 * ----------------------------------------------------------------------------
 *  Enabling / Disabling DCA
 * ----------------------------------------------------------------------------
 */

/**
 * \brief enables direct cache access
 */
errval_t ioat_dma_dca_enable(void)
{
    if (!dca_do_initialization) {
        return SYS_ERR_OK;
    }
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief disables direct cache access
 */
errval_t ioat_dma_dca_disable(void)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief checks whether DCA is enabled
 *
 * \returns 1 if DCA is enabled
 *          0 if DCA is disabled
 */
uint8_t ioat_dma_dca_is_enabled(void)
{
    return dca_do_initialization;
}
