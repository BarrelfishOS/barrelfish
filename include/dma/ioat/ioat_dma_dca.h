/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_DCA_H
#define LIB_IOAT_DMA_DCA_H



/**
 * \brief enables direct cache access
 */
errval_t ioat_dma_dca_enable(void);


/**
 * \brief disables direct cache access
 */
errval_t ioat_dma_dca_disable(void);


/**
 * \brief checks whether DCA is enabled
 *
 * \returns 1 if DCA is enabled
 *          0 if DCA is disabled
 */
uint8_t ioat_dma_dca_is_enabled(void);


#endif  /* LIB_IOAT_DMA_DCA_H */
