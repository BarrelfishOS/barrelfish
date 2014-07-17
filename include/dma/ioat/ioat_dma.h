/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_H
#define LIB_IOAT_DMA_H

/*
 * switch for changing the operation mode of the IOAT DMA driver
 * The options are:
 *  - SERVICE: For each discovered device there is a single DMA service domain
 *             running which is the master of all the channels. Requests are
 *             forwarded to the DMA service domain and handled by it.
 *
 *  - LIBRARY: The domains requests access to devices from the IOAT DMA manager
 *             service which hands over the capabilities to the devices. The
 *             device gets mapped in the domain itself.
 */
#define IOAT_DMA_OPERATION_SERVICE 0
#define IOAT_DMA_OPERATION_LIBRARY 1
#define IOAT_DMA_OPERATION IOAT_DMA_OPERATION_SERVICE


/**
 * \brief initializes the DMA library
 */
errval_t ioat_dma_init(uint8_t dca_enabled);

/**
 * \brief enables direct cache access
 */
void ioat_dma_dca_enable(void);

/**
 * \brief disables direct cache access
 */
void ioat_dma_dca_disable(void);


/**
 * \brief checks whether DCA is enabled
 *
 * \returns 1 if DCA is enabled
 *          0 if DCA is disabled
 */
uint8_t ioat_dma_dca_is_enabled(void);

#endif  /* LIB_IOAT_DMA_H */
