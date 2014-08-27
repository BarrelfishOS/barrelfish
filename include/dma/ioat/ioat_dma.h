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

struct ioat_dma_device;
struct ioat_dma_channel;
struct ioat_dma_request;

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

/// Number of elements the IOAT descriptor ring has (in bits)
#define IOAT_DMA_DESC_RING_SIZE 12

/**
 * \brief initializes the DMA library
 */
errval_t ioat_dma_init(uint8_t dca_enabled);


#endif  /* LIB_IOAT_DMA_H */
