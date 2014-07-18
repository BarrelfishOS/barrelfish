/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_DEVICE_INTERNAL_H
#define DMA_DEVICE_INTERNAL_H

#include <dma/dma_device.h>

struct dma_dev_int
{
    dma_dev_id_t id;                ///< device id
    dma_dev_st_t state;             ///< device state
    struct {
        void *vbase;                ///< virtual base of the mapped region
        lpaddr_t pbase;             ///< physical base of the region
        uint64_t bytes;             ///< size of the region in bytes
        struct capref cap;          ///< devframe capability
    } mmio;                         ///< MMIO register mappings

    struct {
        struct dma_channel **c;     ///< DMA channel pointers
        uint8_t num;                ///< Number of channels of this device
        uint8_t next;               ///< Next channel to allocate for requests
    } channels;                     ///< Channel information
    dma_irq_t irq_type;        ///< DMA interrupt type
};

#endif /* DMA_DEVICE_INTERNAL_H */
