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

/*
 *
 */
typedef errval_t (*register_memory_fn_t)(struct dma_device *chan,
                                         struct capref frame);
typedef errval_t (*deregister_memory_fn_t)(struct dma_device *chan,
                                           struct capref frame);
typedef errval_t (*dev_poll_fn_t)(struct dma_device *dev);

/**
 *
 */
struct dma_device_fn
{
    register_memory_fn_t register_memory;
    deregister_memory_fn_t deregister_memory;
    dev_poll_fn_t poll;
};

/**
 * Represents the generic part of a DMA device
 */
struct dma_device
{
    dma_dev_id_t id;                ///< device id
    dma_dev_st_t state;             ///< device state
    dma_dev_type_t type;            ///< stores the device type
    struct dma_mem mmio;            ///< MMIO register mappings

    struct {
        struct dma_channel **c;     ///< DMA channel pointers
        uint8_t count;              ///< Number of channels of this device
        uint8_t next;               ///< Next channel to allocate for requests
    } channels;                     ///< Channel information

    dma_irq_t irq_type;

    struct dma_device_fn f;
};

#endif /* DMA_DEVICE_INTERNAL_H */
