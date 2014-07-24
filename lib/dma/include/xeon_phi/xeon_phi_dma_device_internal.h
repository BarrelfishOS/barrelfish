/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DMA_DEVICE_INTERNAL_H
#define XEON_PHI_DMA_DEVICE_INTERNAL_H

#include <dma_mem_utils.h>

#include <dma_device_internal.h>
#include <dma/xeon_phi/xeon_phi_dma_device.h>

#include <dev/xeon_phi/xeon_phi_dma_dev.h>


/**
 * \brief globally enables the interrupts for the given device
 *
 * \param dev   IOAT DMA device
 * \param type  the interrupt type to enable
 */
errval_t xeon_phi_dma_device_irq_setup(struct xeon_phi_dma_device *dev,
                                       dma_irq_t type);


/**
 * \brief gets the Xeon Phi virtual base address of the DMA channel with
 *        the given id
 *
 * \param dev   Xeon Phi DMA device
 * \param idx   DMA channel index
 *
 * \returns virtual address of MMIO registers for the channel
 */
void *xeon_phi_dma_device_get_channel_vbase(struct xeon_phi_dma_device *dev,
                                            uint8_t idx);


#endif /* XEON_PHI_DMA_DEVICE_INTERNAL_H */
