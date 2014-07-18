/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_DEVICE_INTERNAL_H
#define IOAT_DMA_DEVICE_INTERNAL_H

#include <dma_mem_utils.h>

#include <dma_device_internal.h>

#include <dma/ioat/ioat_dma_device.h>
#include <dev/ioat_dma_dev.h>




/* device flags */
#define IOAT_DMA_DEV_F_DCA  0x00000001
#define IOAT_DMA_DEV_F_RAID 0x00000002

/*
 * size and mapping information for che completion status field
 * this is where the DMA channel will write the address of the last completed
 * descriptor (a copy of CHANSTS register)
 *
 */
#define IOAT_DMA_COMPLSTATUS_SIZE BASE_PAGE_SIZE
#define IOAT_DMA_COMPLSTATUS_ELEMENT_SIZE 64
#define IOAT_DMA_COMPLSTATUS_FLAGS VREGION_FLAGS_READ_WRITE


void ioat_device_get_complsts_addr(struct ioat_dma_device *dev,
                                   struct dma_mem *mem);

mackerel_addr_t ioat_device_get_mmio_base(struct ioat_dma_device *dev);

#endif /* IOAT_DMA_DEVICE_INTERNAL_H */
