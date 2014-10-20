/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_H
#define LIB_DMA_H

/* forward declarations */
struct dma_device;
struct dma_channel;
struct dma_request;

/* service name for the driver services */

/// the service name for the exported devices
#define IOAT_DMA_SERVICE_NAME "ioat_dma_svc"

/// the service name for the exported devices
#define XEON_PHI_DMA_SERVICE_NAME "xeon_phi_dma_svc"

/* type declarations for the IDs */

/// IOAT DMA device id
typedef uint8_t dma_dev_id_t;

/// DMA channel id
typedef uint16_t dma_chan_id_t;

/// IOAT DMA request ID
typedef uint64_t dma_req_id_t;

/**
 * Enumeration of possible interrupt types supported by the hardware
 */
typedef enum dma_irq {
    DMA_IRQ_DISABLED,     ///< interrupts are disabled
    DMA_IRQ_MSIX,         ///< use MSI-X interrupts
    DMA_IRQ_MSI,          ///< use MSI interrupts
    DMA_IRQ_INTX,         ///< use normal INTx interrupts
} dma_irq_t;

/**
 * Device types. Which DMA engine we have.
 */
typedef enum dma_dev_type {
    DMA_DEV_TYPE_INVALID=0,
    DMA_DEV_TYPE_IOAT=1,
    DMA_DEV_TYPE_XEON_PHI=2,
    DMA_DEV_TYPE_CLIENT=3
} dma_dev_type_t;


/// interrupt handler function type definition
typedef void (*dma_irq_fn_t)(errval_t err, struct dma_channel *dev, void *arg);

/**
 * represents a mapped piece of memory for DMA management purposes
 */
struct dma_mem
{
    lvaddr_t vaddr;         ///< virtual address of the mapped region
    lpaddr_t paddr;         ///< physical address of the underlying frame
    uint64_t bytes;         ///< size of the region in bytes
    struct capref frame;    ///< frame capability backing this region
};

/*
 * service name defines
 */
#define DMA_SVC_NAME_IOAT       "ioat_dma_svc"
#define DMA_SVC_NAME_XEON_PHI   "xeon_phi_dma_svc"


#endif  /* LIB_DMA_H */
