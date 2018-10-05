/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XEON_PHI_DMA_DEVICE_H
#define LIB_XEON_PHI_DMA_DEVICE_H

#include <dma/dma_device.h>
#include <dma/dma_mem_mgr.h>

struct iommu_client;

/// forward declaration of the device
struct xeon_phi_dma_device;
struct xeon_phi_dma_channel;

/// The maximum transfer size per descriptor
#define XEON_PHI_DMA_DEVICE_MAX_XFER (512 * 1024)

/*
 * ----------------------------------------------------------------------------
 * Device Channel Setup
 *
 * The DMA device has 8 channels which can be owned by the card or by the host.
 *
 * We give 4 to the card and 4 to the host side drivers.
 */

/// the maximum number of DMA channels on the card
#define XEON_PHI_DMA_DEVICE_CHAN_TOTAL 8

/// the number of DMA channels for the host
#define XEON_PHI_DMA_DEVICE_CHAN_HOST 4

/// Offset of the DMA register region into the Xeon Phi MMIO base
#define XEON_PHI_DMA_OFFSET 0xA000

#ifdef __k1om__
#define XEON_PHI_DMA_DEVICE_CHANNELS (XEON_PHI_DMA_DEVICE_CHAN_TOTAL- XEON_PHI_DMA_DEVICE_CHAN_HOST)
#define XEON_PHI_DMA_DEVICE_CHAN_OFFSET XEON_PHI_DMA_DEVICE_CHAN_HOST
#else
#define XEON_PHI_DMA_DEVICE_CHANNELS XEON_PHI_DMA_DEVICE_CHAN_HOST
#define XEON_PHI_DMA_DEVICE_CHAN_OFFSET 0
#endif

/**
 * \brief pointer type conversion
 */
static inline struct xeon_phi_dma_device *dma_device_to_xeon_phi(struct dma_device *dev)
{
    return (struct xeon_phi_dma_device *) dev;
}

/*
 * ----------------------------------------------------------------------------
 * device initialization / termination
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes a Xeon Phi DMA device with the giving capability
 *
 * \param mmio capability representing the device's MMIO registers
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xeon_phi_dma_device_init(void * mmio_base, struct iommu_client *iommu,
                                  dma_mem_convert_fn convert, void *convert_arg,
                                  int32_t nodeid,
                                  struct xeon_phi_dma_device **dev);

/**
 * \brief terminates the device operation and frees up the allocated resources
 *
 * \param dev Xeon Phi DMA device to shutdown
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xeon_phi_dma_device_shutdown(struct xeon_phi_dma_device *dev);

/*
 * ----------------------------------------------------------------------------
 * Interrupt management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief enables the interrupts for the device
 *
 * \param dev   Xeon Phi DMA device
 * \param type  interrupt type
 * \param fn    interrupt handler function
 * \param arg   argument supplied to the handler function
 */
errval_t xeon_phi_dma_device_intr_enable(struct xeon_phi_dma_device *dev,
                                         dma_irq_t type,
                                         dma_irq_fn_t fn,
                                         void *arg);

/**
 * \brief disables the interrupts for the device
 *
 * \param dev   Xeon Phi DMA device
 */
void xeon_phi_dma_device_intr_disable(struct xeon_phi_dma_device *dev);


/*
 * ----------------------------------------------------------------------------
 * Device Operation Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief polls the channels of the Xeon Phi DMA device
 *
 * \param dev   Xeon Phi DMA device
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_DEVICE_IDLE if there is nothing completed on the channels
 *          errval on error
 */
errval_t xeon_phi_dma_device_poll_channels(struct dma_device *dev);

#endif  /* LIB_XEON_PHI_DMA_DEVICE_H */
