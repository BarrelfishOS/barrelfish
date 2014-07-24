/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_MGR_CLIENT_H
#define LIB_DMA_MGR_CLIENT_H

#include <dma/dma.h>

/// name of the DMA manager service
#define DMA_MGR_SVC_NAME "dma_mgr_svc"

#define DMA_MGR_REGISTERED_DRIVER "dma_driver"

struct dma_mgr_driver_info
{
    lpaddr_t mem_low;
    lpaddr_t mem_high;
    iref_t iref;
    dma_dev_type_t type;
    uint8_t numa_node;
};

/**
 * \brief initializes the connection to the DMA manager service in an eager way
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_manager_client_init(void);

/**
 * \brief registers a DMA driver with the DMA driver manager
 *
 * \param mem_low   lower end of the supported memory range
 * \param mem_high  upper end of the supported memory range
 * \param type      DMA device type
 * \param iref      the iref of the exported DMA service
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t dma_manager_register_driver(lpaddr_t mem_low,
                                     lpaddr_t mem_high,
                                     uint8_t type,
                                     iref_t iref);

/**
 * \brief queries the DMA manager for a suitable DMA driver for a address, size
 *        pair
 *
 * \param addr      address of the transfer
 * \param size      size of the desired transfer
 * \param info      returns the driver info
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t dma_manager_lookup_driver(lpaddr_t addr,
                                   lpaddr_t size,
                                   struct dma_mgr_driver_info *info);

/**
 * \brief queries the DMA driver manager based on the service iref
 *
 * \param iref      iref ot the exported driver service
 * \param info      returns the driver info
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t dma_manager_lookup_by_iref(iref_t iref,
                                    struct dma_mgr_driver_info *info);

/**
 * \brief waits until a device driver for the supplied device type is ready
 *
 * \param device    DMA device type
 * \param numa_node Numanode of the DMA device driver
 *
 * \returns SYS_ERR_OK when the driver is ready
 *          errval if there was something wrong
 */
errval_t dma_manager_wait_for_driver(dma_dev_type_t device,
                                     uint8_t numa_node);


#endif  /* LIB_DMA_CLIENT_H */
