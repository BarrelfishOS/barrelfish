/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_CLIENT_DEVICE_H
#define LIB_DMA_CLIENT_DEVICE_H

#include <dma/dma_device.h>

/// forward declaration of the device
struct dma_client_device;
struct dma_client_channel;

/// describes the information stored in the DMA device information structure
typedef enum dma_client_info_type {
    DMA_CLIENT_INFO_TYPE_INVALID,
    DMA_CLIENT_INFO_TYPE_IREF,
    DMA_CLIENT_INFO_TYPE_NAME,
    DMA_CLIENT_INFO_TYPE_ADDR
} dma_client_dev_info_type_t;

/**
 * supplies information about the DMA driver this client device connects to
 */
struct dma_client_info
{
    dma_client_dev_info_type_t type;
    dma_dev_type_t device_type;
    union {
        char *name;
        iref_t iref;
        struct  {
            lpaddr_t low;
            lpaddr_t high;
        } addr;
    } args;
};


#define DMA_CLIENT_DEVICE_CONNECTIONS 1

/**
 * \brief pointer type conversion
 */
static inline struct dma_client_device *dma_device_to_client(struct dma_device *dev)
{
    return (struct dma_client_device *)dev;
}


/*
 * ----------------------------------------------------------------------------
 * device initialization / termination
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes a DMA client device with the giving capability
 *
 * \param info stores information how to find the device driver service
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_client_device_init(struct dma_client_info *info,
                                struct dma_client_device **dev);

/**
 * \brief terminates the device operation and frees up the allocated resources
 *
 * \param dev IOAT DMA device to shutdown
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_client_device_shutdown(struct dma_client_device *dev);

/*
 * ----------------------------------------------------------------------------
 * Device Status Queries
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the device type of the connected device
 *
 * \param dev DMA client device
 *
 * \returns DMA device type
 */
dma_dev_type_t dma_client_get_device_type(struct dma_client_device *dev);

/**
 * \brief returns the supported range of a connection
 *
 * \param conn      DMA client connection
 * \param mem_low   minimum physical address supported
 * \param mem_high  maximum physical address supported
 */
void dma_client_device_get_mem_range(struct dma_client_device *dev,
                                     lpaddr_t *mem_low,
                                     lpaddr_t *mem_high);



#endif  /* LIB_DMA_CLIENT_DEVICE_H */
