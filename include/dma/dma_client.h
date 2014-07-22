/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_CLIENT_H
#define LIB_DMA_CLIENT_H

#include <if/dma_defs.h>

struct dma_client;

/**
 *
 */
errval_t dma_client_init(void);

/*
 * ----------------------------------------------------------------------------
 * Memory Registration
 * ----------------------------------------------------------------------------
 */

/**
 * \brief registers a memory region with a specified client connection
 *
 * \param conn  DMA client connection
 * \param frame the memory frame to register
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_register_memory(struct dma_client *conn,
                                    struct capref frame);

/**
 * \brief deregisters a previously registered memory region from the connection
 *
 * \param conn  DMA client connection
 * \param frame the memory frame to deregister
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_deregister_memory(struct dma_client *conn,
                                      struct capref frame);

/*
 * ----------------------------------------------------------------------------
 * Request Execution
 * ----------------------------------------------------------------------------
 */

/**
 * \brief issues a memcopy request to the service using the connection
 *
 * \param setup DMA request setup information
 *
 * \returns SYS_ERR_OK on success
 *
 * NOTE: the connection information has to be present in the setup struct
 */
errval_t dma_client_memcpy(struct dma_req_setup *setup);

/*
 * ----------------------------------------------------------------------------
 * Connection Management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets a connection to the DMA service based on the src and dst addresses
 *        of the DMA request
 *
 * \param src   source address of the operation
 * \param dst   destination address of the operation
 * \param size  transfer size in bytes
 *
 * \returns dma_client connection
 *          NULL if the addresses are not supported
 */
struct dma_client *dma_client_get_connection_by_addr(lpaddr_t src,
                                                     lpaddr_t dst,
                                                     size_t size);

/**
 * \brief waits until a device driver for the supplied device type is ready
 *
 * \param device    DMA device type
 *
 * \returns SYS_ERR_OK when the driver is ready
 *          errval if there was something wrong
 */
errval_t dma_client_wait_for_driver(dma_dev_type_t device,
                                    uint8_t numa_node);

/*
 * ----------------------------------------------------------------------------
 * Connection Information
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the DMA device type of a connection
 *
 * \param conn  DMA client connection
 *
 * \returns DMA device type
 */
dma_dev_type_t dma_client_get_device_type(struct dma_client *conn);

/**
 * \brief returns the supported range of a connection
 *
 * \param conn      DMA client connection
 * \param mem_min   minimum physical address supported
 * \param mem_max   maximum physical address supported
 *
 */
void dma_client_get_supported_range(struct dma_client *conn,
                                    lpaddr_t *mem_min,
                                    lpaddr_t *mem_max);

#endif  /* LIB_DMA_CLIENT_H */
