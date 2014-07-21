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

/**
 *
 */
errval_t dma_client_register(struct dma_client *conn);

/**
 *
 */
errval_t dma_client_deregister(struct dma_client *conn);

/**
 *
 */
errval_t dma_client_memcpy(struct dma_client *conn);


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

#endif  /* LIB_DMA_CLIENT_H */
