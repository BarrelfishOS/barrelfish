/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_MEM_MGR_H
#define LIB_DMA_MEM_MGR_H

/* forward declaration */
struct dma_mem_mgr;

/// typedef for custom address conversion
typedef lpaddr_t (*dma_mem_convert_fn)(void *arg,
                                       struct capref frame,
                                       genpaddr_t *retaddr,
                                       genvaddr_t *local_retaddr);

/// default memory ranges from 0 to 48 bits
#define DMA_MEM_RANGE_MIN 0x0
#define DMA_MEM_RANGE_MAX 0xFFFFFFFFFFFF

/**
 * \brief initializes the DMA memory region manager
 *
 * \param mem_mgr   returned pointer to the mem manager structure
 * \param range_min minimum allowed memory address
 * \param range_max maximum allowed memory address
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t dma_mem_mgr_init(struct dma_mem_mgr **mem_mgr,
                          lpaddr_t range_min,
                          lpaddr_t range_max);

/**
 * \brief sets the address conversion function to be used for translating the
 *        addresses
 *
 * \param mem_mgr   DMA memory manager
 * \param fn        convert function to be called
 * \param arg       argument supplied for the convert function
 */
void dma_mem_mgr_set_convert_fn(struct dma_mem_mgr *mem_mgr,
                                dma_mem_convert_fn fn,
                                void *arg);

/**
 * \brief registers a memory region to be used for DMA transfers
 *
 * \param mem_mgr   DMA memory manager
 * \param cap       frame capability of the memory region to register
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t dma_mem_register(struct dma_mem_mgr *mem_mgr,
                          struct capref cap, genpaddr_t *retaddr);

/**
 * \brief deregisters a memory region that it cannot longer be used for the
 *        memory manager
 *
 * \param mem_mgr   DMA memory manager
 * \param cap       frame capability of the memory region to register
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t dma_mem_deregister(struct dma_mem_mgr *mem_mgr,
                            struct capref cap);

/**
 * \brief verifies if a addres-length pair lies completely within a
 *        registered memory region and translates the address
 *
 * \param mem_mgr   DMA memory manager
 * \param addr      address to be looked up
 * \param bytes     length of the transfer in bytes
 * \param dma_addr  translated base address (if change in address space)
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_MEM_NOT_REGISTERED if the memory region is not registered
 *          DMA_ERR_OUT_OF_RANGE if the memory region is out of range
 */
errval_t dma_mem_verify(struct dma_mem_mgr *mem_mgr,
                        lpaddr_t addr,
                        size_t bytes,
                        lpaddr_t *dma_addr);

#endif /* LIB_DMA_MEM_UTILS_H */
