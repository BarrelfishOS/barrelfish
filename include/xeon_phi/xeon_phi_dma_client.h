/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DMA_CLIENT_H
#define XEON_PHI_DMA_CLIENT_H

/* for xeon_phi_dma_id_t */
#include <if/xeon_phi_dma_defs.h>

/**
 * type definition of the xeon phi done callback
 */
typedef void (*xeon_phi_dma_done_t)(xeon_phi_dma_id_t id,
                                    errval_t err,
                                    void *st);

/**
 * specifies the information about a DMA transfer
 */
struct xeon_phi_dma_info
{
    lpaddr_t src;
    lpaddr_t dest;
    size_t size;
};

/**
 * Continuation to be called when the DMA transfer is completed
 */
struct xeon_phi_dma_cont
{
    xeon_phi_dma_done_t cb;
    void *arg;
};

/**
 * initializes the XEON Phi DMA client library
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on error
 */
errval_t xeon_phi_dma_client_init(void);

/**
 * \brief registers a physical memory region to be used for DMA transfers
 *        this memory region can be in host or card memory
 *
 * \param xphi_id id of the xeon phi
 * \param mem     the memory to be registered
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on error
 */
errval_t xeon_phi_dma_client_register(uint8_t xphi_id,
                                      struct capref mem);

/**
 * \brief deregisters a physical memory region to be used for DMA transfers
 *        this memory region can be in host or card memory
 *
 * \param xphi_id id of the xeon phi
 * \param mem the memory to be deregistered
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on error
 *
 * NOTE: this prevents the memory region from being used in future requests
 *       current active DMA transfers using this memory regions are not stopped.
 */
errval_t xeon_phi_dma_client_deregister(uint8_t xphi_id,
                                        struct capref mem);

/**
 * \brief starts a new DMA transfer
 *
 * \param xphi_id id of the xeon phi
 * \param info pointer to the DMA transfer info structure
 * \param cont continuation to be called when transfer is done
 * \param id   returns the ID of the transfer
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on error
 */
errval_t xeon_phi_dma_client_start(uint8_t xphi_id,
                                   struct xeon_phi_dma_info *info,
                                   struct xeon_phi_dma_cont cont,
                                   xeon_phi_dma_id_t *id);

/**
 * \brief executes a DMA transfer and waits for its completion
 *
 * \param xphi_id id of the xeon phi
 * \param info pointer to the DMA transfer info structure
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on error
 */
errval_t xeon_phi_dma_client_exec(uint8_t xphi_id,
                                  struct xeon_phi_dma_info *info);


/**
 *
 */
errval_t xeon_phi_dma_client_stop(xeon_phi_dma_id_t id);



#endif // XEON_PHI_DMA_CLIENT_H
