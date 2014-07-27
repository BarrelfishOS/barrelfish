/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_SERVICE_H_
#define DMA_SERVICE_H_

/*
 * \brief callback handler for new connect requests
 *
 * \param user_st   pointer to store the user state
 *
 * \returns SYS_ERR_OK if the connection is accepted
 *          errval if connection is rejected
 */
errval_t dma_svc_connect_cb(void *arg, void **user_st);


/**
 * \brief registers a memory region with the client such that it can be used later
 *
 * \param user_st   pointer to stored user state
 * \param cap       the capability to register
 *
 * \returns SYS_ERR_OK if the memory region was accepted
 *          errval if the memory region was rejected
 */
errval_t dma_svc_addregion_cb(dma_svc_handle_t svc_handle,
                              struct capref cap);

/**
 * \brief deregisters a memory region with the client
 *
 * \param user_st   pointer to stored user state
 * \param cap       the capability to deregister
 *
 * \returns SYS_ERR_OK if the memory region was removed
 *          errval if the memory region removal was rejected
 */
errval_t dma_svc_removeregion_cb(dma_svc_handle_t svc_handle,
                                 struct capref cap);

/**
 * \brief executes a DMA memcpy
 *
 * \param user_st   pointer to stored user state
 * \param dst       the physical destination address
 * \param src       the physical source address
 * \param bytes     size of the transfer in bytes
 * \param id        returns the DMA request ID of the transfer
 *
 * \returns SYS_ERR_OK if the memory region was removed
 *          errval if the memory region removal was rejected
 */
errval_t dma_svc_memcpy_cb(dma_svc_handle_t svc_handle,
                           lpaddr_t dst,
                           lpaddr_t src,
                           size_t bytes,
                           dma_req_id_t *id);

#endif /* DMA_SERVICE_H_ */
