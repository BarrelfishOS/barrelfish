/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_SERVICE_H
#define LIB_DMA_SERVICE_H

/// dma service handle
typedef struct dma_svc_st* dma_svc_handle_t;

/**
 * DMA service callbacks called when the message event occurs
 * if a callback is not set, this will trigger a not supported
 * error in the client.
 */
struct dma_service_cb
{
    /** informs about the new connection on the service */
    errval_t (*connect)(void *arg, void **user_st);

    /** registers a memory region to be used */
    errval_t (*addregion)(dma_svc_handle_t svc_handle,
                          struct capref cap);

    /** deregisters a memory region*/
    errval_t (*removeregion)(dma_svc_handle_t svc_handle,
                             struct capref cap);

    /** execute a memcpy request */
    errval_t (*memcpy)(dma_svc_handle_t svc_handle,
                       lpaddr_t dst,
                       lpaddr_t src,
                       size_t bytes,
                       dma_req_id_t *id);
};

/**
 * \brief initializes the DMA service and registers with the DMA manager
 *
 * \param cb        Callback function pointers
 * \param arg       Argument passed to the connect callback
 * \param svc_iref  Returns the exported iref
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_service_init(struct dma_service_cb *cb,
                          void *arg,
                          iref_t *svc_iref);

/**
 * \brief initializes the DMA service and exports it to the nameservice
 *
 * \param svc_name  The name of the service for nameservice registration
 * \param cb        Callback function pointers
 * \param arg       Argument passed to the connect callback
 * \param svc_iref  Returns the exported iref
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_service_init_with_name(char *svc_name,
                                    struct dma_service_cb *cb,
                                    void *arg,
                                    iref_t *svc_iref);

/**
 * \brief sends a done notification about the transfer that has completed
 *
 * \param binding   DMA binding
 * \param err       Outcome of the transfer
 * \param id        The id of the completed transfer
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_service_send_done(dma_svc_handle_t svc_handle,
                               errval_t err,
                               dma_req_id_t id);

static inline void *dma_service_get_user_state(dma_svc_handle_t handle)
{
    return (void *) *((lvaddr_t *)handle);
}

#endif  /* LIB_DMA_CLIENT_H */
