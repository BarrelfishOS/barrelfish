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

#include <if/dma_defs.h>

/**
 * DMA service callbacks called when the message event occurs
 * if a callback is not set, this will trigger a not supported
 * error in the client.
 */
struct dma_service_cb
{
    /** informs about the new connection on the service */
    errval_t (*connect)(void **user_st);

    /** registers a memory region to be used */
    errval_t (*addregion)(void *user_st,
                          struct capref cap);

    /** deregisters a memory region*/
    errval_t (*removeregion)(void *user_st,
                             struct capref cap);

    /** execute a memcpy request */
    errval_t (*memcpy)(void *user_st,
                       lpaddr_t dst,
                       lpaddr_t src,
                       size_t bytes,
                       dma_req_id_t *id);
};

/**
 * \brief initializes the DMA service
 *
 * \param svc_name  The name of the service for nameservice registration
 * \param cb        Callback function pointers
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_service_init(const char *svc_name,
                          struct dma_service_cb *cb);

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
errval_t dma_service_send_done(struct dma_binding *binding,
                               errval_t err,
                               dma_req_id_t id);


#endif  /* LIB_DMA_CLIENT_H */
