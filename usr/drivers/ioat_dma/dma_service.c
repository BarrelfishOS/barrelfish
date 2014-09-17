/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>

#include <dma/dma.h>
#include <dma/dma_service.h>
#include <dma/dma_mem_mgr.h>
#include <dma/ioat/ioat_dma_request.h>

#include "device.h"
#include "dma_service.h"

#include "debug.h"

struct user_st
{
    struct dma_mem_mgr *mem_mgr;
    uint64_t num_req;
};

/*
 * ---------------------------------------------------------------------------
 * Request done callbacks
 * ---------------------------------------------------------------------------
 */

static void memcpy_req_cb(errval_t err, dma_req_id_t id, void *st)
{
    DMA_DEBUG("memcpy_req_cb %lx, %s\n", id, err_getstring(err));
    dma_service_send_done(st, err, id);
}

/*
 * ---------------------------------------------------------------------------
 *
 * ---------------------------------------------------------------------------
 */


/*
 * \brief callback handler for new connect requests
 *
 * \param user_st   pointer to store the user state
 *
 * \returns SYS_ERR_OK if the connection is accepted
 *          errval if connection is rejected
 */
errval_t dma_svc_connect_cb(void *arg, void **user_st)
{
    errval_t err;

    struct user_st *st = calloc(1, sizeof(*st));
    if (st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    DMA_DEBUG("dma_svc_connect_cb user_st = %p\n", st);

    err = dma_mem_mgr_init(&st->mem_mgr, 0x0, (1UL << 48) - 1);
    if (err_is_fail(err)) {
        free(st);
        return err;
    }

    *user_st = st;

    return SYS_ERR_OK;
}

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
                              struct capref cap)
{
    struct user_st *user_st = dma_service_get_user_state(svc_handle);

    DMA_DEBUG("dma_svc_addregion_cb user_st = %p\n", user_st);

    return dma_mem_register(user_st->mem_mgr, cap);
}

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
                                 struct capref cap)
{
    struct user_st *user_st = dma_service_get_user_state(svc_handle);

    DMA_DEBUG("dma_svc_removeregion_cb user_st = %p\n", user_st);

    return dma_mem_deregister(user_st->mem_mgr, cap);
}

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
                           dma_req_id_t *id)
{
    errval_t err;

    struct user_st *st = dma_service_get_user_state(svc_handle);

    DMA_DEBUG("dma_svc_memcpy_cb user_st = %p\n", st);

    lpaddr_t dma_dst, dma_src;
    err = dma_mem_verify(st->mem_mgr, dst, bytes, &dma_dst);
    if (err_is_fail(err)) {
        return err;
    }
    err = dma_mem_verify(st->mem_mgr, src, bytes, &dma_src);
    if (err_is_fail(err)) {
        return err;
    }

    DMA_DEBUG("[%016lx]->[%016lx] of %lu bytes\n", dma_src, dma_dst, bytes);


    /* both addresses are valid and have been translated now */
    struct ioat_dma_device *dev = ioat_device_get_next();
    assert(dev);

    struct dma_req_setup setup = {
        .type = DMA_REQ_TYPE_MEMCPY,
        .done_cb = memcpy_req_cb,
        .cb_arg = svc_handle,
        .args = {
            .memcpy = {
                .src = dma_src,
                .dst = dma_dst,
                .bytes = bytes
            }
        }
    };

    return ioat_dma_request_memcpy((struct dma_device *)dev, &setup, id);
}
