/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <dma/xeon_phi/xeon_phi_dma.h>
#include <dma/xeon_phi/xeon_phi_dma_device.h>
#include <dma/xeon_phi/xeon_phi_dma_request.h>
#include <dma/dma.h>
#include <dma/dma_service.h>
#include <dma/dma_mem_mgr.h>
#include <dma/dma_manager_client.h>
#include <dma/dma_bench.h>
#include <xeon_phi/xeon_phi.h>

#include "xeon_phi_internal.h"
#include "dma_service.h"

struct user_st
{
    struct dma_mem_mgr *mem_mgr;
    struct xeon_phi *phi;
    uint64_t num_req;
};

static void memcpy_req_cb(errval_t err,
                          dma_req_id_t id,
                          void *st)
{
    XDMA_DEBUG("memcpy_req_cb %lx, %s\n", id, err_getstring(err));
    dma_service_send_done(st, err, id);
}

static lpaddr_t translate_address(void *arg,
                                  lpaddr_t addr,
                                  size_t size)
{
#ifdef __k1om__
    if ((addr + size) > (1UL << 40)) {
        return 0;
    }
    return addr;
#else
    struct xeon_phi *phi = ((struct user_st *)arg)->phi;
    lpaddr_t apt_lo = phi->apt.pbase;
    lpaddr_t apt_hi = phi->apt.pbase + phi->apt.bytes;
    if ((addr >= apt_lo) && ((addr + size) <= apt_hi)) {
        /* we are within the GDDR range */
        return addr - apt_lo;
    } else if ((addr + size)< XEON_PHI_SYSMEM_SIZE) {
        /*
         * Xeon Phi does not support more host memory,
         * assume host memory starts at 0x0
         */
        return addr + XEON_PHI_SYSMEM_BASE;
    } else {
        return 0;
    }
#endif
}

/*
 * ---------------------------------------------------------------------------
 * Callbacks for the service events
 * ---------------------------------------------------------------------------
 */

/*
 * \brief callback handler for new connect requests
 *
 * \param arg       argument passed on the init function
 * \param user_st   pointer to store the user state
 *
 * \returns SYS_ERR_OK if the connection is accepted
 *          errval if connection is rejected
 */
static errval_t dma_svc_connect_cb(void *arg,
                                   void **user_st)
{
    errval_t err;

    struct user_st *st = calloc(1, sizeof(*st));
    if (st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    XDMA_DEBUG("dma_svc_connect_cb user_st = %p\n", st);

    err = dma_mem_mgr_init(&st->mem_mgr, 0x0, (1UL << 48) - 1);
    if (err_is_fail(err)) {
        free(st);
        return err;
    }

    st->phi = arg;

    dma_mem_mgr_set_convert_fn(st->mem_mgr, translate_address, st);

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
static errval_t dma_svc_addregion_cb(dma_svc_handle_t svc_handle,
                                     struct capref cap)
{
    struct user_st *user_st = dma_service_get_user_state(svc_handle);

    XDMA_DEBUG("dma_svc_addregion_cb user_st = %p\n", user_st);

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
static errval_t dma_svc_removeregion_cb(dma_svc_handle_t svc_handle,
                                        struct capref cap)
{
    struct user_st *user_st = dma_service_get_user_state(svc_handle);

    XDMA_DEBUG("dma_svc_removeregion_cb user_st = %p\n", user_st);

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
static errval_t dma_svc_memcpy_cb(dma_svc_handle_t svc_handle,
                                  lpaddr_t dst,
                                  lpaddr_t src,
                                  size_t bytes,
                                  dma_req_id_t *id)
{
    errval_t err;

    struct user_st *st = dma_service_get_user_state(svc_handle);

    XDMA_DEBUG("dma_svc_memcpy_cb user_st = %p\n", st);

    lpaddr_t dma_dst, dma_src;
    err = dma_mem_verify(st->mem_mgr, dst, bytes, &dma_dst);
    if (err_is_fail(err)) {
        return err;
    }
    err = dma_mem_verify(st->mem_mgr, src, bytes, &dma_src);
    if (err_is_fail(err)) {
        return err;
    }

    XDMA_DEBUG("[%016lx]->[%016lx] of %lu bytes\n", dma_src, dma_dst, bytes);

    /* both addresses are valid and have been translated now */
    struct dma_device *dev = st->phi->dma;
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

    return dma_request_memcpy(dev, &setup, id);
}

static struct dma_service_cb dma_svc_cb = {
    .connect = dma_svc_connect_cb,
    .addregion = dma_svc_addregion_cb,
    .removeregion = dma_svc_removeregion_cb,
    .memcpy = dma_svc_memcpy_cb
};

/**
 * \brief initializes the Xeon Phi DMA devices and the service
 *
 * \param phi   Xeon Phi handle
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xdma_service_init(struct xeon_phi *phi)
{
    errval_t err;

    void *mmio_base = NULL;
#ifdef __k1om__
    mmio_base = (void *) (phi->mmio.vbase);
#else
    mmio_base = (void *) XEON_PHI_MMIO_TO_SBOX(phi);
#endif

    struct xeon_phi_dma_device *dev;
    err = xeon_phi_dma_device_init(mmio_base, &dev);
    if (err_is_fail(err)) {
        return err;
    }

    phi->dma = (struct dma_device *)dev;

#if DMA_BENCH_RUN_BENCHMARK
    dma_bench_run_default_xphi((struct dma_device *)dev);
#endif

    iref_t svc_iref;
    char svc_name[30];
#ifdef __k1om__
    snprintf(svc_name, 30, "%s", XEON_PHI_DMA_SERVICE_NAME);
#else
    snprintf(svc_name, 30, "%s.%u", XEON_PHI_DMA_SERVICE_NAME, phi->id);
#endif
    err = dma_service_init_with_name(svc_name, &dma_svc_cb, phi, &svc_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to start the DMA service");
    }

    err = dma_manager_register_driver(0, 1ULL << 40, DMA_DEV_TYPE_XEON_PHI,
                                      svc_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to register with the DMA manager\n");
    }

    return SYS_ERR_OK;
}

