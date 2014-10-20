/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <dma_internal.h>
#include <dma/dma_manager_client.h>
#include <debug.h>

#include <if/dma_mgr_defs.h>
#include <if/dma_mgr_rpcclient_defs.h>

/// DMA manager RPC client
static struct dma_mgr_rpc_client dma_mgr_client;

/// connected flag
uint8_t dma_mgr_connected = 0x0;

/// state for binding
struct dma_mgr_bind_st
{
    uint8_t bound;
    errval_t err;
};

/*
 * ---------------------------------------------------------------------------
 * Service Binding
 * ---------------------------------------------------------------------------
 */

static void bind_cb(void *st,
                    errval_t err,
                    struct dma_mgr_binding *b)
{
    assert(st);
    struct dma_mgr_bind_st *bindst = st;

    bindst->err = err;

    if (err_is_fail(err)) {
        DMAMGR_DEBUG("connect: connection to {%s} failed.\n", DMA_MGR_SVC_NAME);
        bindst->bound = 0x1;
        return;
    }

    DMAMGR_DEBUG("connect: connection to {%s} established.\n", DMA_MGR_SVC_NAME);

    bindst->err = dma_mgr_rpc_client_init(&dma_mgr_client, b);

    if (err_is_fail(bindst->err)) {
        DMAMGR_DEBUG("connect: RPC client init failed.\n");
    }

    bindst->bound = 0x1;
}

static errval_t dma_manager_connect(void)
{
    errval_t err;
    iref_t iref;

    DMAMGR_DEBUG("connect: looking up service name {%s}\n", DMA_MGR_SVC_NAME);
    err = nameservice_blocking_lookup(DMA_MGR_SVC_NAME, &iref);
    if (err_is_fail(err)) {
        return err;
    }

    struct dma_mgr_bind_st st = {
        .bound = 0x0
    };

    DMAMGR_DEBUG("connect: binding to iref [%"PRIxIREF"]\n", iref);
    struct waitset *ws = get_default_waitset();
    err = dma_mgr_bind(iref, bind_cb, &st, ws, IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (st.bound == 0x0) {
        messages_wait_and_handle_next();
    }

    if (err_is_ok(st.err)) {
        dma_mgr_connected = 0x1;
    }

    return st.err;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief initializes the connection to the DMA manager service in an eager way
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_manager_client_init(void)
{
    errval_t err;

    if (dma_mgr_connected == 0) {
        err = dma_manager_connect();
        if (err_is_fail(err)) {
            return err;
        }
    }
    return SYS_ERR_OK;
}

/**
 * \brief registers a DMA driver with the DMA driver manager
 *
 * \param mem_low   lower end of the supported memory range
 * \param mem_high  upper end of the supported memory range
 * \param type      DMA device type
 * \param iref      the iref of the exported DMA service
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t dma_manager_register_driver(lpaddr_t mem_low,
                                     lpaddr_t mem_high,
                                     uint8_t type,
                                     iref_t iref)
{
#ifdef __k1om__
    char buf[30];
    snprintf(buf, 30, "%s_%u_%u", DMA_MGR_REGISTERED_DRIVER,
             (uint8_t) DMA_DEV_TYPE_XEON_PHI, disp_xeon_phi_id());

    DMAMGR_DEBUG("registering Xeon Phi Driver: {%s}\n", buf);

    return nameservice_register(buf, iref);
#else
    errval_t err;
    errval_t msgerr;

    if (dma_mgr_connected == 0) {
        err = dma_manager_connect();
        if (err_is_fail(err)) {
            return err;
        }
    }

    DMAMGR_DEBUG("register driver: {%016lx, %016lx} @ %"PRIxIREF"\n", mem_low,
                    mem_high, iref);

    //XXX need to figure this out otherwise
    uint8_t numa_node = (disp_get_core_id() >= 20);

    err = dma_mgr_client.vtbl.register_driver(&dma_mgr_client, mem_low, mem_high,
                    numa_node, type, iref, &msgerr);
    if (err_is_fail(err)) {
        DMAMGR_DEBUG("register driver: RPC failed %s\n", err_getstring(err));
        return err;
    }

    return msgerr;
#endif
}

/**
 * \brief queries the DMA manager for a suitable DMA driver for a address, size
 *        pair
 *
 * \param addr      address of the transfer
 * \param size      size of the desired transfer
 * \param info      returns the driver info
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t dma_manager_lookup_driver(lpaddr_t addr,
                                   lpaddr_t size,
                                   struct dma_mgr_driver_info *info)
{
#ifdef __k1om__
    errval_t err;

    if (addr + size > (1UL << 40)) {
        return DMA_ERR_MEM_OUT_OF_RANGE;
    }

    char buf[30];
    snprintf(buf, 30, "%s_%u_%u", DMA_MGR_REGISTERED_DRIVER,
             (uint8_t) DMA_DEV_TYPE_XEON_PHI, disp_xeon_phi_id());

    err = nameservice_lookup(buf, &info->iref);
    if (err_is_fail(err)) {
        return DMA_ERR_SVC_VOID;
    }
    info->mem_high = (1UL << 40);
    info->mem_low = 0;
    info->numa_node = disp_xeon_phi_id();
    info->type = DMA_DEV_TYPE_XEON_PHI;
    return SYS_ERR_OK;
#else

    errval_t err, msgerr;

    if (dma_mgr_connected == 0) {
        err = dma_manager_connect();
        if (err_is_fail(err)) {
            return err;
        }
    }

    DMAMGR_DEBUG("lookup driver: [%016lx, %016lx]\n", addr, size);

    uint8_t numa_node = (disp_get_core_id() >= 20);

    err = dma_mgr_client.vtbl.lookup_driver(&dma_mgr_client, addr, size,
                    numa_node, &msgerr, &info->mem_low,
                    &info->mem_high, &info->numa_node,
                    (uint8_t*) &info->type, &info->iref);
    if (err_is_fail(err)) {
        DMAMGR_DEBUG("register driver: RPC failed %s\n", err_getstring(err));
        return err;
    }

    return msgerr;
#endif
}

/**
 * \brief queries the DMA driver manager based on the service iref
 *
 * \param iref      iref ot the exported driver service
 * \param info      returns the driver info
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t dma_manager_lookup_by_iref(iref_t iref,
                                    struct dma_mgr_driver_info *info)
{
#ifdef __k1om__
    info->mem_high = (1UL << 40);
    info->mem_low = 0;
    info->numa_node = disp_xeon_phi_id();
    info->type = DMA_DEV_TYPE_XEON_PHI;
    info->iref = iref;
    return SYS_ERR_OK;
#else
    errval_t err, msgerr;

    if (dma_mgr_connected == 0) {
        err = dma_manager_connect();
        if (err_is_fail(err)) {
            return err;
        }
    }

    DMAMGR_DEBUG("lookup driver by iref:%"PRIxIREF"\n", iref);

    err = dma_mgr_client.vtbl.lookup_driver_by_iref(&dma_mgr_client, iref,
                                                    &msgerr, &info->mem_low,
                                                    &info->mem_high,
                                                    &info->numa_node,
                                                    (uint8_t*) &info->type);
    if (err_is_fail(err)) {
        DMAMGR_DEBUG("register driver: RPC failed %s\n", err_getstring(err));
        return err;
    }

    info->iref = iref;

    return msgerr;
#endif
}

/**
 * \brief waits until a device driver for the supplied device type is ready
 *
 * \param device    DMA device type
 * \param numa_node Numanode of the DMA device driver
 *
 * \returns SYS_ERR_OK when the driver is ready
 *          errval if there was something wrong
 */
errval_t dma_manager_wait_for_driver(dma_dev_type_t device,
                                     uint8_t numa_node)
{
    char buf[30];
    snprintf(buf, 30, "%s_%u_%u", DMA_MGR_REGISTERED_DRIVER, (uint8_t) device,
             numa_node);

    DMAMGR_DEBUG("waiting for driver: {%s}\n", buf);

    iref_t dummy_iref;
    return nameservice_blocking_lookup(buf, &dummy_iref);
}

