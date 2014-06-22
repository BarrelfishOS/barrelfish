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

#include <xeon_phi/xeon_phi.h>

#include <xeon_phi/xeon_phi_dma.h>
#include <xeon_phi/xeon_phi_dma_client.h>

#include <if/xeon_phi_dma_defs.h>

/// The maximum number of concurrent DMA requests
#define REQUESTS_COUNT_MAX 16

struct xdma_req
{
    xeon_phi_dma_id_t id;
    struct xeon_phi_dma_cont cont;
    struct xdma_req *next;
    struct xdma_req *prev;
};

#ifdef __k1om__
/// service iref
static iref_t xdma_svc_iref;

/// service binding
struct xeon_phi_dma_binding *xdma_binding;

/// wait reply flag for RPC like functionality where needed
uint8_t xdma_wait_reply = 0;
#else
/// service irefs
static iref_t xdma_svc_iref[XEON_PHI_NUM_MAX];

/// service bindings
struct xeon_phi_dma_binding *xdma_binding[XEON_PHI_NUM_MAX];

/// wait reply flags for RPC like functionality where needed
uint8_t xdma_wait_reply[XEON_PHI_NUM_MAX]
#endif


/// pointer to all DMA requests
static struct xdma_req *requests;

/// the current requests being executed
static struct xdma_req *requests_pending;

/// the number of pending requests
static uint32_t requests_pending_count;

/// unused / free requests
static struct xdma_req *requests_free;

/// the number of free requests
static uint32_t requests_free_count;



#define DEBUG_XDMA(x...) debug_printf(" [xdma] " x)

enum xpm_state
{
    XPM_STATE_NSLOOKUP,
    XPM_STATE_BINDING,
    XPM_STATE_BIND_OK,
    XPM_STATE_BIND_FAIL,
};

static enum xpm_state conn_state = XPM_STATE_NSLOOKUP;

/*
 * forward declarations for the recv messages
 */


struct xeon_phi_dma_rx_vtbl xdma_rx_vtbl = {
    .register_call = NULL
};

/**
 * \brief
 *
 * \param
 * \param
 * \param
 */
static void xdma_bind_cb(void *st,
                         errval_t err,
                         struct xeon_phi_dma_binding *b)
{

    if (err_is_fail(err)) {
        conn_state = XPM_STATE_BIND_FAIL;
        return;
    }

    b->rx_vtbl = xdma_rx_vtbl;
#ifdef __k1om__
    xdma_binding = b;
#else
    uint8_t xphi_id = (uint8_t)(uintptr_t)st;
    xdma_binding[xphi_id] = b;
#endif
    DEBUG_XDMA("Binding to xdma service ok.\n");

    conn_state = XPM_STATE_BIND_OK;
}

/**
 * \brief
 */
#ifdef __k1om__
static errval_t xdma_connect(void)
{
    errval_t err;

    if (xdma_binding != NULL) {
        return SYS_ERR_OK;
    }

#else
static errval_t xdma_connect(uint8_t xphi_id)
{
    errval_t err;

    if (xdma_binding[xphi_id] != NULL) {
        return SYS_ERR_OK;
    }

#endif

    char buf[50];
#if !defined(__k1om__)
    snprintf(buf, 50, "%s.%u", XEON_PHI_DMA_SERVICE_NAME, xphi_id);
#else
    snprintf(buf, 50, "%s", XEON_PHI_DMA_SERVICE_NAME);
#endif

    iref_t svc_iref;

    DEBUG_XDMA("Nameservice lookup: %s\n", buf);
    err = nameservice_blocking_lookup(buf, &svc_iref);
    if (err_is_fail(err)) {
        return err;
    }

    conn_state = XPM_STATE_BINDING;

    DEBUG_XDMA("binding to iref [%u]... \n", svc_iref);
#ifdef __k1om__
    xdma_svc_iref = svc_iref;
    err = xeon_phi_dma_bind(xdma_svc_iref, xdma_bind_cb,
                                     NULL,
                                      get_default_waitset(),
                                      IDC_BIND_FLAGS_DEFAULT);
#else
    xdma_svc_iref[xphi_id] = svc_iref;
    err = xeon_phi_dma_bind(svc_iref, xdma_bind_cb,
                                          (void *)(uintptr_t)xphi_id,
                                          get_default_waitset(),
                                          IDC_BIND_FLAGS_DEFAULT);
#endif
    if (err_is_fail(err)) {
        return err;
    }

    while (conn_state == XPM_STATE_BINDING) {
        messages_wait_and_handle_next();
    }

    if (conn_state == XPM_STATE_BIND_FAIL) {
        return FLOUNDER_ERR_BIND;
    }

    return SYS_ERR_OK;
}

/**
 * initializes the XEON Phi DMA client library
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on error
 */
errval_t xeon_phi_dma_client_init(void)
{
    requests = calloc(REQUESTS_COUNT_MAX, sizeof(*requests));
    if (!requests) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct xdma_req *req = requests;
    for (uint32_t i = 0; i < REQUESTS_COUNT_MAX - 1; ++i) {
        req->next = req + 1;
        req++;
    }

    requests_free = requests;
    requests_free_count = REQUESTS_COUNT_MAX;

    requests_pending = NULL;
    requests_pending_count = 0;

    return SYS_ERR_OK;
}

/*
 * ---------------------------------------------------------------------------
 * DMA register a new memory region to be used
 * ---------------------------------------------------------------------------
 */

struct xdma_reg_msg_st
{


};


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
                                      struct capref mem)
{
    errval_t err = SYS_ERR_OK;
    struct xeon_phi_dma_binding *bind;

#ifdef __k1om__
    if (xdma_binding == NULL) {
        err = xdma_connect();
    }
    bind = xdma_binding;
#else
    if (xdma_binding[xphi_id] == NULL) {
        err = xdma_connect(xphi_id);
    }
    bind = xdma_binding[xphi_id];
#endif
    if(err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}



/*
 * ---------------------------------------------------------------------------
 * DMA deregister memory
 * ---------------------------------------------------------------------------
 */

struct xdma_dereg_msg_st
{


};


/*
 * ---------------------------------------------------------------------------
 * DMA start new transfer
 * ---------------------------------------------------------------------------
 */
struct xdma_reg_start_st
{


};

/*
 * ---------------------------------------------------------------------------
 * DMA stop transfer
 * ---------------------------------------------------------------------------
 */

struct xdma_reg_stop_st
{


};

/*
 * ---------------------------------------------------------------------------
 * DMA dma execute transfer
 * ---------------------------------------------------------------------------
 */

struct xdma_reg_exec_st
{


};


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
                                  struct xeon_phi_dma_info *info)
{

}
}

/*
 * ---------------------------------------------------------------------------
 * DMA transfer done
 * ---------------------------------------------------------------------------
 */


