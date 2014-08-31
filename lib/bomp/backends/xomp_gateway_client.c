/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <flounder/flounder_txqueue.h>

#include <bomp_internal.h>
#include <xomp/xomp.h>
#include <xomp_gateway.h>
#include <xomp_gateway_client.h>

#include <if/xomp_gateway_defs.h>

#include <xomp_debug.h>

/**
 * \brief Message state for the TX queue
 */
struct xgw_msg_st
{
    struct txq_msg_st common;       ///< common msg state
    /* union of arguments */
    uint64_t token;                 ///< memory token
    lpaddr_t addr;                  ///< address to request
};

/// binding transmit queue
struct tx_queue txq;

/// binding to the txq
struct xomp_gateway_binding *xgwbinding = NULL;

/// flag indicating that the RPC is in progress
static volatile uint8_t rpc_in_progress = 0x0;

/// flag indicating that the RPC is done and reply is ready
static volatile uint8_t rpc_reply_ready = 0x0;

/// outcome of the RPC
static errval_t rpc_err;

/// te replied frame
static struct capref rpc_frame;

/**
 * \brief generates a memory token based on the domain
 *
 * \return 64bit token
 */
uint64_t xomp_gateway_make_token(void)
{
    const char *name = disp_name();
    size_t len = strlen(name);

    uint32_t sig = 0xcafebabe;
    uint64_t token = len << 32;
    for (int i = 0; i < len; ++i) {
        sig ^= (((uint32_t) name[i]) << i);
    }
    return token | sig;
}

/*
 * ----------------------------------------------------------------------------
 * XOMP helper send handlers
 * ----------------------------------------------------------------------------
 */

static errval_t get_memory_call_tx(struct txq_msg_st *msg_st)
{

    struct xgw_msg_st *st = (struct xgw_msg_st *) msg_st;

    return xomp_gateway_get_memory_call__tx(msg_st->queue->binding,
                                            TXQCONT(msg_st), st->addr, st->token);
}

/*
 * ----------------------------------------------------------------------------
 * XOMP helper receive handlers
 * ----------------------------------------------------------------------------
 */

static void get_memory_response_rx(struct xomp_gateway_binding *b,
                                   errval_t msgerr,
                                   struct capref frame)
{
    rpc_err = msgerr;
    rpc_frame = frame;

    rpc_reply_ready = 0x1;
}

static struct xomp_gateway_rx_vtbl rx_vtbl = {
    .get_memory_response = get_memory_response_rx
};

/*
 * ----------------------------------------------------------------------------
 * XOMP helper connect / export callbacks
 * ----------------------------------------------------------------------------
 */

static void xgw_bind_cb(void *st,
                        errval_t err,
                        struct xomp_gateway_binding *xh)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "binding failed");
    }

    txq_init(&txq, xh, xh->waitset, (txq_register_fn_t) xh->register_send,
             sizeof(struct xgw_msg_st));

    xh->rx_vtbl = rx_vtbl;

    xgwbinding = xh;
}

/**
 * \brief connects to the gateway service of the first domain spawned on the node
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t xomp_gateway_bind_svc(void)
{
    errval_t err;

    assert(xgwbinding == NULL);

    char svc_name[40];
    snprintf(svc_name, 40, "%s.%s", disp_name(), "xgw");

    iref_t svc_iref;
    err = nameservice_blocking_lookup(svc_name, &svc_iref);
    if (err_is_fail(err)) {
        return err;
    }

    err = xomp_gateway_bind(svc_iref, xgw_bind_cb, NULL, get_default_waitset(),
                            IDC_BIND_FLAGS_DEFAULT);

    while (xgwbinding == NULL) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

/**
 * \brief requests the frame capability which belongs to the virtual addrss
 *
 * \param addr          virtual address of the mapped frame
 * \param ret_frame     returns the frame capability
 *
 * \return SYS_ERR_OK on sucess
 *         errval on failure
 */
errval_t xomp_gateway_get_memory(lpaddr_t addr,
                                 struct capref *ret_frame)
{
    errval_t err;

    if (xgwbinding == NULL) {
        err = xomp_gateway_bind_svc();
        if (err_is_fail(err)) {
            return err;
        }
    }

    while (rpc_in_progress) {
        messages_wait_and_handle_next();
    }

    rpc_in_progress = 0x1;
    rpc_reply_ready = 0x0;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&txq);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->cleanup = NULL;
    msg_st->send = get_memory_call_tx;

    struct xgw_msg_st *st = (struct xgw_msg_st *) msg_st;

    st->addr = addr;
    st->token = xomp_gateway_make_token();

    txq_send(msg_st);

    while (!rpc_reply_ready) {
        messages_wait_and_handle_next();
    }

    if (ret_frame) {
        *ret_frame = rpc_frame;
    }

    rpc_in_progress = 0x0;

    return rpc_err;
}
