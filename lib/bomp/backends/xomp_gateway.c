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

#include <xomp/xomp.h>
#include <xomp_gateway.h>
#include <xomp_debug.h>
#include <bomp_internal.h>

#include <if/xomp_gateway_defs.h>



/**
 * \brief represents a registered memory which can be obtained by the other
 *        domains
 */
struct mem_reg
{
    struct mem_reg *next;           ///< next chunk of registered memory
    lpaddr_t addr;                  ///< physical address of registered memory
    struct capref frame;            ///< capability of the frame
};

/**
 * \brief Message states for the TX queue
 */
struct xh_msg_st
{
    struct txq_msg_st common;       ///< common msg state
    /* union of arguments */
    struct capref frame;            ///< the replied frame
};

/// our token
volatile uint64_t xgw_token = 0x0;

/*
 * ----------------------------------------------------------------------------
 * XOMP helper memory management
 * ----------------------------------------------------------------------------
 *
 * We are using a normal linked list here and put the new memory memory regions
 * in front. When a new memory region is added, its likely that it will be
 * requested soon. Once requested by all workers of the node, the likelyhood that
 * it is requested later should be small
 *
 * Note: we are using virtual addresses here since when sharing a frame,
 *       it should be mapped at the very same address for pointers to it
 *       being valid in all worker domains
 */

/// list of registered memory
static struct mem_reg *mem_regs;

/**
 * \brief allocates a new registered memory region and fills it
 *
 * \param addr  virtual mapped address of the region
 * \param frame frame backing this memory
 *
 * \return pointer to the memory struct
 *         NULL if we could not allocate memory
 */
static inline struct mem_reg *xgw_mem_alloc(lpaddr_t addr,
                                            struct capref frame)
{
    struct mem_reg *mem = calloc(1, sizeof(*mem));
    if (mem == NULL) {
        return NULL;
    }

    mem->addr = addr;
    mem->frame = frame;

    return mem;
}

/**
 * \brief looks up a memory region in the list of registered memory regions
 *
 * \param addr  the address to look for
 *
 * \return pointer ot the memory struct
 *         NULL if there is no such memory region
 */
static struct mem_reg *xgw_do_lookup_mem(lpaddr_t addr)
{
    struct mem_reg *mem = mem_regs;
    while (mem) {
        if (mem->addr == addr) {
            break;
        }
        mem = mem->next;
    }
    return mem;
}

/**
 * \brief inserts a new memory region into the list
 *
 * \param addr  the virtual address of the region
 * \param frame frame capability backing the address
 *
 * \return pointer to the newly allocated memory region
 *         NULL if there was a failure
 */
static struct mem_reg *xgw_do_insert_mem(lpaddr_t addr,
                                         struct capref frame)
{
    struct mem_reg *new_mem = xgw_mem_alloc(addr, frame);
    if (new_mem == NULL) {
        return NULL;
    }

    if (mem_regs) {
        new_mem->next = mem_regs;
        mem_regs = new_mem;
    } else {
        mem_regs = new_mem;
    }

    return new_mem;
}

/**
 * \brief registers a new memory region that it can be requested lateron
 *
 * \param addr  the virtual address of the region
 * \param frame frame capability backing the address
 *
 * \return SYS_ERR_OK on success
 *         LIB_ERR_MALLOC_FAIL on failure
 */
errval_t xomp_gateway_mem_insert(struct capref frame,
                                 lpaddr_t addr)
{
    XWG_DEBUG("inserting memory: [0x%016lx]", addr);
    if (xgw_do_insert_mem(addr, frame)) {
        return SYS_ERR_OK;
    }

    return LIB_ERR_MALLOC_FAIL;
}

/*
 * ----------------------------------------------------------------------------
 * XOMP helper send handlers
 * ----------------------------------------------------------------------------
 */

static errval_t get_memory_response_tx(struct txq_msg_st *msg_st)
{
    struct xh_msg_st *st = (struct xh_msg_st *) msg_st;

    return xomp_gateway_get_memory_response__tx(msg_st->queue->binding,
                                                TXQCONT(msg_st), msg_st->err,
                                                st->frame);
}

/*
 * ----------------------------------------------------------------------------
 * XOMP helper receive handlers
 * ----------------------------------------------------------------------------
 */

static void get_memory_call_rx(struct xomp_gateway_binding *b,
                               lpaddr_t addr,
                               uint64_t token)
{
    XWG_DEBUG("get_memory_call_rx: [0x%016lx]", addr);

    struct tx_queue *txq = b->st;
    struct txq_msg_st *msg_st = txq_msg_st_alloc(txq);
    assert(msg_st != NULL);

    msg_st->cleanup = NULL;
    msg_st->send = get_memory_response_tx;

    if (token != xgw_token) {
        XWG_DEBUG("get_memory_call_rx token do not match: %lx %lx\n", token,
                  xgw_token);
        msg_st->err = XOMP_ERR_INVALID_TOKEN;
        txq_send(msg_st);
        return;
    }

    struct xh_msg_st *st = (struct xh_msg_st *) msg_st;

    struct mem_reg *mem = xgw_do_lookup_mem(addr);
    if (mem == NULL) {
        XWG_DEBUG("get_memory_call_rx: memory lookup failed\n");
        msg_st->err = XOMP_ERR_INVALID_MEMORY;
    } else {
        msg_st->err = SYS_ERR_OK;
        st->frame = mem->frame;
    }

    txq_send(msg_st);
}

static struct xomp_gateway_rx_vtbl rx_vtbl = {
    .get_memory_call = get_memory_call_rx,
};

/*
 * ----------------------------------------------------------------------------
 * XOMP helper connect / export callbacks
 * ----------------------------------------------------------------------------
 */

static errval_t xgw_svc_connect_cb(void *st,
                                   struct xomp_gateway_binding *xb)
{
    XWG_DEBUG("new connection from client\n");

    struct tx_queue *queue = calloc(1, sizeof(*queue));
    if (queue == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    txq_init(queue, xb, xb->waitset, (txq_register_fn_t) xb->register_send,
             sizeof(struct xh_msg_st));

    xb->rx_vtbl = rx_vtbl;
    xb->st = queue;

    return SYS_ERR_OK;
}

static void xh_svc_export_cb(void *st,
                             errval_t err,
                             iref_t iref)
{
    XWG_DEBUG("gateway service exported %s\n", err_getstring(err));

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    char svc_name[40];
    snprintf(svc_name, 40, "%s.%s", disp_name(), "xgw");

    err = nameservice_register(svc_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice registration failed");
    }

    xgw_token = xomp_gateway_make_token();

    XWG_DEBUG("gateway iref:%"PRIxIREF" @ {%s} with token: %lx\n", iref, svc_name,
              xgw_token);
}

/*
 * ----------------------------------------------------------------------------
 * XOMP gateway service initialization
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes the xomp gatway service
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t xomp_gateway_init(void)
{
    errval_t err;

    XWG_DEBUG("xomp gateway initialization\n");

    err = xomp_gateway_export(NULL, xh_svc_export_cb, xgw_svc_connect_cb,
                              get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (!xgw_token) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}
