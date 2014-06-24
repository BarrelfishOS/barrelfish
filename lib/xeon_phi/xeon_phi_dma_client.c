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
    uint8_t xphi_id;
    struct xeon_phi_dma_cont cont;
    struct xdma_req *next;
    struct xdma_req *prev;
};

/// service irefs
static iref_t xdma_svc_iref[XEON_PHI_NUM_MAX];

/// service bindings
struct xeon_phi_dma_binding *xdma_binding[XEON_PHI_NUM_MAX];

/// wait reply flags for RPC like functionality where needed
uint8_t xdma_wait_reply[XEON_PHI_NUM_MAX];

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

static struct xdma_req *xdma_get_pending_request(xeon_phi_dma_id_t id)
{
    if (requests_pending_count == 0) {
        return NULL;
    }
    struct xdma_req *req = requests_pending;
    while (req) {
        if (req->id == id) {
            if (req->prev == NULL) {
                /* beginning */
                requests_pending = req->next;
                requests_pending->prev = NULL;
            } else if (req->next == NULL) {
                req->prev->next = NULL;
            } else {
                req->prev->next = req->next;
                req->next->prev = req->prev;
            }
            req->next = NULL;
            req->prev = NULL;

            requests_pending_count--;

            return req;

        }
    }
    return NULL;
}

static void xdma_insert_pending_request(struct xdma_req *req)
{
    if (requests_pending == NULL) {
        requests_pending = req;
        req->next = NULL;
    } else {
        requests_pending->prev = req;
        req->next = requests_pending;
        requests_pending = req;
    }
    req->prev = NULL;

    requests_pending_count++;
}

static struct xdma_req *xdma_get_free_request(void)
{
    if (requests_free_count == 0) {
        return NULL;
    }

    struct xdma_req *req = requests_free;
    requests_free = req->next;
    req->next = NULL;
    requests_free_count--;
    return req;

}

static void xdma_insert_free_request(struct xdma_req *req)
{
    req->next = requests_free;
    requests_free = req;
    requests_free_count++;
}

/*
 * forward declarations for the recv messages
 */

static void xdma_register_response_rx(struct xeon_phi_dma_binding *_binding,
                                      xeon_phi_dma_errval_t msgerr);
static void xdma_deregister_response_rx(struct xeon_phi_dma_binding *_binding,
                                        xeon_phi_dma_errval_t msgerr);
static void xdma_exec_response_rx(struct xeon_phi_dma_binding *_binding,
                                  xeon_phi_dma_errval_t err,
                                  xeon_phi_dma_id_t id);
static void xdma_stop_response_rx(struct xeon_phi_dma_binding *_binding,
                                  xeon_phi_dma_errval_t err);
static void xdma_done_rx(struct xeon_phi_dma_binding *_binding,
                         xeon_phi_dma_id_t id,
                         xeon_phi_dma_errval_t err);

struct xeon_phi_dma_rx_vtbl xdma_rx_vtbl = {
    .register_response = xdma_register_response_rx,
    .deregister_response = xdma_deregister_response_rx,
    .exec_response = xdma_exec_response_rx,
    .stop_response = xdma_stop_response_rx,
    .done = xdma_done_rx
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

    uint8_t xphi_id = (uint8_t) (uintptr_t) st;
    xdma_binding[xphi_id] = b;

    DEBUG_XDMA("Binding to xdma service ok.\n");

    conn_state = XPM_STATE_BIND_OK;
}

/**
 * \brief
 */
static errval_t xdma_connect(uint8_t xphi_id)
{
    errval_t err;

    if (xdma_binding[xphi_id] != NULL) {
        return SYS_ERR_OK;
    }

    char buf[50];
    snprintf(buf, 50, "%s.%u", XEON_PHI_DMA_SERVICE_NAME, xphi_id);

    iref_t svc_iref;

    DEBUG_XDMA("Nameservice lookup: %s\n", buf);
    err = nameservice_blocking_lookup(buf, &svc_iref);
    if (err_is_fail(err)) {
        return err;
    }

    conn_state = XPM_STATE_BINDING;

    DEBUG_XDMA("binding to iref [%u]... \n", svc_iref);
    xdma_svc_iref[xphi_id] = svc_iref;
    err = xeon_phi_dma_bind(svc_iref,
                            xdma_bind_cb,
                            (void *) (uintptr_t) xphi_id,
                            get_default_waitset(),
                            IDC_BIND_FLAGS_DEFAULT);
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

static struct xdma_reg_msg_st
{
    struct capref mem;
    errval_t err;
    uint8_t xphi_id;
} xdma_reg_msg_st[XEON_PHI_NUM_MAX];

static void xdma_register_response_rx(struct xeon_phi_dma_binding *_binding,
                                      xeon_phi_dma_errval_t msgerr)
{
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;

    DEBUG_XDMA("received register response [%u, %lu]\n", xphi_id, msgerr);

    assert(xdma_reg_msg_st[xphi_id].xphi_id == xphi_id);

    xdma_reg_msg_st[xphi_id].err = msgerr;
    xdma_wait_reply[xphi_id] = 0x0;
}

static void xdma_register_call_tx(void *a)
{
    errval_t err;

    struct xdma_reg_msg_st *msg_st = a;

    struct xeon_phi_dma_binding *b = xdma_binding[msg_st->xphi_id];

    struct event_closure txcont = MKCONT(NULL, a);

    err = xeon_phi_dma_register_call__tx(b, txcont, msg_st->mem);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xdma_register_call_tx, a);
            err = b->register_send(b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                msg_st->err = err;
                xdma_wait_reply[msg_st->xphi_id] = 0x0;
            }
        }
    }
}

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

#ifdef __k1om__
    assert(xphi_id == 0);
#endif
    if (xdma_binding[xphi_id] == NULL) {
        err = xdma_connect(xphi_id);
        if (err_is_fail(err)) {
            return err;
        }
    }

    if (xdma_wait_reply[xphi_id]) {
        return XEON_PHI_ERR_DMA_RPC_IN_PROGRESS;
    }

    xdma_wait_reply[xphi_id] = 0x1;

    struct xdma_reg_msg_st *st = xdma_reg_msg_st + xphi_id;

    st->xphi_id = xphi_id;
    st->err = SYS_ERR_OK;
    st->mem = mem;

    xdma_register_call_tx(st);

    while (xdma_wait_reply[xphi_id]) {
        messages_wait_and_handle_next();
    }

    return st->err;
}

/*
 * ---------------------------------------------------------------------------
 * DMA deregister memory
 * ---------------------------------------------------------------------------
 */

static struct xdma_dereg_msg_st
{
    struct capref mem;
    errval_t err;
    uint8_t xphi_id;
} xdma_dereg_msg_st[XEON_PHI_NUM_MAX];

static void xdma_deregister_response_rx(struct xeon_phi_dma_binding *_binding,
                                        xeon_phi_dma_errval_t msgerr)
{
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;

    DEBUG_XDMA("received deregister response [%u, %lu]\n", xphi_id, msgerr);

    xdma_wait_reply[xphi_id] = 0x0;
}

static void xdma_deregister_call_tx(void *a)
{
    errval_t err;

    struct xdma_dereg_msg_st *msg_st = a;

    struct xeon_phi_dma_binding *b = xdma_binding[msg_st->xphi_id];

    struct event_closure txcont = MKCONT(NULL, a);

    err = xeon_phi_dma_deregister_call__tx(b, txcont, msg_st->mem);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xdma_deregister_call_tx, a);
            err = b->register_send(b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                msg_st->err = err;
                xdma_wait_reply[msg_st->xphi_id] = 0x0;
            }
        }
    }
}

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
                                        struct capref mem)
{
    errval_t err;

#ifdef __k1om__
    assert(xphi_id == 0);
#endif
    if (xdma_binding[xphi_id] == NULL) {
        err = xdma_connect(xphi_id);
        if (err_is_fail(err)) {
            return err;
        }
    }

    if (xdma_wait_reply[xphi_id]) {
        return XEON_PHI_ERR_DMA_RPC_IN_PROGRESS;
    }

    xdma_wait_reply[xphi_id] = 0x1;

    struct xdma_dereg_msg_st *st = xdma_dereg_msg_st + xphi_id;

    st->xphi_id = xphi_id;
    st->err = SYS_ERR_OK;
    st->mem = mem;

    xdma_deregister_call_tx(st);

    while (xdma_wait_reply[xphi_id]) {
        messages_wait_and_handle_next();
    }

    return st->err;

    return SYS_ERR_OK;
}

/*
 * ---------------------------------------------------------------------------
 * DMA start new transfer
 * ---------------------------------------------------------------------------
 */
static struct xdma_reg_start_st
{
    lpaddr_t src;
    lpaddr_t dst;
    uint64_t bytes;
    uint8_t xphi_id;
    xeon_phi_dma_id_t id;
    errval_t err;
} xdma_reg_start_st[XEON_PHI_NUM_MAX];

static void xdma_exec_response_rx(struct xeon_phi_dma_binding *_binding,
                                  xeon_phi_dma_errval_t err,
                                  xeon_phi_dma_id_t id)
{
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;

    DEBUG_XDMA("received exec response [%u, %u]\n", xphi_id, id);

    assert(xdma_reg_msg_st[xphi_id].xphi_id == xphi_id);

    xdma_reg_start_st[xphi_id].err = err;
    xdma_reg_start_st[xphi_id].id = id;

    xdma_wait_reply[xphi_id] = 0x0;
}

static void xdma_exec_call_tx(void *a)
{
    errval_t err;

    struct xdma_reg_start_st *msg_st = a;

    struct xeon_phi_dma_binding *b = xdma_binding[msg_st->xphi_id];

    struct event_closure txcont = MKCONT(NULL, a);

    err = xeon_phi_dma_exec_call__tx(b,
                                     txcont,
                                     msg_st->src,
                                     msg_st->dst,
                                     msg_st->bytes);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xdma_exec_call_tx, a);
            err = b->register_send(b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                msg_st->err = err;
                xdma_wait_reply[msg_st->xphi_id] = 0x0;
            }
        }
    }
}

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
                                   xeon_phi_dma_id_t *id)
{
    errval_t err;

    /*
     * we only allow multiple of 64 bytes for transfers.
     * The Xeon Phi DMA controller supports only 64 byte granilarity in
     * alignment and size.
     */
    if ((info->dest & (XEON_PHI_DMA_ALIGNMENT - 1)) || (info->src
                    & (XEON_PHI_DMA_ALIGNMENT - 1))
        || (info->size & (XEON_PHI_DMA_ALIGNMENT - 1))) {
        return XEON_PHI_ERR_DMA_MEM_ALIGN;
    }

#ifdef __k1om__
    assert(xphi_id == 0);
#endif
    if (xdma_binding[xphi_id] == NULL) {
        err = xdma_connect(xphi_id);
        if (err_is_fail(err)) {
            return err;
        }
    }

    if (xdma_wait_reply[xphi_id]) {
        return XEON_PHI_ERR_DMA_RPC_IN_PROGRESS;
    }

    struct xdma_req *req = xdma_get_free_request();
    if (req == NULL) {
        return XEON_PHI_ERR_DMA_BUSY;
    }

    struct xdma_reg_start_st *msg_st = xdma_reg_start_st + xphi_id;

    msg_st->bytes = info->size;
    msg_st->dst = info->dest;
    msg_st->src = info->src;
    msg_st->xphi_id = xphi_id;

    xdma_wait_reply[xphi_id] = 0x1;

    xdma_exec_call_tx(msg_st);

    while (xdma_wait_reply[xphi_id]) {
        messages_wait_and_handle_next();
    }

    if (id) {
        *id = msg_st->id;
    }

    return msg_st->err;
}

/*
 * ---------------------------------------------------------------------------
 * DMA dma execute transfer
 * ---------------------------------------------------------------------------
 */

static uint8_t execute_wait_flag;

static void xdma_execute_handler(xeon_phi_dma_id_t id,
                                 errval_t err,
                                 void *st)
{
    execute_wait_flag = 0x0;
}

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
    errval_t err;
    xeon_phi_dma_id_t id;
    struct xeon_phi_dma_cont cont = {
        .arg = NULL,
        .cb = xdma_execute_handler
    };

    execute_wait_flag = 0x1;

    err = xeon_phi_dma_client_start(xphi_id, info, cont, &id);

    while (execute_wait_flag) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

/*
 * ---------------------------------------------------------------------------
 * DMA stop transfer
 * ---------------------------------------------------------------------------
 */

static struct xdma_reg_stop_st
{
    struct xdma_req *req;
    xeon_phi_dma_id_t id;
    errval_t err;
} xdma_reg_stop_st[XEON_PHI_NUM_MAX];

static void xdma_stop_response_rx(struct xeon_phi_dma_binding *_binding,
                                  xeon_phi_dma_errval_t err)
{
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;

    struct xdma_reg_stop_st *req_st = xdma_reg_stop_st + xphi_id;

    DEBUG_XDMA("received stop response [%u, %u]\n", xphi_id, req_st->id);
}

static void xdma_stop_call_tx(void *a)
{

}

/**
 * \brief stops a previously started DMA transfer based on its ID
 *
 * \param the ID of the transfer to stop
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on failure
 */
errval_t xeon_phi_dma_client_stop(xeon_phi_dma_id_t id)
{

    struct xdma_req *req = xdma_get_pending_request(id);
    if (req == NULL) {
        return XEON_PHI_ERR_DMA_ID_NOT_EXISTS;
    }

#ifdef __k1om__
    assert(req->xphi_id == 0);
#endif

    assert(xdma_binding[req->xphi_id] != NULL);

    if (xdma_wait_reply[req->xphi_id]) {
        xdma_insert_pending_request(req);
        return XEON_PHI_ERR_DMA_RPC_IN_PROGRESS;
    }

    xdma_wait_reply[req->xphi_id] = 0x1;

    struct xdma_reg_stop_st *msg_st = xdma_reg_stop_st + req->xphi_id;

    xdma_stop_call_tx(msg_st);

    while (xdma_wait_reply[req->xphi_id]) {
        messages_wait_and_handle_next();
    }

    xdma_insert_free_request(req);

    return msg_st->err;
}

/*
 * ---------------------------------------------------------------------------
 * DMA transfer done
 * ---------------------------------------------------------------------------
 */

static void xdma_done_rx(struct xeon_phi_dma_binding *_binding,
                         xeon_phi_dma_id_t id,
                         xeon_phi_dma_errval_t err)
{
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;

    DEBUG_XDMA("received done message [%u, %u]\n", xphi_id, id);

    struct xdma_req *req = xdma_get_pending_request(id);
    assert(req);

    if (req->cont.cb) {
        req->cont.cb(id, err, req->cont.arg);
    }

    xdma_insert_free_request(req);
}

