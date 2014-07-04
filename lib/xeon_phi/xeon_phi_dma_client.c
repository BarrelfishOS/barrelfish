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
#include <xeon_phi/xeon_phi_dma_client.h>

#include <if/xeon_phi_dma_defs.h>

#ifdef XEON_PHI_DEBUG_DMA
#define DEBUG_XDMA(x...) debug_printf(" [xpdma] " x)
#else
#define DEBUG_XDMA(x...)
#endif

/// The maximum number of concurrent DMA requests
#define REQUESTS_COUNT_MAX 16

// forward declaration
static errval_t xdma_connect(uint8_t xphi_id);

/**
 * state enumeration for the Flounder connection to the service
 */
enum xpm_state
{
    XPM_STATE_NSLOOKUP,
    XPM_STATE_BINDING,
    XPM_STATE_BIND_OK,
    XPM_STATE_BIND_FAIL,
    XPM_STATE_CONNECTED
};

/**
 * represents a pending DMA request which is sent to the DMA service
 * but has not yet been completed.
 */
struct xdma_req
{
    xeon_phi_dma_id_t id;           ///< DMA transfer ID
    uint8_t xphi_id;                ///< Xeon Phi ID this transfer is issued
    struct xeon_phi_dma_cont cont;  ///< Continuation to be called when finished
    struct xdma_req *next;          ///< pointer to the next request
    struct xdma_req *prev;          ///< pointer to the previous request
};

/// service irefs
static iref_t xdma_svc_iref[XEON_PHI_NUM_MAX];

/// service bindings
struct xeon_phi_dma_binding *xdma_binding[XEON_PHI_NUM_MAX];

/// wait reply flags for RPC like functionality where needed
static uint8_t xdma_wait_reply[XEON_PHI_NUM_MAX];

/// pointer to all DMA requests
static struct xdma_req *requests = NULL;

/// the current requests being executed
static struct xdma_req *requests_pending = NULL;

/// unused / free requests
static struct xdma_req *requests_free = NULL;

/// initialization flag
static uint8_t xdma_intialized = 0x0;

/// the current service state
static enum xpm_state conn_state = XPM_STATE_NSLOOKUP;

/*
 * ----------------------------------------------------------------------------
 * Request Queue Management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief retrieves the pending request with the given ID
 *
 * \param id DMA transfer ID of this request
 *
 * \returns pointer to the pending request if request found
 *          NULL if there is no such request
 */
static struct xdma_req *xdma_get_pending_request(xeon_phi_dma_id_t id)
{
    struct xdma_req *req = requests_pending;

    while (req) {
        if (req->id == id) {
            if (req->prev != NULL) {
                req->prev->next = req->next;
            } else {
                requests_pending = req->next;
            }
            if (req->next != NULL) {
                req->next->prev = req->prev;
            }
            req->next = NULL;
            req->prev = NULL;

            return req;
        }
    }
    return NULL;
}

/**
 * \brief inserts a request into the pending request list
 *
 * \param req the request to be inserted
 */
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
}

/**
 * \brief tries to get a free unused request form the list of free requests
 *
 * \returns pointer to a request ready to be used
 *          NULL if there is none left
 */
static struct xdma_req *xdma_get_free_request(void)
{
    struct xdma_req *req = requests_free;

    if (req != NULL) {
        requests_free = req->next;
        req->next = NULL;
    }

    return req;
}

/**
 * \brief inserts a request into the free queue for later use
 *
 * \param req Request to be enqueued
 */
static void xdma_insert_free_request(struct xdma_req *req)
{
    DEBUG_XDMA("request free [%016lx]\n", (uintptr_t)req);
    memset(req, 0, sizeof(*req));
    req->next = requests_free;
    requests_free = req;
}

/*
 * ----------------------------------------------------------------------------
 * Request RPC management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief starts a new RPC to the Xeon Phi DMA service
 *
 * \param xphi the ID of the Xeon Phi
 *
 * \returns 1 if the RPC transaction could be started
 *          0 if there was already a transaction in process
 */
static inline uint8_t xdma_rpc_start(uint8_t xphi)
{
    if (!xdma_wait_reply[xphi]) {
        xdma_wait_reply[xphi] = 0x1;
        return 1;
    }
    return 0;
}

/**
 * \brief waits until the started transaction is finished
 *
 * \param xphi  the ID of the Xeon Phi
 */
static inline void xdma_rpc_wait_done(uint8_t xphi)
{
    while (xdma_wait_reply[xphi]) {
        messages_wait_and_handle_next();
    }
}

/**
 * \brief signals the completion of the RPC
 *
 * \param xphi  the ID of the Xeon Phi
 */
static inline void xdma_rpc_done(uint8_t xphi)
{
    xdma_wait_reply[xphi] = 0x0;
}

/*
 * ---------------------------------------------------------------------------
 * DMA memory region management: registration
 * ---------------------------------------------------------------------------
 */

/**
 * message state for memory registration
 */
static struct xdma_reg_msg_st
{
    struct capref mem;
    errval_t err;
    uint8_t xphi_id;
} xdma_reg_msg_st[XEON_PHI_NUM_MAX];

/**
 * \brief Xeon Phi DMA receive callback for register response messages
 *
 * \param _binding  Flounder binding
 * \param msgerr    outcome of the registration attempt
 */
static void xdma_register_response_rx(struct xeon_phi_dma_binding *_binding,
                                      xeon_phi_dma_errval_t msgerr)
{
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;
    assert(xdma_reg_msg_st[xphi_id].xphi_id == xphi_id);

    DEBUG_XDMA("received register response from Xeon Phi %u: %s\n",
               xphi_id, err_getstring(msgerr));

    xdma_reg_msg_st[xphi_id].err = msgerr;

    xdma_rpc_done(xphi_id);
}

/**
 * \brief Sends a register call message to the Xeon Phi DMA service
 *
 * \param a pointer to struct xdma_reg_msg_st
 */
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
            return;
        }
    }
}

/*
 * ---------------------------------------------------------------------------
 * DMA memory region management: registration
 * ---------------------------------------------------------------------------
 */

/**
 * message state for memory de-registration
 */
static struct xdma_dereg_msg_st
{
    struct capref mem;
    errval_t err;
    uint8_t xphi_id;
} xdma_dereg_msg_st[XEON_PHI_NUM_MAX];

/**
 * \brief Xeon Phi DMA receive callback for deregister response messages
 *
 * \param _binding  Flounder binding
 * \param msgerr    outcome of the registration attempt
 */
static void xdma_deregister_response_rx(struct xeon_phi_dma_binding *_binding,
                                        xeon_phi_dma_errval_t msgerr)
{
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;

    DEBUG_XDMA("received deregister response from Xeon Phi %u: %s\n",
               xphi_id, err_getstring(msgerr));

    xdma_rpc_done(xphi_id);
}

/**
 * \brief Sends a deregister call message to the Xeon Phi DMA service
 *
 * \param a pointer to struct xdma_reg_msg_st
 */
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

/*
 * ---------------------------------------------------------------------------
 * DMA start new transfer
 * ---------------------------------------------------------------------------
 */

/**
 * message state for memory transfer requests
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

/**
 * \brief Xeon Phi DMA receive callback for exec response messages
 *
 * \param _binding  Flounder binding
 * \param msgerr    outcome of the transfer request
 * \param id        returned DMA id of this transfer
 */
static void xdma_exec_response_rx(struct xeon_phi_dma_binding *_binding,
                                  xeon_phi_dma_errval_t err,
                                  xeon_phi_dma_id_t id)
{
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;

    assert(xdma_reg_msg_st[xphi_id].xphi_id == xphi_id);

    DEBUG_XDMA("received exec response from Xeon Phi %u: %s\n",
               xphi_id, err_getstring(err));

    xdma_reg_start_st[xphi_id].err = err;
    xdma_reg_start_st[xphi_id].id = id;

    xdma_rpc_done(xphi_id);
}

/**
 * \brief Sends a exec call message to the Xeon Phi DMA service
 *
 * \param a pointer to struct xdma_reg_start_st
 */
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

/*
 * ---------------------------------------------------------------------------
 * DMA dma execute transfer
 * ---------------------------------------------------------------------------
 */
static struct xdma_reg_exec_st
{
    xeon_phi_dma_id_t *id;
    uint8_t xphi_id;
    errval_t err;
} xdma_reg_exec_st[XEON_PHI_NUM_MAX];

/// flag for waiting for execution
static uint8_t execute_wait_flag[XEON_PHI_NUM_MAX];

/**
 * \brief callback hander called when the DMA transfer is completed
 *
 * \param id    id of the transfer
 * \param err   outcome of the transfer
 * \param st    state
 */
static void xdma_execute_handler(xeon_phi_dma_id_t id,
                                 errval_t err,
                                 void *st)
{
    struct xdma_reg_exec_st *msg_st = st;

    assert(id == *msg_st->id);

    msg_st->err = err;
    execute_wait_flag[msg_st->xphi_id] = 0x0;
}

/*
 * ---------------------------------------------------------------------------
 * DMA stop transfer
 * ---------------------------------------------------------------------------
 */

/**
 * message state for request stop messages
 */
static struct xdma_reg_stop_st
{
    struct xdma_req *req;
    xeon_phi_dma_id_t id;
    errval_t err;
} xdma_reg_stop_st[XEON_PHI_NUM_MAX];

/**
 * \brief Xeon Phi DMA receive callback for stop response messages
 *
 * \param _binding  Flounder binding
 * \param err    outcome of the transfer request
 */
static void xdma_stop_response_rx(struct xeon_phi_dma_binding *_binding,
                                  xeon_phi_dma_errval_t err)
{

#ifdef XEON_PHI_DEBUG_DMA
    uint8_t xphi_id = (uint8_t) (uintptr_t) _binding->st;
#endif
    DEBUG_XDMA("received stop response from Xeon Phi %u: %s\n",
               xphi_id, err_getstring(err));

}

/**
 * \brief Sends a stop call message to the Xeon Phi DMA service
 *
 * \param a pointer to struct xdma_reg_stop_st
 */
static void xdma_stop_call_tx(void *a)
{
    errval_t err;

    struct xdma_reg_stop_st *msg_st = a;

    struct xeon_phi_dma_binding *b = xdma_binding[msg_st->req->xphi_id];

    struct event_closure txcont = MKCONT(NULL, a);

    err = xeon_phi_dma_stop_call__tx(b, txcont, msg_st->req->id);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xdma_exec_call_tx, a);
            err = b->register_send(b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                msg_st->err = err;
                xdma_wait_reply[msg_st->req->xphi_id] = 0x0;
            }
        }
    }
}

/*
 * ---------------------------------------------------------------------------
 * DMA transfer done
 * ---------------------------------------------------------------------------
 */

/**
 * \brief Xeon PHI DMA callback function for done messages
 *
 * \param _binding  Flounder binding
 * \param id        DMA ID of the transfer that finished
 * \param err       outcome of the transfer
 */
static void xdma_done_rx(struct xeon_phi_dma_binding *_binding,
                         xeon_phi_dma_id_t id,
                         xeon_phi_dma_errval_t err)
{
    struct xdma_req *req = xdma_get_pending_request(id);
    DEBUG_XDMA("received done message [%u, %lx] @ %p\n", xphi_id, id, req);

    assert(req);

    if (req->cont.cb) {
        req->cont.cb(id, err, req->cont.arg);
    }

    xdma_insert_free_request(req);
}

/*
 * forward declarations for the recv messages
 */

/**
 * Xeon PHI DMA binding receive vtbl
 */
struct xeon_phi_dma_rx_vtbl xdma_rx_vtbl = {
    .register_response = xdma_register_response_rx,
    .deregister_response = xdma_deregister_response_rx,
    .exec_response = xdma_exec_response_rx,
    .stop_response = xdma_stop_response_rx,
    .done = xdma_done_rx
};

/**
 * \brief Xeon Phi DMA service bind callback
 *
 * \param st  our state containing the xeon phi id
 * \param err outcome of the binding attempt
 * \param b   xeon_phi_dma_binding of the new connection
 */
static void xdma_bind_cb(void *st,
                         errval_t err,
                         struct xeon_phi_dma_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_XDMA("xdma_connect: binding failed: %s\n", err_getstring(err));
        conn_state = XPM_STATE_BIND_FAIL;
        return;
    }

    DEBUG_XDMA("xdma_connect: binding to service node %p done.\n", st);

    /*
     * XXX: if we are running on the Xeon Phi, we have just one DMA service
     *      running. Thus we set all the bindings to this one.
     */
#ifdef __k1om__
    for (uint32_t i = 0; i < XEON_PHI_NUM_MAX; ++i) {
        xdma_binding[i] = b;
    }
#else
    uint8_t xphi_id = (uint8_t) (uintptr_t) st;
    xdma_binding[xphi_id] = b;
#endif

    b->st = st;
    b->rx_vtbl = xdma_rx_vtbl;

    conn_state = XPM_STATE_BIND_OK;
}

/**
 * \brief checks if there is already a connection to the Xeon Phi DMA service
 *        and establishes a new one if there was none.
 *
 * \param xeon_phi_id the ID of the card we want to talk to
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
static errval_t xdma_connect(uint8_t xeon_phi_id)
{
    errval_t err;

#ifdef __k1om__
    uint8_t xphi_id = 0;
#else
    uint8_t xphi_id = xeon_phi_id;
#endif

    if (xdma_binding[xphi_id] != NULL) {
        return SYS_ERR_OK;
    }

    if (!xdma_intialized) {
        err = xeon_phi_dma_client_init();
        if (err_is_fail(err)) {
            return err;
        }
    }

    char buf[50];
    snprintf(buf, 50, "%s.%u", XEON_PHI_DMA_SERVICE_NAME, xphi_id);

    DEBUG_XDMA("xdma_connect: NS lookup {%s}\n", buf);
    err = nameservice_blocking_lookup(buf, &xdma_svc_iref[xphi_id]);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG_XDMA("xdma_connect: binding to iref [%u]\n", xdma_svc_iref[xphi_id]);

    conn_state = XPM_STATE_BINDING;
    err = xeon_phi_dma_bind(xdma_svc_iref[xphi_id],
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

    conn_state = XPM_STATE_CONNECTED;

    return SYS_ERR_OK;
}

/*
 * ---------------------------------------------------------------------------
 * Public interface of the Xeon Phi DMA client
 * ---------------------------------------------------------------------------
 */

/**
 * \brief Initializes the DMA client library. This is to be called before the
 *        first DMA request is being executed
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
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
    requests_pending = NULL;

    xdma_intialized = 1;

    return SYS_ERR_OK;
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
    errval_t err;

#ifdef __k1om__
    xphi_id = 0;
#endif

    err = xdma_connect(xphi_id);
    if (err_is_fail(err)) {
        return err;
    }

    if (!xdma_rpc_start(xphi_id)) {
        return XEON_PHI_ERR_DMA_RPC_IN_PROGRESS;
    }

    struct xdma_reg_msg_st *st = xdma_reg_msg_st + xphi_id;
    st->xphi_id = xphi_id;
    st->err = SYS_ERR_OK;
    st->mem = mem;

    xdma_register_call_tx(st);

    xdma_rpc_wait_done(xphi_id);

    return st->err;
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
    xphi_id = 0;
#endif

    err = xdma_connect(xphi_id);
    if (err_is_fail(err)) {
        return err;
    }

    if (!xdma_rpc_start(xphi_id)) {
        return XEON_PHI_ERR_DMA_RPC_IN_PROGRESS;
    }

    struct xdma_dereg_msg_st *st = xdma_dereg_msg_st + xphi_id;
    st->xphi_id = xphi_id;
    st->err = SYS_ERR_OK;
    st->mem = mem;

    xdma_deregister_call_tx(st);

    xdma_rpc_wait_done(xphi_id);

    return st->err;
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

#ifdef __k1om__
    xphi_id = 0;
#endif

    err = xdma_connect(xphi_id);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG_XDMA("New DMA request: [0x%016lx]->[0x%016lx] of 0x%lx bytes \n",
               info->src, info->dest, (uint64_t )info->size);

    /*
     * we only allow multiple of 64 bytes for transfers.
     * The Xeon Phi DMA controller supports only 64 byte granularity in
     * alignment and size.
     */
    if ((info->dest & (XEON_PHI_DMA_ALIGNMENT - 1)) || (info->src
                    & (XEON_PHI_DMA_ALIGNMENT - 1))
        || (info->size & (XEON_PHI_DMA_ALIGNMENT - 1))) {
        return XEON_PHI_ERR_DMA_MEM_ALIGN;
    }

    /*
     * check for overlapping of the memory regions
     */
    if (info->dest < info->src) {
        if ((info->dest + info->size) > info->src) {
            return XEON_PHI_ERR_DMA_MEM_OVERLAP;
        }
    } else if ((info->dest > info->src)) {
        if ((info->src + info->size) > info->dest) {
            return XEON_PHI_ERR_DMA_MEM_OVERLAP;
        }
    } else {
        /* they are equal so they will overlap */
        return XEON_PHI_ERR_DMA_MEM_OVERLAP;
    }

    if (!xdma_rpc_start(xphi_id)) {
        return XEON_PHI_ERR_DMA_RPC_IN_PROGRESS;
    }

    struct xdma_req *req = xdma_get_free_request();
    if (req == NULL) {
        return XEON_PHI_ERR_DMA_BUSY;
    }

    req->cont = cont;
    req->xphi_id = xphi_id;

    struct xdma_reg_start_st *msg_st = xdma_reg_start_st + xphi_id;
    msg_st->bytes = info->size;
    msg_st->dst = info->dest;
    msg_st->src = info->src;
    msg_st->xphi_id = xphi_id;

    xdma_exec_call_tx(msg_st);

    xdma_rpc_wait_done(xphi_id);

    if (err_is_fail(msg_st->err)) {
        xdma_insert_free_request(req);
        return msg_st->err;
    }

    req->id = msg_st->id;

    xdma_insert_pending_request(req);

    if (id) {
        *id = msg_st->id;
    }

    return msg_st->err;
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

    struct xdma_reg_exec_st *st = xdma_reg_exec_st + xphi_id;
    st->xphi_id = xphi_id;
    st->err = SYS_ERR_OK;
    st->id = &id;

    struct xeon_phi_dma_cont cont = {
        .arg = st,
        .cb = xdma_execute_handler
    };

    execute_wait_flag[xphi_id] = 0x1;

    err = xeon_phi_dma_client_start(xphi_id, info, cont, &id);

    while (execute_wait_flag[xphi_id]) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
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

    if (!xdma_rpc_start(req->xphi_id)) {
        xdma_insert_pending_request(req);
        return XEON_PHI_ERR_DMA_RPC_IN_PROGRESS;
    }

    struct xdma_reg_stop_st *msg_st = xdma_reg_stop_st + req->xphi_id;
    msg_st->req = req;

    xdma_stop_call_tx(msg_st);

    xdma_rpc_wait_done(req->xphi_id);

    xdma_insert_free_request(req);

    return msg_st->err;
}
