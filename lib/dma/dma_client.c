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
#include <flounder/flounder_txqueue.h>

#include <dma/dma_manager_client.h>
#include <dma_internal.h>
#include <dma_request_internal.h>
#include <dma_client_internal.h>
#include <debug.h>

#include <if/dma_defs.h>

static uint8_t dma_client_initialized = 0x0;

/**
 * state enumeration for the Flounder connection to the service
 */
enum dma_svc_state
{
    DMA_CLIENT_STATE_NS_LOOKUP,
    DMA_CLIENT_STATE_BINDING,
    DMA_CLIENT_STATE_BIND_OK,
    DMA_CLIENT_STATE_BIND_FAIL,
    DMA_CLIENT_STATE_CONNECTED
};

struct dma_client
{
    struct dma_binding *svc_b;       ///< the DMA service binding
    struct tx_queue txq;             ///< Flounder TX Queue
    enum dma_svc_state svc_st;       ///<
    struct dma_mgr_driver_info info;  ///< if
    errval_t err;                    ///<
    uint8_t wait_reply;              ///< RPC is in progress
    struct dma_request *rpc;         ///< currently pending requests
    struct dma_request *requests;    ///< currently pending requests
    dma_req_id_t last_done_id;       ///<
    struct dma_client *next;         ///<
    struct dma_client *prev;         ///<
};

/*
 * ---------------------------------------------------------------------------
 * Request Cache /  Helper Functions
 * ---------------------------------------------------------------------------
 */
static struct dma_request *dma_request_cache = NULL;

static struct dma_request *request_alloc(void)
{
    if (dma_request_cache) {
        struct dma_request *st = dma_request_cache;
        dma_request_cache = dma_request_cache->next;
        return st;
    }
    return calloc(1, sizeof(struct dma_request));
}

static inline void request_free(struct dma_request *st)
{
    st->next = dma_request_cache;
    dma_request_cache = st;
}

/**
 * \brief retrieves the pending request with the given ID
 *
 * \param id DMA transfer ID of this request
 *
 * \returns pointer to the pending request if request found
 *          NULL if there is no such request
 */
static struct dma_request *request_get_pending(struct dma_client *client,
                                               dma_req_id_t id)
{
    struct dma_request *req = client->requests;

    while (req) {
        if (req->id == id) {
            if (req->prev != NULL) {
                req->prev->next = req->next;
            } else {
                client->requests = req->next;
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
static void request_insert_pending(struct dma_client *client,
                                   struct dma_request *req)
{
    debug_printf("request_insert_pending: %lx\n", req->id);
    if (client->requests == NULL) {
        client->requests = req;
        req->next = NULL;
    } else {
        client->requests->prev = req;
        req->next = client->requests;
        client->requests = req;
    }
    req->prev = NULL;
}

/*
 * ---------------------------------------------------------------------------
 * Transmit State Cache
 * ---------------------------------------------------------------------------
 */
struct svc_msg_st
{
    struct txq_msg_st common;

    /* union of arguments */
    union
    {
        struct capref cap;
        struct dma_request *req;
    } args;
};

/*
 * ---------------------------------------------------------------------------
 * RPC management
 * ---------------------------------------------------------------------------
 */

/**
 * \brief starts a new RPC to the Xeon Phi DMA service
 *
 * \param xphi the ID of the Xeon Phi
 *
 * \returns 1 if the RPC transaction could be started
 *          0 if there was already a transaction in process
 */
static inline uint8_t rpc_start(struct dma_client *client)
{
    if (!client->wait_reply) {
        client->wait_reply = 0x1;
        return 1;
    }
    return 0;
}

/**
 * \brief waits until the started transaction is finished
 *
 * \param xphi  the ID of the Xeon Phi
 */
static inline void rpc_wait_done(struct dma_client *client)
{
    while (client->wait_reply) {
        messages_wait_and_handle_next();
    }
}

/**
 * \brief signals the completion of the RPC
 *
 * \param xphi  the ID of the Xeon Phi
 */
static inline void rpc_done(struct dma_client *client)
{
    client->wait_reply = 0x0;
}

/*
 * ---------------------------------------------------------------------------
 * Connection List Management
 * ---------------------------------------------------------------------------
 */

/// list of known drivers running
static struct dma_client *dma_svc_conn = NULL;

static void dma_svc_conn_insert(struct dma_client *client)
{
    if (dma_svc_conn == NULL) {
        client->next = NULL;
        client->prev = NULL;
        dma_svc_conn = client;
        return;
    }

    struct dma_client *current = dma_svc_conn;
    while (current) {
        if (current->info.mem_low > client->info.mem_high) {
            if (current->prev) {
                client->prev = current->prev;
                current->prev->next = client;
            } else {
                client->prev = NULL;
                dma_svc_conn = client;
            }
            client->next = current;
            current->prev = client;
            break;
        }
        current = current->next;
    }
}

static struct dma_client *dma_svc_conn_get_by_addr(lpaddr_t base,
                                                   size_t size)
{
    errval_t err;

    DMACLIENT_DEBUG("Looking up connection name for address: [%016lx]\n", base);

    struct dma_client *current = dma_svc_conn;
    struct dma_client *best = NULL;
    while (current) {
        if (current->info.mem_low <= base) {
            if (current->info.mem_high >= (base + size)) {
                /* we have a match */
                if (best == NULL) {
                    best = current;
                    break;
                }
            }
        }
        current = current->next;
    }
    if (best == NULL) {
        best = calloc(1, sizeof(*best));
        assert(best);
        err = dma_manager_lookup_driver(base, size, &best->info);
        if (err_is_fail(err)) {
            free(best);
            return NULL;
        }
        dma_svc_conn_insert(best);
    }
    return best;
}

/*
 * ---------------------------------------------------------------------------
 * Message callbacks
 * ---------------------------------------------------------------------------
 */

static void done_msg_rx(struct dma_binding *_binding,
                        dma_id_t id,
                        dma_errval_t msgerr)
{
    DMACLIENT_DEBUG("done_msg_rx: %lx, %s\n", id, err_getstring(msgerr));
    struct dma_client *client = _binding->st;
    struct dma_request *req = request_get_pending(client, id);
    assert(req);

    if (req->setup.done_cb) {
        req->setup.done_cb(msgerr, id, req->setup.cb_arg);
    }

    request_free(req);
}

static void register_response_rx(struct dma_binding *_binding,
                                 dma_errval_t msgerr)
{
    DMACLIENT_DEBUG("register_response_rx: %s\n", err_getstring(msgerr));
    struct dma_client *client = _binding->st;
    client->err = msgerr;
    rpc_done(client);
}

static void deregister_response_rx(struct dma_binding *_binding,
                                   dma_errval_t msgerr)
{
    DMACLIENT_DEBUG("deregister_response_rx: %s\n", err_getstring(msgerr));
    struct dma_client *client = _binding->st;
    client->err = msgerr;
    rpc_done(client);
}

static void memcpy_response_rx(struct dma_binding *_binding,
                               dma_errval_t err,
                               dma_id_t id)
{
    DMACLIENT_DEBUG("memcpy_response_rx: %lx %s\n", id, err_getstring(err));
    struct dma_client *client = _binding->st;
    client->err = err;
    assert(client->rpc);
    if (err_is_fail(err)) {
        client->rpc->state = DMA_REQ_ST_ERR;
    } else {
        client->rpc->state = DMA_REQ_ST_SUBMITTED;
    }
    client->rpc->id = id;
    rpc_done(client);
}

struct dma_rx_vtbl dma_rxvtbl = {
    .register_response = register_response_rx,
    .deregister_response = deregister_response_rx,
    .memcpy_response = memcpy_response_rx,
    .done = done_msg_rx
};

/*
 * ---------------------------------------------------------------------------
 * Service Binding
 * ---------------------------------------------------------------------------
 */

static void bind_cb(void *st,
                    errval_t err,
                    struct dma_binding *b)
{
    assert(st);
    struct dma_client *client = st;

    client->err = err;

    if (err_is_fail(err)) {
        DMACLIENT_DEBUG("svc bind: connection to iref [%"PRIxIREF"] failed.\n",
                        client->info.iref);
        client->svc_st = DMA_CLIENT_STATE_BIND_FAIL;
        return;
    }

    txq_init(&client->txq, b, b->waitset, (txq_register_fn_t) b->register_send,
             sizeof(struct svc_msg_st));

    b->rx_vtbl = dma_rxvtbl;
    b->st = client;
    client->svc_b = b;

    DMACLIENT_DEBUG("svc bind: connection to iref [%"PRIxIREF"] established.\n",
                    client->info.iref);

    client->svc_st = DMA_CLIENT_STATE_BIND_OK;
}

static errval_t dma_service_bind(struct dma_client *client)
{
    errval_t err;

    if (client->svc_b != NULL) {
        return SYS_ERR_OK;
    }

    struct waitset *ws = get_default_waitset();

    client->svc_st = DMA_CLIENT_STATE_BINDING;

    DMACLIENT_DEBUG("svc bind: binding to iref [%"PRIxIREF"]\n", client->info.iref);

    err = dma_bind(client->info.iref, bind_cb, client, ws, IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (client->svc_st == DMA_CLIENT_STATE_BINDING) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            return err;
        }
    }

    if (client->svc_st == DMA_CLIENT_STATE_BIND_FAIL) {
        return client->err;
    }

    client->svc_st = DMA_CLIENT_STATE_CONNECTED;

    return SYS_ERR_OK;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 *
 */
errval_t dma_client_init(void)
{
    dma_client_initialized = 0x1;

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Memory Registration
 * ----------------------------------------------------------------------------
 */

static void register_call_tx(struct txq_msg_st *msg_st)
{
    errval_t err;

    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;

    debug_printf("register_call_tx\n");

    err = dma_register_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                st->args.cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Unexpectedly sending failed\n");
    }
}

/**
 * \brief registers a memory region with a specified client connection
 *
 * \param conn  DMA client connection
 * \param frame the memory frame to register
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_register_memory(struct dma_client *conn,
                                    struct capref frame)
{
    if (!dma_client_initialized) {
        dma_client_init();
    }

    if (!rpc_start(conn)) {
        return DMA_ERR_SVC_BUSY;
    }

    conn->err = SYS_ERR_OK;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&conn->txq);
    if (msg_st == NULL) {
        rpc_done(conn);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = register_call_tx;

    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;
    st->args.cap = frame;

    txq_send(msg_st);

    rpc_wait_done(conn);

    return conn->err;
}

static void deregister_call_tx(struct txq_msg_st *msg_st)
{
    errval_t err;

    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;

    debug_printf("register_call_tx\n");

    err = dma_deregister_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                  st->args.cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Unexpectedly sending failed\n");
    }
}

/**
 * \brief deregisters a previously registered memory region from the connection
 *
 * \param conn  DMA client connection
 * \param frame the memory frame to deregister
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_deregister_memory(struct dma_client *conn,
                                      struct capref frame)
{
    if (!dma_client_initialized) {
        dma_client_init();
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&conn->txq);
    if (msg_st == NULL) {
        rpc_done(conn);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = deregister_call_tx;

    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;
    st->args.cap = frame;

    txq_send(msg_st);

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Request Execution
 * ----------------------------------------------------------------------------
 */

static void memcpy_call_tx(struct txq_msg_st *msg_st)
{
    errval_t err;

    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;
    struct dma_req_setup *setup = &st->args.req->setup;

    err = dma_memcpy_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                              setup->args.memcpy.src, setup->args.memcpy.dst,
                              setup->args.memcpy.bytes);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Unexpectedly sending failed\n");
    }
}

/**
 * \brief issues a memcopy request to the service using the connection
 *
 * \param setup DMA request setup information
 *
 * \returns SYS_ERR_OK on success
 *
 * NOTE: the connection information has to be present in the setup struct
 */
errval_t dma_client_memcpy(struct dma_req_setup *setup)
{
    errval_t err;

    if (!dma_client_initialized) {
        dma_client_init();
    }

    if (setup->client == NULL) {
        return DMA_ERR_SVC_NO_CONNECTION;
    }

    struct dma_client *client = setup->client;

    assert(client->svc_b);

    DMACLIENT_DEBUG("memcpy request [%016lx] -> [%016lx] of %lu bytes\n",
                    setup->args.memcpy.src, setup->args.memcpy.dst,
                    setup->args.memcpy.bytes);

    if (!rpc_start(setup->client)) {
        return DMA_ERR_SVC_BUSY;
    }

    struct dma_request *req = request_alloc();
    if (req == NULL) {
        rpc_done(setup->client);
        return LIB_ERR_MALLOC_FAIL;
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&client->txq);
    if (msg_st == NULL) {
        request_free(req);
        rpc_done(client);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = memcpy_call_tx;

    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;

    req->setup = *setup;
    req->state = DMA_REQ_ST_PREPARED;
    client->rpc = req;
    st->args.req = req;

    txq_send(msg_st);

    rpc_wait_done(setup->client);

    err = req->err;

    if (err_is_fail(err)) {
        request_free(req);
        return err;
    }

    if (client->last_done_id == req->id) {
        client->last_done_id = 0x0;
        if (setup->done_cb) {
            setup->done_cb(err, req->id, setup->cb_arg);
        }
        request_free(req);
        return err;
    }

    request_insert_pending(client, req);

    return err;
}

/*
 * ----------------------------------------------------------------------------
 * Connection Management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets a connection to the DMA service based on the src and dst addresses
 *        of the DMA request
 *
 * \param src   source address of the operation
 * \param dst   destination address of the operation
 * \param size  transfer size in bytes
 *
 * \returns dma_client connection
 *          NULL if the addresses are not supported
 */
struct dma_client *dma_client_get_connection_by_addr(lpaddr_t src,
                                                     lpaddr_t dst,
                                                     size_t size)
{
    DMACLIENT_DEBUG("getting connection for [%016lx] -> [%016lx]\n", src, dst);

    struct dma_client *src_client = dma_svc_conn_get_by_addr(src, size);
    if (src_client == NULL) {
        DMACLIENT_DEBUG("could not connect for source\n");
        return NULL;
    }

    struct dma_client *dst_client = dma_svc_conn_get_by_addr(dst, size);
    if (dst_client == NULL) {
        DMACLIENT_DEBUG("could not connect for destination\n");
        return NULL;
    }

    struct dma_client *ret = src_client;

    if (dst_client->info.type == DMA_DEV_TYPE_XEON_PHI) {
        ret = dst_client;
    } else if (src_client->info.type == DMA_DEV_TYPE_XEON_PHI) {
        ret = src_client;
    }

    if (ret->svc_b == NULL) {
        /* we need to initiate the connection first. */
        dma_service_bind(ret);
    }

    /* TODO: check NUMA */
    return ret;
}

/**
 * \brief waits until a device driver for the supplied device type is ready
 *
 * \param device    DMA device type
 *
 * \returns SYS_ERR_OK when the driver is ready
 *          errval if there was something wrong
 */
errval_t dma_client_wait_for_driver(dma_dev_type_t device,
                                    uint8_t numa_node)
{
    errval_t err;

    char buf[30];
    snprintf(buf, 30, "%s_%u_%u", DMA_MGR_REGISTERED_DRIVER, (uint8_t) device,
             numa_node);

    DMAMGR_DEBUG("waiting for driver: {%s}\n", buf);

    iref_t dummy_iref;
    err = nameservice_blocking_lookup(buf, &dummy_iref);
    if (err_is_fail(err)) {

    }

    return err;
}

/*
 * ----------------------------------------------------------------------------
 * Connection Information
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the DMA device type of a connection
 *
 * \param conn  DMA client connection
 *
 * \returns DMA device type
 */
dma_dev_type_t dma_client_get_device_type(struct dma_client *conn)
{
    return conn->info.type;
}

/**
 * \brief returns the supported range of a connection
 *
 * \param conn      DMA client connection
 * \param mem_min   minimum physical address supported
 * \param mem_max   maximum physical address supported
 *
 */
void dma_client_get_supported_range(struct dma_client *conn,
                                    lpaddr_t *mem_min,
                                    lpaddr_t *mem_max)
{
    if (mem_min) {
        *mem_min = conn->info.mem_low;
    }
    if (mem_max) {
        *mem_max = conn->info.mem_high;
    }
}

