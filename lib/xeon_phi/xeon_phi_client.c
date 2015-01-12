/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <octopus/octopus.h>
#include <flounder/flounder_txqueue.h>

#include <if/xeon_phi_defs.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>
#include <xeon_phi/xeon_phi_domain.h>

#include "xeon_phi_client_internal.h"

#ifdef XEON_PHI_DEBUG_MSG
#define DEBUG_XPHI(x...) debug_printf("[xphi-client] " x);
#else
#define DEBUG_XPHI(x...)
#endif
#define PRINTF_XPHI(x...) debug_printf("[xphi-client] " x);

/**
 * enumeration of all possible states of the service exporting process
 */
enum xpm_svc_state
{
    XPM_SVC_STATE_INVALID,
    XPM_SVC_STATE_BINDING,
    XPM_SVC_STATE_BIND_OK,
    XPM_SVC_STATE_BIND_FAIL,
    XPM_SVC_STATE_REGISTERING,
    XPM_SVC_STATE_REGISTER_OK,
    XPM_SVC_STATE_REGISTER_FAIL,
    XPM_SVC_STATE_CONNECTED
};

struct xeon_phi_client
{
    struct xeon_phi_binding *binding;
    struct tx_queue txq;
    volatile uint8_t rpc_wait_reply;
    uint64_t rpc_data;
    errval_t rpc_err;
    errval_t err;
    xphi_id_t xid;
    enum xpm_svc_state state;
};

struct xeon_phi_client *xphi_svc[XEON_PHI_NUM_MAX] = {
    0
};

uint8_t xphi_present[XEON_PHI_NUM_MAX] = {
    0
};

static struct xeon_phi_callbacks client_cb;

struct xphi_msg_st
{
    struct txq_msg_st common;
    /* union of arguments */
    union
    {
        struct
        {
            const char *name;
            xphi_dom_id_t domid;
            coreid_t core;
        } domain;
        struct
        {
            xphi_id_t xid;
            coreid_t core;
            uint8_t flags;
            char *cmdline;
            size_t cmdlen;
            struct capref cap;
        } spawn;
        struct
        {
            xphi_id_t xid;
            xphi_dom_id_t domid;
        } kill;
        struct
        {
            xphi_id_t xid;
            struct capref msgframe;
            xphi_chan_type_t type;
            xphi_dom_id_t domid;
            uint64_t usrdata;
        } open;
    } args;
};

/*
 *
 */
static inline errval_t check_online(uint8_t xid)
{
    errval_t err;
    if (xphi_present[xid]) {
        return SYS_ERR_OK;
    }

#ifdef __k1om__
    if (xid == disp_xeon_phi_id()) {
        xphi_present[xid] = 1;
        return SYS_ERR_OK;
    }
#endif

    char buf[20];
    snprintf(buf, 20, "xeon_phi.%u.ready", xid);
    xphi_dom_id_t result;
    err = xeon_phi_domain_blocking_lookup(buf, &result);
    if (err_is_fail(err) || result != 0xcafebabe) {
        USER_PANIC_ERR(err, "result: %lx, %x", result, (uint32_t )result);
    }

    xphi_present[xid] = 1;

    return err;
}

/*
 * ---------------------------------------------------------------------------
 * RPC management
 * ---------------------------------------------------------------------------
 */

/**
 * \brief starts a new RPC to the DMA service
 *
 * \param chan  the DMA channel to start the RPC on
 *
 * \returns 1 if the RPC transaction could be started
 *          0 if there was already a transaction in process
 */
static inline uint8_t rpc_start(struct xeon_phi_client *cl)
{
    if (!cl->rpc_wait_reply) {
        cl->rpc_wait_reply = 0x1;
        return 1;
    }
    return 0;
}

/**
 * \brief waits until the started transaction is finished
 *
 * \param chan  the DMA channel
 */
static inline void rpc_wait_done(struct xeon_phi_client *cl)
{
    while (cl->rpc_wait_reply == 0x1) {
        messages_wait_and_handle_next();
    }
}

/**
 * \brief signals the completion of the RPC
 *
 * \param chan  the DMA channel
 */
static inline void rpc_done(struct xeon_phi_client *cl)
{
    cl->rpc_wait_reply = 0x2;
}

/**
 * \brief signals the completion of the RPC
 *
 * \param chan  the DMA channel
 */
static inline void rpc_clear(struct xeon_phi_client *cl)
{
    cl->rpc_wait_reply = 0x0;
}

/*
 * ----------------------------------------------------------------------------
 * Send handlers
 * ----------------------------------------------------------------------------
 */

static errval_t domain_init_call_tx(struct txq_msg_st *msg_st)
{
    struct xphi_msg_st *st = (struct xphi_msg_st *) msg_st;
    size_t length = strlen(st->args.domain.name) + 1;
    return xeon_phi_domain_init_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                         (domainid_t) st->args.domain.domid,
                                         st->args.domain.core, st->args.domain.name,
                                         length);
}
#ifdef __k1om__
static errval_t domain_register_call_tx(struct txq_msg_st *msg_st)
{
    struct xphi_msg_st *st = (struct xphi_msg_st *) msg_st;
    size_t length = strlen(st->args.domain.name) + 1;
    return xeon_phi_domain_register_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                             st->args.domain.name, length,
                                             st->args.domain.domid);
}
static errval_t domain_lookup_call_tx(struct txq_msg_st *msg_st)
{
    struct xphi_msg_st *st = (struct xphi_msg_st *) msg_st;
    size_t length = strlen(st->args.domain.name) + 1;
    return xeon_phi_domain_lookup_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                           st->args.domain.name, length);
}

static errval_t domain_wait_call_tx(struct txq_msg_st *msg_st)
{
    struct xphi_msg_st *st = (struct xphi_msg_st *) msg_st;
    size_t length = strlen(st->args.domain.name) + 1;
    return xeon_phi_domain_wait_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                         st->args.domain.name, length);
}
#endif

static errval_t spawn_call_tx(struct txq_msg_st *msg_st)
{
    struct xphi_msg_st *st = (struct xphi_msg_st *) msg_st;
    return xeon_phi_spawn_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                   st->args.spawn.xid, st->args.spawn.core,
                                   st->args.spawn.cmdline, st->args.spawn.cmdlen,
                                   st->args.spawn.flags);
}

static errval_t spawn_with_cap_call_tx(struct txq_msg_st *msg_st)
{
    struct xphi_msg_st *st = (struct xphi_msg_st *) msg_st;
    return xeon_phi_spawn_with_cap_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                            st->args.spawn.xid, st->args.spawn.core,
                                            st->args.spawn.cmdline,
                                            st->args.spawn.cmdlen,
                                            st->args.spawn.flags,
                                            st->args.spawn.cap);
}

static errval_t kill_call_tx(struct txq_msg_st *msg_st)
{
    struct xphi_msg_st *st = (struct xphi_msg_st *) msg_st;
    return xeon_phi_kill_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                  st->args.kill.xid, st->args.kill.domid);
}

static errval_t chan_open_request_call_tx(struct txq_msg_st *msg_st)
{
    struct xphi_msg_st *st = (struct xphi_msg_st *) msg_st;
    return xeon_phi_chan_open_request_call__tx(msg_st->queue->binding,
                                               TXQCONT(msg_st), st->args.open.xid,
                                               st->args.open.msgframe,
                                               st->args.open.type,
                                               st->args.open.domid,
                                               st->args.open.usrdata);
}

static errval_t chan_open_response_tx(struct txq_msg_st *msg_st)
{
    return xeon_phi_chan_open_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                           msg_st->err);
}

/*
 * ----------------------------------------------------------------------------
 * Receive Handlers
 * ----------------------------------------------------------------------------
 */
static void domain_lookup_response_rx(struct xeon_phi_binding *b,
                                      xphi_dom_id_t domid,
                                      errval_t msgerr)
{
    DEBUG_XPHI("domain_lookup_response_rx: %lx,  %s\n", domid, err_getstring(msgerr));

    struct xeon_phi_client *cl = b->st;
    assert(cl);

    cl->rpc_err = msgerr;
    cl->rpc_data = domid;

    rpc_done(cl);
}

static void domain_wait_response_rx(struct xeon_phi_binding *b,
                                    xphi_dom_id_t domid,
                                    errval_t msgerr)
{
    DEBUG_XPHI("domain_wait_response_rx: %lx,  %s\n", domid, err_getstring(msgerr));

    struct xeon_phi_client *cl = b->st;
    assert(cl);

    cl->rpc_err = msgerr;
    cl->rpc_data = domid;

    rpc_done(cl);
}

static void domain_init_response_rx(struct xeon_phi_binding *b,
                                    errval_t msgerr)
{
    DEBUG_XPHI("domain_init_response_rx: %s\n", err_getstring(msgerr));

    struct xeon_phi_client *cl = b->st;
    assert(cl);
    assert(cl->state == XPM_SVC_STATE_REGISTERING);

    cl->rpc_err = msgerr;

    rpc_done(cl);
}

static void domain_register_response_rx(struct xeon_phi_binding *b,
                                        errval_t msgerr)
{
    DEBUG_XPHI("domain_regoster_response_rx: %s\n", err_getstring(msgerr));

    struct xeon_phi_client *cl = b->st;
    assert(cl);
    assert(cl->state == XPM_SVC_STATE_REGISTERING);

    cl->rpc_err = msgerr;

    rpc_done(cl);
}

static void spawn_response_rx(struct xeon_phi_binding *b,
                              uint64_t domainid,
                              errval_t msgerr)
{
    DEBUG_XPHI("spawn_response_rx: %lx %s\n", domainid, err_getstring(msgerr));

    struct xeon_phi_client *cl = b->st;
    assert(cl);
    assert(cl->state == XPM_SVC_STATE_CONNECTED);

    cl->rpc_err = msgerr;
    cl->rpc_data = domainid;

    rpc_done(cl);
}

static void spawn_with_cap_response_rx(struct xeon_phi_binding *b,
                                       uint64_t domainid,
                                       errval_t msgerr)
{
    DEBUG_XPHI("spawn_with_cap_response_rx: %lx %s\n", domainid,
               err_getstring(msgerr));

    struct xeon_phi_client *cl = b->st;
    assert(cl);
    assert(cl->state == XPM_SVC_STATE_CONNECTED);

    cl->rpc_err = msgerr;
    cl->rpc_data = domainid;

    rpc_done(cl);
}

static void kill_response_rx(struct xeon_phi_binding *b,
                             errval_t msgerr)
{
    DEBUG_XPHI("kill_response_rx: %s\n", err_getstring(msgerr));

    struct xeon_phi_client *cl = b->st;
    assert(cl);
    assert(cl->state == XPM_SVC_STATE_CONNECTED);

    cl->rpc_err = msgerr;

    rpc_done(cl);
}

static void chan_open_request_response_rx(struct xeon_phi_binding *b,
                                          errval_t msgerr)
{
    DEBUG_XPHI("chan_open_request_response_rx: %s\n", err_getstring(msgerr));

    struct xeon_phi_client *cl = b->st;
    assert(cl);
    assert(cl->state == XPM_SVC_STATE_CONNECTED);

    cl->rpc_err = msgerr;

    rpc_done(cl);
}

static void chan_open_call_rx(struct xeon_phi_binding *b,
                              uint64_t domain,
                              uint64_t usrdata,
                              struct capref msgframe,
                              uint8_t type)
{
    DEBUG_XPHI("chan_open_request_call_rx: from domain:%lx, type:%u\n", domain, type);

    struct xeon_phi_client *cl = b->st;
    assert(cl);
    assert(cl->state == XPM_SVC_STATE_CONNECTED);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl->txq);
    if (msg_st == NULL) {
        USER_PANIC("could not allocate reply state");
    }

    msg_st->cleanup = NULL;
    msg_st->send = chan_open_response_tx;
    msg_st->err = XEON_PHI_ERR_CLIENT_OPEN_REJECT;

    if (client_cb.open) {
        msg_st->err = client_cb.open(domain, usrdata, msgframe, type);
    }

    txq_send(msg_st);
}

struct xeon_phi_rx_vtbl xphi_svc_rx_vtbl = {
    .domain_init_response = domain_init_response_rx,
    .domain_register_response = domain_register_response_rx,
    .domain_lookup_response = domain_lookup_response_rx,
    .domain_wait_response = domain_wait_response_rx,
    .spawn_response = spawn_response_rx,
    .spawn_with_cap_response = spawn_with_cap_response_rx,
    .kill_response = kill_response_rx,
    .chan_open_request_response = chan_open_request_response_rx,
    .chan_open_call = chan_open_call_rx
};

/*
 * ----------------------------------------------------------------------------
 * Client Initialization
 * ----------------------------------------------------------------------------
 */

static errval_t xphi_client_register(struct xeon_phi_client *cl)
{
    errval_t err;

    assert(cl->binding != NULL);

    while (!rpc_start(cl)) {
        messages_wait_and_handle_next();
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl->txq);
    if (msg_st == NULL) {
        rpc_clear(cl);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->cleanup = NULL;
    msg_st->send = domain_init_call_tx;

    struct xphi_msg_st *svc_st = (struct xphi_msg_st *) msg_st;
    svc_st->args.domain.domid = disp_get_domain_id();
    svc_st->args.domain.core = disp_get_core_id();

    /// XXX: bug, this fails if disp_name() length == DISP_NAME_LEN
    svc_st->args.domain.name = disp_name();

    DEBUG_XPHI("registration {%s} with domid:%x\n", svc_st->args.domain.name,
               (domainid_t )svc_st->args.domain.domid);

    txq_send(msg_st);

    rpc_wait_done(cl);

    if (err_is_fail(cl->rpc_err)) {
        cl->state = XPM_SVC_STATE_REGISTER_FAIL;
    } else {
        cl->state = XPM_SVC_STATE_REGISTER_OK;
    }

    err = cl->rpc_err;

    rpc_clear(cl);

    return err;
}

static void xphi_bind_cb(void *st,
                         errval_t err,
                         struct xeon_phi_binding *b)
{
    struct xeon_phi_client *cl = st;

    DEBUG_XPHI("bound to service: %s\n", err_getstring(err));

    if (err_is_fail(err)) {
        cl->err = err;
        cl->state = XPM_SVC_STATE_BIND_FAIL;
        return;
    }

    txq_init(&cl->txq, b, get_default_waitset(),
             (txq_register_fn_t) b->register_send, sizeof(struct xphi_msg_st));

    b->rx_vtbl = xphi_svc_rx_vtbl;
    b->st = cl;

    cl->binding = b;
    cl->state = XPM_SVC_STATE_BIND_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Initialization
 * ----------------------------------------------------------------------------
 */

/**
 *
 */
static errval_t xphi_client_init(xphi_id_t xid)
{
    errval_t err;

    if (xid >= XEON_PHI_NUM_MAX) {
        return XEON_PHI_ERR_INVALID_ID;
    }

    struct xeon_phi_client *cl = xphi_svc[xid];
    if (cl == NULL) {
        cl = calloc(1, sizeof(*cl));
        if (cl == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
    } else {
        return cl->err;
    }

    iref_t svc_iref;
#ifdef __k1om__
    DEBUG_XPHI("looking up service {%s}\n", XEON_PHI_SERVICE_NAME);
    err = nameservice_blocking_lookup(XEON_PHI_SERVICE_NAME, &svc_iref);
#else
    char iface[30];
    snprintf(iface, sizeof(iface), "%s.%u", XEON_PHI_SERVICE_NAME, xid);
    DEBUG_XPHI("looking up service {%s}\n", iface);
    err = nameservice_blocking_lookup(iface, &svc_iref);
#endif
    if (err_is_fail(err)) {
        free(cl);
        return err;
    }

    DEBUG_XPHI("initializing client to xid:%u @ iref:%"PRIxIREF"\n", xid, svc_iref);

    cl->state = XPM_SVC_STATE_BINDING;
    cl->xid = xid;
    struct waitset *ws = get_default_waitset();

    err = xeon_phi_bind(svc_iref, xphi_bind_cb, cl, ws, IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (cl->state == XPM_SVC_STATE_BINDING) {
        messages_wait_and_handle_next();
    }

    if (cl->state == XPM_SVC_STATE_BIND_FAIL) {
        err = cl->err;
        free(cl);
        return err;
    }

    cl->state = XPM_SVC_STATE_REGISTERING;
    err = xphi_client_register(cl);
    if (err_is_fail(err)) {
        free(cl);
        return err;
    }

    if (cl->state == XPM_SVC_STATE_REGISTER_FAIL) {
        err = cl->err;
        free(cl);
        return err;
    }

    cl->state = XPM_SVC_STATE_CONNECTED;

    xphi_svc[xid] = cl;

#ifdef __k1om__
    for (uint32_t i = 0; i < XEON_PHI_NUM_MAX; ++i) {
        xphi_svc[i] = cl;
    }
#endif

    return SYS_ERR_OK;
}

/*
 * ============================================================================
 * Public interface
 * ============================================================================
 */

/**
 * \brief sets the callbacks for incoming messages
 *
 * \param cb    Xeon Phi callbacks
 */
void xeon_phi_client_set_callbacks(struct xeon_phi_callbacks *cb)
{
    client_cb = *cb;
}

/**
 * \brief initializes the Xeon Phi client
 *
 * \param xid   Xeon Phi ID of the card to initialize
 */
errval_t xeon_phi_client_init(xphi_id_t xid)
{
    assert(xid < XEON_PHI_NUM_MAX);

    if (xphi_svc[xid] == NULL) {
        return xphi_client_init(xid);
    }
    return SYS_ERR_OK;
}

/**
 * \brief spawns a new domain on the Xeon Phi or on the host
 *
 * \param xid       Xeon Phi ID to start the domain
 * \param core      Core to start
 * \param path      Program to spawn
 * \param argv      Program arguments
 * \param cap       Capability to pass
 * \param flags     spawn flags
 * \param domid     returns the domain id of the spawned domain
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t xeon_phi_client_spawn(xphi_id_t xid,
                               coreid_t core,
                               char *path,
                               char *argv[],
                               struct capref cap,
                               uint8_t flags,
                               xphi_dom_id_t *domid)
{
    errval_t err;

    if (xid >= XEON_PHI_NUM_MAX) {
        return XEON_PHI_ERR_INVALID_ID;
    }

    err = check_online(xid);
    if (err_is_fail(err)) {
        return err;
    }

    if (xphi_svc[xid] == NULL) {
        err = xphi_client_init(xid);
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xeon_phi_client *cl = xphi_svc[xid];
    assert(cl);

    if (!rpc_start(cl)) {
        return XEON_PHI_ERR_CLIENT_BUSY;
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl->txq);
    if (msg_st == NULL) {
        rpc_clear(cl);
        return LIB_ERR_MALLOC_FAIL;
    }

    size_t argstrlen = strlen(path) + 1;

    if (argv) {
        for (int i = 0; argv[i] != NULL; i++) {
            argstrlen += strlen(argv[i]) + 1;
        }
    }

    char cmdline[argstrlen];
    size_t argstrpos = strlen(path);
    strcpy(&cmdline[0], path);
    cmdline[argstrpos++] = '\0';

    if (argv) {
        for (int i = 0; argv[i] != NULL; i++) {
            strcpy(&cmdline[argstrpos], argv[i]);
            argstrpos += strlen(argv[i]);
            cmdline[argstrpos++] = '\0';
        }
    }

    msg_st->cleanup = NULL;

    if (capref_is_null(cap)) {
        DEBUG_XPHI("spawning %s on core:%u @ xid:%u\n", cmdline, core, xid);
        msg_st->send = spawn_call_tx;
    } else {
        DEBUG_XPHI("spawning  %s with cap on core:%u @ xid:%u\n", cmdline, core, xid);
        msg_st->send = spawn_with_cap_call_tx;
    }

    cl->rpc_data = 0;

    struct xphi_msg_st *svc_st = (struct xphi_msg_st *) msg_st;
    svc_st->args.spawn.xid = xid;
    svc_st->args.spawn.core = core;
    svc_st->args.spawn.cmdline = cmdline;
    svc_st->args.spawn.cmdlen = argstrlen;
    svc_st->args.spawn.flags = flags;
    svc_st->args.spawn.cap = cap;

    txq_send(msg_st);

    rpc_wait_done(cl);

    if (domid) {
        *domid = cl->rpc_data;
    }

    DEBUG_XPHI("spawned %s: domid:%lx %s\n", cmdline, cl->rpc_data,
               err_getstring(cl->rpc_err));

    err = cl->rpc_err;

    rpc_clear(cl);

    return err;
}

/**
 * \brief sends a kill request to the Xeon Phi
 *
 * \param xid   Xeon Phi ID
 * \param domid ID of the domain to kill
 *
 * \returns SYS_ERR_OK on success,
 *          XEON_PHI_ERR_CLIENT_DOMAIN_VOID,
 *          errval on error
 */
errval_t xeon_phi_client_kill(xphi_id_t xid,
                              xphi_dom_id_t domid)
{
    errval_t err;

    if (xid >= XEON_PHI_NUM_MAX) {
        return XEON_PHI_ERR_INVALID_ID;
    }

    err = check_online(xid);
    if (err_is_fail(err)) {
        return err;
    }

    if (xphi_svc[xid] == NULL) {
        err = xphi_client_init(xid);
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xeon_phi_client *cl = xphi_svc[xid];

    if (!rpc_start(cl)) {
        return XEON_PHI_ERR_CLIENT_BUSY;
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl->txq);
    if (msg_st == NULL) {
        rpc_clear(cl);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->cleanup = NULL;
    msg_st->send = kill_call_tx;

    struct xphi_msg_st *svc_st = (struct xphi_msg_st *) msg_st;

    DEBUG_XPHI("killing domain:%lx @ xid:%u", domid, xid);

    svc_st->args.kill.xid = xid;
    svc_st->args.kill.domid = domid;

    txq_send(msg_st);

    rpc_wait_done(cl);

    DEBUG_XPHI("killed %lx: %s\n", domid, err_getstring(cl->rpc_err));

    err = cl->rpc_err;

    rpc_clear(cl);

    return err;
}

/**
 * \brief sends an channel open request to the domain
 *
 * \param xid       Xeon Phi ID
 * \param domid     Domain ID
 * \param usrdata   Supplied data for the other side
 * \param iface     Interface name of the domain
 * \param msgframe  Message frame
 * \param chantype  Type of the channel
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_CLIENT_OPEN_REJCT if the client rejected
 *          errval on error
 *
 * The function expectes to be either the domain or the interface specified.
 * If both are non-null then the domain ID is taken
 */
errval_t xeon_phi_client_chan_open(xphi_id_t xid,
                                   xphi_dom_id_t domid,
                                   uint64_t usrdata,
                                   struct capref msgframe,
                                   xphi_chan_type_t chantype)
{
    errval_t err;
    assert(domid);
    assert(!capref_is_null(msgframe));

    if (xid >= XEON_PHI_NUM_MAX) {
        return XEON_PHI_ERR_INVALID_ID;
    }

    err = check_online(xid);
    if (err_is_fail(err)) {
        return err;
    }

    if (xphi_svc[xid] == NULL) {
        err = xphi_client_init(xid);
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xeon_phi_client *cl = xphi_svc[xid];

    if (!rpc_start(cl)) {
        return XEON_PHI_ERR_CLIENT_BUSY;
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl->txq);
    if (msg_st == NULL) {
        rpc_clear(cl);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->cleanup = NULL;

    DEBUG_XPHI("xeon_phi_client_chan_open: domid:%lx, type:%u, @ xid:%u\n", domid,
               chantype, xid);
    msg_st->send = chan_open_request_call_tx;

    struct xphi_msg_st *svc_st = (struct xphi_msg_st *) msg_st;

    svc_st->args.open.domid = domid;
    svc_st->args.open.usrdata = usrdata;
    svc_st->args.open.xid = xid;
    svc_st->args.open.type = chantype;
    svc_st->args.open.msgframe = msgframe;

    txq_send(msg_st);

    rpc_wait_done(cl);

    err = cl->rpc_err;

    rpc_clear(cl);

    return err;
}

/**
 * \brief looks up the domain ID given the information
 *
 * \param iface     Interface name of the domain
 * \param retdom    returned domain id
 *
 * \returns SYS_ERR_OK on success,
 *          XEON_PHI_ERR_CLIENT_DOMAIN_VOID,
 *          errval on error
 */
errval_t xeon_phi_client_domain_lookup(const char *iface,
                                       xphi_dom_id_t *retdom)
{
#ifndef __k1om__
    USER_PANIC("xeon_phi_client_domain_wait: not supporte on the host\n");
    return SYS_ERR_OK;
#else
    errval_t err;

    uint8_t xid = disp_xeon_phi_id();

    if (xphi_svc[xid] == NULL) {
        err = xphi_client_init(xid);
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xeon_phi_client *cl = xphi_svc[xid];

    if (!rpc_start(cl)) {
        return XEON_PHI_ERR_CLIENT_BUSY;
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl->txq);
    if (msg_st == NULL) {
        rpc_clear(cl);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->cleanup = NULL;

    DEBUG_XPHI("xeon_phi_client_domain_lookup: iface:%s\n", iface);

    msg_st->send = domain_lookup_call_tx;

    struct xphi_msg_st *svc_st = (struct xphi_msg_st *) msg_st;

    svc_st->args.domain.name = iface;

    txq_send(msg_st);

    rpc_wait_done(cl);

    if (err_is_ok(cl->rpc_err)) {
        if (retdom) {
            *retdom = cl->rpc_data;
        }
    }

    err = cl->rpc_err;

    rpc_clear(cl);

    return err;
#endif
    return SYS_ERR_OK;
}

/**
 * \brief looks up the domain ID given the information and waits
 *        until the domain registers
 *
 * \param iface     Interface name of the domain
 * \param retdom    returned domain id
 *
 * \returns SYS_ERR_OK on success,
 *          XEON_PHI_ERR_CLIENT_DOMAIN_VOID,
 *          errval on error
 */
errval_t xeon_phi_client_domain_wait(const char *iface,
                                     xphi_dom_id_t *retdom)
{
#ifndef __k1om__
    USER_PANIC("xeon_phi_client_domain_wait: not supporte on the host\n");
    return SYS_ERR_OK;
#else
    errval_t err;

    uint8_t xid = disp_xeon_phi_id();

    if (xphi_svc[xid] == NULL) {
        err = xphi_client_init(xid);
        if (err_is_fail(err)) {
            return err;
        }
    }

    static char* format = "%s { domid: _ }";
    int length = snprintf(NULL, 0, format, iface);

    char* query = malloc(length + 1);
    if (query == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    snprintf(query, length + 1, format, iface);

    struct xeon_phi_client *cl = xphi_svc[xid];
    if (!rpc_start(cl)) {
        free(query);
        return XEON_PHI_ERR_CLIENT_BUSY;
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl->txq);
    if (msg_st == NULL) {
        rpc_clear(cl);
        free(query);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->cleanup = NULL;

    DEBUG_XPHI("xeon_phi_client_domain_wait: iface:%s\n", iface);

    msg_st->send = domain_wait_call_tx;

    struct xphi_msg_st *svc_st = (struct xphi_msg_st *) msg_st;

    svc_st->args.domain.name = query;

    txq_send(msg_st);

    rpc_wait_done(cl);

    if (err_is_ok(cl->rpc_err)) {
        if (retdom) {
            *retdom = cl->rpc_data;
        }
    }

    free(query);

    err = cl->rpc_err;

    rpc_clear(cl);

    return err;
#endif
}

/**
 * \brief registers a a domain
 *
 * \param iface     Interface name of the domain
 * \param retdom    returned domain id
 *
 * \returns SYS_ERR_OK on success,
 *          errval on error
 */
errval_t xeon_phi_client_domain_register(const char *iface,
                                         xphi_dom_id_t dom)
{
#ifndef __k1om__
    USER_PANIC("xeon_phi_client_domain_wait: not supported on the host\n");
    return SYS_ERR_OK;
#else

    errval_t err;

    uint8_t xid = disp_xeon_phi_id();

    if (xphi_svc[xid] == NULL) {
        err = xphi_client_init(xid);
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xeon_phi_client *cl = xphi_svc[xid];
    while (!rpc_start(cl)) {
        return XEON_PHI_ERR_CLIENT_BUSY;
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl->txq);
    if (msg_st == NULL) {
        rpc_clear(cl);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->cleanup = NULL;
    msg_st->send = domain_register_call_tx;

    struct xphi_msg_st *svc_st = (struct xphi_msg_st *) msg_st;

    svc_st->args.domain.name = iface;
    svc_st->args.domain.domid = dom;

    DEBUG_XPHI("registration {%s} with domid:%lx\n", svc_st->args.domain.name,
               svc_st->args.domain.domid);

    txq_send(msg_st);

    rpc_wait_done(cl);

    if (err_is_fail(cl->rpc_err)) {
        cl->state = XPM_SVC_STATE_REGISTER_FAIL;
    } else {
        cl->state = XPM_SVC_STATE_REGISTER_OK;
    }

    err = cl->rpc_err;

    rpc_clear(cl);

    return err;
#endif
}

