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
#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_domain.h>

#include <if/xeon_phi_defs.h>

#include "xeon_phi_internal.h"
#include "interphi.h"
#include "xphi_service.h"
#include "domain.h"
#include "debug.h"

#define XEON_PHI_SERVICE_NAME "xeon_phi_svc"

#define RPC_STATE_IDLE     0x00
#define RPC_STATE_PROGRESS 0x01
#define RPC_STATE_DONE     0x80

struct dma_mem_mgr;

struct xphi_svc_st
{
    struct xeon_phi *phi;
    struct tx_queue queue;
    char *name;
    errval_t rpc_err;
    uint8_t rpc_state;
    uint64_t domainid;
    struct dma_mem_mgr *dma_mem_mgr;
    struct xeon_phi_binding *binding;
    struct xphi_svc_st *next;
};

/**
 * enumeration of all possible states of the service exporting process
 */
enum xpm_svc_state
{
    XPM_SVC_STATE_INVALID,
    XPM_SVC_STATE_EXPORTING,
    XPM_SVC_STATE_EXPORT_OK,
    XPM_SVC_STATE_EXPORT_FAIL,
    XPM_SVC_STATE_RUNNING
};

/// represents the current state of the exporting process
static enum xpm_svc_state xphi_svc_state = XPM_SVC_STATE_INVALID;

/// error value for exporting
static errval_t xphi_svc_err;

struct xphi_svc_msg_st
{
    struct txq_msg_st common;
    /* union of arguments */
    union
    {
        struct
        {
            xphi_dom_id_t domainid;
        } spawn;
        struct
        {
            xphi_dom_id_t domainid;
            uint64_t usrdata;
            struct capref cap;
            uint8_t type;
        } open;
        struct
        {
            uint64_t domid;
        } domain;
        struct
        {
            int32_t nodeid;
        } nodeid;
        struct
        {
            uint64_t devaddr;
        } dma_register;
        struct {
            struct capref cap;
        } alloc;
    } args;
};

/*
 * ----------------------------------------------------------------------------
 * Connected Client management
 * ----------------------------------------------------------------------------
 */

struct xphi_svc_st *xphi_svc_clients;

static void xphi_svc_clients_insert(struct xphi_svc_st *new)
{
    XSERVICE_DEBUG("inserting client: {%s} domainid:%lx\n", new->name,
                   new->domainid);
    new->next = xphi_svc_clients;
    xphi_svc_clients = new;
}

static struct xphi_svc_st *xphi_svc_clients_lookup_by_did(uint64_t did)
{
    XSERVICE_DEBUG("lookup client: domainid:%lx\n", did);
    struct xphi_svc_st *current = xphi_svc_clients;
    while (current) {
        if (current->domainid == did) {
            return current;
        }
        current = current->next;
    }

    return current;
}

/*
 * ----------------------------------------------------------------------------
 * Send handlers
 * ----------------------------------------------------------------------------
 */

static errval_t chan_open_call_tx(struct txq_msg_st* msg_st)
{
    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

    return xeon_phi_chan_open_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                       xphi_st->args.open.domainid,
                                       xphi_st->args.open.usrdata,
                                       xphi_st->args.open.cap,
                                       xphi_st->args.open.type);
}

static errval_t chan_open_request_response_tx(struct txq_msg_st* msg_st)
{
    return xeon_phi_chan_open_request_response__tx(msg_st->queue->binding,
                                                   TXQCONT(msg_st), msg_st->err);
}

static errval_t kill_response_tx(struct txq_msg_st* msg_st)
{
    return xeon_phi_kill_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                      msg_st->err);
}

static errval_t dma_register_response_tx(struct txq_msg_st* msg_st)
{
    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

    return xeon_phi_dma_register_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                              xphi_st->args.dma_register.devaddr,
                                              msg_st->err);
}

static errval_t dma_memcpy_response_tx(struct txq_msg_st* msg_st)
{
    return xeon_phi_dma_memcpy_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                            msg_st->err);
}

static errval_t get_nodeid_response_tx(struct txq_msg_st* msg_st)
{
    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

    return xeon_phi_get_nodeid_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                            xphi_st->args.nodeid.nodeid);
}

static errval_t spawn_with_cap_response_tx(struct txq_msg_st* msg_st)
{
    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

    return xeon_phi_spawn_with_cap_response__tx(msg_st->queue->binding,
                                                TXQCONT(msg_st),
                                                xphi_st->args.spawn.domainid,
                                                msg_st->err);
}

static errval_t spawn_response_tx(struct txq_msg_st* msg_st)
{
    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

    return xeon_phi_spawn_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                       xphi_st->args.spawn.domainid, msg_st->err);
}

static errval_t domain_init_response_tx(struct txq_msg_st* msg_st)
{
    return xeon_phi_domain_init_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                             msg_st->err);
}

static errval_t domain_register_response_tx(struct txq_msg_st* msg_st)
{
    return xeon_phi_domain_register_response__tx(msg_st->queue->binding,
                                                 TXQCONT(msg_st), msg_st->err);
}

static errval_t domain_lookup_response_tx(struct txq_msg_st* msg_st)
{
    struct xphi_svc_msg_st *st = (struct xphi_svc_msg_st *) msg_st;
    return xeon_phi_domain_lookup_response__tx(msg_st->queue->binding,
                                               TXQCONT(msg_st),
                                               st->args.domain.domid, msg_st->err);
}

static errval_t domain_wait_response_tx(struct txq_msg_st* msg_st)
{
    struct xphi_svc_msg_st *st = (struct xphi_svc_msg_st *) msg_st;

    return xeon_phi_domain_wait_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                             st->args.domain.domid, msg_st->err);
}

static errval_t alloc_mem_response_tx(struct txq_msg_st* msg_st)
{
    struct xphi_svc_msg_st *st = (struct xphi_svc_msg_st *) msg_st;

    return xeon_phi_alloc_mem_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                             st->args.alloc.cap, msg_st->err);
}

/*
 * ----------------------------------------------------------------------------
 * Receive Handlers
 * ----------------------------------------------------------------------------
 */

static void domain_lookup_call_rx(struct xeon_phi_binding *binding,
                                  const char *name,
                                  size_t length)
{
    XSERVICE_DEBUG("domain_lookup_call_rx: %s\n", name);

    struct xphi_svc_st *svc_st = binding->st;
    struct xeon_phi *phi = svc_st->phi;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = domain_lookup_response_tx;
    msg_st->cleanup = NULL;

    struct xphi_svc_msg_st *st = (struct xphi_svc_msg_st *) msg_st;

    msg_st->err = interphi_domain_lookup(&phi->topology[phi->id], (CONST_CAST)name,
                                         &st->args.domain.domid);
    txq_send(msg_st);
}

static void domain_wait_call_rx(struct xeon_phi_binding *binding,
                                const char *name,
                                size_t length)
{
    XSERVICE_DEBUG("domain_wait_call_rx: %s\n", name);

    struct xphi_svc_st *svc_st = binding->st;
    struct xeon_phi *phi = svc_st->phi;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    /* TODO: allocate reply state */

    msg_st->err = interphi_domain_wait(&phi->topology[phi->id], (CONST_CAST)name, svc_st);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
    }
}

static void domain_register_call_rx(struct xeon_phi_binding *binding,
                                    const char *name,
                                    size_t length,
                                    xphi_dom_id_t domid)
{
    XSERVICE_DEBUG("domain_init_call_rx: %s @ domainid:%lx\n", name, domid);

    struct xphi_svc_st *svc_st = binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = domain_register_response_tx;
    msg_st->cleanup = NULL;

    svc_st->domainid = domid;
    svc_st->name = (CONST_CAST)name;

#ifdef __k1om__
    struct xeon_phi *phi = svc_st->phi;
    msg_st->err = interphi_domain_register(&phi->topology[phi->id], name, domid);
#else
    msg_st->err = domain_register(name, domid);
#endif
    txq_send(msg_st);
}

static void domain_init_call_rx(struct xeon_phi_binding *binding,
                                domainid_t domain,
                                coreid_t core,
                                const char *name,
                                size_t length)
{
    XSERVICE_DEBUG("domain_init_call_rx: %s @ domainid:%"PRIuDOMAINID"\n", name,
                   domain);

    assert(domain != XEON_PHI_DOMAIN_DONT_CARE);
    assert(core != XEON_PHI_DOMAIN_DONT_CARE);

    struct xphi_svc_st *svc_st = binding->st;
    struct xeon_phi *phi = svc_st->phi;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = domain_init_response_tx;
    msg_st->cleanup = NULL;

    if (svc_st->next != NULL) {
        msg_st->err = XEON_PHI_ERR_CLIENT_REGISTER;
        txq_send(msg_st);
        return;
    }

#ifdef __k1om__
    svc_st->domainid = xeon_phi_domain_build_id(phi->id, core, 0x0, domain);
    svc_st->name = xeon_phi_domain_build_iface(name, disp_xeon_phi_id(), core);
    msg_st->err = interphi_domain_register(&phi->topology[phi->id], svc_st->name,
                                           svc_st->domainid);

#else
    svc_st->name = xeon_phi_domain_build_iface(name, XEON_PHI_DOMAIN_HOST, core);
    svc_st->domainid = xeon_phi_domain_build_id(phi->id, core, 0x1, domain);
    msg_st->err = domain_register(svc_st->name, svc_st->domainid);
#endif
    xphi_svc_clients_insert(svc_st);
    txq_send(msg_st);
}

static void chan_open_response_rx(struct xeon_phi_binding *binding,
                                  errval_t msgerr)
{
    XSERVICE_DEBUG("chan_open_response_rx: %s\n", err_getstring(msgerr));

    struct xphi_svc_st *svc_st = binding->st;

    svc_st->rpc_state |= RPC_STATE_DONE;
    svc_st->rpc_err |= msgerr;
}

static void chan_open_request_call_rx(struct xeon_phi_binding *binding,
                                      uint8_t xphi,
                                      struct capref msgframe,
                                      uint8_t type,
                                      uint64_t domain,
                                      uint64_t usrdata)
{
    XSERVICE_DEBUG("chan_open_request_did_call_rx: xphi:%u, domain:%lx\n", xphi,
                   domain);

    struct xphi_svc_st *svc_st = binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = chan_open_request_response_tx;
    msg_st->cleanup = NULL;

#ifdef __k1om__
    struct xnode *node = &svc_st->phi->topology[xphi];
#else
    struct xnode *node = &svc_st->phi->topology[svc_st->phi->id];
#endif
    msg_st->err = interphi_chan_open(node, domain, svc_st->domainid, usrdata,
                                     msgframe, type);

    txq_send(msg_st);
}

static void kill_call_rx(struct xeon_phi_binding *binding,
                         uint8_t xid,
                         uint64_t domainid)
{
    struct xphi_svc_st *svc_st = binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = kill_response_tx;
    msg_st->cleanup = NULL;

#ifdef __k1om__
    struct xnode *node = &svc_st->phi->topology[xid];
#else
    struct xnode *node = &svc_st->phi->topology[svc_st->phi->id];
#endif

    msg_st->err = interphi_kill(node, domainid);

    txq_send(msg_st);
}

static void spawn_with_cap_call_rx(struct xeon_phi_binding *binding,
                                   uint8_t xid,
                                   uint8_t core,
                                   const char *cmdline,
                                   size_t length,
                                   uint8_t flags,
                                   struct capref cap)
{
    struct xphi_svc_st *svc_st = binding->st;

    XSERVICE_DEBUG("spawn_with_cap_call_rx: %s of length %lu\n", cmdline, length);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = spawn_with_cap_response_tx;
    msg_st->cleanup = NULL;

    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

#ifdef __k1om__
    struct xnode *node = &svc_st->phi->topology[xid];
#else
    struct xnode *node = &svc_st->phi->topology[svc_st->phi->id];
#endif
    msg_st->err = interphi_spawn_with_cap(node, core, (CONST_CAST)cmdline, length,
                                          flags, cap,
                                          &xphi_st->args.spawn.domainid);
    txq_send(msg_st);
}

static void spawn_call_rx(struct xeon_phi_binding *binding,
                          uint8_t xid,
                          uint8_t core,
                          const char *cmdline,
                          size_t length,
                          uint8_t flags)
{
    struct xphi_svc_st *svc_st = binding->st;

    XSERVICE_DEBUG("spawn_call_rx: %s of length %lu\n", cmdline, length);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = spawn_response_tx;
    msg_st->cleanup = NULL;

    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

#ifdef __k1om__
    struct xnode *node = &svc_st->phi->topology[xid];
#else
    struct xnode *node = &svc_st->phi->topology[svc_st->phi->id];
#endif
    msg_st->err = interphi_spawn(node, core, (CONST_CAST)cmdline, length, flags,
                                 &xphi_st->args.spawn.domainid);
    txq_send(msg_st);
}

#include "dma_service.h"
static void dma_register_call_rx(struct xeon_phi_binding *binding,
                                 struct capref mem)
{
    struct xphi_svc_st *svc_st = binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = dma_register_response_tx;
    msg_st->cleanup = NULL;

    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;
    xphi_st->args.dma_register.devaddr = 0;

    if (svc_st->dma_mem_mgr == NULL) {
        msg_st->err  = xdma_state_init(svc_st->phi, &svc_st->dma_mem_mgr);
        if (err_is_fail(msg_st->err )) {
            goto send;
        }
    }

    msg_st->err= xdma_register_region(svc_st->phi, svc_st->dma_mem_mgr, mem,
                                      &xphi_st->args.dma_register.devaddr);
send:
    txq_send(msg_st);
}

static void dma_memcpy_call_rx(struct xeon_phi_binding *binding,
                               uint64_t to, uint64_t from, uint64_t bytes)
{
    struct xphi_svc_st *svc_st = binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    XSERVICE_DEBUG("dma_memcpy_call_rx(%lx, %lx)\n", to, from);

    msg_st->send = dma_memcpy_response_tx;
    msg_st->cleanup = NULL;
    msg_st->err = SYS_ERR_OK;

    if (svc_st->dma_mem_mgr == NULL) {
        msg_st->err = DMA_ERR_MEM_NOT_REGISTERED;
        goto send_reply;
    }

    errval_t err;

    err =  xdma_memcpy(svc_st->phi, svc_st->dma_mem_mgr, to, from, bytes, msg_st);
    if (err_is_fail(msg_st->err)) {
        msg_st->err = err;
        goto send_reply;
    }

    /* return sending */
    return;

    send_reply:
    txq_send(msg_st);
}
#include <driverkit/iommu.h>
static void get_nodeid_call_rx(struct xeon_phi_binding *binding,
                               uint64_t arg)
{
    errval_t err;

    struct xphi_svc_st *svc_st = binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = get_nodeid_response_tx;
    msg_st->cleanup = NULL;

    msg_st->err = SYS_ERR_OK;

    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

    int32_t dma, core;
    err = xeon_phi_hw_model_lookup_nodeids(svc_st->phi->nodeid, NULL, NULL, NULL, &dma,
            &core);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "lookup nodeids\n");
        msg_st->err = err;
        txq_send(msg_st);
        return;
    }

    if (!(arg & (1UL << 63))) {
        // invalid
        xphi_st->args.nodeid.nodeid = -1;
    } else if (arg & (1UL<<32)) {
        // DMA engine
        // TODO: This has been tested only with KNC Socket.
        xphi_st->args.nodeid.nodeid = dma;
    } else if (arg  & (1UL<<33)) {
        // cores
        // coreid_t coreid = arg & 0xffff;
        // TODO: add individual nodes for each core to model.
        xphi_st->args.nodeid.nodeid = core;
    } else {
        // PCI card
        xphi_st->args.nodeid.nodeid = svc_st->phi->nodeid;
    }

    txq_send(msg_st);
}

static void alloc_mem_call_rx(struct xeon_phi_binding *binding, uint64_t bytes)
{
    struct xphi_svc_st *svc_st = binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = alloc_mem_response_tx;
    msg_st->cleanup = NULL;

    struct xnode *node = &svc_st->phi->topology[svc_st->phi->id];

    struct xphi_svc_msg_st *st = (struct xphi_svc_msg_st *)msg_st;

    msg_st->err = interphi_alloc_mem(node, bytes, &st->args.alloc.cap);



    struct frame_identity id;
    errval_t err = invoke_frame_identify(st->args.alloc.cap, &id);

    debug_printf("Obtained cap. %lx..%lx, %s\n", id.base, id.base + id.bytes - 1,
                 err_getstring(err));


    txq_send(msg_st);
}

static struct xeon_phi_rx_vtbl xphi_svc_rx_vtbl = {
    .domain_init_call = domain_init_call_rx,
    .domain_register_call = domain_register_call_rx,
    .domain_lookup_call = domain_lookup_call_rx,
    .domain_wait_call = domain_wait_call_rx,
    .spawn_call = spawn_call_rx,
    .spawn_with_cap_call = spawn_with_cap_call_rx,
    .kill_call = kill_call_rx,
    .chan_open_request_call = chan_open_request_call_rx,
    .chan_open_response = chan_open_response_rx,
    .dma_register_call = dma_register_call_rx,
    .dma_memcpy_call = dma_memcpy_call_rx,
    .get_nodeid_call = get_nodeid_call_rx,
    .alloc_mem_call = alloc_mem_call_rx
};

/*
 * ----------------------------------------------------------------------------
 * Export / Connect / Bind Callbacks
 * ----------------------------------------------------------------------------
 */

static errval_t xphi_svc_connect_cb(void *st,
                                    struct xeon_phi_binding *binding)
{
    XSERVICE_DEBUG("xphi_svc_connect_cb: new connection from domain.\n");

    struct xphi_svc_st *svc_st = calloc(1, sizeof(*svc_st));
    if (svc_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    svc_st->phi = st;

    txq_init(&svc_st->queue, binding, binding->waitset,
             (txq_register_fn_t) binding->register_send,
             sizeof(struct xphi_svc_msg_st));

    binding->st = svc_st;
    binding->rx_vtbl = xphi_svc_rx_vtbl;
    svc_st->binding = binding;

    return SYS_ERR_OK;

}

static void xphi_svc_export_cb(void *st,
                               errval_t err,
                               iref_t iref)
{
    XSERVICE_DEBUG("xphi_svc_export_cb @ iref:%"PRIxIREF" result: %s\n", iref,
                   err_getstring(err));

    if (err_is_ok(err)) {
        struct xeon_phi *phi = st;
        phi->xphi_svc_iref = iref;
        xphi_svc_state = XPM_SVC_STATE_EXPORT_OK;
        return;
    }

    xphi_svc_state = XPM_SVC_STATE_EXPORT_FAIL;
}

/*
 * ----------------------------------------------------------------------------
 * Initialization
 * ----------------------------------------------------------------------------
 */

/**
 *
 */
errval_t xeon_phi_service_init(struct xeon_phi *phi)
{
    errval_t err;

    XSERVICE_DEBUG("initializing Xeon Phi service\n");

    if (xphi_svc_state != XPM_SVC_STATE_INVALID) {
        return SYS_ERR_OK;
    }

    xphi_svc_state = XPM_SVC_STATE_EXPORTING;

    err = xeon_phi_export(phi, xphi_svc_export_cb, xphi_svc_connect_cb,
                          get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (xphi_svc_state == XPM_SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (xphi_svc_state == XPM_SVC_STATE_EXPORT_FAIL) {
        return xphi_svc_err;
    }

#ifdef __k1om__
    XSERVICE_DEBUG("registering {%s} with iref:%"PRIxIREF"\n", XEON_PHI_SERVICE_NAME,
                   phi->xphi_svc_iref);
    err = nameservice_register(XEON_PHI_SERVICE_NAME, phi->xphi_svc_iref);
    if (err_is_fail(err)) {
        return err;
    }
#else
    char iface[30];
    snprintf(iface, sizeof(iface), "%s.%u", XEON_PHI_SERVICE_NAME, phi->id);
    XSERVICE_DEBUG("registering {%s} with iref:%"PRIxIREF"\n", iface, phi->xphi_svc_iref);
    err = nameservice_register(iface, phi->xphi_svc_iref);
#endif

    xphi_svc_state = XPM_SVC_STATE_RUNNING;

    XSERVICE_DEBUG("service up and running\n");

    return SYS_ERR_OK;
}

errval_t xeon_phi_service_open_channel(struct capref cap,
                                       uint8_t type,
                                       xphi_dom_id_t target_domain,
                                       xphi_dom_id_t src_domain,
                                       uint64_t userdata)
{
    errval_t err;

    struct xphi_svc_st *st = NULL;

    st = xphi_svc_clients_lookup_by_did(target_domain);
    XSERVICE_DEBUG("xeon_phi_service_open_channel: target_domain, st:%p\n", st);

    if (st == NULL) {
        return XEON_PHI_ERR_CLIENT_DOMAIN_VOID;
    }

    while (st->rpc_state != RPC_STATE_IDLE) {
        err = xeon_phi_event_poll(st->phi, true);
        if (err_is_fail(err)) {
            return err;
        }
    }

    st->rpc_state = RPC_STATE_PROGRESS;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = chan_open_call_tx;
    msg_st->cleanup = NULL;

    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

    xphi_st->args.open.cap = cap;
    xphi_st->args.open.type = type;
    xphi_st->args.open.domainid = src_domain;
    xphi_st->args.open.usrdata = userdata;

    txq_send(msg_st);

    while (!(st->rpc_state & RPC_STATE_DONE)) {
        err = xeon_phi_event_poll(st->phi, true);
        if (err_is_fail(err)) {
            return err;
        }
    }

    st->rpc_state = RPC_STATE_IDLE;

    return st->rpc_err;
}

errval_t xeon_phi_service_domain_wait_response(struct xphi_svc_st *st,
                                               errval_t err,
                                               xphi_dom_id_t domain)
{
    struct txq_msg_st *msg_st = txq_msg_st_alloc(&st->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = domain_wait_response_tx;
    msg_st->cleanup = NULL;

    struct xphi_svc_msg_st *xphi_st = (struct xphi_svc_msg_st *) msg_st;

    xphi_st->args.domain.domid = domain;

    txq_send(msg_st);

    return SYS_ERR_OK;
}
