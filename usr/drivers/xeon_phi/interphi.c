/**
 * \file
 * \brief Boot module for the Xeon Phi
 *
 * Loads the co processor OS onto the card and boots it
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>

#include <flounder/flounder_txqueue.h>

#include <if/interphi_defs.h>

#include <xeon_phi/xeon_phi.h>

#include "xeon_phi_internal.h"
#include "interphi.h"
#include "smpt.h"
#include "service.h"
#include "sysmem_caps.h"

/**
 * represents the information for the messaging channel between the host and
 * the card.
 */
struct msg_info
{
    struct capref frame;
    struct interphi_frameinfo fi;
    struct interphi_binding *binding;
    struct tx_queue queue;
    errval_t rpc_err;
    uint64_t rpc_data;
    uint8_t is_client;
    uint8_t wait_reply;
};

/*
 *
 */
struct interphi_msg_st
{
    struct txq_msg_st common;
    /* union of arguments */
    union
    {
        struct
        {
            uint8_t core;
            char *cmdline;
            uint64_t cap_base;
            uint8_t cap_size_bits;
        } spawn_call;
        struct
        {
            uint64_t domainid;
        } spawn_reply;
        struct
        {
            uint64_t domainid;
        } kill;
        struct
        {
            uint64_t base;
            uint64_t offset;
            uint8_t bits;
            uint8_t xid;
            uint8_t is_client;
        } bootstrap;
    } args;
};

/*
 * ---------------------------------------------------------------------------
 * RPC management
 * ---------------------------------------------------------------------------
 */

/**
 * \brief starts a new RPC to the inter Xeon Phi connection
 *
 * \param mi    Xeon Phi message info
 *
 * \returns 1 if the RPC transaction could be started
 *          0 if there was already a transaction in process
 */
static inline uint8_t rpc_start(struct msg_info *mi)
{
    if (!mi->wait_reply) {
        mi->wait_reply = 0x1;
        return 1;
    }
    return 0;
}

/**
 * \brief waits until the started transaction is finished
 *
 * \param mi    Xeon Phi message info
 */
static inline void rpc_wait_done(struct msg_info *mi)
{

    while (mi->wait_reply) {
#ifndef __k1om__
        errval_t err;
        uint32_t data = 0x0;
        uint32_t serial_recv = 0xF;
        while (serial_recv--) {
            data |= xeon_phi_serial_handle_recv();
        }

        err = event_dispatch_non_block(get_default_waitset());
        switch (err_no(err)) {
            case SYS_ERR_OK:
            break;
            case LIB_ERR_NO_EVENT:
            if (!data) {
                thread_yield();
            }
            break;
            default:
            USER_PANIC_ERR(err, "in event dispatch\n");
            break;
        }

#else
        messages_wait_and_handle_next();
#endif
    }
}

/**
 * \brief signals the completion of the RPC
 *
 * \param mi    Xeon Phi message info
 */
static inline void rpc_done(struct msg_info *mi)
{
    mi->wait_reply = 0x0;
}

/*
 * ----------------------------------------------------------------------------
 * Message Send Handlers
 * ----------------------------------------------------------------------------
 */
static errval_t spawn_response_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_spawn_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                       st->args.spawn_reply.domainid, msg_st->err);
}

static errval_t spawn_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    size_t length = strlen(st->args.spawn_call.cmdline);

    return interphi_spawn_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                   st->args.spawn_call.core,
                                   st->args.spawn_call.cmdline, length);
}

static errval_t spawn_with_cap_response_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_spawn_with_cap_response__tx(msg_st->queue->binding,
                                                TXQCONT(msg_st),
                                                st->args.spawn_reply.domainid,
                                                msg_st->err);
}

static errval_t spawn_with_cap_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    size_t length = strlen(st->args.spawn_call.cmdline);

    return interphi_spawn_with_cap_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                            st->args.spawn_call.core,
                                            st->args.spawn_call.cmdline, length,
                                            st->args.spawn_call.cap_base,
                                            st->args.spawn_call.cap_size_bits);
}

static errval_t kill_response_tx(struct txq_msg_st *msg_st)
{
    return interphi_kill_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                      msg_st->err);
}

static errval_t kill_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_kill_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                  st->args.kill.domainid);
}

static errval_t bootstrap_response_tx(struct txq_msg_st *msg_st)
{
    return interphi_bootstrap_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                           msg_st->err);
}

static errval_t bootstrap_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_bootstrap_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                       st->args.bootstrap.base,
                                       st->args.bootstrap.offset,
                                       st->args.bootstrap.bits,
                                       st->args.bootstrap.xid,
                                       st->args.bootstrap.is_client);
}

/*
 * ----------------------------------------------------------------------------
 * Message Callbacks
 * ----------------------------------------------------------------------------
 */

static void spawn_call_rx(struct interphi_binding *_binding,
                          uint8_t core,
                          char *cmdline,
                          size_t length)
{
    XINTER_DEBUG("spawn_call_rx: {%s} @ core:%u\n", cmdline, core);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->err = SYS_ERR_OK;
    msg_st->send = spawn_response_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    char *argv[1];
    argv[0] = NULL;

    domainid_t domid;
    msg_st->err = spawn_program(core, cmdline, argv, NULL, 0, &domid);
    if (err_is_ok(msg_st->err)) {
        st->args.spawn_reply.domainid = ((uint64_t) phi->id) << 48 | domid;
    }
    txq_send(msg_st);
}

static void spawn_response_rx(struct interphi_binding *_binding,
                              uint64_t domainid,
                              interphi_errval_t msgerr)
{
    XINTER_DEBUG("spawn_response_rx: %lu, %s\n", domainid, err_getstring(msgerr));

    struct xnode *local_node = _binding->st;

    local_node->msg->rpc_err = msgerr;
    local_node->msg->rpc_data = domainid;
    rpc_done(local_node->msg);
}

static void spawn_with_cap_call_rx(struct interphi_binding *_binding,
                                   uint8_t core,
                                   char *cmdline,
                                   size_t length,
                                   uint64_t cap_base,
                                   uint8_t cap_size_bits)
{
    XINTER_DEBUG("spawn_with_cap_call_rx: {%s} @ core:%u\n", cmdline, core);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->err = SYS_ERR_OK;
    msg_st->send = spawn_with_cap_response_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    char *argv[1];
    argv[0] = NULL;

    struct capref cap;
    msg_st->err = sysmem_cap_request(cap_base, cap_size_bits, &cap);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }
    domainid_t domid;
    msg_st->err = spawn_program_with_caps(core, cmdline, argv, NULL, NULL_CAP
    ,
                                          cap, 0, &domid);

    if (err_is_ok(msg_st->err)) {
        st->args.spawn_reply.domainid = ((uint64_t) phi->id) << 48 | domid;
    }
    txq_send(msg_st);
}

static void spawn_with_cap_response_rx(struct interphi_binding *_binding,
                                       uint64_t domainid,
                                       interphi_errval_t msgerr)
{
    XINTER_DEBUG("spawn_with_cap_response_rx: %lu, %s\n", domainid,
                 err_getstring(msgerr));

    struct xnode *local_node = _binding->st;

    local_node->msg->rpc_err = msgerr;
    local_node->msg->rpc_data = domainid;
    rpc_done(local_node->msg);
}

static void kill_call_rx(struct interphi_binding *_binding,
                         uint64_t domainid)
{
    XINTER_DEBUG("kill_call_rx: %lu,\n", domainid);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->err = SYS_ERR_OK;
    msg_st->send = kill_response_tx;
    msg_st->cleanup = NULL;

    msg_st->err = spawn_kill(domainid);

    txq_send(msg_st);
}

static void kill_response_rx(struct interphi_binding *_binding,
                             interphi_errval_t msgerr)
{
    XINTER_DEBUG("kill_response_rx: %s\n", err_getstring(msgerr));

    struct xnode *local_node = _binding->st;

    local_node->msg->rpc_err = msgerr;

    rpc_done(local_node->msg);
}

static void bootstrap_call_rx(struct interphi_binding *_binding,
                              uint64_t base,
                              uint64_t offset,
                              uint8_t bits,
                              uint8_t xid,
                              uint8_t is_client)
{
    XINTER_DEBUG("bootstrap_call_rx: {%016lx, %02x} of:%016lx, xid:%u, c:%u\n", base,
                 bits, offset, xid, is_client);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->err = SYS_ERR_OK;
    msg_st->send = bootstrap_response_tx;
    msg_st->cleanup = NULL;

    struct xnode *node = &phi->topology[xid];
    if (node->msg) {
        XINTER_DEBUG("already established a connection to xid:%u\n", xid);
        txq_send(msg_st);
    }

    struct capref msg_frame;
    msg_st->err = sysmem_cap_request(base, bits, &msg_frame);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }

    smpt_set_coprocessor_offset(phi, xid, offset);

    msg_st->err = interphi_init_xphi(xid, phi, msg_frame, is_client);

    txq_send(msg_st);
}

static void bootstrap_response_rx(struct interphi_binding *_binding,
                                  interphi_errval_t msgerr)
{
    XINTER_DEBUG("bootstrap_response_rx: %s\n", err_getstring(msgerr));

    struct xnode *local_node = _binding->st;

    local_node->msg->rpc_err = msgerr;

    rpc_done(local_node->msg);
}

struct interphi_rx_vtbl rx_vtbl = {
    .spawn_call = spawn_call_rx,
    .spawn_response = spawn_response_rx,
    .spawn_with_cap_call = spawn_with_cap_call_rx,
    .spawn_with_cap_response = spawn_with_cap_response_rx,
    .kill_call = kill_call_rx,
    .kill_response = kill_response_rx,
    .bootstrap_call = bootstrap_call_rx,
    .bootstrap_response = bootstrap_response_rx
};

/*
 * ----------------------------------------------------------------------------
 * Flounder Connect / Accept Callbacks
 * ----------------------------------------------------------------------------
 */

static void interphi_bind_cb(void *st,
                             errval_t err,
                             struct interphi_binding *_binding)
{
    XINTER_DEBUG("interphi_bind_cb: bound to host driver.\n");

    struct xnode *node = st;

    _binding->rx_vtbl = rx_vtbl;
    _binding->st = st;

    node->msg->binding = _binding;

    txq_init(&node->msg->queue, _binding, _binding->waitset,
             (txq_register_fn_t) _binding->register_send,
             sizeof(struct interphi_msg_st));

    node->state = XNODE_STATE_READY;
}

static void interphi_connect_cb(void *st,
                                errval_t err,
                                struct interphi_binding *_binding)
{
    XINTER_DEBUG("interphi_connect_cb: client driver connected\n");

    struct xnode *node = st;

    _binding->rx_vtbl = rx_vtbl;
    _binding->st = st;

    node->msg->binding = _binding;

    txq_init(&node->msg->queue, _binding, _binding->waitset,
             (txq_register_fn_t) _binding->register_send,
             sizeof(struct interphi_msg_st));

    node->state = XNODE_STATE_READY;
}

/**
 * \brief waits for the client driver to connect
 *
 * \param phi Xeon Phi
 *
 * \return SYS_ERR_OK when then client driver successfully connected
 */
errval_t interphi_wait_for_client(struct xeon_phi *phi)
{
#ifndef __k1om__
    errval_t err;

    XINTER_DEBUG("interphi_wait_for_client\n");

    struct xnode *node = &phi->topology[phi->id];

    assert(node->state == XNODE_STATE_WAIT_CONNECTION);

    while (node->state == XNODE_STATE_WAIT_CONNECTION) {
        uint32_t data = 0x0;
        uint32_t serial_recv = 0xF;
        while (serial_recv--) {
            data |= xeon_phi_serial_handle_recv();
        }

        err = event_dispatch_non_block(get_default_waitset());
        switch (err_no(err)) {
            case SYS_ERR_OK:
            break;
            case LIB_ERR_NO_EVENT:
            if (!data) {
                thread_yield();
            }
            break;
            default:
            return err;
            break;
        }
    }
#endif
    return SYS_ERR_OK;

}

/*
 * ----------------------------------------------------------------------------
 * Initialization
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes the messaging boostrap infrastructure between the
 *        two Xeon Phi cards
 *
 * \param phi the xeon phi to initialize the basic messaging bootstrap
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t interphi_init_xphi(uint8_t xphi,
                            struct xeon_phi *phi,
                            struct capref frame,
                            uint8_t is_client)
{
    errval_t err;

    XINTER_DEBUG("initializing intra Xeon Phi [%u <-> %u] client=%u\n", phi->id,
                 xphi, is_client);

    assert(xphi < XEON_PHI_NUM_MAX);
    assert(xphi != phi->id);

    assert(phi->topology[xphi].msg == NULL);

    struct msg_info *mi = malloc(sizeof(struct msg_info));
    if (mi == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    size_t frame_size;

    if (capref_is_null(frame)) {
        err = frame_alloc(&mi->frame, XEON_PHI_INTERPHI_FRAME_SIZE, &frame_size);
        if (err_is_fail(err)) {
            return err;
        }
    } else {
        mi->frame = frame;
    }

    struct frame_identity id;
    err = invoke_frame_identify(mi->frame, &id);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    mi->is_client = is_client;

    frame_size = (1UL << id.bits);

#ifdef __k1om__
    /*
     * XXX: the host does not need to do this
     */
    void *addr;
    err = vspace_map_one_frame(&addr, frame_size, mi->frame, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    XINTER_DEBUG("Messaging frame mapped: [%016lx -> %016lx, size = %lx ]\n",
                 id.base, (uintptr_t )addr, frame_size);

    mi->fi.outbufsize = (frame_size >> 1);
    mi->fi.inbufsize = (frame_size >> 1);

    struct waitset *ws = get_default_waitset();

    phi->topology[phi->id].msg = mi;
    phi->topology[phi->id].local = phi;
    phi->topology[phi->id].state = XNODE_STATE_WAIT_CONNECTION;

    if (mi->is_client) {
        mi->fi.inbuf = ((uint8_t*) addr) + mi->fi.inbufsize;
        mi->fi.outbuf = addr;
        mi->fi.sendbase = id.base;

        err = interphi_connect(&mi->fi, interphi_bind_cb, &phi->topology[phi->id],
                               ws, IDC_EXPORT_FLAGS_DEFAULT);
    } else {
        mi->fi.inbuf = addr;
        mi->fi.outbuf = ((uint8_t*) addr) + mi->fi.outbufsize;
        mi->fi.sendbase = id.base + mi->fi.outbufsize;

        err = interphi_accept(&mi->fi, &phi->topology[phi->id], interphi_connect_cb,
                              ws, IDC_EXPORT_FLAGS_DEFAULT);
    }
    if (err_is_fail(err)) {
        vspace_unmap(addr);
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

#else
    struct xnode *node = &phi->topology[xphi];
    lpaddr_t offset = ((node->apt_base >> 32) - ((node->apt_base >> 34)<<2))<<32;
    err = interphi_bootstrap(phi, id.base, id.bits, offset, xphi, mi->is_client);
    if (err_is_fail(err)) {
        free(mi);
        return err;
    }

    XINTER_DEBUG("Local bootstrap succeeded. Sending to other node.\n");

    err = service_bootstrap(phi, xphi, mi->frame);
    if (err_is_fail(err)) {
        XINTER_DEBUG("Could not initialize messaging\n");
        return err;
    }
#endif

    return SYS_ERR_OK;
}

/**
 * \brief initializes the communication between the host and the card Xeon Phi
 *        drivers using a bootstraped flounder channel
 *
 * \param phi   Xeon Phi to initialize
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t interphi_init(struct xeon_phi *phi,
                       struct capref frame)
{
    errval_t err;

    assert(phi->msg == NULL);

    struct msg_info *mi = malloc(sizeof(struct msg_info));
    if (mi == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    size_t frame_size;

    if (capref_is_null(frame)) {
        err = frame_alloc(&mi->frame, XEON_PHI_INTERPHI_FRAME_SIZE, &frame_size);
        if (err_is_fail(err)) {
            return err;
        }
    } else {
        mi->frame = frame;
    }

    struct frame_identity id;
    err = invoke_frame_identify(mi->frame, &id);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    frame_size = (1UL << id.bits);

    void *addr;
    err = vspace_map_one_frame(&addr, frame_size, mi->frame, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    XINTER_DEBUG("Messaging frame mapped: [%016lx->%016lx, size = %lx]\n", id.base,
                 (uintptr_t )addr, frame_size);

    mi->is_client = phi->is_client;

    mi->fi.outbufsize = (frame_size >> 1);
    mi->fi.inbufsize = (frame_size >> 1);

    struct waitset *ws = get_default_waitset();

    phi->msg = mi;

    phi->topology[phi->id].msg = mi;
    phi->topology[phi->id].local = phi;
    phi->topology[phi->id].state = XNODE_STATE_WAIT_CONNECTION;

    if (phi->is_client) {
        mi->fi.inbuf = ((uint8_t*) addr) + mi->fi.inbufsize;
        mi->fi.outbuf = addr;
        mi->fi.sendbase = id.base;

        err = interphi_connect(&mi->fi, interphi_bind_cb, &phi->topology[phi->id],
                               ws, IDC_EXPORT_FLAGS_DEFAULT);
    } else {
        mi->fi.inbuf = addr;
        mi->fi.outbuf = ((uint8_t*) addr) + mi->fi.outbufsize;
        mi->fi.sendbase = id.base + mi->fi.outbufsize;

        err = interphi_accept(&mi->fi, &phi->topology[phi->id], interphi_connect_cb,
                              ws, IDC_EXPORT_FLAGS_DEFAULT);
    }
    if (err_is_fail(err)) {
        vspace_unmap(addr);
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    if (!phi->is_client) {
        struct xeon_phi_boot_params *bp;
        bp = (struct xeon_phi_boot_params *) (phi->apt.vbase + phi->os_offset);
        bp->msg_base = id.base;
        bp->msg_size_bits = id.bits;
    }

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Message Sending
 * ----------------------------------------------------------------------------
 */

/**
 * \brief sends a bootstrap request to the Xeon Phi client driver
 *
 * \param phi        Xeon Phi
 * \param frame_base base address of the messaging frame
 * \param frame_bits size of the messaging frame in bits
 * \param offset     offset into the SMPT
 * \param xid        ID of the other Xeon Phi
 * \param is_client  flag indicating if this is the client of the connection
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_bootstrap(struct xeon_phi *phi,
                            lpaddr_t frame_base,
                            uint8_t frame_bits,
                            lpaddr_t offset,
                            uint8_t xid,
                            uint8_t is_client)
{
#ifdef __k1om__
    USER_PANIC("This function should not be called on the Xeon Phi\n");
#endif

    XINTER_DEBUG("sending bootstrap to card. [%u] client:%u\n", xid, is_client);

    if (!rpc_start(phi->msg)) {
        assert(!"NYI");
    }

    phi->msg->rpc_err = SYS_ERR_OK;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&phi->msg->queue);
    if (msg_st == NULL) {
        rpc_done(phi->msg);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = bootstrap_call_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.bootstrap.xid = xid;
    svc_st->args.bootstrap.offset = offset;
    svc_st->args.bootstrap.is_client = is_client;
    svc_st->args.bootstrap.base = frame_base;
    svc_st->args.bootstrap.bits = frame_bits;

    txq_send(msg_st);

    rpc_wait_done(phi->msg);

    return phi->msg->rpc_err;
}

/**
 * \brief sends a spawn request to the Xeon Phi driver
 *
 * \param phi       Xeon Phi
 * \param core      which core to spawn the domain on
 * \param cmdline   Commandline of the domain to spawn
 * \param domain    Domain identifier returned
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_spawn(struct xeon_phi *phi,
                        uint8_t core,
                        char *cmdline,
                        uint64_t *domain)
{
    XINTER_DEBUG("spawning %s on core %u\n", cmdline, core);

    if (phi->msg->binding == NULL) {
        assert(!"NYI");
    }

    if (!rpc_start(phi->msg)) {
        assert(!"NYI");
    }

    phi->msg->rpc_err = SYS_ERR_OK;
    phi->msg->rpc_data = 0x0;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&phi->msg->queue);
    if (msg_st == NULL) {
        rpc_done(phi->msg);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = spawn_call_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.spawn_call.cmdline = cmdline;
    svc_st->args.spawn_call.core = core;

    txq_send(msg_st);

    rpc_wait_done(phi->msg);

    if (err_is_ok(phi->msg->rpc_err)) {
        if (domain) {
            *domain = phi->msg->rpc_data;
        }
    }

    return phi->msg->rpc_err;
}

/**
 * \brief sends a spawn request to the Xeon Phi driver
 *
 * \param phi       Xeon Phi
 * \param core      which core to spawn the domain on
 * \param cmdline   Commandline of the domain to spawn
 * \param cap       Cap to hand over to the domain at boot
 * \param domain    Domain identifier returned
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_spawn_with_cap(struct xeon_phi *phi,
                                 uint8_t core,
                                 char *cmdline,
                                 struct capref cap,
                                 uint64_t *domain)
{
    errval_t err;

    XINTER_DEBUG("spawning %s with cap on core %u\n", cmdline, core);

    if (phi->msg->binding == NULL) {
        assert(!"NYI");
    }

    struct frame_identity id;
    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    if (!rpc_start(phi->msg)) {
        assert(!"NYI");
    }

    phi->msg->rpc_err = SYS_ERR_OK;
    phi->msg->rpc_data = 0x0;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&phi->msg->queue);
    if (msg_st == NULL) {
        rpc_done(phi->msg);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = spawn_with_cap_call_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.spawn_call.cmdline = cmdline;
    svc_st->args.spawn_call.core = core;
    svc_st->args.spawn_call.cap_size_bits = id.bits;
    svc_st->args.spawn_call.cap_base = id.base;

    txq_send(msg_st);

    rpc_wait_done(phi->msg);

    if (err_is_ok(phi->msg->rpc_err)) {
        if (domain) {
            *domain = phi->msg->rpc_data;
        }
    }

    return phi->msg->rpc_err;
}

/**
 * \brief sends a kill request for a domain
 *
 * \param phi       Xeon Phi
 * \param domain    Domain identifier
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_kill(struct xeon_phi *phi,
                       uint64_t domain)
{
    if (phi->msg->binding == NULL) {
        assert(!"NYI");
    }

    XINTER_DEBUG("sending kill signal for domain:%lu\n", domain);

    if (!rpc_start(phi->msg)) {
        assert(!"NYI");
    }

    phi->msg->rpc_err = SYS_ERR_OK;
    phi->msg->rpc_data = 0x0;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&phi->msg->queue);
    if (msg_st == NULL) {
        rpc_done(phi->msg);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = kill_call_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.kill.domainid = domain;

    txq_send(msg_st);

    rpc_wait_done(phi->msg);

    return phi->msg->rpc_err;
}

