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
#include <xeon_phi/xeon_phi_domain.h>

#include "xeon_phi_internal.h"
#include "interphi.h"
#include "smpt.h"
#include "domain.h"
#include "service.h"
#include "xphi_service.h"
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
            size_t cmdlen;
            uint8_t flags;
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
        struct
        {
            lpaddr_t msgbase;
            uint8_t msgbits;
            char *iface;
            xphi_dom_id_t source;
            uint64_t usrdata;
            xphi_dom_id_t target;
            xphi_chan_type_t type;
        } open;
        struct
        {
            char *name;
            xphi_dom_id_t domid;
            uintptr_t state;
        } domain;
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

static struct txq_msg_st *rpc_preamble(struct msg_info *mi)
{
    assert(mi);

    if (mi->binding == NULL) {
        assert(!"NYI");
    }

    if (!rpc_start(mi)) {
        XINTER_DEBUG("waiting until previous rpc is finished\n");
        rpc_wait_done(mi);
        rpc_start(mi);
    }

    mi->rpc_err = SYS_ERR_OK;
    mi->rpc_data = 0x0;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&mi->queue);
    if (msg_st == NULL) {
        rpc_done(mi);
    }

    msg_st->cleanup = NULL;

    return msg_st;
}

/*
 * ----------------------------------------------------------------------------
 * Helper functions
 * ----------------------------------------------------------------------------
 */
static errval_t spawn_cmdline_extract_argv(char *cmdline,
                                           size_t cmdlen,
                                           char *argv[],
                                           uint8_t argcmax)
{
       int i = 0;
       size_t pos = 0;
       while (pos < cmdlen && i < argcmax) {
           argv[i++] = &cmdline[pos];
           char *end = memchr(&cmdline[pos], '\0', cmdlen - pos);
           if (end == NULL) {
               return SPAWN_ERR_GET_CMDLINE_ARGS;

           }
           pos = end - cmdline + 1;
       }
       assert(i <= argcmax);
       argv[i] = NULL;

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Message Send Handlers
 * ----------------------------------------------------------------------------
 */

#ifdef __k1om__

static errval_t domain_wait_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    size_t length = strlen(st->args.domain.name) + 1;

    return interphi_domain_wait_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                         st->args.domain.name, length,
                                         st->args.domain.state);
}

#else

static errval_t domain_wait_response_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_domain_wait_response__tx(msg_st->queue->binding,
                    TXQCONT(msg_st),
                    st->args.domain.domid,
                    st->args.domain.state,
                    msg_st->err);
}
#endif

#ifdef __k1om__

static errval_t domain_lookup_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    size_t length = strlen(st->args.domain.name) + 1;

    return interphi_domain_lookup_call__tx(msg_st->queue->binding,
                                           TXQCONT(msg_st), st->args.domain.name,
                                           length);
}

#else

static errval_t domain_lookup_response_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_domain_lookup_response__tx(msg_st->queue->binding,
                    TXQCONT(msg_st),
                    st->args.domain.domid, msg_st->err);
}

#endif

#ifdef __k1om__

static errval_t domain_register_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    size_t length = strlen(st->args.domain.name) + 1;

    return interphi_domain_register_call__tx(msg_st->queue->binding,
                                             TXQCONT(msg_st),
                                             st->args.domain.name, length,
                                             st->args.domain.domid);
}

#else

static errval_t domain_register_response_tx(struct txq_msg_st *msg_st)
{
    return interphi_domain_register_response__tx(msg_st->queue->binding,
                    TXQCONT(msg_st), SYS_ERR_OK);
}

#endif

static errval_t spawn_response_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_spawn_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                       st->args.spawn_reply.domainid, msg_st->err);
}

static errval_t spawn_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_spawn_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                   st->args.spawn_call.core,
                                   st->args.spawn_call.cmdline,
                                   st->args.spawn_call.cmdlen,
                                   st->args.spawn_call.flags);
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

    return interphi_spawn_with_cap_call__tx(msg_st->queue->binding,
                                            TXQCONT(msg_st),
                                            st->args.spawn_call.core,
                                            st->args.spawn_call.cmdline,
                                            st->args.spawn_call.cmdlen,
                                            st->args.spawn_call.flags,
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
    return interphi_bootstrap_response__tx(msg_st->queue->binding,
                                           TXQCONT(msg_st), msg_st->err);
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

static errval_t chan_open_call_tx(struct txq_msg_st *msg_st)
{
    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    return interphi_chan_open_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                       st->args.open.source, st->args.open.target,
                                       st->args.open.usrdata,
                                       st->args.open.msgbase,
                                       st->args.open.msgbits, st->args.open.type);
}

static errval_t chan_open_response_tx(struct txq_msg_st *msg_st)
{
    return interphi_chan_open_response__tx(msg_st->queue->binding,
                                           TXQCONT(msg_st), msg_st->err);
}

/*
 * ----------------------------------------------------------------------------
 * Message Callbacks
 * ----------------------------------------------------------------------------
 */

static void domain_wait_call_rx(struct interphi_binding *_binding,
                                char *name,
                                size_t length,
                                uintptr_t state)
{
#ifdef __k1om__
    USER_PANIC("domain_wait_call_rx: not supported on the Xeon Phi\n");
#else
    XINTER_DEBUG("domain_wait_call_rx: {%s}\n", name);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = domain_wait_response_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    st->args.domain.state = state;

    msg_st->err = domain_wait(name, local_node, (void *)state, &st->args.domain.domid);
    switch(err_no(msg_st->err)) {
        case SYS_ERR_OK:
            /* there was a record, reply */
            txq_send(msg_st);
            break;
        case OCT_ERR_NO_RECORD:
            /* trigger installed */
            txq_msg_st_free(msg_st);
            break;
        default:
            /* error condition */
            txq_send(msg_st);
        break;
    }
    free(name);
#endif
}

static void domain_wait_response_rx(struct interphi_binding *_binding,
                                    xphi_dom_id_t domain,
                                    uintptr_t state,
                                    errval_t msgerr)
{
#ifdef __k1om__
    XINTER_DEBUG("domain_wait_response_rx: domid:%lx, st:%p,  %s\n", domain,
                 (void * )state, err_getstring(msgerr));

    struct xphi_svc_st *st = (struct xphi_svc_st *) state;

    xeon_phi_service_domain_wait_response(st, msgerr, domain);
#else
    USER_PANIC("domain_wait_call_rx: not supported on the host\n");
#endif
}

static void domain_lookup_call_rx(struct interphi_binding *_binding,
                                  char *name,
                                  size_t length)
{
#ifdef __k1om__
    USER_PANIC("domain_lookup_call_rx: not supported on the Xeon Phi\n");
#else
    XINTER_DEBUG("domain_lookup_call_rx: {%s}\n", name);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = domain_lookup_response_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *st = (struct interphi_msg_st *)msg_st;

    msg_st->err = domain_lookup(name, &st->args.domain.domid);

    free(name);

    txq_send(msg_st);
#endif
}

static void domain_lookup_response_rx(struct interphi_binding *_binding,
                                      xphi_dom_id_t domain,
                                      errval_t msgerr)
{
#ifdef __k1om__
    XINTER_DEBUG("domain_lookup_response_rx: %lx, %s\n", domain,
                 err_getstring(msgerr));

    struct xnode *local_node = _binding->st;

    local_node->msg->rpc_err = msgerr;
    rpc_done(local_node->msg);
#else
    USER_PANIC("domain_lookup_response_rx: not supported on the host\n");
#endif
}

static void domain_register_call_rx(struct interphi_binding *_binding,
                                    char *name,
                                    size_t length,
                                    xphi_dom_id_t domid)
{
#ifdef __k1om__
    /* register calls on the K1OM are not valid */
    USER_PANIC("domain_register_call_rx: not supported on the Xeon Phi\n");
#else
    XINTER_DEBUG("domain_register_call_rx: {%s} @ domid:%lx\n", name, domid);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->err = SYS_ERR_OK;
    msg_st->send = domain_register_response_tx;
    msg_st->cleanup = NULL;

    msg_st->err = domain_register(name, domid);

    free(name);

    txq_send(msg_st);
#endif
}

static void domain_register_response_rx(struct interphi_binding *_binding,
                                        errval_t msgerr)
{
#ifdef __k1om__
    XINTER_DEBUG("domain_register_response_rx:%s\n", err_getstring(msgerr));

    struct xnode *local_node = _binding->st;

    local_node->msg->rpc_err = msgerr;
    rpc_done(local_node->msg);
#else
    USER_PANIC("domain_register_response_rx: not supported on the host\n");
#endif
}

static void spawn_call_rx(struct interphi_binding *_binding,
                          uint8_t core,
                          char *cmdline,
                          size_t length,
                          uint8_t flags)
{
    XINTER_DEBUG("spawn_call_rx: {%s} of length %lu, @ core:%u\n", cmdline,
                 length, core);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = spawn_response_tx;
    msg_st->cleanup = NULL;

    char *argv[MAX_CMDLINE_ARGS+1];
    msg_st->err = spawn_cmdline_extract_argv(cmdline, length, argv, MAX_CMDLINE_ARGS);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }
    argv[MAX_CMDLINE_ARGS] = NULL;

    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    domainid_t domid;

    /*
     * TODO: check if we have that core present...
     */

    msg_st->err = spawn_program(core, cmdline, argv, NULL, flags, &domid);
    if (err_is_ok(msg_st->err)) {
#ifdef __k1om__
        uint8_t is_host = 0x0;
#else
        uint8_t is_host = 0x1;
#endif
        st->args.spawn_reply.domainid = xeon_phi_domain_build_id(phi->id, core,
                                                                 is_host, domid);
    }

    free(cmdline);

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
                                   uint8_t flags,
                                   uint64_t cap_base,
                                   uint8_t cap_size_bits)
{
    XINTER_DEBUG("spawn_with_cap_call_rx: {%s} of length %lu @ core:%u\n", cmdline,
                 length, core);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = spawn_with_cap_response_tx;
    msg_st->cleanup = NULL;

    char *argv[MAX_CMDLINE_ARGS+1];
    msg_st->err = spawn_cmdline_extract_argv(cmdline, length, argv, MAX_CMDLINE_ARGS);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }
    argv[MAX_CMDLINE_ARGS] = NULL;

    struct interphi_msg_st *st = (struct interphi_msg_st *) msg_st;

    struct capref cap;
    msg_st->err = sysmem_cap_request(cap_base, cap_size_bits, &cap);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }

    domainid_t domid;
    msg_st->err = spawn_program_with_caps(core, cmdline, argv, NULL, NULL_CAP,
                                          cap, flags, &domid);
    if (err_is_ok(msg_st->err)) {
#ifdef __k1om__
        st->args.spawn_reply.domainid = xeon_phi_domain_build_id(
                        disp_xeon_phi_id(), core, 0, domid);
#else
        st->args.spawn_reply.domainid = xeon_phi_domain_build_id(
                        XEON_PHI_DOMAIN_HOST, core, 1, domid);
#endif
    }

    free(cmdline);

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
    XINTER_DEBUG("bootstrap_call_rx: {%016lx, %02x} of:%016lx, xid:%u, c:%u\n",
                 base, bits, offset, xid, is_client);

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

static void chan_open_call_rx(struct interphi_binding *_binding,
                              uint64_t source_did,
                              uint64_t target_did,
                              uint64_t usrdata,
                              uint64_t msgbase,
                              uint8_t msgbits,
                              uint8_t type)
{
    XINTER_DEBUG("chan_open_call_rx: %lx -> %lx\n", source_did, target_did);

    struct xnode *local_node = _binding->st;

    struct xeon_phi *phi = local_node->local;
    assert(phi);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&local_node->msg->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = chan_open_response_tx;
    msg_st->cleanup = NULL;

    struct capref msgcap;

    lpaddr_t offset = smpt_get_coprocessor_address(phi, local_node->id);

    msgbase += offset;

    msg_st->err = sysmem_cap_request(msgbase, msgbits, &msgcap);
    if (err_is_fail(msg_st->err)) {
        txq_send(msg_st);
        return;
    }

    msg_st->err = xeon_phi_service_open_channel(msgcap, type, target_did,
                                                source_did, usrdata);
    if (err_is_fail(msg_st->err)) {
        sysmem_cap_return(msgcap);
    }
    txq_send(msg_st);
}

static void chan_open_response_rx(struct interphi_binding *_binding,
                                  errval_t msgerr)
{
    XINTER_DEBUG("chan_open_did_response_rx: %s\n", err_getstring(msgerr));

    struct xnode *local_node = _binding->st;

    local_node->msg->rpc_err = msgerr;

    rpc_done(local_node->msg);
}

struct interphi_rx_vtbl rx_vtbl = {
    .domain_lookup_call = domain_lookup_call_rx,
    .domain_lookup_response = domain_lookup_response_rx,
    .domain_wait_call = domain_wait_call_rx,
    .domain_wait_response = domain_wait_response_rx,
    .domain_register_call = domain_register_call_rx,
    .domain_register_response = domain_register_response_rx,
    .chan_open_call = chan_open_call_rx,
    .chan_open_response = chan_open_response_rx,
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
    XINTER_DEBUG("interphi_bind_cb: driver bound  %s\n", err_getstring(err));

    assert(_binding);

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

    struct msg_info *mi = calloc(1, sizeof(struct msg_info));
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

    struct xnode *node = &phi->topology[xphi];

    node->msg = mi;
    node->local = phi;
    node->state = XNODE_STATE_WAIT_CONNECTION;

    if (mi->is_client) {
        mi->fi.inbuf = ((uint8_t*) addr) + mi->fi.inbufsize;
        mi->fi.outbuf = addr;
        mi->fi.sendbase = id.base;

        err = interphi_connect(&mi->fi, interphi_bind_cb, node,
                               ws, IDC_EXPORT_FLAGS_DEFAULT);
    } else {
        mi->fi.inbuf = addr;
        mi->fi.outbuf = ((uint8_t*) addr) + mi->fi.outbufsize;
        mi->fi.sendbase = id.base + mi->fi.outbufsize;

        err = interphi_accept(&mi->fi, node, interphi_connect_cb,
                              ws, IDC_EXPORT_FLAGS_DEFAULT);
    }
    if (err_is_fail(err)) {
        vspace_unmap(addr);
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    if (mi->is_client) {
        XINTER_DEBUG("Waiting for connect callback...\n");
        while(node->state == XNODE_STATE_WAIT_CONNECTION) {
            messages_wait_and_handle_next();
        }
        XINTER_DEBUG("connected to pier.\n");
    }

    phi->connected++;

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

    struct msg_info *mi = calloc(1, sizeof(struct msg_info));
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

    XINTER_DEBUG("Messaging frame mapped: [%016lx->%016lx, size = %lx]\n",
                 id.base, (uintptr_t )addr, frame_size);

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

        err = interphi_accept(&mi->fi, &phi->topology[phi->id],
                              interphi_connect_cb, ws, IDC_EXPORT_FLAGS_DEFAULT);
    }
    if (err_is_fail(err)) {
        vspace_unmap(addr);
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }



    phi->connected = 1;

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

    struct msg_info *mi = phi->msg;

    struct txq_msg_st *msg_st = rpc_preamble(mi);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = bootstrap_call_tx;

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
 * \param node      Xeon Phi Node
 * \param core      which core to spawn the domain on
 * \param cmdline   Commandline of the domain to spawn (marshalled)
 * \param cmdlen    length of the command line
 * \param domain    Domain identifier returned
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_spawn(struct xnode *node,
                        uint8_t core,
                        char *cmdline,
                        size_t cmdlen,
                        uint8_t flags,
                        uint64_t *domain)
{
    XINTER_DEBUG("spawning %s on core %u\n", cmdline, core);
    struct msg_info *mi = node->msg;

    struct txq_msg_st *msg_st = rpc_preamble(mi);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = spawn_call_tx;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.spawn_call.cmdline = cmdline;
    svc_st->args.spawn_call.cmdlen = cmdlen;
    svc_st->args.spawn_call.core = core;
    svc_st->args.spawn_call.flags = flags;

    txq_send(msg_st);

    rpc_wait_done(node->msg);

    if (err_is_ok(node->msg->rpc_err)) {
        if (domain) {
            *domain = node->msg->rpc_data;
        }
    }

    return node->msg->rpc_err;
}

/**
 * \brief sends a spawn request to the Xeon Phi driver
 *
 * \param node      Xeon Phi Node
 * \param core      which core to spawn the domain on
 * \param cmdline   Commandline of the domain to spawn (marshalled args)
 * \param cmdlen    length of the cmd line
 * \param cap       Cap to hand over to the domain at boot
 * \param domain    Domain identifier returned
 *
 * \returns SYS_ERR_OK on success
 *          errval on faiure
 */
errval_t interphi_spawn_with_cap(struct xnode *node,
                                 uint8_t core,
                                 char *cmdline,
                                 size_t cmdlen,
                                 uint8_t flags,
                                 struct capref cap,
                                 uint64_t *domain)
{
    errval_t err;
    struct msg_info *mi = node->msg;

    XINTER_DEBUG("spawning %s with cap on core %u\n", cmdline, core);

    struct frame_identity id;
    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    struct txq_msg_st *msg_st = rpc_preamble(mi);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = spawn_with_cap_call_tx;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.spawn_call.cmdline = cmdline;
    svc_st->args.spawn_call.cmdlen = cmdlen;
    svc_st->args.spawn_call.core = core;
    svc_st->args.spawn_call.flags = flags;
    svc_st->args.spawn_call.cap_size_bits = id.bits;
    svc_st->args.spawn_call.cap_base = id.base;

    txq_send(msg_st);

    rpc_wait_done(node->msg);

    if (err_is_ok(node->msg->rpc_err)) {
        if (domain) {
            *domain = node->msg->rpc_data;
        }
    }

    return node->msg->rpc_err;
}

/**
 * \brief sends a kill request for a domain
 *
 * \param node      Target Xeon Phi node
 * \param domain    Domain identifier
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t interphi_kill(struct xnode *node,
                       xphi_dom_id_t domain)
{
    XINTER_DEBUG("sending kill signal for domain:%lu\n", domain);

    struct msg_info *mi = node->msg;

    struct txq_msg_st *msg_st = rpc_preamble(mi);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = kill_call_tx;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.kill.domainid = domain;

    txq_send(msg_st);

    rpc_wait_done(mi);

    return mi->rpc_err;
}

/**
 * \brief sends a channel open messages to another Xeon Phi driver
 *
 * \param node      Xeon Phi Node to send the message to
 * \param target    target domain id
 * \param source    source domain id
 * \param usedata   usr specified data
 * \param msgframe  capability of the messaging frame
 * \param type      Channel type
 *
 * \returns SYS_ERR_OK on success
 */
errval_t interphi_chan_open(struct xnode *node,
                            xphi_dom_id_t target,
                            xphi_dom_id_t source,
                            uint64_t usrdata,
                            struct capref msgframe,
                            xphi_chan_type_t type)
{
    errval_t err;

    XINTER_DEBUG("sending channel open to domain {%lx}\n", target);

    struct msg_info *mi = node->msg;

    struct frame_identity id;
    err = invoke_frame_identify(msgframe, &id);
    if (err_is_fail(err)) {
        return err;
    }

    struct txq_msg_st *msg_st = rpc_preamble(mi);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.open.msgbase = id.base;
    svc_st->args.open.msgbits = id.bits;
    svc_st->args.open.source = source;
    svc_st->args.open.usrdata = usrdata;
    svc_st->args.open.type = type;

    if (target) {
        msg_st->send = chan_open_call_tx;
        svc_st->args.open.target = target;
    } else {
        rpc_done(node->msg);
        txq_msg_st_free(msg_st);
        return -1;
    }

    txq_send(msg_st);

    rpc_wait_done(mi);

    return mi->rpc_err;
}

/**
 * \brief registers a ready domain with the Xeon Phi Domain Service
 *
 * \param node  Xeon Phi Node to send the message to
 * \param name  Name to register
 * \param domid Xeon Phi Domain ID
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t interphi_domain_register(struct xnode *node,
                                  char *name,
                                  xphi_dom_id_t domid)
{
#ifdef __k1om__
    XINTER_DEBUG("domain register {%s} with domainid:%lx @ xnode:%u\n", name,
                 domid, node->id);

    assert(node->msg);

    struct msg_info *mi = node->msg;

    struct txq_msg_st *msg_st = rpc_preamble(mi);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    msg_st->send = domain_register_call_tx;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.domain.domid = domid;
    svc_st->args.domain.name = name;

    txq_send(msg_st);

    rpc_wait_done(mi);

    return mi->rpc_err;
#else
    USER_PANIC("interphi_domain_lookup: not supporte on host.\n");

    return SYS_ERR_OK;
#endif
}

/**
 * \brief checks if a domain is running and returns its domain id if it is.
 *
 * \param node  Xeon Phi Node to send the message to
 * \param name  Name of the Domain
 * \param domid returned Xeon Phi Domain ID
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t interphi_domain_lookup(struct xnode *node,
                                char *name,
                                xphi_dom_id_t *retdomid)
{
#ifdef __k1om__
    XINTER_DEBUG("domain lookup {%s} @ xnode:%u\n", name, node->id);

    struct msg_info *mi = node->msg;

    struct txq_msg_st *msg_st = rpc_preamble(mi);
    if (msg_st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = domain_lookup_call_tx;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.domain.name = name;

    txq_send(msg_st);

    rpc_wait_done(mi);

    return mi->rpc_err;
#else
    USER_PANIC("interphi_domain_lookup: not supporte on host.\n");

    return SYS_ERR_OK;
#endif
}

/**
 * \brief checks if a domain is running and installs a trigger to reply
 *
 * \param node  Xeon Phi Node to send the message to
 * \param name  Name of the Domain
 * \param state user state
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t interphi_domain_wait(struct xnode *node,
                              char *name,
                              void *state)
{
#ifdef __k1om__
    XINTER_DEBUG("domain wait {%s} @ xnode:%u\n", name, node->id);

    assert(node->msg);

    struct msg_info *mi = node->msg;
    if (mi->binding == NULL) {
        assert(!"NYI");
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&mi->queue);
    if (msg_st == NULL) {
        rpc_done(node->msg);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = domain_wait_call_tx;
    msg_st->cleanup = NULL;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.domain.name = name;
    svc_st->args.domain.state = (uintptr_t) state;

    txq_send(msg_st);
#else
    USER_PANIC("interphi_domain_wait: not supporte on host\n");
#endif
    return SYS_ERR_OK;
}

/**
 * \brief sends a reply when the Octopus trigger fired
 *
 * \param node  Xeon Phi Node
 * \param domid Xeon Phi Domain ID
 * \param err   Outcome of the reply
 * \param state State pointer supplied by the card.
 *
 * \returns SYS_ERR_OK on success
 */
errval_t interphi_domain_wait_reply(struct xnode *node,
                                    errval_t err,
                                    void *state,
                                    xphi_dom_id_t domid)
{
#ifndef __k1om__
    XINTER_DEBUG("domain interphi_domain_wait_reply domid:%lx @ xnode:%u, st:%p\n",
                    domid, node->id, state);

    struct msg_info *mi = node->msg;
    if (mi->binding == NULL) {
        assert(!"NYI");
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&mi->queue);
    if (msg_st == NULL) {
        rpc_done(node->msg);
        return LIB_ERR_MALLOC_FAIL;
    }

    msg_st->send = domain_wait_response_tx;
    msg_st->cleanup = NULL;
    msg_st->err = err;

    struct interphi_msg_st *svc_st = (struct interphi_msg_st *) msg_st;

    svc_st->args.domain.domid = domid;
    svc_st->args.domain.state = (uintptr_t)state;

    txq_send(msg_st);

#else
    USER_PANIC("interphi_domain_wait_reply: Not supported on Xeon Phi\n");
#endif
    return SYS_ERR_OK;
}
