/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
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
#include <xeon_phi/xeon_phi.h>

#include <if/xeon_phi_driver_defs.h>

#include "xeon_phi_internal.h"
#include "service.h"
#include "interphi.h"
#include "dma_service.h"
#include "smpt.h"

static uint32_t is_exported;

static iref_t svc_iref;

errval_t bootstrap_errors[XEON_PHI_NUM_MAX];

/*
 * ---------------------------------------------------------------------------
 * Intra Xeon Phi Driver Communication bootstrap
 */

struct msg_open_st
{
    struct xeon_phi_driver_binding *b;
    uint64_t base;
    uint8_t bits;
    errval_t err;
};

static void bootstrap_response_tx(void *a)
{
    errval_t err;

    struct msg_open_st *st = a;

    struct event_closure txcont = MKCONT(free, a);

    err = xeon_phi_driver_bootstrap_response__tx(st->b, txcont, st->err);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            txcont = MKCONT(bootstrap_response_tx, a);
            err = st->b->register_send(st->b, ws, txcont);
            if (err_is_fail(err)) {
                XSERVICE_DEBUG("Could not send!");
            }
        }
    }
}



static void bootstrap_response_rx(struct xeon_phi_driver_binding *b,
                                  errval_t msgerr)
{
    struct xnode *node = b->st;

    XSERVICE_DEBUG("Xeon Phi Node %u recv bootstrap_response_rx: %s\n",
                   node->local->id, err_getstring(msgerr));

    node->bootstrap_done = 0x1;
    node->err = msgerr;
}

static void bootstrap_call_rx(struct xeon_phi_driver_binding *b,
                              uint64_t base,
                              uint8_t bits)
{
    errval_t err;

    struct xnode *node = b->st;

    XSERVICE_DEBUG("Xeon Phi Node %u bootstrap_call_rx: [0x%016lx] from %u\n",
                   node->local->id, base, node->id);

    lpaddr_t offset = ((node->apt_base >> 32) - ((node->apt_base >> 34) << 2)) << 32;

    err = interphi_bootstrap(node->local, base, bits, offset, node->id, 0x1);

    struct msg_open_st *st = malloc(sizeof(*st));
    if (st == NULL) {
        USER_PANIC("could not allocate state");
    }

    st->err = err;
    st->b = node->binding;

    bootstrap_response_tx(st);
}

static void bootstrap_call_tx(void *a)
{
    errval_t err;

    struct msg_open_st *st = a;

    struct event_closure txcont = MKCONT(free, a);

    err = xeon_phi_driver_bootstrap_call__tx(st->b, txcont, st->base, st->bits);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            txcont = MKCONT(bootstrap_call_tx, a);
            err = st->b->register_send(st->b, ws, txcont);
            if (err_is_fail(err)) {
                XSERVICE_DEBUG("Could not send!");
            }
        }
    }
}



/**
 * \brief registers an intra card communication frame
 *
 * \param phi      the local xeon phi card
 * \param xphi_id  target xeon phi id
 */
errval_t service_bootstrap(struct xeon_phi *phi,
                           uint8_t xphi_id,
                           struct capref frame)
{
    assert(xphi_id < XEON_PHI_NUM_MAX);

    if (phi->id == xphi_id) {
        XSERVICE_DEBUG("The IDs were the same. Skipping.\n");
        return SYS_ERR_OK;
    }

    XSERVICE_DEBUG("sending bootstrap to node {xid:%u}.\n", xphi_id);

    struct xnode *node = &phi->topology[xphi_id];

    assert(node->bootstrap_done == 0);

    errval_t err;
    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    if (node->state != XNODE_STATE_READY) {
        return -1;  // TODO: error code
    }

    struct msg_open_st *st = malloc(sizeof(struct msg_open_st));
    if (st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    node->err = SYS_ERR_OK;

    st->b = node->binding;
    st->base = id.base;
    st->bits = id.bits;

    bootstrap_call_tx(st);

    XSERVICE_DEBUG("waiting for bootstrap done:%u.\n", xphi_id);

    while(!node->bootstrap_done) {
        xeon_phi_event_poll(0x1);
    }

    return node->err;
}

/*
 * ---------------------------------------------------------------------------
 * Intra Xeon Phi Driver Communication Regigistration
 */

static void register_response_send(void *a)
{
    errval_t err;

    struct xnode *topology = a;

    struct event_closure txcont = MKCONT(NULL, a);

    if (topology->state == XNODE_STATE_READY) {
        err = SYS_ERR_OK;
    } else {
        err = -1;  // TODO> ERROR NUMBEr
    }

    struct xeon_phi *phi = topology->local;

    err = xeon_phi_driver_register_response__tx(topology->binding, txcont, err,
                                                phi->apt.pbase, phi->apt.length);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            txcont = MKCONT(register_response_send, a);
            err = topology->binding->register_send(topology->binding, ws, txcont);
            if (err_is_fail(err)) {
                topology->state = XNODE_STATE_FAILURE;
            }
        }
    }
}

/**
 *
 */
static void register_call_recv(struct xeon_phi_driver_binding *_binding,
                               uint8_t id,
                               uint64_t other_apt_base,
                               uint64_t other_apt_size)
{
    struct xeon_phi *phi = _binding->st;

    interphi_wait_for_client(phi);
    
    assert(id < XEON_PHI_NUM_MAX);
    phi->topology[id].binding = _binding;
    phi->topology[id].state = XNODE_STATE_READY;
    phi->topology[id].apt_base = other_apt_base;
    phi->topology[id].apt_size = other_apt_size;
    phi->connected++;

    if (!smpt_set_coprocessor_address(phi, id, other_apt_base)) {
        assert(!"Setting page table entry failed");  // TODO: proper error handling
    };

    XSERVICE_DEBUG("Xeon Phi Node %u: New register call: id=0x%x @ [0x%016lx]\n",
                   phi->id, id, other_apt_base);

    _binding->st = &phi->topology[id];

    register_response_send(&phi->topology[id]);
}

/**
 *
 */
static void register_response_recv(struct xeon_phi_driver_binding *_binding,
                                   xeon_phi_driver_errval_t msgerr,
                                   uint64_t other_apt_base,
                                   uint64_t other_apt_size)
{
    assert(((struct xnode * )(_binding->st))->binding == _binding);

    struct xnode *topology = _binding->st;

    if (err_is_fail(msgerr)) {
        topology->state = XNODE_STATE_FAILURE;
        XSERVICE_DEBUG("Xeon Phi node %u: Registering FAILED\n",
                       topology->local->id);
    } else {
        topology->local->connected++;
        topology->state = XNODE_STATE_READY;
        topology->apt_base = other_apt_base;
        topology->apt_size = other_apt_size;
        XSERVICE_DEBUG("Xeon Phi node %u: Registering response. Node %u @ 0x%016lx\n",
                       topology->local->id, topology->id, topology->apt_base);

        smpt_set_coprocessor_address(topology->local, topology->id, other_apt_base);
    }
}

static void register_call_sent_cb(void *a)
{

}

static void register_call_send(void *a)
{
    errval_t err;

    struct xnode *topology = a;

    struct xeon_phi *phi = topology->local;

    struct event_closure txcont = MKCONT(register_call_sent_cb, a);

    topology->state = XNODE_STATE_REGISTERING;

    err = xeon_phi_driver_register_call__tx(topology->binding, txcont, phi->id,
                                            phi->apt.pbase, phi->apt.length);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            txcont = MKCONT(register_call_send, a);
            err = topology->binding->register_send(topology->binding, ws, txcont);
            if (err_is_fail(err)) {
                topology->state = XNODE_STATE_FAILURE;
            }
        }
    }
}

/// Receive handler table
static struct xeon_phi_driver_rx_vtbl xps_rx_vtbl = {
    .register_call = register_call_recv,
    .register_response = register_response_recv,
    .bootstrap_call = bootstrap_call_rx,
    .bootstrap_response = bootstrap_response_rx
};

/*
 * ---------------------------------------------------------------------------
 * Service Setup
 */
static void svc_bind_cb(void *st,
                        errval_t err,
                        struct xeon_phi_driver_binding *b)
{
    struct xnode *node = st;
    b->rx_vtbl = xps_rx_vtbl;
    node->binding = b;
    b->st = node;
    node->state = XNODE_STATE_REGISTERING;
}

static errval_t svc_register(struct xnode *node)
{
    errval_t err;

    XSERVICE_DEBUG("Initiate binding to Xeon Phi node %i @ iref=0x%x\n", node->id,
                   node->iref);

    err = xeon_phi_driver_bind(node->iref, svc_bind_cb, node, get_default_waitset(),
    IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        node->state = XNODE_STATE_FAILURE;
        return err;
    }

    return SYS_ERR_OK;
}

static errval_t svc_connect_cb(void *st,
                               struct xeon_phi_driver_binding *b)
{
    XSERVICE_DEBUG("Xeon Phi Node %u got a new connection to other node.\n",
                   (struct xeon_phi * st)->id);

    b->st = st;
    b->rx_vtbl = xps_rx_vtbl;
    return SYS_ERR_OK;
}

static void svc_export_cb(void *st,
                          errval_t err,
                          iref_t iref)
{
    if (err_is_fail(err)) {
        svc_iref = 0x0;
        return;
    }

    svc_iref = iref;

    struct xeon_phi *phi = st;
    phi->iref = iref;

    is_exported = 0x1;
}

/**
 * \brief initializes the service
 *
 * \param iref  returns the iref of the initialized service
 *
 * \return SYS_ERR_OK on success
 */
errval_t service_init(struct xeon_phi *phi)
{
    errval_t err;

    for (uint32_t i = 0; i < XEON_PHI_NUM_MAX; ++i) {
        phi->topology[i].local = phi;
        phi->topology[i].id = i;
        phi->topology[i].state = XNODE_STATE_NONE;
    }

    err = xeon_phi_driver_export(phi, svc_export_cb, svc_connect_cb,
                                 get_default_waitset(),
                                 IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (!is_exported) {
        messages_wait_and_handle_next();
    }

    if (svc_iref == 0x0) {
        return -1;
    }

    return SYS_ERR_OK;

}

/**
 * \brief registers the local service with the other Xeon Phi drivers
 *        in the topology
 *
 * \param phi   pointer to the local card structure
 * \param irefs the irefs of the other cards
 * \param num   the number of irefs in the array
 */
errval_t service_register(struct xeon_phi *phi,
                          iref_t *irefs,
                          uint8_t num)
{
    errval_t err;

    struct xnode *xnode;
    XSERVICE_DEBUG("start binding to %u Xeon Phi nodes\n", num - 1);
    for (uint32_t i = 0; i < num; ++i) {
        xnode = &phi->topology[i];
        xnode->local = phi;
        if (i == phi->id) {
            xnode->iref = phi->iref;
            xnode->id = i;
            xnode->state = XNODE_STATE_READY;
            xnode->apt_base = phi->apt.pbase;
            xnode->apt_size = phi->apt.length;
            continue;
        }

        xnode->iref = irefs[i];
        xnode->id = i;
        xnode->state = XNODE_STATE_NONE;
        svc_register(xnode);
        while (xnode->state == XNODE_STATE_NONE) {
            err = xeon_phi_event_poll(0x1);
            if (err_is_fail(err)) {
                return err;
            }
        }
    }

    XSERVICE_DEBUG("Start registering with %u Xeon Phi nodes\n", num - 1);

    for (uint32_t i = 0; i < num; ++i) {
        if (i == phi->id) {
            continue;
        }
        xnode = &phi->topology[i];
        register_call_send(xnode);
        while (xnode->state == XNODE_STATE_REGISTERING) {
            err = xeon_phi_event_poll(0x1);
            if (err_is_fail(err)) {
                return err;
            }
        }
        if (xnode->state == XNODE_STATE_FAILURE) {
            XSERVICE_DEBUG("Registering with Xeon Phi node %u failed.\n", xnode->id);
        }
    }

    XSERVICE_DEBUG("Registering with other %i Xeon Phi done.\n", (uint32_t )num - 1);

    return SYS_ERR_OK;
}

/**
 * \brief starts the service request handling
 */
errval_t service_start(struct xeon_phi *phi)
{
    errval_t err;

    while (1) {
        err = xeon_phi_event_poll(0x1);
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}

