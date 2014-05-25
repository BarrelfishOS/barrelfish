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

#include <if/xeon_phi_defs.h>

#include "xeon_phi.h"
#include "service.h"

static uint32_t is_exported;

static iref_t svc_iref;

static inline errval_t
handle_messages(void)
{
    uint32_t data = xeon_phi_serial_handle_recv();
    errval_t err = event_dispatch_non_block(get_default_waitset());
    if (err_is_fail(err)) {
        if (err_no(err) == LIB_ERR_NO_EVENT) {
            if (!data) {
                thread_yield();
            }
            return SYS_ERR_OK;
        }
        return err;
    }
    return SYS_ERR_OK;
}

/*
 * ---------------------------------------------------------------------------
 * Intra Xeon Phi Driver Communication Regigistration
 */

static void
register_response_sent_cb(void *a)
{

}

static void
register_response_send(void *a)
{
    errval_t err;

    struct xnode *topology = a;

    struct event_closure txcont = MKCONT(register_response_sent_cb, a);

    if (topology->state == XNODE_STATE_READY) {
        err = SYS_ERR_OK;
    } else {
        err = -1;  // TODO> ERROR NUMBEr
    }

    err = xeon_phi_register_response__tx(topology->binding, txcont, err);
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
static void
register_call_recv(struct xeon_phi_binding *_binding,
                   uint8_t id)
{

    assert(((struct xnode * )(_binding->st))->binding != _binding);
    struct xeon_phi *phi = _binding->st;

    assert(id < XEON_PHI_NUM_MAX);
    phi->topology[id].binding = _binding;
    phi->topology[id].state = XNODE_STATE_READY;

    phi->connected++;

    XSERVICE_DEBUG("[%u] New register call: id=0x%x, num_connected=%i\n", phi->id,
                   id, phi->connected);

    _binding->st = &phi->topology[id];

    register_response_send(&phi->topology[id]);
}

/**
 *
 */
static void
register_response_recv(struct xeon_phi_binding *_binding,
                       xeon_phi_errval_t msgerr)
{
    assert(((struct xnode * )(_binding->st))->binding == _binding);

    struct xnode *topology = _binding->st;

    if (err_is_fail(msgerr)) {
        topology->state = XNODE_STATE_FAILURE;
    } else {
        topology->local->connected++;
        topology->state = XNODE_STATE_READY;
    }

    XSERVICE_DEBUG("[%u] New register response: 0x%lx, num_connected=%u\n",
                   topology->local->id, msgerr, topology->local->connected);
}

static void
register_call_sent_cb(void *a)
{

}

static void
register_call_send(void *a)
{
    errval_t err;

    struct xnode *topology = a;

    struct event_closure txcont = MKCONT(register_call_sent_cb, a);

    topology->state = XNODE_STATE_REGISTERING;

    err = xeon_phi_register_call__tx(topology->binding, txcont, topology->local->id);
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
static struct xeon_phi_rx_vtbl xps_rx_vtbl = {
    .register_call = register_call_recv,
    .register_response = register_response_recv };

/*
 * ---------------------------------------------------------------------------
 * Service Setup
 */
static void
svc_bind_cb(void *st,
            errval_t err,
            struct xeon_phi_binding *b)
{
    struct xnode *node = st;

    XSERVICE_DEBUG("binding done.\n");
    b->rx_vtbl = xps_rx_vtbl;
    node->binding = b;
    b->st = node;
    node->state = XNODE_STATE_REGISTERING;
}

static errval_t
svc_register(struct xnode *node)
{
    XSERVICE_DEBUG("binding to node %i (iref = 0x%x)\n", node->id, node->iref);
    errval_t err;

    err = xeon_phi_bind(node->iref, svc_bind_cb, node, get_default_waitset(),
    IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        node->state = XNODE_STATE_FAILURE;
        return err;
    }

    return SYS_ERR_OK;
}

static errval_t
svc_connect_cb(void *st,
               struct xeon_phi_binding *b)
{
    struct xeon_phi *phi = st;

    XSERVICE_DEBUG("[%u] New connection\n", phi->id);

    b->st = st;
    b->rx_vtbl = xps_rx_vtbl;
    return SYS_ERR_OK;
}

static void
svc_export_cb(void *st,
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
errval_t
service_init(struct xeon_phi *phi)
{
    errval_t err;

    for (uint32_t i = 0; i < XEON_PHI_NUM_MAX; ++i) {
        phi->topology[i].local = phi;
    }

    err = xeon_phi_export(phi, svc_export_cb, svc_connect_cb, get_default_waitset(),
    IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (!is_exported) {
        handle_messages();
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
errval_t
service_register(struct xeon_phi *phi,
                 iref_t *irefs,
                 size_t num)
{
    errval_t err;
    struct xnode *xnode;
    XSERVICE_DEBUG("start binding to nodes\n");
    for (uint32_t i = 0; i < num; ++i) {
        xnode = &phi->topology[i];
        if (i == phi->id) {
            xnode->iref = phi->iref;
            xnode->id = i;
            xnode->state = XNODE_STATE_READY;
            continue;
        }

        xnode->iref = irefs[i];
        xnode->id = i;
        xnode->state = XNODE_STATE_NONE;
        svc_register(xnode);
        while (xnode->state == XNODE_STATE_NONE) {
            err = handle_messages();
            if (err_is_fail(err)) {
                return err;
            }
        }
    }

    XSERVICE_DEBUG("Start registring\n");

    for (uint32_t i = 0; i < num; ++i) {
        if (i == phi->id) {
            continue;
        }
        xnode = &phi->topology[i];
        register_call_send(xnode);
        while (xnode->state == XNODE_STATE_READY) {
            err = handle_messages();
            if (err_is_fail(err)) {
                return err;
            }
        }
    }

    XSERVICE_DEBUG("Registring with other %i Xeon Phi done.\n", (uint32_t)num - 1);

    return SYS_ERR_OK;
}

/**
 * \brief starts the service request handling
 */
errval_t
service_start(void)
{
    errval_t err;
    while (1) {
        err = handle_messages();
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}

