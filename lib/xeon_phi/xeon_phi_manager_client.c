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

#include <xeon_phi/xeon_phi_manager_client.h>

#include <if/xeon_phi_manager_defs.h>

enum xpm_state
{
    XPM_STATE_NSLOOKUP,
    XPM_STATE_BINDING,
    XPM_STATE_BIND_OK,
    XPM_STATE_BIND_FAIL,
    XPM_STATE_REGISTERING,
    XPM_STATE_REGISTER_OK,
    XPM_STATE_REGISTER_FAIL
};

static iref_t xpm_iref = 0;
struct xeon_phi_manager_binding *xpm_binding = NULL;

enum xpm_state conn_state = XPM_STATE_NSLOOKUP;

/*
 * --------------------------------------------------------------------------
 * Registration Protocol
 */

struct xpm_register_data
{
    uint8_t id;
    iref_t *cards;
    uint8_t num;
};

static struct xpm_register_data reg_data;

static void xpm_recv_register_response(struct xeon_phi_manager_binding *_binding,
                                       uint8_t id,
                                       uint8_t *cards,
                                       size_t n,
                                       xeon_phi_manager_errval_t msgerr)
{
    DEBUG_XPMC("Registration response: ID=0x%x, num=0x%lx, err=0x%lx\n", id, n/sizeof(iref_t), msgerr);

    if (err_is_fail(msgerr)) {
        conn_state = XPM_STATE_REGISTER_FAIL;
        return;
    }

    if (n % sizeof(iref_t)) {
        // the data seems to be corrupt
        conn_state = XPM_STATE_REGISTER_FAIL;
    }

    reg_data.id = id;
    reg_data.num = n / sizeof(iref_t);
    reg_data.cards = (iref_t *) cards;

    xpm_binding->st = &reg_data;

    conn_state = XPM_STATE_REGISTER_OK;
}

static void xpm_send_register_request_cb(void *a)
{

}

static void xpm_send_register_request(void *a)
{
    errval_t err;
    iref_t *svc_iref = a;

    conn_state = XPM_STATE_REGISTERING;

    DEBUG_XPMC("Registering with Xeon Phi manager...\n");

    struct event_closure txcont = MKCONT(xpm_send_register_request_cb, NULL);

    err = xeon_phi_manager_register_call__tx(xpm_binding, txcont, *svc_iref);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xpm_send_register_request, xpm_binding);
            err = xpm_binding->register_send(xpm_binding,
                                             get_default_waitset(),
                                             txcont);
            if (err_is_fail(err)) {
                conn_state = XPM_STATE_REGISTER_FAIL;
            }
        }
    }
}

struct xeon_phi_manager_rx_vtbl xpm_rx_vtbl = {
    .register_response = xpm_recv_register_response
};

/*
 * --------------------------------------------------------------------------
 * Xeon Phi Manager Binding
 */

/**
 * \brief
 *
 * \param
 * \param
 * \param
 */
static void xpm_bind_cb(void *st,
                        errval_t err,
                        struct xeon_phi_manager_binding *b)
{

    if (err_is_fail(err)) {
        conn_state = XPM_STATE_BIND_FAIL;
        return;
    }

    xpm_binding = b;

    xpm_binding->rx_vtbl = xpm_rx_vtbl;

    DEBUG_XPMC("Binding ok.\n");

    conn_state = XPM_STATE_BIND_OK;
}

/**
 * \brief
 */
static errval_t xpm_bind(void)
{
    errval_t err;

    if (xpm_binding != NULL) {
        return SYS_ERR_OK;
    }

    DEBUG_XPMC("Nameservice lookup: "XEON_PHI_MANAGER_SERVICE_NAME"\n");
    err = nameservice_blocking_lookup(XEON_PHI_MANAGER_SERVICE_NAME, &xpm_iref);
    if (err_is_fail(err)) {
        return err;
    }

    conn_state = XPM_STATE_BINDING;

    DEBUG_XPMC("binding... \n");
    err = xeon_phi_manager_bind(xpm_iref, xpm_bind_cb, NULL,
                                get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * --------------------------------------------------------------------------
 * Public Interface
 */

/**
 * \brief   registers the Xeon Phi driver card with the Xeon Phi Manager
 *          this function blocks until we have a connection to the manager
 *
 * \param   svc_iref    the iref of the drivers service
 * \param   id          the own card id
 * \param   num         returns the number of returned irefs / number of cards
 * \param   cards       returns the array of irefs
 *
 * \return SYS_ERR_OK on success
 */
errval_t xeon_phi_manager_client_register(iref_t svc_iref,
                                          uint8_t *id,
                                          uint8_t *num,
                                          iref_t **cards)
{
    DEBUG_XPMC("Registring with Xeon Phi Manager\n");
    errval_t err;
    err = xpm_bind();
    if (err_is_fail(err)) {
        return err;
    }

    while (conn_state == XPM_STATE_BINDING) {
        messages_wait_and_handle_next();
    }

    if (conn_state == XPM_STATE_BIND_FAIL) {
        return FLOUNDER_ERR_BIND;
    }

    xpm_send_register_request(&svc_iref);

    while (conn_state == XPM_STATE_REGISTERING) {
        messages_wait_and_handle_next();
    }

    if (conn_state == XPM_STATE_REGISTER_FAIL) {
        // TODO ERROR CODE
        DEBUG_XPMC("Registration failed.\n");
        return -1;
    }

    assert(xpm_binding->st == &reg_data);

    struct xpm_register_data *rd = xpm_binding->st;
    *id = rd->id;
    *num = rd->num;
    *cards = rd->cards;

    xpm_binding->st = NULL;

    return SYS_ERR_OK;
}

/**
 * \brief   deregisters the Xeon Phi driver with the Xeon Phi Manager
 *
 * \return SYS_ERR_OK on success
 */
errval_t xeon_phi_manager_client_deregister(void)
{
    return SYS_ERR_OK;
}
