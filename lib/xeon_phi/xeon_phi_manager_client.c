/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_manager_client.h>

#include <if/xeon_phi_manager_defs.h>
#include <if/xeon_phi_manager_rpcclient_defs.h>

/// the name of the Xeon Phi Manager service
#define XEON_PHI_MANAGER_SERVICE_NAME "xeon_phi_manager"

/// Enabling the debug output of the Xeon Phi Manager client
#ifdef XEON_PHI_DEBUG_MANAGER
#define DEBUG_XPMC(x...) debug_printf(" [xpmc] " x)
#else
#define DEBUG_XPMC(x...)
#endif

/**
 * represents the connection state of the Xeon Phi Manager client with the
 * Xeon Phi Manager
 */
enum xpm_state
{
    XPM_STATE_INVALID = 0,
    XPM_STATE_NSLOOKUP,
    XPM_STATE_BINDING,
    XPM_STATE_BIND_OK,
    XPM_STATE_BIND_FAIL,
    XPM_STATE_REGISTERING,
    XPM_STATE_REGISTER_OK,
    XPM_STATE_REGISTER_FAIL,
    XPM_STATE_CONNECTED
};

/// iref of the Xeon Phi manager service
static iref_t xpm_iref = 0;

/// Flounder binind go the Xeon Phi manager
static struct xeon_phi_manager_binding *xpm_binding = NULL;

static struct xeon_phi_manager_rpc_client xpm_rpc_client;

/// connection state
static enum xpm_state conn_state = XPM_STATE_INVALID;

/*
 * --------------------------------------------------------------------------
 * Registration Protocol
 * ----------------------------------------------------------------------------
 */

/**
 * contains the necessary data for the Xeon Phi manager connection
 */
struct xpm_register_data
{
    iref_t svc_iref;                  ///< our exported service iref
    errval_t err;                     ///< error code of the registration
    uint8_t id;                       ///< our own Xeon Phi ID.
    xeon_phi_manager_cards_t irefs;   ///< irefs to the other Xeon Phi drivers
} xpm_reg_data;

/*
 * ----------------------------------------------------------------------------
 * Xeon Phi Manager binding functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief Flounder bind callback
 *
 * \param st      state associated with the binding
 * \param err     error code of the binding attempt
 * \param binding Xeon Phi Manager Flounder binding
 */
static void xpm_bind_cb(void *st,
                        errval_t err,
                        struct xeon_phi_manager_binding *binding)
{

    if (err_is_fail(err)) {
        conn_state = XPM_STATE_BIND_FAIL;
        xpm_reg_data.err = err;
        return;
    }

    xpm_binding = binding;

    conn_state = XPM_STATE_BIND_OK;

    xeon_phi_manager_rpc_client_init(&xpm_rpc_client, binding);

    DEBUG_XPMC("binding to "XEON_PHI_MANAGER_SERVICE_NAME" succeeded\n");
}

/**
 * \brief binds to the Xeon Phi Manager service
 *
 * \returns SYS_ERR_OK on success
 *          FLOUNDER_ERR_* on failure
 */
static errval_t xpm_bind(void)
{
    errval_t err;

    if (xpm_binding != NULL) {
        return SYS_ERR_OK;
    }

    assert(conn_state == XPM_STATE_INVALID);

    conn_state = XPM_STATE_NSLOOKUP;

    DEBUG_XPMC("nameservice lookup: "XEON_PHI_MANAGER_SERVICE_NAME"\n");

    err = nameservice_blocking_lookup(XEON_PHI_MANAGER_SERVICE_NAME, &xpm_iref);
    if (err_is_fail(err)) {
        return err;
    }

    conn_state = XPM_STATE_BINDING;

    DEBUG_XPMC("binding: "XEON_PHI_MANAGER_SERVICE_NAME" @ iref:%u\n", xpm_iref);

    err = xeon_phi_manager_bind(xpm_iref, xpm_bind_cb, NULL, get_default_waitset(),
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

/*
 * ----------------------------------------------------------------------------
 * Public Interface
 * ----------------------------------------------------------------------------
 */

/**
 * \brief registers the Xeon Phi driver card with the Xeon Phi Manager
 *
 * \param svc_iref  iref of the own exported Xeon Phi driver interface
 * \param id        returns the assigned Xeon Phi card ID
 * \param num       returns the size of the cards array
 * \param irefs     returns array of irefs to the other cards
 *
 * NOTE: this is a blocking function. The function will only return after
 *       the Xeon Phi manager connection has been fully established and the
 *       registration protocol has been executed.
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_manager_client_register(iref_t svc_iref,
                                          uint8_t *id,
                                          uint8_t *num,
                                          iref_t **irefs)
{
    errval_t err, msgerr;

    if (strcmp(disp_name(), "xeon_phi") != 0) {
        USER_PANIC("client register called on non xeon phi driver");
        return -1;
    }

    if (conn_state >= XPM_STATE_REGISTER_OK) {
        return SYS_ERR_OK;
    }

    DEBUG_XPMC("Registration with Xeon Phi Manager service.\n");

    err = xpm_bind();
    if (err_is_fail(err)) {
        return err;
    }

    xpm_reg_data.svc_iref = svc_iref;

    xeon_phi_manager_cards_t cards;

    err = xpm_rpc_client.vtbl.register_driver(&xpm_rpc_client, svc_iref, id,
                                              &cards, &msgerr);
    if (err_is_fail(err)) {
        return err;
    }

    if (err_is_fail(msgerr)) {
        return msgerr;
    }
    conn_state = XPM_STATE_REGISTER_OK;

    iref_t *cardiref =calloc(cards.num, sizeof(iref_t));
    assert(cardiref);
    for(uint32_t i = 0; i < cards.num; ++i) {
        cardiref[i] = ((iref_t *)&cards.card0)[i];
    }
    *irefs = cardiref;
    *num = cards.num;

    return SYS_ERR_OK;
}

/**
 * \brief   deregisters the Xeon Phi driver with the Xeon Phi Manager
 *
 * \return SYS_ERR_OK on success
 */
errval_t xeon_phi_manager_client_deregister(void)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief looks up the iref of a Xeon Phi with the given id
 *
 * \param xid       Xeon Phi ID
 * \param svc_iref  the returned svc_iref of the xeon phi
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_manager_lookup(xphi_id_t xid,
                                 iref_t *svc_iref)
{
    errval_t err, msgerr;

    DEBUG_XPMC("Registration with Xeon Phi Manager service.\n");

    err = xpm_bind();
    if (err_is_fail(err)) {
        return err;
    }

    err = xpm_rpc_client.vtbl.lookup(&xpm_rpc_client, xid, svc_iref, &msgerr);
    if (err_is_fail(err)) {
        return err;
    }

    return msgerr;
}
