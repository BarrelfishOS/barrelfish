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

#include <xeon_phi/xeon_phi_messaging.h>
#include <xeon_phi/xeon_phi_messaging_client.h>

#include <if/xeon_phi_messaging_defs.h>

#define DEBUG_XPMC(x...) debug_printf(" XPMC | " x)

enum xpm_state
{
    XPM_STATE_NSLOOKUP,
    XPM_STATE_BINDING,
    XPM_STATE_BIND_OK,
    XPM_STATE_BIND_FAIL,
};

static iref_t xpm_iref = 0;
struct xeon_phi_messaging_binding *xpm_binding = NULL;

enum xpm_state conn_state = XPM_STATE_NSLOOKUP;

/*
 * --------------------------------------------------------------------------
 * Handling of OPEN commands
 */
struct xpm_msg_param_open
{
    struct capref frame;
    uint8_t type;
    char iface[44];
};

static void xpm_msg_open_tx(void *a)
{
    errval_t err;

    struct xpm_msg_param_open *param = a;

    struct event_closure txcont = MKCONT(free, a);

    size_t length = strlen(param->iface)+1;

    err = xeon_phi_messaging_open_iface__tx(xpm_binding,
                                      txcont,
                                      param->frame,
                                      param->type,
                                      param->iface,
                                      length);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xpm_msg_open_tx, a);
            err = xpm_binding->register_send(xpm_binding,
                                             get_default_waitset(),
                                             txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "failed to register send\n");
            }
        }
    }
}

/*
 * --------------------------------------------------------------------------
 * Handling of SPAWN commands
 */
struct xpm_msg_param_spawn
{
    coreid_t core;
    char name[55];
};

static void xpm_msg_spawn_tx(void *a)
{
    errval_t err;

    struct xpm_msg_param_spawn *param = a;

    struct event_closure txcont = MKCONT(free, a);

    size_t length = strlen(param->name)+1;

    err = xeon_phi_messaging_spawn__tx(xpm_binding,
                                      txcont,
                                      param->core,
                                      param->name,
                                      length);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xpm_msg_spawn_tx, a);
            err = xpm_binding->register_send(xpm_binding,
                                             get_default_waitset(),
                                             txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "failed to register send\n");
            }
        }
    }
}


struct xeon_phi_messaging_rx_vtbl xpm_rx_vtbl = {
    .open = NULL
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
                        struct xeon_phi_messaging_binding *b)
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
static errval_t xpm_connect(void)
{
    errval_t err;

    if (xpm_binding != NULL) {
        return SYS_ERR_OK;
    }

    DEBUG_XPMC("Nameservice lookup: "XEON_PHI_MESSAGING_NAME"\n");
    err = nameservice_blocking_lookup(XEON_PHI_MESSAGING_NAME, &xpm_iref);
    if (err_is_fail(err)) {
        return err;
    }

    conn_state = XPM_STATE_BINDING;

    DEBUG_XPMC("binding to iref [%u]... \n", xpm_iref);
    err = xeon_phi_messaging_bind(xpm_iref, xpm_bind_cb,
                                  NULL,
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

    return SYS_ERR_OK;
}

/*
 * --------------------------------------------------------------------------
 * Public Interface
 */

/**
 * \brief sends a OPEN command message over the Xeon Phi channel
 *
 * \param iface the name of the iface to talk to
 * \param frame capability representing the allocated frame
 * \param type  type of the channel
 *
 * \return SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_open(char *iface,
                                 struct capref frame,
                                 uint8_t type)
{
    errval_t err;
    if (xpm_binding == NULL) {
        err = xpm_connect();
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xpm_msg_param_open *param = malloc(sizeof(struct xpm_msg_param_open));
    if (param == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    param->frame = frame;
    param->type = type;

    snprintf(param->iface, sizeof(param->iface), "%s", iface);

    xpm_msg_open_tx(param);

    return SYS_ERR_OK;
}

/**
 * \brief sends a SPAWN command message over the Xeon Phi channel
 *        to spawn a new domain on the other side
 *
 * \param core  core ID of the core to spawn the program on
 * \param name  path to the program to spawn
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_spawn(coreid_t core,
                                  char *name)
{
    errval_t err;
    if (xpm_binding == NULL) {
        err = xpm_connect();
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xpm_msg_param_spawn *param = malloc(sizeof(struct xpm_msg_param_spawn));
    if (param == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    param->core = core;
    snprintf(param->name, sizeof(param->name), "%s", name);

    xpm_msg_spawn_tx(param);

    return SYS_ERR_OK;
}

/**
 * \brief sends a KILL command over the Xeon Phi channel to terminated
 *        a previously spawned domain
 *
 * \param d the domain ID of the domain to terminate
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_kill(domainid_t d)
{
    errval_t err;
    if (xpm_binding == NULL) {
        err = xpm_connect();
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}
