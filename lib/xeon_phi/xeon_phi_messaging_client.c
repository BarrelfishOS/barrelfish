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

#include <xeon_phi/xeon_phi.h>

#include <xeon_phi/xeon_phi_messaging.h>
#include <xeon_phi/xeon_phi_messaging_client.h>

#include <if/xeon_phi_messaging_defs.h>

#ifdef XEON_PHI_DEBUG_MSG
#define DEBUG_XPMC(x...) debug_printf(" [xpmsg] " x);
#else
#define DEBUG_XPMC(x...)
#endif
#define PRINT_XPMC(x...) debug_printf(" [xpmsg] " x);

/**
 * state enumeration for the connection state
 */
enum xpm_state
{
    XPM_STATE_INVALID = 0,
    XPM_STATE_NSLOOKUP,
    XPM_STATE_BINDING,
    XPM_STATE_BIND_OK,
    XPM_STATE_BIND_FAIL,
    XPM_STATE_CONNECTED
};

/// the irefs of the messaging services
static iref_t xpm_iref[XEON_PHI_NUM_MAX];

/// Xeon Phi Messaging bindings
struct xeon_phi_messaging_binding *xpm_binding[XEON_PHI_NUM_MAX];

/// connection states
static enum xpm_state conn_state[XEON_PHI_NUM_MAX] = {
    XPM_STATE_INVALID
};

/*
 * ----------------------------------------------------------------------------
 * Handling of OPEN commands
 * ----------------------------------------------------------------------------
 */

/**
 * stores the state of the open message being transmitted
 */
struct xpm_msg_param_open
{
    struct capref frame;
    struct xeon_phi_messaging_binding *b;
    errval_t err;
    uint8_t type;
    uint8_t xphi_id;
    volatile uint8_t sent;
    char iface[44];
};

/**
 * \brief message sent callback handler for open messages
 *
 * \param a pointer to struct xpm_msg_param_open
 */
static void xpm_msg_open_sent_cb(void *a)
{
    struct xpm_msg_param_open *st = a;
    st->err = SYS_ERR_OK;
    st->sent = 0x1;
}

/**
 * \brief sends the open message
 *
 * \param a pointer to struct xpm_msg_param_open
 */
static void xpm_msg_open_tx(void *a)
{
    errval_t err;

    struct xpm_msg_param_open *param = a;

    struct event_closure txcont = MKCONT(xpm_msg_open_sent_cb, a);

    size_t length = strlen(param->iface) + 1;

#ifndef __k1om__
    err = xeon_phi_messaging_open_iface__tx(param->b,
                    txcont,
                    param->frame,
                    param->type,
                    param->iface,
                    length);
#else
    err = xeon_phi_messaging_open_card__tx(param->b,
                                           txcont,
                                           param->xphi_id,
                                           param->frame,
                                           param->type,
                                           param->iface,
                                           length);
#endif
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xpm_msg_open_tx, a);
            err = param->b->register_send(param->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                param->err = err;
                param->sent = 0x1;
            }
        }
    }
}

/*
 * ----------------------------------------------------------------------------
 * Handling of SPAWN commands
 * ----------------------------------------------------------------------------
 */

/**
 * stores the spawn related information for sending the message
 */
struct xpm_msg_param_spawn
{
    struct xeon_phi_messaging_binding *b;
    errval_t err;
    coreid_t core;
    uint8_t xphi_id;
    uint8_t sent;
    char name[55];
};

/**
 * \brief callback for the message sent event.
 *
 * \param a pointer to struct xpm_msg_param_spawn
 */
static void xpm_msg_spawn_sent_cb(void *a)
{
    struct xpm_msg_param_spawn *st = a;
    st->err = SYS_ERR_OK;
    st->sent = 0x1;
}

/**
 * \brief send handler for spawn requests to the messaging service
 *
 * \param a pointer to struct xpm_msg_param_spawn
 */
static void xpm_msg_spawn_tx(void *a)
{
    errval_t err;

    struct xpm_msg_param_spawn *param = a;

    struct event_closure txcont = MKCONT(xpm_msg_spawn_sent_cb, a);

    size_t length = strlen(param->name) + 1;

    /*
     * when running on the Xeon Phi, we can spawn the domain on another Xeon Phi
     * or on the host.
     */
#ifndef __k1om__
    err = xeon_phi_messaging_spawn__tx(param->b,
                    txcont,
                    param->core,
                    param->name,
                    length);
#else
    err = xeon_phi_messaging_spawn_card__tx(param->b,
                                            txcont,
                                            param->xphi_id,
                                            param->core,
                                            param->name,
                                            length);
#endif
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xpm_msg_spawn_tx, a);
            err = param->b->register_send(param->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                param->err = err;
                param->sent = 0x1;
            }
        }
    }
}

/// Flounder recv vtbl
static struct xeon_phi_messaging_rx_vtbl xpm_rx_vtbl = {
    .open = NULL
};

/*
 * ----------------------------------------------------------------------------
 * Xeon Phi Manager Binding
 * ----------------------------------------------------------------------------
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
    uint8_t xeon_phi_id = (uint8_t) (uintptr_t) st;

    if (err_is_fail(err)) {
        conn_state[xeon_phi_id] = XPM_STATE_BIND_FAIL;
        DEBUG_XPMC("binding FAILED.\n");
        return;
    }

    assert(xpm_binding[xeon_phi_id] == 0);

    DEBUG_XPMC("binding to Xeon Phi %u succeeded.\n", xeon_phi_id);

    b->rx_vtbl = xpm_rx_vtbl;
    b->st = (void *) (uintptr_t) xeon_phi_id;

    xpm_binding[xeon_phi_id] = b;

    conn_state[xeon_phi_id] = XPM_STATE_BIND_OK;
}

/**
 * \brief
 */
static errval_t xpm_connect(uint8_t xeon_phi_id)
{
    errval_t err;

    if (xpm_binding[xeon_phi_id] != NULL) {
        return SYS_ERR_OK;
    }

    char buf[50];
#if !defined(__k1om__)
    snprintf(buf, 50, "%s.%u", XEON_PHI_MESSAGING_NAME, xeon_phi_id);
#else
    snprintf(buf, 50, "%s", XEON_PHI_MESSAGING_NAME);
#endif

    DEBUG_XPMC("nameservice lookup: %s\n", buf);
    err = nameservice_blocking_lookup(buf, &xpm_iref[xeon_phi_id]);
    if (err_is_fail(err)) {
        return err;
    }

    conn_state[xeon_phi_id] = XPM_STATE_BINDING;

    DEBUG_XPMC("binding to %s @ iref:%u\n", buf, xpm_iref[xeon_phi_id]);

    err = xeon_phi_messaging_bind(xpm_iref[xeon_phi_id],
                                  xpm_bind_cb,
                                  (void *) (uintptr_t) xeon_phi_id,
                                  get_default_waitset(),
                                  IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (conn_state[xeon_phi_id] == XPM_STATE_BINDING) {
        messages_wait_and_handle_next();
    }

    if (conn_state[xeon_phi_id] == XPM_STATE_BIND_FAIL) {
        return FLOUNDER_ERR_BIND;
    }

    conn_state[xeon_phi_id] = XPM_STATE_CONNECTED;

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
errval_t xeon_phi_messaging_open(uint8_t xeon_phi_id,
                                 char *iface,
                                 struct capref frame,
                                 uint8_t type)
{
    errval_t err;

    /*
     * if we are running on the Xeon Phi, there is just a single service instace
     * running, so we enforce the Xeon Phi ID to be zero
     */
#ifdef __k1om__
    uint8_t xphi = 0;
#else
    uint8_t xphi = xeon_phi_id;
#endif
    if (xpm_binding[xphi] == NULL) {
        err = xpm_connect(xphi);
        if (err_is_fail(err)) {
            return err;
        }
    }

    DEBUG_XPMC("open request to %s.%u type=%02x\n", iface, xeon_phi_id, type);

    struct xpm_msg_param_open param = {
        .sent = 0x0,
        .frame = frame,
        .type = type,
        .b = xpm_binding[xphi],
        .xphi_id = xeon_phi_id  // here we have to take the original ID
    };

    snprintf(param.iface, sizeof(param.iface), "%s", iface);

    xpm_msg_open_tx(&param);

    while (!param.sent) {
        messages_wait_and_handle_next();
    }

    return param.err;
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
errval_t xeon_phi_messaging_spawn(uint8_t xeon_phi_id,
                                  coreid_t core,
                                  char *name)
{
    errval_t err;

    /*
     * if we are running on the Xeon Phi, there is just a single service instace
     * running, so we enforce the Xeon Phi ID to be zero
     */
#ifdef __k1om__
    uint8_t xphi = 0;
#else
    uint8_t xphi = xeon_phi_id;
#endif

    DEBUG_XPMC("spawn request %s on core %u.%u\n", name, xeon_phi_id, core);

    if (xpm_binding[xphi] == NULL) {
        err = xpm_connect(xphi);
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xpm_msg_param_spawn param = {
        .sent = 0x0,
        .b = xpm_binding[xphi],
        .core = core,
        .xphi_id = xeon_phi_id  // here we have to take the original ID
    };

    snprintf(param.name, sizeof(param.name), "%s", name);

    xpm_msg_spawn_tx(&param);

    while (!param.sent) {
        messages_wait_and_handle_next();
    }

    return param.err;
}

/**
 * \brief sends a KILL command over the Xeon Phi channel to terminated
 *        a previously spawned domain
 *
 * \param d the domain ID of the domain to terminate
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_kill(uint8_t xeon_phi_id,
                                 domainid_t d)
{
    errval_t err;

#ifdef __k1om__
    uint8_t xphi = 0;
#else
    uint8_t xphi = xeon_phi_id;
#endif
    if (xpm_binding[xphi] == NULL) {
        err = xpm_connect(xphi);
        if (err_is_fail(err)) {
            return err;
        }
    }

    assert(!"NYI");

    return SYS_ERR_OK;
}
