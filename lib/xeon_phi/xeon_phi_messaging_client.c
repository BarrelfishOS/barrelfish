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

#define DEBUG_XPMC(x...) debug_printf(" XPMC | " x)

enum xpm_state
{
    XPM_STATE_NSLOOKUP,
    XPM_STATE_BINDING,
    XPM_STATE_BIND_OK,
    XPM_STATE_BIND_FAIL,
};

/* todo: extend this with array of 8 */
static iref_t xpm_iref[XEON_PHI_NUM_MAX];
struct xeon_phi_messaging_binding *xpm_binding[XEON_PHI_NUM_MAX];

static enum xpm_state conn_state = XPM_STATE_NSLOOKUP;


/*
 * --------------------------------------------------------------------------
 * Handling of OPEN commands
 */

struct xpm_msg_param_open
{
    struct capref frame;
    struct xeon_phi_messaging_binding *b;
    uint8_t type;
    char iface[44];
};

static void xpm_msg_open_tx(void *a)
{
    errval_t err;

    struct xpm_msg_param_open *param = a;

    struct event_closure txcont = MKCONT(free, a);

    size_t length = strlen(param->iface)+1;

    err = xeon_phi_messaging_open_iface__tx(param->b,
                                      txcont,
                                      param->frame,
                                      param->type,
                                      param->iface,
                                      length);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xpm_msg_open_tx, a);
            err = param->b->register_send(param->b,
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
    struct xeon_phi_messaging_binding *b;
    coreid_t core;
    char name[55];
};

static void xpm_msg_spawn_tx(void *a)
{
    errval_t err;

    struct xpm_msg_param_spawn *param = a;

    struct event_closure txcont = MKCONT(free, a);

    size_t length = strlen(param->name)+1;

    err = xeon_phi_messaging_spawn__tx(param->b,
                                       txcont,
                                       param->core,
                                       param->name,
                                       length);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(xpm_msg_spawn_tx, a);
            err = param->b->register_send(param->b,
                                             get_default_waitset(),
                                             txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "failed to register send\n");
            }
        }
    }
}


static struct xeon_phi_messaging_rx_vtbl xpm_rx_vtbl = {
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

    uint8_t xeon_phi_id = (uint8_t)(uintptr_t)st;

    assert(xpm_binding[xeon_phi_id] == 0);

    b->rx_vtbl = xpm_rx_vtbl;
    b->st = (void *)(uintptr_t)xeon_phi_id;

    xpm_binding[xeon_phi_id] = b;

    DEBUG_XPMC("Binding ok.\n");

    conn_state = XPM_STATE_BIND_OK;
}

/**
 * \brief
 */
static errval_t xpm_connect(uint8_t xeon_phi_id)
{
    errval_t err;

#ifdef __k1om__
    assert(xeon_phi_id == 0);
#endif

    if (xpm_binding[xeon_phi_id] != NULL) {
        return SYS_ERR_OK;
    }

    char buf[50];
#if !defined(__k1om__)
    snprintf(buf, 50, "%s.%u", XEON_PHI_MESSAGING_NAME, xeon_phi_id);
#else
    snprintf(buf, 50, "%s", XEON_PHI_MESSAGING_NAME);
#endif

    DEBUG_XPMC("Nameservice lookup: %s\n", buf);
    err = nameservice_blocking_lookup(buf, &xpm_iref[xeon_phi_id]);
    if (err_is_fail(err)) {
        return err;
    }

    conn_state = XPM_STATE_BINDING;

    DEBUG_XPMC("binding to iref [%u]... \n", xpm_iref[xeon_phi_id]);
    err = xeon_phi_messaging_bind(xpm_iref[xeon_phi_id], xpm_bind_cb,
                                  (void *)(uintptr_t)xeon_phi_id,
                                  get_default_waitset(),
                                  IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (conn_state == XPM_STATE_BINDING) {
        messages_wait_and_handle_next();
    }

    DEBUG_XPMC("binding to iref [%u] done. \n", xpm_iref[xeon_phi_id]);

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
errval_t xeon_phi_messaging_open(uint8_t xeon_phi_id,
                                 char *iface,
                                 struct capref frame,
                                 uint8_t type)
{
    errval_t err;
    if (xpm_binding[xeon_phi_id] == NULL) {
        err = xpm_connect(xeon_phi_id);
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
    param->b = xpm_binding[xeon_phi_id];

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
errval_t xeon_phi_messaging_spawn(uint8_t xeon_phi_id,
                                  coreid_t core,
                                  char *name)
{
    errval_t err;

    DEBUG_XPMC("Send spawn request %s on core %u.%u\n", name, xeon_phi_id, core);

    if (xpm_binding[xeon_phi_id] == NULL) {
        err = xpm_connect(xeon_phi_id);
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct xpm_msg_param_spawn *param = malloc(sizeof(struct xpm_msg_param_spawn));
    if (param == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    param->b = xpm_binding[xeon_phi_id];
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
errval_t xeon_phi_messaging_kill(uint8_t xeon_phi_id,
                                 domainid_t d)
{
    errval_t err;
    if (xpm_binding[xeon_phi_id] == NULL) {
        err = xpm_connect(xeon_phi_id);
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}
