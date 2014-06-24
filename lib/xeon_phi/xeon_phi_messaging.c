/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/xeon_phi_messaging_defs.h>

#include <xeon_phi/xeon_phi_messaging.h>

#define XPHI_MSG_DBG(x...) debug_printf(" MSGSVC | " x);
//#define XPHI_MSG_DBG(x...)

/**
 * enumration of all possible states of the service exportation process
 */
enum xpm_svc_state
{
    XPM_SVC_STATE_EXPORTING,
    XPM_SVC_STATE_EXPORT_OK,
    XPM_SVC_STATE_EXPORT_FAIL,
    XPM_SVC_STATE_NS_REGISTERING,
    XPM_SVC_STATE_NS_REGISTER_OK,
    XPM_SVC_STATE_NS_REGISTER_FAIL,
    XPM_SVC_STATE_RUNNING
};

/// represents the current state of the exporting process
static enum xpm_svc_state svc_state = XPM_SVC_STATE_EXPORTING;

/// our own iref of the exported service
static iref_t messaging_iref;

/// stores the callbacks to inform the user about arrived messages
static struct xeon_phi_messaging_cb callbacks;

static void open_call_rx(struct xeon_phi_messaging_binding *_binding,
                         struct capref msgframe,
                         uint8_t type)
{
    XPHI_MSG_DBG("Received channel open request type: %x\n", type);

    if (callbacks.open != NULL) {
        callbacks.open(msgframe, type);
    }
}

static void open_iface_call_rx(struct xeon_phi_messaging_binding *_binding,
                               struct capref msgframe,
                               uint8_t type,
                               char *iface,
                               size_t length)
{
    XPHI_MSG_DBG("Received channel OPEN cmd: [%s, 0x%x]\n", iface, type);

    if (callbacks.open_iface != NULL) {
        callbacks.open_iface(msgframe, type, iface);
    }
}

static void spawn_call_rx(struct xeon_phi_messaging_binding *_binding,
                          uint8_t core,
                          char *name,
                          size_t length)
{
    XPHI_MSG_DBG("Received spawn request: %s\n", name);
    if (callbacks.spawn != NULL) {
        callbacks.spawn(core, name);
    }
}

static struct xeon_phi_messaging_rx_vtbl xpm_rx_vtbl = {
    .open_iface = open_iface_call_rx,
    .open = open_call_rx,
    .spawn = spawn_call_rx
};

/*
 * --------------------------------------------------------------------------
 * Export and Connect functions
 */

static errval_t svc_connect_cb(void *st,
                               struct xeon_phi_messaging_binding *b)
{
    XPHI_MSG_DBG("New connection request\n");
    b->rx_vtbl = xpm_rx_vtbl;

    return SYS_ERR_OK;
}

/**
 * \brief
 */
static void svc_export_cb(void *st,
                          errval_t err,
                          iref_t iref)
{
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_EXPORT_FAIL;
        return;
    }

    messaging_iref = iref;

    svc_state = XPM_SVC_STATE_EXPORT_OK;
}

/**
 * \brief initializes the messaging listener for messages over the Xeon Phi
 *        channel.
 *
 * \param fn    callback functions which are invoked when a message arrives
 *
 * \returns SYS_ERR_OK un success
 *          errorcode on failure
 */
errval_t xeon_phi_messaging_service_init(struct xeon_phi_messaging_cb *fn)
{
    errval_t err;

    XPHI_MSG_DBG("Service initialization... callbacks=%p\n", fn);

    if (fn) {
        callbacks = *fn;
    } else {
        memset(&callbacks, 0, sizeof(callbacks));
    }

    struct waitset *ws = get_default_waitset();

    err = xeon_phi_messaging_export(NULL,
                                    svc_export_cb,
                                    svc_connect_cb,
                                    ws,
                                    IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    XPHI_MSG_DBG("Waiting for export... callbacks=%p\n", fn);
    while (svc_state == XPM_SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (svc_state == XPM_SVC_STATE_EXPORT_FAIL) {
        return FLOUNDER_ERR_BIND;
    }

    return SYS_ERR_OK;
}

/**
 * \brief starts the service by registring the service with the name server
 *
 * \returns SYS_ERR_OK on success
 *
 * NOTE: this also executes the messaging handler loop.
 */
errval_t xeon_phi_messaging_service_start(uint8_t start_handler)
{
    errval_t err;
    svc_state = XPM_SVC_STATE_NS_REGISTERING;

    char buf[50];
    snprintf(buf, 50, "%s.%u", disp_name(), disp_get_core_id());

    XPHI_MSG_DBG("Registering iref [%u] with name [%s]\n", messaging_iref, buf);
    err = nameservice_register(buf, messaging_iref);
    if (err_is_fail(err)) {
        return err;
    }

    svc_state = XPM_SVC_STATE_NS_REGISTER_OK;

    if (start_handler) {
        XPHI_MSG_DBG("Starting service...\n");

        svc_state = XPM_SVC_STATE_RUNNING;
        messages_handler_loop();
    }

    return SYS_ERR_OK;
}

/**
 * \brief starts the service when running on the xeon phi driver
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_service_start_phi(uint8_t xeon_phi_id)
{
    errval_t err;
    svc_state = XPM_SVC_STATE_NS_REGISTERING;

    char buf[50];
#if !defined(__k1om__)
    snprintf(buf, 50, "%s.%u", XEON_PHI_MESSAGING_NAME, xeon_phi_id);
#else
    snprintf(buf, 50, "%s", XEON_PHI_MESSAGING_NAME);
#endif
    XPHI_MSG_DBG("Registering iref [%u] with name [%s]\n", messaging_iref, buf);
    err = nameservice_register(buf, messaging_iref);
    if (err_is_fail(err)) {
        return err;
    }

    svc_state = XPM_SVC_STATE_NS_REGISTER_OK;

    return SYS_ERR_OK;
}
