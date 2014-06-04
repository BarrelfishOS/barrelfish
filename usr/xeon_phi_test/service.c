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
#include <xeon_phi/xeon_phi_manager_client.h>


#include "service.h"

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

static enum xpm_svc_state svc_state = XPM_SVC_STATE_EXPORTING;

static iref_t messaging_iref;




static struct xeon_phi_messaging_rx_vtbl xpm_rx_vtbl = {

};

/*
 * --------------------------------------------------------------------------
 * Export and Connect functions
 */

static errval_t svc_connect_cb(void *st,
                               struct xeon_phi_messaging_binding *b)
{
    DEBUG_SVC("connect request\n");
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
    DEBUG_SVC("exported\n");
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_EXPORT_FAIL;
        return;
    }

    messaging_iref = iref;

    svc_state = XPM_SVC_STATE_NS_REGISTERING;

    err = nameservice_register((const char *)st, iref);
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_NS_REGISTER_FAIL;
    }
    DEBUG_SVC("ns registered\n");
    svc_state = XPM_SVC_STATE_NS_REGISTER_OK;
}

/**
 * \brief   starts Xeon Phi manager service
 *
 * \return  SYS_ERR_OK on succes
 */
errval_t service_start(char *iface)
{
    DEBUG_SVC("starting service...\n");
    errval_t err;

    struct waitset *ws = get_default_waitset();

    err = xeon_phi_messaging_export(iface,
                                  svc_export_cb,
                                  svc_connect_cb,
                                  ws,
                                  IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (svc_state == XPM_SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (svc_state == XPM_SVC_STATE_EXPORT_FAIL) {
        return FLOUNDER_ERR_BIND;
    } else if (svc_state == XPM_SVC_STATE_NS_REGISTER_FAIL) {
        return LIB_ERR_NAMESERVICE_CLIENT_INIT;
    }

    DEBUG_SVC("Service up and running.\n");

    svc_state = XPM_SVC_STATE_RUNNING;
    messages_handler_loop();

    DEBUG_SVC("Message handler terminated.\n");
    return -1;
}
