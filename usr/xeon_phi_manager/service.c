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

#include <if/xeon_phi_manager_defs.h>

#include "service.h"
#include "cardmanager.h"

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

static iref_t manager_iref;

/**
 * --------------------------------------------------------------------------
 * Registration protocol
 */

struct reg_data
{
    errval_t err;
    uint8_t id;
    iref_t *cards;
    uint8_t num;
    struct xeon_phi_manager_binding *b;
};

struct reg_data reg_data_fail = {
    // TODO: ERROR CODE
    .err = -1
};

static void register_response_sent_cb(void *a)
{
    if (a != &reg_data_fail) {
        free(a);
    }
}

static void register_response_send(void *a)
{
    errval_t err;

    struct reg_data *rd = a;

    DEBUG_SVC("Registration response: id=%x, num=%x\n", rd->id, rd->num);

    struct event_closure txcont = MKCONT(register_response_sent_cb, a);
    size_t n = rd->num * sizeof(iref_t);
    uint8_t *irefs = (uint8_t *) rd->cards;
    err = xeon_phi_manager_register_response__tx(rd->b,
                                                 txcont,
                                                 rd->id,
                                                 irefs,
                                                 n,
                                                 rd->err);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(register_response_send, a);
        }
        struct waitset *ws = get_default_waitset();
        err = rd->b->register_send(rd->b, ws, txcont);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "register_send on binding failed!");
            register_response_sent_cb(a);
        }
    }
}

static void register_call_recv(struct xeon_phi_manager_binding *_binding,
                               iref_t svc)
{
    DEBUG_SVC("New registration: iref=%x\n", svc);

    struct reg_data *reply = malloc(sizeof(struct reg_data));
    if (!reply) {
        register_response_send(&reg_data_fail);
    }
    reply->b = _binding;
    reply->err = cm_new_xeon_phi(_binding, svc, &reply->id);
    if (err_is_fail(reply->err)) {
        register_response_send(reply);
    }

    reply->err = cm_get_irefs(&reply->cards, &reply->num);
    if (err_is_fail(reply->err)) {
        register_response_send(reply);
    }

    register_response_send(reply);
}

static struct xeon_phi_manager_rx_vtbl xpm_rx_vtbl = {
    .register_call = register_call_recv
};

/*
 * --------------------------------------------------------------------------
 * Export and Connect functions
 */

static errval_t svc_connect_cb(void *st,
                               struct xeon_phi_manager_binding *b)
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

    manager_iref = iref;

    svc_state = XPM_SVC_STATE_NS_REGISTERING;

    err = nameservice_register(XEON_PHI_MANAGER_SERVICE_NAME, iref);
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
errval_t service_start(void)
{
    DEBUG_SVC("starting service...\n");
    errval_t err;

    struct waitset *ws = get_default_waitset();

    err = xeon_phi_manager_export(NULL,
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
