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

#include <if/xeon_phi_manager_defs.h>

#include "service.h"
#include "cardmanager.h"

#define XEON_PHI_MANAGER_SERVICE_NAME "xeon_phi_manager"

#ifdef XEON_PHI_DEBUG_MANAGER
#define DEBUG_SVC(x...) debug_printf(" svc | " x)
#else
#define DEBUG_SVC(x...)
#endif

#define PRINTF_SVC(x...) debug_printf(" svc | " x)


/// enumeration of possible service state
enum xpm_svc_state
{
    XPM_SVC_STATE_INVALID = 0,
    XPM_SVC_STATE_EXPORTING,
    XPM_SVC_STATE_EXPORT_OK,
    XPM_SVC_STATE_EXPORT_FAIL,
    XPM_SVC_STATE_NS_REGISTERING,
    XPM_SVC_STATE_NS_REGISTER_OK,
    XPM_SVC_STATE_NS_REGISTER_FAIL,
    XPM_SVC_STATE_RUNNING
};

/// service state
static enum xpm_svc_state svc_state = XPM_SVC_STATE_INVALID;

/// our exported iref
static iref_t manager_iref;

/**
 * --------------------------------------------------------------------------
 * Registration protocol
 */

struct reg_data
{
    errval_t err;
    uint8_t id;
    xeon_phi_manager_cards_t irefs;
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

    DEBUG_SVC("Registration response: id=%u, #irefs=%u\n", rd->id, rd->irefs.num);

    struct event_closure txcont = MKCONT(register_response_sent_cb, a);

    err = xeon_phi_manager_register_driver_response__tx(rd->b,
                                                        txcont,
                                                        rd->id,
                                                        rd->irefs,
                                                        rd->err);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(register_response_send, a);
            err = rd->b->register_send(rd->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "register_send on binding failed!");
            }
        }
    }
}

static void register_call_recv(struct xeon_phi_manager_binding *_binding,
                               iref_t svc)
{
    PRINTF_SVC("New registration request: iref=%u\n", svc);

    struct reg_data *reply = malloc(sizeof(struct reg_data));
    if (!reply) {
        reg_data_fail.err = LIB_ERR_MALLOC_FAIL;
        reg_data_fail.b = _binding;
        register_response_send(&reg_data_fail);
        return;
    }
    reply->b = _binding;
    reply->err = cm_new_xeon_phi(_binding, svc, &reply->id);
    if (err_is_fail(reply->err)) {
        register_response_send(reply);
        return;
    }

    reply->err = cm_get_irefs(&reply->irefs.card0, &reply->irefs.num);

    register_response_send(reply);
}

static struct xeon_phi_manager_rx_vtbl xpm_rx_vtbl = {
    .register_driver_call = register_call_recv
};

/*
 * --------------------------------------------------------------------------
 * Export and Connect functions
 */

static errval_t svc_connect_cb(void *st,
                               struct xeon_phi_manager_binding *b)
{
    DEBUG_SVC("New connection from a Xeon Phi Driver\n");

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

    manager_iref = iref;
    svc_state = XPM_SVC_STATE_NS_REGISTERING;

    DEBUG_SVC("registering "XEON_PHI_MANAGER_SERVICE_NAME" with iref:%u\n", iref);

    err = nameservice_register(XEON_PHI_MANAGER_SERVICE_NAME, iref);
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_NS_REGISTER_FAIL;
    }
    svc_state = XPM_SVC_STATE_NS_REGISTER_OK;
}

/**
 * \brief   starts Xeon Phi manager service
 *
 * \returns  SYS_ERR_OK on success
 *           errval on failure
 *
 * NOTE: this function should not return.
 */
errval_t service_start(void)
{
    errval_t err;

    DEBUG_SVC("starting service {"XEON_PHI_MANAGER_SERVICE_NAME"}\n");

    err = xeon_phi_manager_export(NULL,
                                  svc_export_cb,
                                  svc_connect_cb,
                                  get_default_waitset(),
                                  IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (svc_state == XPM_SVC_STATE_EXPORTING || svc_state
                    == XPM_SVC_STATE_NS_REGISTERING) {
        messages_wait_and_handle_next();
    }

    if (svc_state == XPM_SVC_STATE_EXPORT_FAIL) {
        return FLOUNDER_ERR_BIND;
    } else if (svc_state == XPM_SVC_STATE_NS_REGISTER_FAIL) {
        return LIB_ERR_NAMESERVICE_CLIENT_INIT;
    }

    svc_state = XPM_SVC_STATE_RUNNING;

    PRINTF_SVC("Xeon Phi Manager service up and running.\n");

    messages_handler_loop();

    USER_PANIC("Xeon Phi Manager service terminated!\n");

    return SYS_ERR_OK;
}
