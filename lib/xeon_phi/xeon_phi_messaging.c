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

#include <if/xeon_phi_messaging_defs.h>

#include <xeon_phi/xeon_phi_messaging.h>

#ifdef XEON_PHI_DEBUG_MSG
#define DEBUG_XMSG(x...) debug_printf(" [xpmsg-svc] " x);
#else
#define DEBUG_XMSG(x...)
#endif
#define PRINTF_XMSG(x...) debug_printf(" [xpmsg-svc] " x);


/// flag indicating if we are running on the Xeon Phi driver
uint8_t xpm_is_phi = 0x0;

/**
 * enumeration of all possible states of the service exporting process
 */
enum xpm_svc_state
{
    XPM_SVC_STATE_INVALID,
    XPM_SVC_STATE_EXPORTING,
    XPM_SVC_STATE_EXPORT_OK,
    XPM_SVC_STATE_EXPORT_FAIL,
    XPM_SVC_STATE_NS_REGISTERING,
    XPM_SVC_STATE_NS_REGISTER_OK,
    XPM_SVC_STATE_NS_REGISTER_FAIL,
    XPM_SVC_STATE_RUNNING
};

/// represents the current state of the exporting process
static enum xpm_svc_state svc_state = XPM_SVC_STATE_INVALID;

/// our own iref of the exported service
static iref_t messaging_iref;

/// stores the callbacks to inform the user about arrived messages
static struct xeon_phi_messaging_cb callbacks;

/*
 * ----------------------------------------------------------------------------
 * Callbacks for the Flounder messages
 * ----------------------------------------------------------------------------
 */

/**
 * \brief handles the open requests targeting a specific card.
 *
 * \param _binding  Flounder binding we received the message from
 * \param xid       ID of the Xeon Phi where the target is located
 * \param msgframe  Capability of the message frame
 * \param type      The type of the channel
 * \param iface     Name of the interface
 * \param length    length of the interface name
 *
 * This function may only be called on the Xeon Phi driver.
 */
static void open_card_call_rx(struct xeon_phi_messaging_binding *_binding,
                              uint8_t xid,
                              struct capref msgframe,
                              uint8_t type,
                              char *iface,
                              size_t length)
{
    DEBUG_XMSG("recv open: xphi=%u, iface=%s, type=0x%02x\n", xid, iface, type);

    assert(xpm_is_phi == 0x1);
    if (callbacks.open_card != NULL) {
        callbacks.open_card(xid, msgframe, type, iface);
    }
}



/**
 * \brief handles the open requests targeting the the associated card/host
 *
 * \param _binding  Flounder binding we received the message from
 * \param msgframe  Capability of the message frame
 * \param type      The type of the channel
 * \param iface     Name of the interface
 * \param length    length of the interface name
 *
 * This function may only be called on the Xeon Phi driver.
 */
static void open_iface_call_rx(struct xeon_phi_messaging_binding *_binding,
                               struct capref msgframe,
                               uint8_t type,
                               char *iface,
                               size_t length)
{
    DEBUG_XMSG("recv open: iface=%s, type=0x%02x\n", iface, type);

    assert(xpm_is_phi == 0x1);

    if (callbacks.open_iface != NULL) {
        callbacks.open_iface(msgframe, type, iface);
    }
}

/**
 * \brief handles spawn requests on the Xeon Phi driver
 *
 * \param xid    ID of the target Xeon Phi
 * \param core   Which core to start the domain
 * \param name   Name of the domain to spawn
 * \param length length of the name
 *
 * This function may only be called on the Xeon Phi driver
 */
static void spawn_card_call_rx(struct xeon_phi_messaging_binding *_binding,
                               uint8_t xid,
                               uint8_t core,
                               char *name,
                               size_t length)
{
    DEBUG_XMSG("recv spawn: %s.%u.%u\n", name, xid, core);

    assert(xpm_is_phi == 0x1);

    if (callbacks.spawn_card != NULL) {
        callbacks.spawn_card(xid, core, name);
    }
}

/**
 * \brief handles spawn requests on the Xeon Phi driver on the associated
 *        Xeon Phi card or on the host.
 *
 * \param core   Which core to start the domain
 * \param name   Name of the domain to spawn
 * \param length length of the name
 *
 * This function may only be called on the Xeon Phi driver
 */
static void spawn_call_rx(struct xeon_phi_messaging_binding *_binding,
                          uint8_t core,
                          char *name,
                          size_t length)
{
    DEBUG_XMSG("recv spawn: %s.%u\n", name, core);

    assert(xpm_is_phi == 0x1);

    if (callbacks.spawn != NULL) {
        callbacks.spawn(core, name);
    }
}


/**
 * \brief handles the open requests of on the domain side
 *
 * \param _binding  Flounder binind we received the message from
 * \param msgframe  capability of the message frame
 * \param type      the type of the channel
 */
static void open_call_rx(struct xeon_phi_messaging_binding *_binding,
                         struct capref msgframe,
                         uint8_t type)
{
    DEBUG_XMSG("recv open: type=0x%02x\n", type);
    assert(xpm_is_phi == 0x0);
    if (callbacks.open != NULL) {
        callbacks.open(msgframe, type);
    }
}

/// Flounder receive vtbl
static struct xeon_phi_messaging_rx_vtbl xpm_rx_vtbl = {
    .open_iface = open_iface_call_rx,
    .open = open_call_rx,
    .open_card = open_card_call_rx,
    .spawn = spawn_call_rx,
    .spawn_card = spawn_card_call_rx
};

/*
 * --------------------------------------------------------------------------
 * Export and Connect functions
 */

/**
 * \brief Xeon Phi Messaging connect handler
 *
 * \param st       state, currently NULL
 * \param binding  the Flounder binding of the new connection
 */
static errval_t svc_connect_cb(void *st,
                               struct xeon_phi_messaging_binding *binding)
{
    DEBUG_XMSG("new connection to the service.\n");
    binding->rx_vtbl = xpm_rx_vtbl;

    return SYS_ERR_OK;
}

/**
 * \brief Xeon Phi Messaging export handler
 *
 * \param st   state, currently NULL
 * \param err  outcome of the exportation process
 * \param iref the iref of the exported interface
 */
static void svc_export_cb(void *st,
                          errval_t err,
                          iref_t iref)
{
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_EXPORT_FAIL;
        return;
    }

    DEBUG_XMSG("service exported @ iref:%u\n", iref);

    messaging_iref = iref;

    svc_state = XPM_SVC_STATE_EXPORT_OK;
}

/**
 * \brief initializes the messaging listener for messages over the Xeon Phi
 *        channel.
 *
 * \param fn    callback functions which are invoked when a message arrives
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_messaging_service_init(struct xeon_phi_messaging_cb *fn)
{
    errval_t err;

    DEBUG_XMSG("initializing Xeon Phi messaging service.\n");

    xeon_phi_messaging_set_callbacks(fn);

    svc_state = XPM_SVC_STATE_EXPORTING;

    err = xeon_phi_messaging_export(NULL,
                                    svc_export_cb,
                                    svc_connect_cb,
                                    get_default_waitset(),
                                    IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (svc_state == XPM_SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (svc_state == XPM_SVC_STATE_EXPORT_FAIL) {
        return FLOUNDER_ERR_BIND;
    }

    return SYS_ERR_OK;
}

/**
 * \brief sets the callback functions for the message handlers in the library
 *
 * \param fn the new function pointers to set.
 *
 * To disable callbacks either have the entry set to NULL or if fn==NULL then
 * all callbacks are disabled.
 */
void xeon_phi_messaging_set_callbacks(struct xeon_phi_messaging_cb *fn)
{
    if (fn) {
        callbacks = *fn;
    } else {
        memset(&callbacks, 0, sizeof(callbacks));
    }
}

/**
 * \brief starts the service by registering the service with the service name
 *
 * \param start_handler if non zero this will start the message handler loop
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 *
 * This function must not be called on the Xeon  Phi driver.
 */
errval_t xeon_phi_messaging_service_start(uint8_t start_handler)
{
    errval_t err;

    if (svc_state != XPM_SVC_STATE_EXPORT_OK) {
        /* service state is not exported */
        return XEON_PHI_ERR_MSG_NOT_INITIALIZED;
    }

    svc_state = XPM_SVC_STATE_NS_REGISTERING;

    char buf[50];
    snprintf(buf, 50, "%s.%u", disp_name(), disp_get_core_id());

    DEBUG_XMSG("registering iref:%u with name [%s]\n", messaging_iref, buf);
    err = nameservice_register(buf, messaging_iref);
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_NS_REGISTER_FAIL;
        return err;
    }

    svc_state = XPM_SVC_STATE_NS_REGISTER_OK;

    PRINTF_XMSG("service started on non-driver domain.\n");

    xpm_is_phi = 0x0;

    if (start_handler) {
        svc_state = XPM_SVC_STATE_RUNNING;
        messages_handler_loop();
    }

    return SYS_ERR_OK;
}

/**
 * \brief starts the service by registering the service with the service name
 *
 * \param start_handler if non zero this will start the message handler loop
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 *
 * This function MUST only be called on the Xeon Phi driver!
 */
errval_t xeon_phi_messaging_service_start_phi(uint8_t xeon_phi_id)
{
    errval_t err;

    /// a check that this function is called on the Xeon Phi driver domain.
    assert(strcmp("xeon_phi", disp_name()) == 0);

    if (svc_state != XPM_SVC_STATE_EXPORT_OK) {
        /* service state is not exported */
        return XEON_PHI_ERR_MSG_NOT_INITIALIZED;
    }

    svc_state = XPM_SVC_STATE_NS_REGISTERING;

    /*
     * building the service name for exporting the iref. This is a bit different
     * when running on the card because there is always just one driver running,
     * whereas on the host, there may be up to 8 running.
     */
    char buf[50];
#if !defined(__k1om__)
    snprintf(buf, 50, "%s.%u", XEON_PHI_MESSAGING_NAME, xeon_phi_id);
#else
    snprintf(buf, 50, "%s", XEON_PHI_MESSAGING_NAME);
#endif

    DEBUG_XMSG("registering iref:%u with name [%s]\n", messaging_iref, buf);
    err = nameservice_register(buf, messaging_iref);
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_NS_REGISTER_FAIL;
        return err;
    }

    svc_state = XPM_SVC_STATE_RUNNING;

    xpm_is_phi = 0x1;

    PRINTF_XMSG("service started on Xeon Phi driver\n");

    return SYS_ERR_OK;
}
