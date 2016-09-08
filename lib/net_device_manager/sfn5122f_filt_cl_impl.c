/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <contmng/contmng.h>
#include <ipv4/lwip/inet.h>

#include <if/sfn5122f_defs.h>

#include "port_management_support.h"
#include "device_manager_debug.h"



/******************************************************************************
 * Local state
 ******************************************************************************/

/** Connection to sfn5122f management service */
struct sfn5122f_binding *sfn5122f_binding = NULL;

struct cont_queue *sfn5122f_c_queue = NULL;


/******************************************************************************
 * Operations for filter interface
 ******************************************************************************/

// Callback from sfn5122f
static void idc_filter_registered(struct sfn5122f_binding *b,
                                  uint64_t buf_id_rx,
                                  uint64_t buf_id_tx,
                                  errval_t err,
                                  uint64_t filter)
{
    NDM_DEBUG("sfn5122f_idc_filter_registered(f=%"PRIu64" rx=%"PRIu64" tx=%"PRIu64
            ")\n", filter, buf_id_rx, buf_id_tx);
    handle_filter_response(filter, err, filter, buf_id_rx, buf_id_tx, 1);
}

// Callback from sfn5122f
static void idc_filter_unregistered(struct sfn5122f_binding *b,
                                    uint64_t filter,
                                    errval_t err)
{
    NDM_DEBUG("sfn5122f_idc_filter_unregistered(%"PRIu64")\n", filter);
}

static errval_t send_register_port_filter(struct q_entry e)
{
    if (sfn5122f_binding->can_send(sfn5122f_binding)) {
        return sfn5122f_binding->tx_vtbl.register_port_filter(
                sfn5122f_binding, MKCONT(cont_queue_callback, sfn5122f_c_queue),
                e.plist[0], e.plist[1], e.plist[2], e.plist[3], e.plist[4]);
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/** Register filter with sfn5122f card driver */
static void idc_register_port_filter(uint64_t buf_id_rx,
                                     uint64_t buf_id_tx,
                                     uint16_t queue,
                                     sfn5122f_port_type_t type,
                                     uint16_t port)
{
    struct q_entry entry;
    NDM_DEBUG("sfn5122f_idc_register_port_filter(q=%d p=%d rx=%"PRIu64" tx=%"
            PRIu64")\n", queue, port, buf_id_rx, buf_id_tx);

    memset(&entry, 0, sizeof(struct q_entry));

    entry.handler = send_register_port_filter;
    entry.binding_ptr = sfn5122f_binding;
    entry.plist[0] = buf_id_rx;
    entry.plist[1] = buf_id_tx;
    entry.plist[2] = queue;
    entry.plist[3] = type;
    entry.plist[4] = port;

    enqueue_cont_q(sfn5122f_c_queue, &entry);
}

static errval_t send_unregister_filter(struct q_entry e)
{
    if (sfn5122f_binding->can_send(sfn5122f_binding)) {
        return sfn5122f_binding->tx_vtbl.unregister_filter(
                sfn5122f_binding, MKCONT(cont_queue_callback, sfn5122f_c_queue),
                e.plist[0]);
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/** Unregister filter with sfn5122f card driver */
static void idc_unregister_filter(uint64_t filter)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));

    entry.handler = send_unregister_filter;
    entry.binding_ptr = sfn5122f_binding;
    entry.plist[0] = filter;

    enqueue_cont_q(sfn5122f_c_queue, &entry);
}

static struct sfn5122f_rx_vtbl rx_vtbl = {
    .filter_registered = idc_filter_registered,
    .filter_unregistered = idc_filter_unregistered,
};

// Callback for bind
static void bind_cb(void *st, errval_t err, struct sfn5122f_binding *b)
{
    assert(err_is_ok(err));

    NDM_DEBUG("Sucessfully connected to management interface\n");

    b->rx_vtbl = rx_vtbl;

    sfn5122f_binding = b;
    sfn5122f_c_queue = create_cont_q("sfn5122f_filters");
}

/** Open connection to management interface */
static void connect_to_mngif(char *dev_name)
{
    errval_t r;
    iref_t iref;
    const char *suffix = "_sfn5122fmng";
    char name[strlen(dev_name) + strlen(suffix) + 1];

    // Build label for management service
    sprintf(name, "%s%s", dev_name, suffix);

    // Connect to service
    r = nameservice_blocking_lookup(name, &iref);
    assert(err_is_ok(r));

    r = sfn5122f_bind(iref, bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}

/******************************************************************************
 * Operations for filter interface
 ******************************************************************************/

static void init_filters(char *dev_name, qid_t qid)
{
    NDM_DEBUG("sfn5122f_flt: init %s %d\n", dev_name, (int) qid);

    // Check if we are already initialized from another queue
    if (sfn5122f_binding != NULL) {
        return;
    }

    connect_to_mngif(dev_name);

    // waiting for connection to succeed.
    NDM_DEBUG("sfn5122f_init_filters: wait connection\n");
    while (sfn5122f_binding == NULL) {
        messages_wait_and_handle_next();
    }
}

static void reg_arp_filters(uint64_t id, uint64_t len_rx,
                            uint64_t len_tx)
{
    USER_PANIC("reg_arp_filters() not supported yet in sfn5122f filters");
}

static errval_t reg_filters(uint16_t port,
                            port_type_t type,
                            bufid_t buffer_id_rx,
                            bufid_t buffer_id_tx,
                            appid_t appid,
                            qid_t qid)
{
    sfn5122f_port_type_t t;
    assert(sfn5122f_binding != NULL);

    NDM_DEBUG("sfn5122f_reg_filters()\n");

    if (type == net_ports_PORT_TCP) {
        t = sfn5122f_PORT_TCP;
    } else {
        t = sfn5122f_PORT_UDP;
    }

    idc_register_port_filter(buffer_id_rx, buffer_id_tx, qid, t, port);

    return SYS_ERR_OK;
}

static void unreg_filters(uint64_t filter_id, qid_t qid)
{
    assert(sfn5122f_binding != NULL);

    NDM_DEBUG("sfn5122f_unreg_filters()\n");
    idc_unregister_filter(filter_id);
}


/******************************************************************************
 * Get signature of this service
 ******************************************************************************/

static struct filters_tx_vtbl sfn5122f_filts_mng = {
    .type = "sfn5122f_filters",
    .init_filters = init_filters,
    .reg_arp_filters = reg_arp_filters,
    .reg_filters = reg_filters,
    .unreg_filters = unreg_filters,
};

struct filters_tx_vtbl *get_sfn5122f_filt_mng_sign(void)
{
    return &sfn5122f_filts_mng;
}


