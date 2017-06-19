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
#include <lwip/inet.h>

#include <if/e10k_defs.h>

#include "port_management_support.h"
#include "device_manager_debug.h"



/******************************************************************************
 * Local state
 ******************************************************************************/

/** Connection to e10k management service */
struct e10k_binding *binding = NULL;

/******************************************************************************
 * Operations for filter interface
 ******************************************************************************/

// Callback for bind
static void bind_cb(void *st, errval_t err, struct e10k_binding *b)
{
    assert(err_is_ok(err));

    NDM_DEBUG("Sucessfully connected to management interface\n");

    binding = b;
    e10k_rpc_client_init(binding);
}

/** Open connection to management interface */
static void connect_to_mngif(char *dev_name)
{
    errval_t r;
    iref_t iref;
    const char *suffix = "_e10kmng";
    char name[strlen(dev_name) + strlen(suffix) + 1];

    // Build label for management service
    sprintf(name, "%s%s", dev_name, suffix);

    // Connect to service
    r = nameservice_blocking_lookup(name, &iref);
    assert(err_is_ok(r));

    r = e10k_bind(iref, bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}

/******************************************************************************
 * Operations for filter interface
 ******************************************************************************/

static void init_filters(char *dev_name, qid_t qid)
{
    NDM_DEBUG("e10_flt: init %s %d\n", dev_name, (int) qid);

    // Check if we are already initialized from another queue
    if (binding != NULL) {
        return;
    }

    connect_to_mngif(dev_name);

    // waiting for connection to succeed.
    NDM_DEBUG("e10k_init_filters: wait connection\n");
    while (binding == NULL) {
        messages_wait_and_handle_next();
    }
}

static void reg_arp_filters(uint64_t id, uint64_t len_rx,
                            uint64_t len_tx)
{
    USER_PANIC("reg_arp_filters() not supported in e10k filters");
}

static errval_t reg_filters(uint16_t port,
                            port_type_t type,
                            bufid_t buffer_id_rx,
                            bufid_t buffer_id_tx,
                            appid_t appid,
                            qid_t qid,
                            uint64_t *id, errval_t *rerr, uint64_t *filter_id)
{
    e10k_port_type_t t;
    assert(binding != NULL);

    NDM_DEBUG("e10k_reg_filters()\n");

    if (type == net_ports_PORT_TCP) {
        t = e10k_PORT_TCP;
    } else {
        t = e10k_PORT_UDP;
    }
    errval_t err;
    err = binding->rpc_tx_vtbl.register_port_filter(binding, buffer_id_rx, buffer_id_tx, qid, t, port, rerr, filter_id);

    return err;
}

static errval_t unreg_filters(uint64_t filter_id, qid_t qid)
{
    assert(binding != NULL);

    NDM_DEBUG("e10k_unreg_filters()\n");
    errval_t err, rerr;
    err = binding->rpc_tx_vtbl.unregister_filter(binding, qid, &rerr);
    assert(err_is_ok(err));
    
    return rerr;
}


/******************************************************************************
 * Get signature of this service
 ******************************************************************************/

static struct filters_tx_vtbl e10k_filts_mng = {
    .type = "e10k_filters",
    .init_filters = init_filters,
    .reg_arp_filters = reg_arp_filters,
    .reg_filters = reg_filters,
    .unreg_filters = unreg_filters,
};

struct filters_tx_vtbl *get_e10k_filt_mng_sign(void)
{
    return &e10k_filts_mng;
}
