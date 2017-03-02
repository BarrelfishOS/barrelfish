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
#include <ipv4/lwip/inet.h>

#include <if/sfn5122f_defs.h>

#include "port_management_support.h"
#include "device_manager_debug.h"



/******************************************************************************
 * Local state
 ******************************************************************************/

/** Connection to sfn5122f management service */
struct sfn5122f_binding *sfn5122f_binding = NULL;

/******************************************************************************
 * Operations for filter interface
 ******************************************************************************/

// Callback for bind
static void bind_cb(void *st, errval_t err, struct sfn5122f_binding *b)
{
    assert(err_is_ok(err));

    NDM_DEBUG("Sucessfully connected to management interface\n");

    sfn5122f_binding = b;
    sfn5122f_rpc_client_init(sfn5122f_binding);
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
                            qid_t qid,
                            uint64_t *id, errval_t *rerr, uint64_t *filter_id)
{
    sfn5122f_port_type_t t;
    assert(sfn5122f_binding != NULL);

    NDM_DEBUG("sfn5122f_reg_filters()\n");

    if (type == net_ports_PORT_TCP) {
        t = sfn5122f_PORT_TCP;
    } else {
        t = sfn5122f_PORT_UDP;
    }
    errval_t err;
    err = sfn5122f_binding->rpc_tx_vtbl.register_port_filter(sfn5122f_binding, buffer_id_rx, buffer_id_tx, qid, t, port, rerr, filter_id);

    return SYS_ERR_OK;
}

static errval_t unreg_filters(uint64_t filter_id, qid_t qid)
{
    assert(sfn5122f_binding != NULL);

    NDM_DEBUG("sfn5122f_unreg_filters()\n");
    errval_t err, rerr;
    err = sfn5122f_binding->rpc_tx_vtbl.unregister_filter(sfn5122f_binding, filter_id, &rerr);
    assert(err_is_ok(err));
    
    return rerr;
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
