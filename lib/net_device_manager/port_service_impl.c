/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
//#include <barrelfish/net_constants.h>
#include <stdio.h>
#include <string.h>
#include <net_device_manager/net_ports_service.h>
#include <if/net_soft_filters_defs.h>
#include <if/net_ports_defs.h>

#include "device_manager_debug.h"


/****************************************************************
* Global datastructure
*****************************************************************/

/*****************************************************************
* Prototypes
*****************************************************************/

typedef net_ports_port_type_t port_type_t;
typedef net_ports_appid_t appid_t;
typedef net_ports_qid_t qid_t;
typedef net_ports_bufid_t bufid_t;

// gets any next available port number
// To be used on client side which does not care about the port number
// Will be called by calls like connect, sendto
// FIXME: eventually buffer_ids should be removed and only appid should be used
// Algorithm
//   *. Find free port number
//   *. Create appropriate filter
//   *. Insert appropriate filter
//   *. Once you get ack for filter being inserted successfully,
//          send back the response (done by another funcation)
static void get_port(struct net_ports_binding *cc,
                    port_type_t type,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid);

// Allocates the specified port number to the application
// To be used on server side who wants to listen on perticular port number
// will be called by listen
// FIXME: eventually buffer_ids should be removed and only appid should be used
// Algorithm
//   *. Make sure that requested port is available
//   *. Create appropriate filter
//   *. Insert appropriate filter
//   *. Once you get ack for filter being inserted successfully,
//          send back the response (done by another funcation)
static void bind_port(struct net_ports_binding *cc,
                    port_type_t type,
                    uint16_t port_no,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid);


// Close the specified port number
// Algorithm
//   *. Make sure that requested port is open and belongs to requesting app
//   *. Find out the filter number associated with this port
//   *. Send request to remove the filter
//   *. Once you get ack for filter being removed successfully,
//          send back the response (done by another funcation)
static void close_port(struct net_ports_binding *cc,
                    port_type_t type,
                    uint16_t port_no,
                    uint64_t appid,
                    uint64_t queueid);


// Get the mac address for given machine
static void get_mac_address(struct net_ports_binding *cc);

// service mappings
static struct net_ports_rx_vtbl rx_net_ports_vtbl = {
//    .get_ip_info_call = get_ip_info,
    .get_mac_address_call = get_mac_address,
    .get_port_call = get_port,
    .bind_port_call = bind_port,
    .close_port_call = close_port,
};

/*****************************************************************
* Dealing with new connections
*****************************************************************/

static errval_t connect_ports_cb(void *st, struct net_ports_binding *b)
{
    errval_t err = SYS_ERR_OK;
    NDM_DEBUG("new application came in\n");
    b->st = NULL; // FIXME: malloc and init the app_closure
    b->rx_vtbl = rx_net_ports_vtbl;
    return err;
} // end function: connect_ports_cb

/*****************************************************************
* exporting service
*****************************************************************/

static void export_ports_cb(void *st, errval_t err, iref_t iref)
{
    NDM_DEBUG("export successful!\n");
} // end function: export_ports_cb


// Initialzes the port number management service
int init_ports_service(void)
{
    NDM_DEBUG("init_ports_service called\n");
   // exporting net_ports interface
    errval_t err = net_ports_export(NULL, export_ports_cb, connect_ports_cb,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC("net_ports_export failed!");
    }
    return 0;
} // end function: init_ports_service

/*****************************************************************
* Actual service
*****************************************************************/

// gets any next available port number
// To be used on client side which does not care about the port number
// Will be called by calls like connect, sendto
// FIXME: eventually buffer_ids should be removed and only appid should be used
// Algorithm
//   *. Find free port number
//   *. Create appropriate filter
//   *. Insert appropriate filter
//   *. Once you get ack for filter being inserted successfully,
//          send back the response (done by another funcation)
static void get_port(struct net_ports_binding *cc,
                    port_type_t type,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid)
{
    NDM_DEBUG("get_port called\n");
    return;
} // end function: get_port

// Allocates the specified port number to the application
// To be used on server side who wants to listen on perticular port number
// will be called by listen
// FIXME: eventually buffer_ids should be removed and only appid should be used
// Algorithm
//   *. Make sure that requested port is available
//   *. Create appropriate filter
//   *. Insert appropriate filter
//   *. Once you get ack for filter being inserted successfully,
//          send back the response (done by another funcation)
static void bind_port(struct net_ports_binding *cc,
                    port_type_t type,
                    uint16_t port_no,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid)
{
    NDM_DEBUG("bind_port called\n");
    return;
} // end function: bind_port



// Close the specified port number
// Algorithm
//   *. Make sure that requested port is open and belongs to requesting app
//   *. Find out the filter number associated with this port
//   *. Send request to remove the filter
//   *. Once you get ack for filter being removed successfully,
//          send back the response (done by another funcation)
static void close_port(struct net_ports_binding *cc,
                    port_type_t type,
                    uint16_t port_no,
                    uint64_t appid,
                    uint64_t queueid)
{
    NDM_DEBUG("close_port called\n");
    return;
} // end function: close_port


// Get the mac address for given machine
static void get_mac_address(struct net_ports_binding *cc)
{
    NDM_DEBUG("get_mac_address called\n");
    return;
} // end function: get_mac_address


