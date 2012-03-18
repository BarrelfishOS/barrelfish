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
#include <barrelfish/net_constants.h>
#include <stdio.h>
#include <string.h>
#include <net_device_manager/net_ports_service.h>
#include <if/net_soft_filters_defs.h>
#include <if/net_ports_defs.h>

#include "port_management_support.h"
#include "device_manager_debug.h"


/****************************************************************
* Global datastructure
*****************************************************************/

/****************************************************************
* Local states
*****************************************************************/
static char my_dev_name[MAX_NET_SERVICE_NAME_LEN] = {0};


/*****************************************************************
* Prototypes
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
    char service_name[MAX_NET_SERVICE_NAME_LEN];

    snprintf(service_name, sizeof(service_name), "%s%s", my_dev_name,
             NET_PORTS_MNG_SUFFIX);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service[%s] export failed", service_name);
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }

    NDM_DEBUG("service [%s] exported at iref %u\n", service_name, iref);

    // register this iref with the name service
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [%s]", service_name);
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }

    NDM_DEBUG("export successful!\n");
} // end function: export_ports_cb


// Initialzes the port number management service
int init_ports_service(char *dev_name)
{
    assert(dev_name != NULL);

    // FIXME: for every queue available
    // Connect with soft_filters_service
    connect_soft_filters_service(dev_name, 0);

    // start the port management service
    strncpy(my_dev_name, dev_name, sizeof(my_dev_name));

    NDM_DEBUG("init_ports_service called for device [%s]\n", my_dev_name);
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

#if 0
    errval_t err = SYS_ERR_OK;
    uint64_t port;
    int32_t len_rx, len_tx;
    struct buffer_port_translation *bp;
    struct net_user *this_net_app = (struct net_user *) cc->st;

    NETD_DEBUG("get_port: called\n");

    /* NOTE: check if someone else is using the filter location */
    if (filter_mem_lock) {
        err = FILTER_ERR_FILTER_BUSY;
        /* FIXME: as there is only one registered location for filter
           transfer, only one filter registration can be done at one time. */
        NETD_DEBUG("netd is busy.\n");
        /* send continuation msg about new port */
        idc_new_port(cc, err, 0);
        return;
    }

    /* Record the state that this port is allocated to this app */
    bp =
      (struct buffer_port_translation *)
      malloc(sizeof(struct buffer_port_translation));
    if (bp == NULL) {
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        NETD_DEBUG("netd is out of memory.\n");
        /* send continuation msg about new port */
        idc_new_port(cc, err, 0);
        return;
    }
    memset(bp, 0, sizeof(struct buffer_port_translation));

    /* FIXME: get free port from portalloc system */
    if (type == netd_PORT_TCP) {
        port = alloc_tcp_port();
    } else {
        port = alloc_udp_port();
    }

    if (port == 0) {
        err = PORT_ERR_NO_MORE_PORT;
        NETD_DEBUG("all the ports for this user are allocated!\n");
        free(bp);
        /* send continuation msg about new port */
        idc_new_port(cc, err, 0);
        return;
    }

    /* FIXME: these things won't be present right now.. */
//    assert(local_ip.addr);
//    assert(soft_filters_conn != NULL);

    /* add information about this session to the list of live sessions */
    bp->st = cc;
    bp->local_port = port;
    bp->type = type;
    bp->buffer_id_rx = buffer_id_rx;
    bp->buffer_id_tx = buffer_id_tx;
    bp->active = false;
    bp->bind = false;
    bp->closing = false;
    bp->redirected = false;
    bp->next = this_net_app->open_ports;
    this_net_app->open_ports = bp;


    /* create rx, tx filter around that port */
    filter_mem_lock = true;     /* NOTE: filter memory is in use
                                   till "registered_filter" is called by filter_manager */
    uint64_t id = populate_rx_tx_filter_mem(port, type, &len_rx, &len_tx);

    /* Register the filter with soft_filters */
    NETD_DEBUG("get_port: trying to register the filter with id %" PRIu64 "\n",
               id);
    idc_register_filter(id, len_rx, len_tx, buffer_id_rx, buffer_id_tx,
                        NORMAL_FILTER, 0);

    NETD_DEBUG("get_port: exiting\n");
#endif // 0

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


