/**
 * \file
 * \brief Communication between LWIP and net_ports deamon
 *
 *  This code provides interface to commuincate with net_ports for purposes like
 *  opening/closing ports, get IP address, etc
 */

/*
 * Copyright (c) 2007-11 ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <net_interfaces/net_interfaces.h>
#include <trace/trace.h>
#include <netif/etharp.h>
#include <netif/bfeth.h>
#include <contmng/contmng.h>
#include <contmng/netbench.h>
#include <if/net_ports_defs.h>
#include <if/net_ports_rpcclient_defs.h>
#include <stdio.h>
#include <assert.h>

#include "idc_net_control.h"


/*************************************************************
 * \defGroup LocalStates Local states
 *
 * @{
 *
 ****************************************************************/
static net_ports_appid_t appid_delete = 0;



/***************************************************************
    Adding new code to communicate with net_ports server
*/

/****************************************************************
 * \defGroup net_ports_connectivity  Code to connect and work with net_ports.
 *
 * @{
 *
 *****************************************************************/
/**
 * \brief Callback function when bind is successful.
 *  Code inspired (ie. copied) from "start_client" function.
 */
static void net_ports_bind_cb(void *st, errval_t err, struct net_ports_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed for net_ports");
        abort();
    }
    debug_printf("net_ports_bind_cb: called\n");

    struct netg_interface* netif = st;

    err = net_ports_rpc_client_init(&netif->net_ports_rpc, b);
    if (!err_is_ok(err)) {
        printf("net_ports_bind_cb failed in init\n");
        abort();
    }

    netif->net_ports_service_connected = true;
    debug_printf("net_ports_bind_cb: net_ports bind successful!\n");
}

/**
 * \brief Connects the lwip instance with net_ports daemon.
 *  Code inspired (ie. copied) from "start_client" function.
 */
static void init_net_ports_connection(struct netg_interface* netif, char *service_name)
{
    debug_printf("init_net_ports_connection: called\n");
    assert(service_name != NULL);
    debug_printf("init_net_ports_connection: connecting to [%s]\n", service_name);

    errval_t err;
    iref_t iref;

    debug_printf("init_net_ports_connection: resolving driver %s\n", service_name);

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lwip: could not connect to the net_ports driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    debug_printf("init_net_ports_connection: connecting\n");

    err = net_ports_bind(iref, net_ports_bind_cb, netif, get_default_waitset(),
                    IDC_BIND_FLAGS_DEFAULT);
    if (!err_is_ok(err)) {
        printf("net_ports_bind_cb failed in init\n");
        abort();
    }

    debug_printf("init_net_ports_connection: terminated\n");
}


// Connects to the port manager service
// Blocking call: returns only when connection is done
// In case of error, it will panic!!
void idc_connect_port_manager_service(struct netg_interface* netif, char *service_name)
{
    //LWIPBF_DEBUG
    printf("idc_c_port_mng_srv: trying to [%s]\n", service_name);
    netif->net_ports_service_connected = false;

    /* FIXME: decide if this is the best place to connect with net_ports */
    init_net_ports_connection(netif, service_name);

    // XXX: dispatch on default waitset until bound
    struct waitset *dws = get_default_waitset();

    while (!netif->net_ports_service_connected) {
        errval_t err = event_dispatch(dws);

        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch while binding");
        }
    }
    //LWIPBF_DEBUG
    printf("idc_c_port_mng_srv: success [%s]\n", service_name);
}




/***********************************************************/
/************* Port management *******************/

static errval_t idc_close_port(struct netg_interface* netif, uint16_t port, int port_type, uint64_t queue_id)
{
    debug_printf("idc_close_port: called\n");
    assert(netif->net_ports_service_connected);

    errval_t err, msgerr;

    err = netif->net_ports_rpc.vtbl.close_port(&netif->net_ports_rpc, port_type, port,
                                  appid_delete, queue_id,
                                  &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    debug_printf("idc_close_port: returning\n");

    if (msgerr == PORT_ERR_IN_USE) {
    	//fixme wrong error
        return NETG_ERR_NOT_BOUND;
    }
    return SYS_ERR_OK;
}


errval_t idc_close_udp_port(struct netg_interface* netif, uint16_t port, uint64_t queue_id)
{
    return idc_close_port(netif, port, net_ports_PORT_UDP, queue_id);
}


errval_t idc_close_tcp_port(struct netg_interface* netif, uint16_t port, uint64_t queue_id)
{
    return idc_close_port(netif, port, net_ports_PORT_TCP, queue_id);
}

static errval_t idc_bind_port(struct netg_interface* netif, int16_t port, net_ports_port_type_t port_type, uint64_t queue_id)
{
    debug_printf("idc_bind_port: called\n");

    assert(netif->net_ports_service_connected);

    errval_t err, msgerr;

    /* getting the proper buffer id's here */
    err = netif->net_ports_rpc.vtbl.bind_port(&netif->net_ports_rpc, port_type, port,
                                  /* buffer for RX */
                                   netif->queues->rx_binding.bufid,
                                  /* buffer for TX */
                                   netif->queues->tx_binding.bufid,
                                  appid_delete, queue_id,
                                  &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    debug_printf("idc_bind_port: terminated\n");

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    }                           // FIXME: other errors?
    return ERR_OK;
}


errval_t idc_bind_udp_port(struct netg_interface* netif, uint16_t port, uint64_t queue_id)
{
    return idc_bind_port(netif, port, net_ports_PORT_UDP, queue_id);
}


errval_t idc_bind_tcp_port(struct netg_interface* netif, uint16_t port, uint64_t queue_id)
{
    return idc_bind_port(netif, port, net_ports_PORT_TCP, queue_id);
}

static errval_t idc_new_port(struct netg_interface* netif, uint16_t * port_no, net_ports_port_type_t port_type, uint64_t queue_id)
{
    /* NOTE: function with same name exists in Kaver's code for reference
       purpose */
    errval_t err, msgerr;

    debug_printf("idc_new_port: called\n");

    assert(netif->net_ports_service_connected);

    /* getting the proper buffer id's here */
    err = netif->net_ports_rpc.vtbl.get_port(&netif->net_ports_rpc, port_type,
    							 /* buffer for RX */
								 netif->queues->rx_binding.bufid,
								 /* buffer for TX */
								 netif->queues->tx_binding.bufid,
                                 appid_delete, queue_id,
                                 &msgerr, port_no);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    debug_printf("idc_new_port: terminated\n");
    return msgerr;
}

errval_t idc_tcp_new_port(struct netg_interface* netif, uint16_t * port_no, uint64_t queue_id)
{
    return idc_new_port(netif, port_no, net_ports_PORT_TCP, queue_id);
}


errval_t idc_udp_new_port(struct netg_interface* netif, uint16_t * port_no, uint64_t queue_id)
{
    return idc_new_port(netif, port_no, net_ports_PORT_UDP, queue_id);
}
