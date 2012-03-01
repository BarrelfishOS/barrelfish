/**
 * \file
 * \brief Communication between LWIP and netd deamon
 *
 *  This code provides interface to commuincate with netd for purposes like
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
#include <trace/trace.h>
#include <netif/etharp.h>
#include <netif/bfeth.h>
#include "lwip/init.h"
#include <contmng/contmng.h>
#include <contmng/netbench.h>
#include <if/netd_defs.h>
#include <if/netd_rpcclient_defs.h>
#include <stdio.h>
#include <assert.h>
#include "idc_barrelfish.h"

#include "lwip_barrelfish_debug.h"

/*
 * If we are the owner of lwip stack, then we dont need rpc
 */
static bool is_owner = 0;
static uint16_t(*alloc_tcp_port) (void) = NULL;
static uint16_t(*alloc_udp_port) (void) = NULL;
static uint16_t(*bind_port) (uint16_t port, netd_port_type_t type) = NULL;
static void (*close_port) (uint16_t port, netd_port_type_t type) = NULL;


/*************************************************************
 * \defGroup LocalStates Local states
 *
 * @{
 *
 ****************************************************************/
static struct netd_rpc_client netd_rpc;
static bool netd_service_connected = false;


static struct netif netif;

static struct thread *trace_thread = NULL;
void thread_debug_regs(struct thread *t);

// Variables shared with idc_barrelfish.c
extern struct waitset *lwip_waitset;
extern struct net_queue_manager_binding *driver_connection[2];

/**
 * \brief handle msgs on the tx, rx and then the rest connections in that priority
 */
void network_polling_loop(void)
{
    errval_t err;

    while (1) {
        err = event_dispatch(lwip_waitset);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
#if 0
        if (trace_thread != NULL) {
            static int iter = 0;

            iter++;
            if (iter % 10 == 0) {
                thread_debug_regs(trace_thread);
            }
        }
#endif
    }
}





/*
 * @}
 */
// FIXME: not used.  Remove it
errval_t lwip_err_to_errval(err_t e)
{
    switch (e) {
        case ERR_OK:
            return SYS_ERR_OK;
        case ERR_MEM:
            return LWIP_ERR_MEM;
        case ERR_BUF:
            return LWIP_ERR_BUF;
        case ERR_TIMEOUT:
            return LWIP_ERR_TIMEOUT;
        case ERR_RTE:
            return LWIP_ERR_RTE;
        case ERR_ABRT:
            return LWIP_ERR_ABRT;
        case ERR_RST:
            return LWIP_ERR_RST;
        case ERR_CLSD:
            return LWIP_ERR_CLSD;
        case ERR_CONN:
            return LWIP_ERR_CONN;
        case ERR_VAL:
            return LWIP_ERR_VAL;
        case ERR_ARG:
            return LWIP_ERR_ARG;
        case ERR_USE:
            return LWIP_ERR_USE;
        case ERR_IF:
            return LWIP_ERR_IF;
        case ERR_ISCONN:
            return LWIP_ERR_ISCONN;
        case ERR_INPROGRESS:
            return LWIP_ERR_INPROGRESS;
        default:
            USER_PANIC("unknown LWIP error in lwip_err_to_errval");
    }
}


/***************************************************************
    Adding new code to communicate with netd server
*/

/****************************************************************
 * \defGroup netd_connectivity  Code to connect and work with netd.
 *
 * @{
 *
 *****************************************************************/
/**
 * \brief Callback function when bind is successful.
 *  Code inspired (ie. copied) from "start_client" function.
 */
static void netd_bind_cb(void *st, errval_t err, struct netd_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed for netd");
        abort();
    }
    LWIPBF_DEBUG("netd_bind_cb: called\n");

    err = netd_rpc_client_init(&netd_rpc, b);
    if (!err_is_ok(err)) {
        printf("netd_bind_cb failed in init\n");
        abort();
    }

    netd_service_connected = true;
    LWIPBF_DEBUG("netd_bind_cb: netd bind successful!\n");
}

/**
 * \brief Connects the lwip instance with netd daemon.
 *  Code inspired (ie. copied) from "start_client" function.
 */
static void init_netd_connection(char *service_name)
{
    LWIPBF_DEBUG("init_netd_connection: called\n");
    assert(service_name != NULL);
    LWIPBF_DEBUG("init_netd_connection: connecting to [%s]\n", service_name);

    errval_t err;
    iref_t iref;

    LWIPBF_DEBUG("init_netd_connection: resolving driver %s\n", service_name);

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lwip: could not connect to the netd driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    LWIPBF_DEBUG("init_netd_connection: connecting\n");

    err = netd_bind(iref, netd_bind_cb, NULL, lwip_waitset,
                    IDC_BIND_FLAGS_DEFAULT);
    if (!err_is_ok(err)) {
        printf("netd_bind_cb failed in init\n");
        abort();
    }

    LWIPBF_DEBUG("init_netd_connection: terminated\n");
}



void idc_connect_to_netd(char *server_name)
{
    LWIPBF_DEBUG("idc_connect_to_netd: wait for netd connection\n");

    /* FIXME: decide if this is the best place to connect with netd */
    init_netd_connection(server_name);

    // XXX: dispatch on default waitset until bound
    struct waitset *dws = get_default_waitset();

    while (!netd_service_connected) {
        errval_t err = event_dispatch(dws);

        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch while binding");
        }
    }
    LWIPBF_DEBUG("idc_connect_to_netd: terminated\n");
}


void idc_get_ip(void)
{
    if (is_owner) {
        assert(!"owner of lwip should never ask for ip through API\n");
        abort();
    }
    LWIPBF_DEBUG("On the way of getting IP\n");

    errval_t err;
    struct ip_addr ip, gw, nm;

    err = netd_rpc.vtbl.get_ip_info(&netd_rpc, &ip.addr, &gw.addr, &nm.addr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("got answer, now setting up things\n");
    netif_add(&netif, &ip, &nm, &gw, NULL, bfeth_init, ethernet_input);
    netif_set_default(&netif);
    netif_set_up(&netif);

    LWIPBF_DEBUG("client: owner has the IP address %d.%d.%d.%d\n",
                 ip4_addr1(&netif.ip_addr), ip4_addr2(&netif.ip_addr),
                 ip4_addr3(&netif.ip_addr), ip4_addr4(&netif.ip_addr));
}



/***********************************************************/
/************* Port management *******************/

static err_t idc_close_port(uint16_t port, int port_type)
{
    LWIPBF_DEBUG("idc_close_port: called\n");
    if (is_owner) {
        close_port((uint64_t) port, port_type);
        return ERR_OK;
    }

    LWIPBF_DEBUG("idc_close_port: called\n");

    errval_t err, msgerr;

    err = netd_rpc.vtbl.close_port(&netd_rpc, port_type, port, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("idc_close_tcp_port: returning\n");

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    }                           // FIXME: other errors?
    return ERR_OK;
}


err_t idc_close_udp_port(uint16_t port)
{
    return idc_close_port(port, netd_PORT_UDP);
}


err_t idc_close_tcp_port(uint16_t port)
{
    return idc_close_port(port, netd_PORT_TCP);
}

static err_t idc_bind_port(uint16_t port, netd_port_type_t port_type)
{
    if (is_owner) {
        LWIPBF_DEBUG("idc_bind_port: called by owner\n");
        return bind_port(port, port_type);
    }

    LWIPBF_DEBUG("idc_bind_port: called\n");

    errval_t err, msgerr;

    /* getting the proper buffer id's here */
    err = netd_rpc.vtbl.bind_port(&netd_rpc, port_type, port,
                                  /* buffer for RX */
                                  ((struct client_closure_NC *)
                                   driver_connection[RECEIVE_CONNECTION]->st)->
                                  buff_ptr->buffer_id,
                                  /* buffer for TX */
                                  ((struct client_closure_NC *)
                                   driver_connection[TRANSMIT_CONNECTION]->st)->
                                  buff_ptr->buffer_id, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("idc_new_tcp_port: terminated\n");

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    }                           // FIXME: other errors?
    return ERR_OK;
}


err_t idc_bind_udp_port(uint16_t port)
{
    return idc_bind_port(port, netd_PORT_UDP);
}


err_t idc_bind_tcp_port(uint16_t port)
{
    return idc_bind_port(port, netd_PORT_TCP);
}

static err_t idc_new_port(uint16_t * port_no, netd_port_type_t port_type)
{
    /* NOTE: function with same name exists in Kaver's code for reference
       purpose */
    errval_t err, msgerr;

    LWIPBF_DEBUG("idc_new_port: called\n");


    /* getting the proper buffer id's here */
    err = netd_rpc.vtbl.get_port(&netd_rpc, port_type,
                                 /* buffer for RX */
                                 ((struct client_closure_NC *)
                                  driver_connection[RECEIVE_CONNECTION]->st)->
                                 buff_ptr->buffer_id,
                                 /* buffer for TX */
                                 ((struct client_closure_NC *)
                                  driver_connection[TRANSMIT_CONNECTION]->st)->
                                 buff_ptr->buffer_id, &msgerr, port_no);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("idc_new_tcp_port: terminated\n");
    return msgerr;
}

err_t idc_tcp_new_port(uint16_t * port_no)
{
    if (is_owner) {
        *port_no = alloc_tcp_port();
        return SYS_ERR_OK;
    }

    return idc_new_port(port_no, netd_PORT_TCP);
}


err_t idc_udp_new_port(uint16_t * port_no)
{
    if (is_owner) {
        *port_no = alloc_udp_port();
        return SYS_ERR_OK;

    }

    return idc_new_port(port_no, netd_PORT_UDP);
}


static err_t idc_redirect(struct ip_addr *local_ip, u16_t local_port,
                          struct ip_addr *remote_ip, u16_t remote_port,
                          netd_port_type_t port_type)
{
    if (is_owner) {
        // redirecting doesn't make sense if we are the owner
        return ERR_USE;         // TODO: correct error
    }

    errval_t err, msgerr;

    /* getting the proper buffer id's here */
    err =
      netd_rpc.vtbl.redirect(&netd_rpc, port_type, local_ip->addr, local_port,
                             remote_ip->addr, remote_port,
                             /* buffer for RX */
                             ((struct client_closure_NC *)
                              driver_connection[RECEIVE_CONNECTION]->st)->
                             buff_ptr->buffer_id,
                             /* buffer for TX */
                             ((struct client_closure_NC *)
                              driver_connection[TRANSMIT_CONNECTION]->st)->
                             buff_ptr->buffer_id, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending redirect");
    }

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    } else if (msgerr == PORT_ERR_REDIRECT) {
        return ERR_USE;         // TODO: correct error
    }
// FIXME: other errors?
    return ERR_OK;
}

static err_t idc_pause(struct ip_addr *local_ip, u16_t local_port,
                       struct ip_addr *remote_ip, u16_t remote_port,
                       netd_port_type_t port_type)
{
    if (is_owner) {
        // redirecting doesn't make sense if we are the owner
        return ERR_USE;         // TODO: correct error
    }

    errval_t err, msgerr;

    /* getting the proper buffer id's here */
    err =
      netd_rpc.vtbl.redirect_pause(&netd_rpc, port_type, local_ip->addr,
                                   local_port, remote_ip->addr, remote_port,
                                   /* buffer for RX */
                                   ((struct client_closure_NC *)
                                    driver_connection[RECEIVE_CONNECTION]->st)->
                                   buff_ptr->buffer_id,
                                   /* buffer for TX */
                                   ((struct client_closure_NC *)
                                    driver_connection[TRANSMIT_CONNECTION]->
                                    st)->buff_ptr->buffer_id, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending pause");
    }

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    } else if (msgerr == PORT_ERR_REDIRECT) {
        return ERR_USE;         // TODO: correct error
    }
// FIXME: other errors?
    return ERR_OK;
}


/*
err_t idc_redirect_udp_port(uint16_t port)
{
    return idc_redirect_port(port, netd_PORT_UDP);
}
*/

err_t idc_redirect_tcp(struct ip_addr * local_ip, u16_t local_port,
                       struct ip_addr * remote_ip, u16_t remote_port)
{
    return idc_redirect(local_ip, local_port, remote_ip, remote_port,
                        netd_PORT_TCP);
}

err_t idc_pause_tcp(struct ip_addr * local_ip, u16_t local_port,
                    struct ip_addr * remote_ip, u16_t remote_port)
{
    return idc_pause(local_ip, local_port, remote_ip, remote_port,
                     netd_PORT_TCP);
}


void perform_ownership_housekeeping(uint16_t(*alloc_tcp_ptr) (void),
                                    uint16_t(*alloc_udp_ptr) (void),
                                    uint16_t(*bind_port_ptr) (uint16_t,
                                                              netd_port_type_t),
                                    void (*close_port_ptr) (uint16_t,
                                                            netd_port_type_t))
{
    is_owner = true;
    alloc_tcp_port = alloc_tcp_ptr;
    alloc_udp_port = alloc_udp_ptr;
    bind_port = bind_port_ptr;
    close_port = close_port_ptr;
}

