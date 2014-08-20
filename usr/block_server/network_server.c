/**
 * \file
 * \brief Network server thread of the bulk server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

#include <lwip/tcp.h>
#include <lwip/init.h>

#include "network_common.h"
#include "network_server.h"

#if IMPLEMENTED

/**
 * \brief handles block read request of a connected client
 */
static errval_t handle_block_read(size_t start_block, size_t count)
{
    assert(!"NYI: block_net_init");
    return SYS_ERR_OK;
}

/**
 * \brief handles block write requests of a connected client
 */
static errval_t handle_block_write(size_t start_block, size_t count)
{
    assert(!"NYI: block_net_init");
    return SYS_ERR_OK;
}

/**
 * \brief handler for disconnect requests
 */
static errval_t handle_disconnect(void)
{
    // free up resources

    // close the network connection
    assert(!"NYI: block_net_init");
    return SYS_ERR_OK;
}

/**
 * \brief handles a generic request from a client and checks the request type
 */
static errval_t handle_request_common(void)
{
    // check request type

    // defer handling to specialiced function

    assert(!"NYI: block_net_init");
    return SYS_ERR_OK;
}

/**
 * \brief handles the connection event of a new network block server clients
 */
static errval_t client_connect_cb(void)
{
    // setup data structurs for the newly connected client

    assert(!"NYI: block_net_init");
    return SYS_ERR_OK;
}

#endif

static struct tcp_pcb *server_pcb;

static err_t bs_net_recv(void *arg, struct tcp_pcb *tpcb, struct pbuf *p,
                              err_t err)
{
    assert(!"NYI: bs_net_recv");
    return ERR_OK;
}

/*
 * This function is called periodically from TCP.
 * and is also responsible for taking care of stale connections.
 */
static err_t bs_net_poll(void *arg, struct tcp_pcb *tpcb)
{
    assert(!"NYI: bs_net_poll");
    return ERR_OK;
}

static void bs_net_err(void *arg, err_t err)
{
    assert(!"NYI: bs_net_err");
}

static err_t bs_net_accept(void *arg, struct tcp_pcb *tpcb, err_t err)
{
#if TCP_LISTEN_BACKLOG
    /* Decrease the listen backlog counter */
    struct tcp_pcb_listen *lpcb = (struct tcp_pcb_listen*)arg;
    tcp_accepted(lpcb);
#endif
    tcp_setprio(tpcb, TCP_PRIO_NORMAL);

    /*
     * TODO:  allocate a new connection control structure for this
     */
    void *bs_conn = NULL;

    tcp_arg(tpcb, bs_conn);

    tcp_recv(tpcb, bs_net_recv);
    tcp_err(tpcb, bs_net_err);
    tcp_poll(tpcb, bs_net_poll, 4);



    return ERR_OK;
}

/**
 * \brief initializes the network server of the block service
 */
errval_t block_net_init(uint16_t port)
{
    if (lwip_init_auto() == false) {
        printf("ERROR: lwip_init_auto failed!\n");
        return 1;
    }

    server_pcb = tcp_new();

    err_t e = tcp_bind(server_pcb, IP_ADDR_ANY, port);
    if (e != ERR_OK) {
        printf("ERROR: tcp_bind failed!\n");
        return 2;
    }


    // set up the network sockets / queues

    // setup the worker thread pool

    assert(!"NYI: block_net_init");
    return SYS_ERR_OK;
}

static bool server_running = false;

/**
 * \brief starts the network server of block service to accept requests
 */
errval_t block_net_start(void)
{
    errval_t err;

    server_pcb = tcp_listen(server_pcb);
    if (server_pcb == NULL) {
        printf("ERROR: tcp_listen failed!\n");
        return 1;
    }

    tcp_arg(server_pcb, server_pcb);
    tcp_accept(server_pcb, bs_net_accept);

    server_running = true;

    struct waitset *ws = get_default_waitset();
    while(server_running) {
        err = event_dispatch_non_block(ws);
        if (err != LIB_ERR_NO_EVENT) {
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }

        wrapper_perform_lwip_work();
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    assert(!"NYI: block_net_start");
    return SYS_ERR_OK;
}

/**
 * \brief stops the request handling of the network block service
 */
errval_t block_net_stop(void)
{
    server_running = false;
    return SYS_ERR_OK;
}

/**
 * \brief lookup of the block server connection based on the requested block
 *
 * The client may be connected to multiple network block servers. The request
 * needs to be forwarded to the correct block server based in the requested
 * block id.
 *
 * XXX: Supply the block server ID instead? or just say there is one block server?
 */
struct block_net_server *block_net_server_lookup(size_t block_start)
{
    assert(!"NYI: block_net_server_lookup");
    return NULL;
}

