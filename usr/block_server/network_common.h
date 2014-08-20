/**
 * \file
 * \brief Network client of the block service
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BLOCK_NETWORK_COMMON_H
#define BLOCK_NETWORK_COMMON_H

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_net.h>

#include "block_server.h"


#if BULK_NET_BACKEND_PROXY
#include <bulk_transfer/bulk_net_proxy.h>
#include <bulk_transfer/bulk_local.h>
#endif



enum block_net_msg_type
{
    BLOCK_NET_MSG_INIT,     ///< transfer setup information of the bulk channel
    BLOCK_NET_MSG_ENDP,     ///< bulk endpoint information
    BLOCK_NET_MSG_FIN,      ///< connection teardown
    BLOCK_NET_MSG_READ,     ///< issue a block read request
    BLOCK_NET_MSG_WRITE,    ///< issue a block write request
    BLOCK_NET_MSG_STATUS,   ///< error message
};

enum block_net_err
{
    BLOCK_NET_ERR_OK,
    BLOCK_NET_ERR_BAD_REQUEST,
    BLOCK_NET_ERR_NO_BUFS,
    BLOCK_NET_ERR_BLOCK_ID,
};

/**
 * data structure representing the data of the messages transferred over the
 * service channel
 */
struct block_net_msg
{
    enum block_net_msg_type type;
    size_t size;
    union
    {
        struct {
            /* direction */
            bool           do_bind;
            struct bulk_net_endpoint_descriptor rx_ep;
            struct bulk_net_endpoint_descriptor tx_ep;
        } setup;



        struct {
            uint64_t    block_id;
            size_t      count;
            uint32_t    req_id;
            struct bulk_continuation cont;
        } read;

        struct {
            uint64_t      block_id;
            size_t        count;
            uint32_t      reqid;
        } write;

        struct {
            enum block_net_err code;
            enum block_net_msg_type req;
            uint32_t reqid;
        } status;


    } msg;

};


struct block_net_service {
    struct ip_addr                      ip;
    uint16_t                            port;
    struct tcp_pcb                     *tpcb;
    uint32_t                            bound;
    struct bulk_channel                 tx_chan;
    struct bulk_channel                 rx_chan;
#if BULK_NET_BACKEND_PROXY
    struct bulk_net_proxy               tx_proxy;
    struct bulk_net_proxy               rx_proxy;
    struct bulk_local_endpoint          rx_ep;
    struct bulk_local_endpoint          rx_p_ep;
    struct bulk_local_endpoint          tx_ep;
    struct bulk_local_endpoint          tx_p_ep;
#else
    struct bulk_net_endpoint_descriptor rx_ep;
    struct bulk_net_endpoint_descriptor tx_ep;
#endif
};

#define MIN(a,b) ((a) < (b) ? (a) : (b))

#endif /* BLOCK_NETWORK_COMMON_H */

