/**
 * \file
 * \brief the main header file for the net "daemon"
 *
 * This file is part of the net "daemon"
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: NetOS Group.
 */

#ifndef _NETD_H_
#define _NETD_H_

#include "portalloc.h"
#include <netif/bfeth.h>
#include <if/ether_defs.h>
#include <if/netd_defs.h>

#include <contmng/contmng.h>


/** 
 * This is the structure used to hold all of the ports allocated to a 
 * networking application and their relavanet buffer. This also keeps the 
 * state of the filter registration/deregistration sequence.
 */
struct buffer_port_translation {
    uint64_t buffer_id_rx;
    uint64_t buffer_id_tx;
    uint64_t filter_id;
    uint64_t type;
    uint16_t local_port;
    uint32_t local_ip;
    uint16_t remote_port;
    uint32_t remote_ip;
    bool redirected;
    bool active;
    bool bind;
    bool closing;
    bool paused;
    struct netd_binding *st;
    struct buffer_port_translation *next;
};


/**
 * Represents a network user application. This can easily be extended later.
 */
struct net_user {
    struct cont_queue *q;
    struct buffer_port_translation *open_ports;
    bool died;
    struct net_user *next;
};

/* FIXME: check if you can remove any of following global variables. */
/**
 * Networking daemon state
 */
struct net_user *registerd_app_list;

/*
 * The IP assigned to this netd and its card interface
 */
struct ip_addr local_ip;

struct ether_client_response *card_conn[2];


/** 
 * @brief initializes LWIP. not a lot left after I changed the subsystems
 * 
 * @param card_name the of the card. it is usualy "e1000" for now
 */
void startlwip(char *card_name);

#endif
