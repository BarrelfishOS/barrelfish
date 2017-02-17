/**
 * \file
 * \brief the private header file for the net "daemon" library
 *
 * This file is part of the net "daemon" library
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: NetOS Group.
 */

#ifndef _NETD_PRIVATE_H_
#define _NETD_PLIVATE_H_

// keeping it for DHCP timers
#include <barrelfish/deferred.h>

#include <netif/bfeth.h>
#include <if/net_ARP_defs.h>

#include <procon/procon.h>

struct netd_state;

struct ARP_user_cl {
    // This will be used to remember who all are waiting for response
    struct net_ARP_binding *cl; // binding
    struct ARP_user_cl *next; // for singly linked list
    bool died; // is the user still connected
    struct netd_state *state;
};


struct netd_state {
    bool do_dhcp;
    // IP information for static configuration
    char *ip_addr_str;
    char *netmask_str;
    char *gateway_str;
    char *dns_str;
    
    struct netif *netif_ptr;

    struct periodic_event dhcp_fine_timer; // fine-grain timer for DHCP
    struct periodic_event dhcp_coarse_timer; // coarse-grain timer for DHCP

    // local ip address.  Used to keep track of changing IP addresses
    struct ip_addr local_ip;
    bool subsequent_call;

    // state variable indicating if dhcp is done or not
    bool dhcp_completed;
    
    // The name of exported service for ARP lookup (which we are implementing)
    char ARP_service_name[MAX_NET_SERVICE_NAME_LEN];
    // is service exported? marks that initialization is done
    bool ARP_service_exported;
    // singly linked list of apps connected with this service
    struct ARP_user_cl *registered_user_list;
};

typedef net_ARP_ipv4addr_t ipv4addr_t;

/**
 * @brief initializes LWIP. not a lot left after I changed the subsystems
 *
 * @param state netd library state
 * @param card_name the name of the card.
 * @param queueid the queue number
 */
void startlwip(struct netd_state *state, char *card_name, uint64_t queueid);

int init_ARP_lookup_service(struct netd_state *state, char *dev_name);

#endif // _NETD_PRIVATE_H_
