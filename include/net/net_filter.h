/**
 * @brief 
 *  net_filter.h
 *  Install filters (mostly HW)
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_NET_INCLUDE_NETWORKING_FILTER_H_
#define LIB_NET_INCLUDE_NETWORKING_FILTER_H_

#include <barrelfish/barrelfish.h>

#define NET_FILTER_TCP 0
#define NET_FILTER_UDP 1
#define NET_FILTER_MAC 2

struct net_filter_ip {
    uint64_t qid;
    uint32_t ip_src;
    uint32_t ip_dst;          
    uint16_t port_src;
    uint16_t port_dst;
    uint8_t type;
};

struct net_filter_mac {
    uint8_t type;
    uint64_t vlan_id;
    uint64_t mac;
};


struct net_filter_ele {
    union {
        struct net_filter_ip ip;
        struct net_filter_mac mac;
    } filter;
    uint64_t filter_id;
    struct net_filter_ele* next;
    struct net_filter_ele* prev;
};

struct filter_list {
    struct net_filter_ele* start;
    uint64_t num_ele;
};

struct net_filter_state {
    struct filter_list filters_ip;
    struct filter_list filters_mac;
    struct net_filter_binding* b;
    volatile bool bound;
};

errval_t net_filter_init(struct net_filter_state** st, 
                         const char* cardname);

errval_t net_filter_init_with_ep(struct net_filter_state** st,
                                 struct capref ep);

errval_t net_filter_ip_install(struct net_filter_state* st,
                               struct net_filter_ip* filt);

errval_t net_filter_mac_install(struct net_filter_state* st,
                                struct net_filter_mac* filt);

errval_t net_filter_ip_remove(struct net_filter_state* st,
                              struct net_filter_ip* filt);

errval_t net_filter_mac_remove(struct net_filter_state* st,
                               struct net_filter_mac* filt);

#endif /* LIB_NET_INCLUDE_NETWORKING_FILTER_H_ */
