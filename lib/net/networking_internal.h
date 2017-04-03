/**
 * @brief 
 *  net.h
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef LIB_NET_INCLUDE_NETWORKING_INTERNAL_H_
#define LIB_NET_INCLUDE_NETWORKING_INTERNAL_H_


#include <barrelfish/barrelfish.h>

#include <devif/queue_interface.h>
#include <devif/backends/loopback_devif.h>

#include <lwip/netif.h>

#include <net/net.h>
#include <net/netbufs.h>
#include <net/netif.h>

#include "debug.h"

/**
 * @brief encapsulates the state of the networking library
 */
struct net_state {
    uint64_t queueid;
    char *cardname;
    bool initialized;
    struct devq *queue;
    struct net_buf_pool *pool;
    struct netif netif;

  //  ip4_addr_t ipaddr, netmask, gw;
};

extern struct net_state state;


struct net_buf_pool;

struct net_buf_p
{
    struct pbuf_custom pbuf;
    lpaddr_t offset;
    void *vbase;
    struct net_buf_region *region;
};

struct net_buf_region
{
    void *vbase;
    struct frame_identity frame;
    struct capref framecap;
    struct net_buf_region *next;
    size_t buffer_size;
    regionid_t regionid;
    struct net_buf_pool *pool;
    struct net_buf_p *netbufs;    /// array of netbufs
};

struct net_buf_pool
{
    struct net_buf_region *regions;
    struct devq *dev_q;
    struct pbuf *pbufs;
    // stats
    size_t buffer_count;
    size_t buffer_free;

};

#endif /* LIB_NET_INCLUDE_NETWORKING_INTERNAL_H_ */
