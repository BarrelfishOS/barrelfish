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
#include <barrelfish/deferred.h>

#include <devif/queue_interface.h>
#include <devif/backends/loopback_devif.h>
#include <devif/backends/net/sfn5122f_devif.h>
#include <devif/backends/net/e10k_devif.h>
#include <devif/backends/net/e1000_devif.h>
#include <devif/backends/net/mlx4_devif.h>

#include <lwip/netif.h>

#include <net/net_filter.h>
#include <net/net.h>
#include <net/netbufs.h>
#include <net/netif.h>
#include <net/dhcp.h>
#include <net/arp.h>

#include <collections/list.h>

#include <if/net_ARP_defs.h>

#include "debug.h"

// enable benchmarking
#define BENCH_LWIP_STACK 0
#define BENCH_DEVQ_ENQUEUE 0
#define BENCH_DEVQ_DEQUEUE 0
#define BENCH_NUM_MEASUREMENTS_BITS 16
#define BENCH_NUM_MEASUREMENTS (1UL << BENCH_NUM_MEASUREMENTS_BITS)

#define NETBUF_DEBGUG 0

/**
 * @brief encapsulates the state of the networking library
 */
struct net_state {
    uint64_t queueid;
    const char *cardname;
    net_flags_t flags;
    bool initialized;

    /* DHCP timer events */
    uint64_t dhcp_ticks;
    struct periodic_event dhcp_timer;
    bool dhcp_done;
    bool dhcp_running;

    bool arp_running;
    bool arp_connected;
    uint64_t arp_triggerid;
    struct net_ARP_binding* arp;
    collections_listnode *outstanding_arp;
    struct periodic_event arp_send;

    struct waitset *waitset;

    struct devq *queue;
    struct net_buf_pool *pool;
    struct netif netif;
    bool hw_filter;
    struct net_filter_state* filter;

  //  ip4_addr_t ipaddr, netmask, gw;
};

extern struct net_state state;

static inline struct net_state *get_default_net_state(void)
{
    return &state;
}


struct net_buf_pool;

struct net_buf_p
{
    struct pbuf_custom pbuf;
#if NETBUF_DEBGUG
    uint64_t flags;
    bool allocated;
    bool enqueued;
#endif
    lpaddr_t offset;
    void *vbase;
    struct net_buf_region *region;
#if BENCH_LWIP_STACK
    cycles_t timestamp;
    cycles_t timestamp2;
#endif
#if NETBUF_DEBGUG
    uint64_t magic;
#endif
};

struct net_buf_region
{
    void *vbase;
    struct frame_identity frame;
    struct capref framecap;
    struct net_buf_region *next;
    size_t buffer_size;
    uint8_t buffer_shift;
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
