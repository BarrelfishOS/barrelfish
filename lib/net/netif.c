/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <devif/queue_interface.h>
#include <net_interfaces/flags.h>


#include <lwip/opt.h>
#include <lwip/netif.h>
#include <lwip/timeouts.h>
#include "include/net/netif.h"

#include <netif/etharp.h>

#include "networking_internal.h"


#define NETDEBUG_SUBSYSTEM "net_if"


///< the default MTU for the net interfaces
#define NET_IF__MTU 1500

///< the networking interface flags
#define NETWORING_NETIF_FLAGS \
    (NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_ETHERNET);

///< the network interface name
#define NET_IF__NAME0 'e'
#define NET_IF__NAME1 'n'



static err_t net_if_linkoutput(struct netif *netif, struct pbuf *p)
{
    errval_t err;
    err = net_if_add_tx_buf(netif, p);
    if (err_is_fail(err)) {
        return ERR_IF;
    }

    return ERR_OK;
}


static void net_if_status_cb(struct netif *netif)
{
    debug_printf("######################################\n");
    debug_printf("# IP Addr %s\n", ip4addr_ntoa(netif_ip4_addr(netif)));
    debug_printf("# GW Addr %s\n", ip4addr_ntoa(netif_ip4_gw(netif)));
    debug_printf("# Netmask %s\n", ip4addr_ntoa(netif_ip4_netmask(netif)));
    debug_printf("######################################\n");
}


static err_t netif_init_cb(struct netif *netif)
{

    netif->flags      = NETWORING_NETIF_FLAGS;
    netif->mtu        = NET_IF__MTU;
    netif_set_status_callback(netif, net_if_status_cb);
    netif_set_up(netif);
    netif_set_link_up(netif);

    return ERR_OK;
}


/*
 * ===============================================================================
 * Network Interface Management
 * ===============================================================================
 */


/**
 * @brief initializes a netif structure for LWIP with a device queue
 *
 * @param netif     the netif to be initialized
 * @param devq      the device queue to be used
 *
 * @return SYS_ERR_OK on success, errva on failure
 */
errval_t net_if_init_devq(struct netif *netif, struct devq *devq)
{
    errval_t err;

    NETDEBUG("netif=%p, devq=%p\n", netif, devq);

    netif->hwaddr_len = ETHARP_HWADDR_LEN;

    // obtain the mac address
    err = networking_get_mac(devq, netif->hwaddr, netif->hwaddr_len);
    if (err_is_fail(err)) {
        return err;
    }

    /* set the output functions */
    netif->output     = etharp_output;
    netif->linkoutput = net_if_linkoutput;

    /* set the interface name */
    netif->name[0] = NET_IF__NAME0;
    netif->name[1] = NET_IF__NAME1;

    return SYS_ERR_OK;
}


/**
 * @brief initializes the netif
 *
 * @param netif
 * @param devq
 * @param mac
 *
 * @return
 */
errval_t net_if_add(struct netif *netif, void *st)
{
    NETDEBUG("netif=%p, state=%p\n", netif, st);

    /* TODO: use something sensible here ?? -> RUN DHCP*/
    ip4_addr_t ipaddr, netmask, gw;
    IP4_ADDR(&gw, 192,168,0,1);
    IP4_ADDR(&ipaddr, 192,168,0,2);
    IP4_ADDR(&netmask, 255,255,255,0);

    netif_add(netif, IP_ADDR_ANY, IP_ADDR_ANY, IP_ADDR_ANY, st,
              netif_init_cb, netif_input);

    return SYS_ERR_OK;
}


/**
 * @brief removes a network interface
 *
 * @param netif     the LWIP netif
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_remove(struct netif *netif)
{
    NETDEBUG("netif=%p\n", netif);

    /* TODO: need other things to do here ? */
    netif_remove(netif);

    return SYS_ERR_OK;
}


/*
 * ===============================================================================
 * Buffer Management
 * ===============================================================================
 */


#if BENCH_DEVQ_ENQUEUE
#define BENCH_DEVQ_MEASUREMENT_SHIFT 12
#define BENCH_DEVQ_MEASUREMENT (1 << BENCH_DEVQ_MEASUREMENT_SHIFT)
static cycles_t bench_devq_processing = 0;
static size_t bench_devq_processing_count = 0;
#endif

/**
 * @brief adds a new receive buffer to the interface
 *
 * @param netif     the LWIP netif
 * @param pbuf      packet buffer to be added
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_add_rx_buf(struct netif *netif, struct pbuf *pbuf)
{
    struct net_state *st = netif->state;
    struct net_buf_p *nb = (struct net_buf_p *)pbuf;

    NETDEBUG("netif=%p <- pbuf=%p   (reg=%u, offset=%" PRIxLPADDR ")\n", netif,
             pbuf, nb->region->regionid, nb->offset);


#if BENCH_DEVQ_ENQUEUE
    cycles_t tsc_start = rdtsc();
#endif
    errval_t err;
    err =  devq_enqueue(st->queue, nb->region->regionid, nb->offset,
                        nb->region->buffer_size, 0, nb->region->buffer_size,
                        NETIF_RXFLAG);

#if BENCH_DEVQ_ENQUEUE
    if (bench_devq_processing_count == BENCH_DEVQ_MEASUREMENT) {
        debug_printf("BENCH ENQUEUE: %lu\n", bench_devq_processing >> BENCH_DEVQ_MEASUREMENT_SHIFT);
        bench_devq_processing = 0;
        bench_devq_processing_count = 0;
    }
    bench_devq_processing += rdtsc() - tsc_start;
    bench_devq_processing_count++;
#endif

    return err;
}

#if BENCH_LWIP_STACK
#define BENCH_LWIP_STACK_MEASUREMENT_SHIFT 12
#define BENCH_LWIP_STACK_MEASUREMENT (1 << BENCH_LWIP_STACK_MEASUREMENT_SHIFT)
static cycles_t bench_lwip_processing = 0;
static size_t bench_lwip_processing_count = 0;
#endif


/**
 * @brief adds a new transmit buffer to the interface
 *
 * @param netif     the LWIP netif
 * @param pbuf      packt boffer to be transmitted
 *
 * @return  SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_add_tx_buf(struct netif *netif, struct pbuf *pbuf)
{
    errval_t err;

    struct net_state *st = netif->state;


    LINK_STATS_INC(link.xmit);

    uint64_t flags = NETIF_TXFLAG;
    for (struct pbuf * tmpp = pbuf; tmpp != 0; tmpp = tmpp->next) {
        pbuf_ref(tmpp);


        NETDEBUG("netif=%p <- pbuf=%p   (reg=%u, offset=%" PRIxLPADDR ")\n", netif,
                 pbuf, nb->region->regionid, nb->offset);

        struct net_buf_p *nb = (struct net_buf_p *)tmpp;

        if (tmpp->next == NULL) {
            flags |= NETIF_TXFLAG_LAST;
        }
#if BENCH_LWIP_STACK
        if (nb->timestamp) {
            if (bench_lwip_processing_count == BENCH_LWIP_STACK_MEASUREMENT) {
                debug_printf("BENCH LWIP PROCESS: %lu\n", bench_lwip_processing >> BENCH_LWIP_STACK_MEASUREMENT_SHIFT);
                bench_lwip_processing = 0;
                bench_lwip_processing_count = 0;
            }
            bench_lwip_processing += rdtsc() - nb->timestamp;
            bench_lwip_processing_count++;
        }
#endif
        err = devq_enqueue(st->queue, nb->region->regionid, nb->offset,
                           nb->region->buffer_size,
                           ((uintptr_t)tmpp->payload - (uintptr_t)nb->vbase),
                           tmpp->len, flags);

        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}


/*
 * ===============================================================================
 * Polling the interfaces
 * ===============================================================================
 */


#define NET_IF_POLL_MAX 50

/**
 * @brief polls then network interface for new incoming packets
 *
 * @param netif     the LWIP netif to be polled
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_poll(struct netif *netif)
{
    //NETDEBUG("netif=%p\n", netif);

    errval_t err;

    sys_check_timeouts();

    struct net_state *st = netif->state;
    if (st == NULL) {
        /* XXX: return an error code ?? */
        debug_printf("FOOBAR\n");
        return SYS_ERR_OK;
    }

    //for (int i = 0; i < NET_IF_POLL_MAX; i++) {
    for (;;) {
        struct devq_buf buf;
        err = devq_dequeue(st->queue, &buf.rid, &buf.offset, &buf.length,
                           &buf.valid_data, &buf.valid_length, &buf.flags);
        if (err_is_fail(err)) {
            NETDEBUG("netif=%p, polling %u/%u: %s\n", netif, i, NET_IF_POLL_MAX,
                     err_getstring(err));
            if (err_no(err) == DEVQ_ERR_QUEUE_EMPTY) {
                return SYS_ERR_OK;
            }
            return err;
        }

        struct pbuf *p = net_buf_get_by_region(st->pool, buf.rid, buf.offset);
        if (p == NULL) {
            NETDEBUG("netif=%p, polling %u/%u. ERROR. No PBUF found for rid=%u, "
                            "offset=%"PRIxLPADDR "\n", netif, i, NET_IF_POLL_MAX,
                            buf.rid, buf.offset);
            debug_printf("BUFFER NOT FOUND!!!!");
            continue;
        }

#if BENCH_LWIP_STACK
        ((struct net_buf_p *)p)->timestamp = rdtsc();
#endif
        if (buf.flags & NETIF_TXFLAG) {
            NETDEBUG("netif=%p, polling %u/%u. TX done of pbuf=%p (rid=%u, "
                      "offset=%"PRIxLPADDR ")\n", netif, i, NET_IF_POLL_MAX,
                      p, buf.rid, buf.offset);
            net_buf_free(p);
        }

        if (buf.flags & NETIF_RXFLAG) {
            NETDEBUG("netif=%p, polling %u/%u. RX done of pbuf=%p (rid=%u, "
                     "offset=%"PRIxLPADDR ")\n", netif, i, NET_IF_POLL_MAX,
                     p, buf.rid, buf.offset);

            p->len = buf.valid_length;
            p->tot_len = p->len;
            p->payload += buf.valid_data;
#if 0
#include <lwip/pbuf.h>
#include <lwip/prot/ethernet.h>
#include <lwip/prot/ip.h>
            #include <lwip/prot/ip4.h>
#include <lwip/prot/udp.h>
            p->len = 64 + SIZEOF_ETH_HDR + IP_HLEN + UDP_HLEN; //buf.valid_length;
            p->tot_len = p->len;

            struct eth_hdr* ethhdr = p->payload;
            struct ip_hdr *iphdr   = p->payload + SIZEOF_ETH_HDR;
            struct udp_hdr *udphdr = p->payload  + SIZEOF_ETH_HDR + IP_HLEN;

            memset(ethhdr->dest.addr, 0xaa, sizeof(ethhdr->dest.addr));
            memset(ethhdr->src.addr, 0xbb, sizeof(ethhdr->src.addr));
            ethhdr->type = PP_HTONS(ETHTYPE_IP);

            iphdr->_len = lwip_htons(64 + UDP_HLEN + IP_HLEN);
            IPH_VHL_SET(iphdr, 4, 5);

            IP4_ADDR(&(iphdr->dest), 192,168,0,2);
            IP4_ADDR(&(iphdr->src), 192,168,0,3);

            iphdr->_proto = IP_PROTO_UDP;

            udphdr->dest = PP_HTONS(7);
            udphdr->src = PP_HTONS(11);
            udphdr->len = PP_HTONS(64 + UDP_HLEN);

#endif
            netif_input(p, &st->netif);

            /* XXX: do this at another time ? */
            p = net_buf_alloc(st->pool);
            if (p) {
                net_if_add_rx_buf(&st->netif, p);
            } else {
                USER_PANIC("Could not allocate a receive buffer\n");
            }
        }
    }




    return SYS_ERR_OK;
}

/**
 * @brief polls all added network interfaces
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_poll_all(void)
{
    NETDEBUG("polling all interfaces\n");

    errval_t err;
    struct netif *netif = netif_list;
    while(netif) {
        err = net_if_poll(netif);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to poll network interface");
        }
    }
    return SYS_ERR_OK;
}


