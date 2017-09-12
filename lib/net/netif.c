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
#include <octopus/octopus.h>


#include <lwip/opt.h>
#include <lwip/netif.h>
#include <lwip/timeouts.h>
#include <net/netif.h>

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

#if (BENCH_LWIP_STACK || BENCH_DEVQ_ENQUEUE || BENCH_DEVQ_DEQUEUE)
#include <barrelfish/sys_debug.h>
static cycles_t tsc_per_us = 0;
static inline uint64_t cycles_to_us(cycles_t cycles)
{
    if (tsc_per_us == 0) {
        sys_debug_get_tsc_per_ms(&tsc_per_us);
        tsc_per_us /= 1000;
    }

    return cycles / (tsc_per_us);
}
#endif

#if BENCH_LWIP_STACK
static cycles_t bench_lwip_processing = 0;
static cycles_t bench_lwip_processing2 = 0;
static size_t bench_lwip_processing_count = 0;
#endif

#if BENCH_DEVQ_ENQUEUE
static cycles_t bench_devq_enq_rx = 0;
static size_t bench_devq_enq_rx_count = 0;
static cycles_t bench_devq_enq_tx = 0;
static size_t bench_devq_enq_tx_count = 0;
#endif

#if BENCH_DEVQ_DEQUEUE
static cycles_t bench_devq_deq_rx = 0;
static size_t bench_devq_deq_count_rx = 0;
static cycles_t bench_devq_deq_tx = 0;
static size_t bench_devq_deq_count_tx = 0;
#endif

#define net_if_get_net_state(netif) ((struct net_state*)netif->state)

errval_t net_if_get_hwaddr(struct netif *netif);

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
    if (!ip_addr_cmp(&netif->ip_addr, IP_ADDR_ANY)) {
        printf("######################################\n");
        printf("# Cardname %s\n", net_if_get_net_state(netif)->cardname);
        printf("# IP Addr %s\n", ip4addr_ntoa(netif_ip4_addr(netif)));
        printf("# GW Addr %s\n", ip4addr_ntoa(netif_ip4_gw(netif)));
        printf("# Netmask %s\n", ip4addr_ntoa(netif_ip4_netmask(netif)));
        printf("######################################\n");

        netif_set_default(netif);
        printf("setting default interface\n");

        NETDEBUG("setting system ip config record to IP: %s\n",
                ip4addr_ntoa(netif_ip4_addr(netif)));
        /* register IP with octopus */
        errval_t err;
        err = oct_set(NET_CONFIG_CURRENT_IP_RECORD_FORMAT, netif_ip4_addr(netif)->addr,
                      netif_ip4_gw(netif)->addr,
                      netif_ip4_netmask(netif)->addr);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to set the DHCP record\n");
        }
    }
}


static err_t netif_init_cb(struct netif *netif)
{
    errval_t err;

    netif->hwaddr_len = ETHARP_HWADDR_LEN;
    netif->flags      = NETWORING_NETIF_FLAGS;
    netif->mtu        = NET_IF__MTU;

    err =  net_if_get_hwaddr(netif);
    if (err_is_fail(err)) {
        return ERR_IF;
    }

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
    NETDEBUG("netif=%p, devq=%p\n", netif, devq);

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

/**
 * @brief obtains the hardware address of the interface
 *
 * @param netif      the networking interface

 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_get_hwaddr(struct netif *netif)
{
    errval_t err;

    struct devq *q = net_if_get_net_state(netif)->queue;

    uint64_t card_mac;
    err = devq_control(q, 0, 0, &card_mac);
    if (err_is_fail(err)) {
        return err;
    }

    SMEMCPY(netif->hwaddr, &card_mac, netif->hwaddr_len);

    return SYS_ERR_OK;
}

/*
 * ===============================================================================
 * Buffer Management
 * ===============================================================================
 */



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

#if NETBUF_DEBGUG
    assert(nb->magic == 0xdeadbeefcafebabe);
    assert(nb->allocated == 1);
    assert(nb->enqueued == 0);
    assert(nb->flags == 0);
    nb->enqueued = 1;
    nb->flags = NETIF_RXFLAG;
#endif


#if BENCH_DEVQ_ENQUEUE
    cycles_t tsc_start = rdtsc();
#endif
    errval_t err;
    err =  devq_enqueue(st->queue, nb->region->regionid, nb->offset,
                        nb->region->buffer_size, 0, nb->region->buffer_size,
                        NETIF_RXFLAG);

#if BENCH_DEVQ_ENQUEUE
    bench_devq_enq_rx += rdtsc() - tsc_start;
    bench_devq_enq_rx_count++;
    if (bench_devq_enq_rx_count== BENCH_NUM_MEASUREMENTS) {
        debug_printf("BENCH ENQUEUE RX: %lu us (%lu)\n", cycles_to_us(bench_devq_enq_rx >> BENCH_NUM_MEASUREMENTS_BITS), bench_devq_enq_rx >> BENCH_NUM_MEASUREMENTS_BITS);
        bench_devq_enq_rx = 0;
        bench_devq_enq_rx_count = 0;
    }
#endif

    return err;
}




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

        struct net_buf_p *nb = (struct net_buf_p *)tmpp;

        NETDEBUG("netif=%p <- pbuf=%p   (reg=%u, offset=%" PRIxLPADDR ")\n", netif,
                 pbuf, nb->region->regionid, nb->offset);

        if (tmpp->next == NULL) {
            flags |= NETIF_TXFLAG_LAST;
        }

#if NETBUF_DEBGUG
        assert(nb->magic == 0xdeadbeefcafebabe);
        assert(nb->allocated == 1);
        assert(nb->enqueued == 0);
        assert(nb->flags == 0);
        nb->enqueued = 1;
        nb->flags = flags;
#endif


#if BENCH_LWIP_STACK
        if (nb->timestamp) {
            cycles_t now = rdtsc();
            bench_lwip_processing += now- nb->timestamp;
            bench_lwip_processing2 += now - nb->timestamp2;
            bench_lwip_processing_count++;
            if (bench_lwip_processing_count == BENCH_NUM_MEASUREMENTS) {
                debug_printf("BENCH LWIP PROCESS: %lu us (%lu)\n", cycles_to_us(bench_lwip_processing >> BENCH_NUM_MEASUREMENTS_BITS), bench_lwip_processing >> BENCH_NUM_MEASUREMENTS_BITS);
                debug_printf("BENCH LWIP PROCESS2: %lu us (%lu)\n", cycles_to_us(bench_lwip_processing2 >> BENCH_NUM_MEASUREMENTS_BITS), bench_lwip_processing2>> BENCH_NUM_MEASUREMENTS_BITS);
                bench_lwip_processing = 0;
                bench_lwip_processing2 = 0;
                bench_lwip_processing_count = 0;
            }
        }
#endif

        size_t valid_data = (uintptr_t)tmpp->payload - (uintptr_t)nb->vbase;
        assert((valid_data + tmpp->len) < nb->region->buffer_size);
        assert(((uintptr_t)tmpp->payload - valid_data) == (uintptr_t)nb->vbase);

#if BENCH_DEVQ_ENQUEUE
    cycles_t tsc_start = rdtsc();
#endif
        err = devq_enqueue(st->queue, nb->region->regionid, nb->offset,
                           nb->region->buffer_size,
                           ((uintptr_t)tmpp->payload - (uintptr_t)nb->vbase),
                           tmpp->len, flags);
#if BENCH_DEVQ_ENQUEUE
    bench_devq_enq_tx += rdtsc() - tsc_start;
    bench_devq_enq_tx_count++;
    if (bench_devq_enq_tx_count== BENCH_NUM_MEASUREMENTS) {
        debug_printf("BENCH ENQUEUE TX: %lu us (%lu)\n", cycles_to_us(bench_devq_enq_tx >> BENCH_NUM_MEASUREMENTS_BITS), bench_devq_enq_tx >> BENCH_NUM_MEASUREMENTS_BITS);
        bench_devq_enq_tx = 0;
        bench_devq_enq_tx_count = 0;
    }

#endif

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

    struct net_state *st = netif->state;
    if (st == NULL) {
        /* XXX: return an error code ?? */
        debug_printf("FOOBAR\n");
        return SYS_ERR_OK;
    }

    //for (int i = 0; i < NET_IF_POLL_MAX; i++) {
    for (;;) {
#if BENCH_DEVQ_DEQUEUE
        cycles_t tsc_start = rdtsc();
#endif
        struct devq_buf buf;
        err = devq_dequeue(st->queue, &buf.rid, &buf.offset, &buf.length,
                           &buf.valid_data, &buf.valid_length, &buf.flags);


#if BENCH_DEVQ_DEQUEUE
        if (err == SYS_ERR_OK) {
            cycles_t end = rdtsc();
            if (buf.flags & NETIF_TXFLAG) {
                bench_devq_deq_tx += end - tsc_start;
                bench_devq_deq_count_tx++;
                if (bench_devq_deq_count_tx == BENCH_NUM_MEASUREMENTS) {
                    debug_printf("BENCH DEQUEUE TX: %lu\n", bench_devq_deq_tx >> BENCH_NUM_MEASUREMENTS_BITS);
                    bench_devq_deq_tx = 0;
                    bench_devq_deq_count_tx = 0;
                }
            }

            if (buf.flags & NETIF_RXFLAG) {
                bench_devq_deq_rx += end - tsc_start;
                bench_devq_deq_count_rx++;
                if (bench_devq_deq_count_rx == BENCH_NUM_MEASUREMENTS) {
                    debug_printf("BENCH DEQUEUE RX: %lu\n", bench_devq_deq_rx >> BENCH_NUM_MEASUREMENTS_BITS);
                    bench_devq_deq_rx = 0;
                    bench_devq_deq_count_rx = 0;
                }
            }
        }

#endif
        if (err_is_fail(err)) {
            if (err_no(err) == DEVQ_ERR_QUEUE_EMPTY) {
                return SYS_ERR_OK;
            }
            return err;
        }

        struct pbuf *p = net_buf_get_by_region(st->pool, buf.rid, buf.offset);
        if (p == NULL) {
            NETDEBUG("netif=%p, ERROR. No PBUF found for rid=%u, "
                     "offset=%"PRIxLPADDR "\n", netif, buf.rid, buf.offset);
            debug_printf("BUFFER NOT FOUND!!!!");
            continue;
        }



#if NETBUF_DEBGUG
        struct net_buf_p *nb = (struct net_buf_p *)p;

        assert((buf.valid_data + buf.valid_length) < nb->region->buffer_size);

        if (nb->magic != 0xdeadbeefcafebabe || nb->enqueued != 1 || nb->allocated != 1 || nb->flags != buf.flags) {
            debug_printf("ERROR: pbuf=%p, rid=%u, offset=%lx magic=%lx, enq=%u, alloc=%u, flags=%lx (%lx)\n",
                         p, nb->region->regionid, nb->offset, nb->magic, nb->enqueued, nb->allocated, nb->flags, buf.flags);
        }

        assert(nb->magic == 0xdeadbeefcafebabe);
        assert(nb->flags == buf.flags);
        assert(nb->enqueued == 1);
        assert(nb->allocated == 1);

        nb->enqueued = 0;
        nb->flags = 0;
#endif

#if BENCH_LWIP_STACK
        ((struct net_buf_p *)p)->timestamp = rdtsc();
#endif

        if (buf.flags & NETIF_TXFLAG) {
            NETDEBUG("netif=%p, TX done of pbuf=%p (rid=%u, offset=%"PRIxLPADDR ")\n",
                     netif, p, buf.rid, buf.offset);

            pbuf_free(p);

            assert(!(buf.flags & NETIF_RXFLAG));

        } else if (buf.flags & NETIF_RXFLAG) {
            NETDEBUG("netif=%p, RX done of pbuf=%p (rid=%u, offset=%"PRIxLPADDR ")\n",
                     netif, p, buf.rid, buf.offset);

            p->len = buf.valid_length;
            p->tot_len = p->len;
            p->payload += buf.valid_data;

            assert(!(buf.flags & NETIF_TXFLAG));

            if (st->netif.input(p, &st->netif) != ERR_OK) {
                net_if_add_rx_buf(&st->netif, p);
            } else {
                /* XXX: do this at another time ? */
                p = net_buf_alloc(st->pool);
#if NETBUF_DEBGUG
        nb = (struct net_buf_p *)p;
        assert(nb->magic == 0xdeadbeefcafebabe);
        assert(nb->allocated == 1);
        assert(nb->enqueued == 0);
        assert(nb->flags == 0);

#endif
                if (p) {
                    net_if_add_rx_buf(&st->netif, p);
                } else {
                    USER_PANIC("Could not allocate a receive buffer\n");
                }
            }
        } else {
            debug_printf("WARNING: got buffer without a flag\n");
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
    while (netif) {
        err = net_if_poll(netif);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to poll network interface");
        }
    }
    netif_poll_all();
    net_lwip_timeout();
    return SYS_ERR_OK;
}
