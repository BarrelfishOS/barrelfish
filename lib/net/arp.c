/**
 * \file arp.c
 * \brief
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>


#include <lwip/opt.h>
#include <lwip/netif.h>
#include <lwip/timeouts.h>
#include <net/netif.h>

#include <netif/etharp.h>

#include <if/octopus_defs.h>
#include <octopus/octopus.h>
#include <octopus/trigger.h>


#include "networking_internal.h"

///< the debug subsystem
#define NETDEBUG_SUBSYSTEM "arpd"


#define ARP_ENTRY_FIELDS "{mac: %d, ip: %d}"
#define ARP_ENTRY "net.arp.%d {mac: %lu, ip: %d}"

#define ARP_ENTRY_REGEX "r'net\\.arp\\.[0-9]+' { mac: _, ip: _}"

struct netif *arp_filter_netif(struct pbuf *p, struct netif *netif, uint16_t type)
{
    if (type != ETHTYPE_ARP) {
        return netif;
    }

    struct net_state *st = netif->state;

    if (!st->arp_running) {
        return netif;
    }

    if (p->len < SIZEOF_ETH_HDR || pbuf_header(p, (s16_t)-SIZEOF_ETH_HDR)) {
        NETDEBUG("wrong packet size received\n");
        return netif;
    }

    struct etharp_hdr *hdr = (struct etharp_hdr *)p->payload;

    pbuf_header(p, (s16_t)SIZEOF_ETH_HDR);

    /* RFC 826 "Packet Reception": */
    if ((hdr->hwtype != PP_HTONS(HWTYPE_ETHERNET)) ||
        (hdr->hwlen != ETH_HWADDR_LEN) ||
        (hdr->protolen != sizeof(ip4_addr_t)) ||
        (hdr->proto != PP_HTONS(ETHTYPE_IP)))  {
      LWIP_DEBUGF(ETHARP_DEBUG | LWIP_DBG_TRACE | LWIP_DBG_LEVEL_WARNING,
        ("etharp_input: packet dropped, wrong hw type, hwlen, proto, protolen or ethernet type (%"U16_F"/%"U16_F"/%"U16_F"/%"U16_F")\n",
        hdr->hwtype, (u16_t)hdr->hwlen, hdr->proto, (u16_t)hdr->protolen));
      return netif;
    }

    ip_addr_t ip;
    IPADDR2_COPY(&ip, &hdr->sipaddr);

    /* don't store any IPs */
    if (ip_addr_cmp(&st->netif.ip_addr, IP_ADDR_ANY)) {
        return netif;
    }

    uint64_t hwaddr = 0;
    if (etharp_find_addr(netif, &ip, (struct eth_addr **)&hwaddr,
                         (const ip4_addr_t **)&hwaddr) != -1) {
        return netif;
    }

    /*
     * If already exists, return
     */

    hwaddr = 0;
    SMEMCPY(&hwaddr, hdr->shwaddr.addr, sizeof(hdr->shwaddr));

    NETDEBUG("set " ARP_ENTRY "\n", ip.addr, hwaddr, ip.addr);

    oct_set(ARP_ENTRY, ip.addr, hwaddr, ip.addr);

    etharp_add_static_entry(&ip, &hdr->shwaddr);

    return netif;
}

static errval_t arp_service_start_st(struct net_state *st)
{
    errval_t err;

    err = oct_init();
    if (err_is_fail(err)) {
        return err;
    }

    st->arp_running = true;

    return SYS_ERR_OK;
}

errval_t arp_service_start(void)
{
    return arp_service_start_st(get_default_net_state());
}

static  void arp_change_event(octopus_mode_t mode, const char* record, void* st)
{
    errval_t err;

    uint64_t ip, hwaddr;
    err = oct_read(record, "_" ARP_ENTRY_FIELDS, &hwaddr, &ip);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to read the entrie\n");
    }

    ip_addr_t ipaddr;
    ipaddr.addr = (uint32_t)ip;

    if (mode & OCT_ON_SET) {

        NETDEBUG("adding ARP entries: ip=%u, mac=%lx\n", ipaddr.addr, hwaddr);

        struct eth_addr mac;
        SMEMCPY(mac.addr, &hwaddr, sizeof(mac));

        etharp_add_static_entry(&ipaddr, &mac);
    } else if (mode & OCT_ON_DEL) {
        NETDEBUG("deleting ARP entries: ip=%u, mac=%lx\n", ipaddr.addr, hwaddr);
        etharp_remove_static_entry(&ipaddr);
    }
}

static errval_t arp_service_subscribe_st(struct net_state *st)
{
    NETDEBUG("subscribing to ARP updates..\n");

    errval_t err;
    err = oct_init();
    if (err_is_fail(err)) {
        return err;
    }

    st->arp_running = false;

    return oct_trigger_existing_and_watch(ARP_ENTRY_REGEX, arp_change_event,
                                             st, &st->arp_triggerid);
}

errval_t arp_service_subscribe(void)
{
    struct net_state *st = get_default_net_state();
    return arp_service_subscribe_st(st);
}
