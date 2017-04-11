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

#define LWIP_ARP_FILTER_NETIF_FN(p, netif, type) arp_filter_netif(p, netif, type)

#if LWIP_ARP_FILTER_NETIF
  netif = LWIP_ARP_FILTER_NETIF_FN(p, netif, lwip_htons(type));
#endif /* LWIP_ARP_FILTER_NETIF*/

#if 0
  err_t etharp_add_static_entry(const ip4_addr_t *ipaddr, struct eth_addr *ethaddr);
  err_t etharp_remove_static_entry(const ip4_addr_t *ipaddr);
#endif

#define ARP_ENTRY "net.arp.%d {mac: %d}"

#define ARP_ENTRY_REGEX "r'net\\.arp\\.[0-9]+' { mac: _ }"

struct netif *arp_filter_netif(struct pbuf *p, struct netif *netif, uint16_t type)
{
    debug_printf("arp_filter_netif");

    if (type != ETHTYPE_ARP) {
        return netif;
    }

    struct etharp_hdr *hdr = (struct etharp_hdr *)p->payload;

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


    /*
     * If already exists, return
     */

    uint64_t hwaddr = 0;
    SMEMCPY(&hwaddr, hdr->shwaddr, sizeof(hdr->shwaddr));
    oct_publish(ARP_ENTRY, hdr->sipaddr, hdr->shwaddr);

    etharp_add_static_entry(hdr->sipaddr, hdr->shwaddr);

    return netif;
}

static  void handle_arp_entry(octopus_mode_t mode, const char* record, void* state)
{

    uint64_t ip,hwaddr;
    oct_read(record, ARP_ENTRY, &ip, &hwaddr);

    ip_addr_t ipaddr;
    ipaddr.addr = (uint32_t)ip;

    if (mode & OCT_ON_SET) {

        struct eth_addr mac;
        SMEMCPY(mac.addr, &hwaddr, sizeof(mac));

        etharp_add_static_entry(&ipaddr, &mac);
    } else if (mode & OCT_ON_DEL) {
        etharp_remove_static_entry(&ipaddr);
    }
}

errval_t arp_filter_subscribe(void)
{
    struct net_state *st = get_default_net_state();
    subscription_t sub;
    return  oct_subscribe(handle_arp_entry, st, &sub, ARP_ENTRY_REGEX);
}
