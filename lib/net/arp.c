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
#include <barrelfish/nameservice_client.h>
#include <collections/list.h>


#include <lwip/opt.h>       
#include <lwip/netif.h>
#include <lwip/timeouts.h>
#include <net/netif.h>

#include <netif/etharp.h>

#include <if/octopus_defs.h>
#include <if/net_ARP_defs.h>
#include <octopus/octopus.h>
#include <octopus/trigger.h>


#include "networking_internal.h"

///< the debug subsystem
#define NETDEBUG_SUBSYSTEM "arpd"


#define ARP_ENTRY_FIELDS "{mac: %d, ip: %d}"
#define ARP_ENTRY "net.arp.%d {mac: %lu, ip: %d}"

#define ARP_ENTRY_REGEX "r'net\\.arp\\.[0-9]+' { mac: _, ip: _}"
#define ARP_RESEND_FREQ 500*1000

struct arp_request {
    uint32_t ip;
};

static void arp_timer_callback(void *data)
{
    errval_t err;
    char* record;
    char query[128];

    struct net_state *st = (struct net_state*) data;

    // remove all the outstanding ARPs that are finished
    for (int i = 0; i < collections_list_size(st->outstanding_arp) ; i++) {
        struct arp_request* req = (struct arp_request*) collections_list_get_ith_item(st->outstanding_arp, i);
        sprintf(query, "net.arp.%d {mac: _ , ip: %d }", req->ip, req->ip);
        err = oct_get(&record, query);
        if (err_is_ok(err)) {
            // remove from outstanding arps
            collections_list_remove_ith_item(st->outstanding_arp, i);
        }
    }
    
    // no outstanding ARPs
    if (collections_list_size(st->outstanding_arp) == 0) {
        NETDEBUG("Stopping ARP periodic event \n");
        err = periodic_event_cancel(&st->arp_send);
        assert(err_is_ok(err));
        return;
    }

    // Resend all other outstanding ARPs
    for (int i = 0; i < collections_list_size(st->outstanding_arp) ; i++) {
        struct arp_request* req = (struct arp_request*) collections_list_get_ith_item(st->outstanding_arp, i);
        NETDEBUG("Starting ARP for ip %u \n", req->ip);
        err = etharp_request(&st->netif, (ip4_addr_t*) &req->ip);
        assert(err_is_ok(err));
    }
}

static void arp_request_free(void *data)
{
    free(data);
}

// Reuse existing Flounder interface
static void arp_force_lookup(struct net_ARP_binding *b,
                             uint32_t ip)
{
    errval_t err;
    struct net_state *sta = (struct net_state*) b->st;

    if (sta->outstanding_arp == NULL) {
       collections_list_create(&sta->outstanding_arp, arp_request_free);
       assert(sta->outstanding_arp != NULL); 
    }    

    if (collections_list_size(sta->outstanding_arp) == 0) {
        NETDEBUG("Starting ARP periodic event\n");
        err = periodic_event_create(&sta->arp_send, get_default_waitset(),
                                    ARP_RESEND_FREQ, MKCLOSURE(arp_timer_callback, sta));
        assert(err_is_ok(err));
    }

    struct arp_request* new_arp = malloc(sizeof(struct arp_request));
    collections_list_insert(sta->outstanding_arp, new_arp);

    // send one but if it fails, periodic event will call it again
    err = etharp_request(&sta->netif, (ip4_addr_t*) &ip);
    assert(err_is_ok(err));
} // end function: ARP_resolve_request


static struct net_ARP_rx_vtbl rx_arp_vtbl = {
    .arp_force_lookup = arp_force_lookup,
};

/*****************************************************************
* Dealing with new connections
*****************************************************************/
static errval_t connect_ARP_cb(void *st, struct net_ARP_binding *b)
{   
    b->st = st;
    b->rx_vtbl = rx_arp_vtbl;
    return SYS_ERR_OK;
} 

/*****************************************************************
* exporting service
*****************************************************************/

static void export_ARP_cb(void *st, errval_t err, iref_t iref)
{
    struct net_state *sta = st;

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service export failed");
    }

    // register this iref with the name service
    err = nameservice_register("libnet_arp", iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [libnet_arp]");
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }
    sta->arp_running = true;
}

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

    err = net_ARP_export(st, export_ARP_cb, connect_ARP_cb,
                         get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
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



static void bind_cb(void *st, errval_t err, struct net_ARP_binding *b)
{
    assert(err_is_ok(err));
    struct net_state* sta = (struct net_state*) st;

    sta->arp = b;
    sta->arp_connected = true;
}

static errval_t arp_connect(struct net_state* st)
{
    errval_t err;
    if (!st->arp_connected) {
        iref_t iref;
        err = nameservice_blocking_lookup("libnet_arp", &iref);
        if (err_is_fail(err)) {
            return err;
        }

        err = net_ARP_bind(iref, bind_cb, st, get_default_waitset(), 
                           IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            return err;
        }
    
        while(!st->arp_connected) {
            event_dispatch(get_default_waitset());
        }
    }
    return SYS_ERR_OK;
}

errval_t arp_service_get_mac(uint32_t ip, uint64_t* mac)
{
    errval_t err;

    err = oct_init();
    if (err_is_fail(err)) {
        return err;
    }
    
    char* record = NULL;
    char query[256] ;

    sprintf(query, "net.arp.%d {mac: _ , ip: %d }", ip, ip);
    err = oct_get(&record, query);
    if (err_no(err) == OCT_ERR_NO_RECORD) {
        struct net_state *st = get_default_net_state();
        err = arp_connect(st);
        if (err_is_fail(err)) {
            return err;
        }       

        err = st->arp->tx_vtbl.arp_force_lookup(st->arp, NOP_CONT, ip);
        if (err_is_fail(err)) {
            return err;
        }

        err = oct_wait_for(&record, query);
        if (err_is_fail(err)) {
            return err;
        }
    } else if (err_is_fail(err)) {
        DEBUG_ERR(err, "cannot get mac address\n");
        return err;
    }

    uint64_t ip_adr;
    err = oct_read(record, "_" ARP_ENTRY_FIELDS, mac, &ip_adr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to read the entrie\n");
    }

    return SYS_ERR_OK;
}

