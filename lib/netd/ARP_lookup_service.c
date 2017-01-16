/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/net_constants.h>
#include <if/net_ARP_defs.h>

#include <barrelfish/waitset.h>

// standard libraries
#include <stdio.h>
#include <string.h>

#include <lwip/init.h>
#include <netif/etharp.h>

// local includes
#include "netd_private.h"
#include <netd/netd_debug.h>

// ***** Special cases ********
// local ip address (your own ip address) (valid request)
// broadcast ip address (invalid request)
// multicast IP address (invalid request)
//
//
//


// How can I get my own MAC address
// Where can I find the code which is looking up local MAC address?
//    struct eth_addr *srcaddr = (struct eth_addr *) netif->hwaddr;
//
//
//        return etharp_query(netif, ipaddr, q); // q is pbuf
//        lib/lwip/src/netif/etharp.c

//        find_entry
//        src/netif/etharp.c

/*****************************************************************
* Prototypes
*****************************************************************/

static errval_t get_ip_info(struct net_ARP_binding *cc, uint32_t iface,
                        errval_t *err, net_ARP_ipv4addr_t *ip,
                        net_ARP_ipv4addr_t *gw, net_ARP_ipv4addr_t *mask);
static errval_t ARP_resolve_request(struct net_ARP_binding *cc,
            ipv4addr_t ip, uint32_t iface, bool force,
            errval_t *err, uint64_t *mac);

// service mappings
static struct net_ARP_rpc_rx_vtbl rpc_rx_ARP_vtbl = {
    .ip_info_call = get_ip_info,
    .ARP_lookup_call = ARP_resolve_request,
};


/*****************************************************************
* Dealing with new connections
*****************************************************************/
static errval_t connect_ARP_cb(void *st, struct net_ARP_binding *b)
{
    errval_t err = SYS_ERR_OK;
    struct netd_state *state = st;
    NETD_DEBUG("########### new application came in\n");

    // using the b->st to store session specific data (net_user)
    struct ARP_user_cl *new_app = malloc(sizeof(struct ARP_user_cl));

    if (new_app == NULL) {
        NETD_DEBUG("error: malloc failed...\n");
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        return err;
    }

    memset(new_app, 0, sizeof(struct ARP_user_cl));
    new_app->state = state;
    new_app->next = state->registered_user_list;
    state->registered_user_list = new_app;
    b->st = (void *) new_app;

    b->rpc_rx_vtbl = rpc_rx_ARP_vtbl;
    return err;
} // end function: connect_ARP_cb


/*****************************************************************
* exporting service
*****************************************************************/

static void export_ARP_cb(void *st, errval_t err, iref_t iref)
{
    struct netd_state *state = st;

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service[%s] export failed", state->ARP_service_name);
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }

    NETD_DEBUG("service [%s] exported at iref %u\n", state->ARP_service_name, iref);

    // register this iref with the name service
    err = nameservice_register(state->ARP_service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [%s]", state->ARP_service_name);
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }
    state->ARP_service_exported = true;
    NETD_DEBUG("service [%s] export successful!\n", state->ARP_service_name);
} // end function: export_ARP_cb



// ************************************************************************
//                 ARP lookup interface function
// ************************************************************************

static errval_t get_ip_info(struct net_ARP_binding *cc, uint32_t iface,
                        errval_t *err, net_ARP_ipv4addr_t *ip,
                        net_ARP_ipv4addr_t *gw, net_ARP_ipv4addr_t *mask)
{
    printf("####### get IP info called ######\n");
    NETD_DEBUG("get_ip_info: client asking for ip over %"PRIu32"\n", iface);
    struct ARP_user_cl *app = cc->st;
    struct netd_state *state = app->state;

    *err = SYS_ERR_OK;
    *ip = state->netif_ptr->ip_addr.addr;
    *gw = state->netif_ptr->gw.addr;
    *mask = state->netif_ptr->netmask.addr;
    NETD_DEBUG("get_ip_info: terminating\n");
    return SYS_ERR_OK;
}

static uint64_t refresh_cache(uint32_t dst_ip_addr)
{
    struct ip_addr dst_ip;
    struct netif *netif;

    dst_ip.addr = dst_ip_addr;
    netif = ip_route(&dst_ip);

    NETD_DEBUG("refresh_cache: calling etharp_request\n");
    errval_t r = etharp_request(netif, &dst_ip);
    assert(err_is_ok(r));

    struct waitset *ws = NULL;
    ws = get_default_waitset();
    while (is_ip_present_in_arp_cache(&dst_ip) == false) {
//        NETD_DEBUG("refresh_arp_cache: event dispatched\n");
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            abort();
        }
   } // end while: till arp not present
   return find_ip_arp_cache(&dst_ip);
}

static errval_t ARP_resolve_request(struct net_ARP_binding *cc,
            ipv4addr_t ip, uint32_t iface, bool force,
            errval_t *err, uint64_t *mac)
{
    NETD_DEBUG("ARP_resolve_request: client asking ARP lookup for ip %"
            PRIu32" over iface %"PRIu32"\n", ip, iface);

    *mac = refresh_cache(ip);
    assert(*mac != 0);
//    assert(!"NYI ARP resolve request");
    NETD_DEBUG("ARP_resolve_request: MAC found for ARP request ip %"
            PRIu32" over iface %"PRIu32" == %"PRIx64"\n",
            ip, iface, *mac);
    *err = SYS_ERR_OK;
    return SYS_ERR_OK;
} // end function: ARP_resolve_request


// Initialzes the ARP lookup service
int init_ARP_lookup_service(struct netd_state *state, char *dev_name)
{
    errval_t err = SYS_ERR_OK; // default return value

    // sanity check on parameter
    assert(dev_name != NULL);

    // start the port management service
    snprintf(state->ARP_service_name, sizeof(state->ARP_service_name), "%s%s", dev_name,
             NET_ARP_LOOKUP_SUFFIX);
    state->ARP_service_exported = false;
    state->registered_user_list = NULL;

    NETD_DEBUG("init_ARP_lookup_service called [%s]\n", state->ARP_service_name);

   // exporting net_ports interface
    err = net_ARP_export(state, export_ARP_cb, connect_ARP_cb,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC("net_ARP_export failed!");
        return err;
    }

    // wait till ports export is actually done
    struct waitset *ws = get_default_waitset();


    while (!state->ARP_service_exported) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch for init_ARP_service");
            return err;
        }
    } // end while:

    return err;
} // end function: init_ARP_service
