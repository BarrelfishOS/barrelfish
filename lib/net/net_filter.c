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
#include <barrelfish/nameservice_client.h>
#include <if/net_filter_defs.h>
#include <if/net_filter_rpcclient_defs.h>

#include <net/net_filter.h>

#include "networking_internal.h"
#include "debug.h"

#define MAX_NAME 128

#define NETDEBUG_SUBSYSTEM "filter"

/******************************************************************************
 * Connection setup
 ******************************************************************************/

// Callback for bind
static void bind_cb(void *st, errval_t err, struct net_filter_binding *b)
{
    struct net_filter_state* filt = (struct net_filter_state*) st;
    assert(err_is_ok(err));

    NETDEBUG("Sucessfully connected to management interface\n");

    filt->b = b;
    net_filter_rpc_client_init(filt->b);
    filt->bound = true;
}


/** Open connection to management interface */
static errval_t connect_to_net_filter(struct net_filter_state* st,
                                      const char *dev_name)
{
    errval_t r;
    iref_t iref;
    const char* prefix = "net_filter_";
    char name[strlen(dev_name) + strlen(prefix) + 1];
   
    // Build label for management service
    sprintf(name, "%s%s", prefix, dev_name);

    NETDEBUG("Name lookup\n");
    // Connect to service
    r = nameservice_blocking_lookup(name, &iref);
    if (err_is_fail(r)) {
        return r;
    }

    NETDEBUG("Binding\n");
    r = net_filter_bind(iref, bind_cb, st, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(r)) {
        return r;
    }

    NETDEBUG("Waiting to bind\n");
    while(st->bound == false) {
        event_dispatch(get_default_waitset());
    }
    
    NETDEBUG("finished connecting\n");
    return SYS_ERR_OK;
}

/******************************************************************************
 * Helper functions
 ******************************************************************************/

static bool filter_cmp_ip(struct net_filter_ip* f1, struct net_filter_ip* f2)
{
    if (f1->ip_src == f2->ip_src &&
        f1->ip_dst == f2->ip_dst &&
        f1->port_src == f2->port_src &&
        f1->port_dst == f2->port_dst &&
        f1->qid == f2->qid &&
        f1->type == f2->type) {
        return true;
    }
    return false;
}

/*
static bool filter_cmp_mac(struct net_filter_mac* f1, struct net_filter_mac* f2)
{
    if (f1->vlan_id == f2->vlan_id &&
        f1->mac == f2->mac &&
        f1->type == f2->type) {
        return true;
    }
    return false;
}
*/
/*
static bool is_reachable(struct net_filter_ip* filt)
{
    struct net_filter_ele* cur = filter_state.filters_ip.start;
    
    while(cur != NULL) {
        printf("reachable: port_dst: %"PRIu16" %"PRIu16" \n", cur->filter.ip.port_dst, filt->port_dst);
        if (filter_cmp_ip(&cur->filter.ip, filt)) {
            return true;
        }
        cur = cur->next;
    }
    return false;
}
*/
/******************************************************************************
 * Library function implementation
 ******************************************************************************/

/**
 * @brief initalized network filtering. Sets up connection to drivers
 *        which support hardware filtering
 *
 * @param st        returned net filter state;
 * @param cardname  the card name to be used
 *
 * @return SYS_ERR_OK on success, error on failure
 */
errval_t net_filter_init(struct net_filter_state** st,
                         const char* cardname)
{
    errval_t err;

    struct net_filter_state* tmp = calloc(1, sizeof(struct net_filter_state));
    assert(tmp != NULL);
    
    tmp->filters_ip.start = NULL;
    tmp->filters_ip.num_ele = 0;
    tmp->filters_mac.start = NULL;
    tmp->filters_mac.num_ele = 0;

    // cardname are of the form name:vendor:device:bus:function ..
    int end = 0;
    for (; end < strlen(cardname); end++) {
        if (cardname[end] == ':') {
            break;
        }
    }

    char name[64];
    strncpy(name, cardname, end);
    name[end] = '\0';

    printf("cardname %s \n", name);
    err = connect_to_net_filter(tmp, name);
    *st = tmp;
    return err;
}


/**
 * @brief Installs an L3/L4 filter in the hardware filter
 *        tables
 *
 * @param st    net filter state
 * @param filt  filter struct
 *
 * @return SYS_ERR_OK on success, error on failure
 */
errval_t net_filter_ip_install(struct net_filter_state* st,
                               struct net_filter_ip* filt)
{

    assert(st->bound);
    errval_t err;
    uint64_t filter_id;

    struct net_filter_ele* cur = st->filters_ip.start;
    struct net_filter_ele* prev = NULL;

    /* go through linked list and find last element
     (and check if filter is already installed) */
    if (cur == NULL) {
        st->filters_ip.start = malloc(sizeof(struct net_filter_ele));
        cur = st->filters_ip.start;
    } else {
        while(cur->next != NULL) {
            if (filter_cmp_ip(&cur->filter.ip, filt)) {
                return NET_FILTER_ERR_ALREADY_EXISTS;
            }
            prev = cur;
            cur = cur->next;
        }

        if (filter_cmp_ip(&cur->filter.ip, filt)) {
            return NET_FILTER_ERR_ALREADY_EXISTS;
        }

        cur->next = malloc(sizeof(struct net_filter_ele));
        cur = cur->next;
    }

    cur->filter.ip.ip_src = filt->ip_src;
    cur->filter.ip.ip_dst = filt->ip_dst;
    cur->filter.ip.port_src = filt->port_src;
    cur->filter.ip.port_dst = filt->port_dst;
    cur->filter.ip.qid = filt->qid;
    cur->filter.ip.type = filt->type;
    cur->next = NULL;
    cur->prev = prev;

    st->filters_ip.num_ele++;

    err = st->b->rpc_tx_vtbl.install_filter_ip(st->b,
                                               filt->type,
                                               filt->qid,
                                               filt->ip_src,
                                               filt->ip_dst,
                                               filt->port_src,
                                               filt->port_dst,
                                               &filter_id);
    if (err_is_fail(err)) {
        free(cur);
        return err;
    }

    cur->filter_id = filter_id;
    return SYS_ERR_OK;
}


/**
 * @brief Removes an L3/L4 filter in the hardware filter
 *        tables
 *
 * @param st    net filter state
 * @param filt  filter struct
 *
 * @return SYS_ERR_OK on success, error on failure
 */
errval_t net_filter_ip_remove(struct net_filter_state* st,
                              struct net_filter_ip* filt)
{

    assert(st->bound);
    errval_t err, err2;
    uint64_t filter_id = (uint64_t)-1;

    struct net_filter_ele* cur = st->filters_ip.start;
    struct net_filter_ele* prev = NULL;


    // no entries
    if (cur == NULL) {
        return NET_FILTER_ERR_NOT_FOUND;
    }

    // Multiple entries
    while(cur != NULL) {
        if (filter_cmp_ip(&cur->filter.ip, filt)) {
            filter_id = cur->filter_id;
            break;
        }
        prev = cur;
        cur = cur->next;
    }

   
    if (filter_id == (uint64_t) -1) {
        return NET_FILTER_ERR_NOT_FOUND;
    }

    err = st->b->rpc_tx_vtbl.remove_filter(st->b,
                                           filt->type,
                                           filter_id,
                                           &err2);
    if (err_is_fail(err) || err_is_fail(err2)) {
        return err_is_fail(err) ? err: err2;
    }

    // remove from queue
    if (prev != NULL) { // check if first
        prev->next = cur->next;
        if (cur->next != NULL) { // check if last
            cur->next->prev = prev;
        }
    } else {
        st->filters_ip.start = cur->next;
    }
    

    free(cur);

    st->filters_ip.num_ele--;

    return SYS_ERR_OK;
}

errval_t net_filter_mac_install(struct net_filter_state* st,
                                struct net_filter_mac* filt)
{
   USER_PANIC("NYI \n");
}


errval_t net_filter_mac_remove(struct net_filter_state* st,
                               struct net_filter_mac* filt)
{
   USER_PANIC("NYI \n");
}
