/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

// stdlib includes

// barrelfish includes

// lwip includes
#include "lwip/init.h"
#include "lwip/netif.h"
#include "lwip/ip.h"
#include "lwip/dhcp.h"
#include "lwip/prot/ethernet.h"

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

#include <net/net_filter.h>
#include <net_interfaces/flags.h>
#include "networking_internal.h"

struct net_state state = {0};

#define NETWORKING_DEFAULT_QUEUE_ID 0
#define NETWORKING_BUFFER_COUNT (4096 * 3)
#define NETWORKING_BUFFER_RX_POPULATE (4096 - 10)
#define NETWORKING_BUFFER_SIZE  2048


#define NETDEBUG_SUBSYSTEM "net"

/**
 * @brief obtains the default setting for initializaion of the driver
 *
 * @param queue     returns the queue to be used
 * @param cardname  returns the card name to be used
 *
 * @return SYS_ERR_OK on success, SKB_ERR_* on failure
 */
errval_t networking_get_defaults(uint64_t *queue, const char **cardname, uint32_t *flags)
{
    /* TODO: get the values from the SKB */

    *queue = NETWORKING_DEFAULT_QUEUE_ID;
    //*cardname = "e10k";
    *cardname = "sfn5122f";
    *flags = NET_FLAGS_POLLING | NET_FLAGS_BLOCKING_INIT;
    //*flags = NET_FLAGS_POLLING;

    return SYS_ERR_OK;
}

static void int_handler(void* args)
{
    struct net_state *st = devq_get_state(args);

    net_if_poll(&st->netif);
}

static errval_t create_loopback_queue (struct net_state *st, uint64_t* queueid,
                                       struct devq **retqueue)
{
    errval_t err;

    debug_printf("net: creating loopback queue.\n");

    *queueid = 0;
    err = loopback_queue_create((struct loopback_queue **)retqueue);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

static errval_t create_driver_queue (struct net_state *st, uint64_t* queueid,
                                     struct devq **retqueue)
{
    *queueid = 0;
    return SYS_ERR_OK;
}


static errval_t create_e10k_queue (struct net_state *st, uint64_t* queueid,
                                   struct devq **retqueue)
{
    errval_t err;
    err = e10k_queue_create((struct e10k_queue**)retqueue, int_handler,
                            false /*virtual functions*/,
                            !(st->flags & NET_FLAGS_POLLING) /* user interrupts*/);
    *queueid = e10k_queue_get_id((struct e10k_queue*)*retqueue);
    return err;
}

static errval_t create_sfn5122f_queue (struct net_state *st, uint64_t* queueid, 
                                       struct devq **retqueue)
{
    errval_t err;
    err = sfn5122f_queue_create((struct sfn5122f_queue**)retqueue, int_handler,
                                false /*userlevel network feature*/,
                                !(st->flags & NET_FLAGS_POLLING) /* user interrupts*/,
                                (st->flags & NET_FLAGS_DEFAULT_QUEUE));
    *queueid = sfn5122f_queue_get_id((struct sfn5122f_queue*)*retqueue);
    return err;
}


typedef errval_t (*queue_create_fn)(struct net_state *, uint64_t*, struct devq **);
struct networking_card
{
    char *cardname;
    queue_create_fn createfn;
} networking_cards [] = {
    { "loopback", create_loopback_queue},
    { "e1000", create_driver_queue},
    { "e10k", create_e10k_queue},
    { "sfn5122f", create_sfn5122f_queue},
    { NULL, NULL}
};


/**
 * @brief creates a queue to the given card and the queueid
 *
 * @param cardname  network card to create the queue for
 * @param queueid   queueid of the network card
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
static errval_t net_create_queue(struct net_state *st, const char *cardname,
                                 uint64_t* queueid, struct devq **retqueue)
{
    debug_printf("net: creating queue for card='%s'...\n",
                  cardname);

    struct networking_card *nc = networking_cards;
    while(nc->cardname != NULL) {
        if (strncmp(cardname, nc->cardname, strlen(nc->cardname)) == 0) {
            return nc->createfn(st, queueid, retqueue);
        }
        nc++;
    }

    debug_printf("net: ERROR unknown queue. card='%s', queueid=%" PRIu64 "\n",
                  cardname, *queueid);

    return -1;
}

/**
 * @brief creates a queue to the given card and the queueid
 *
 * @param cardname  network card to create the queue for
 * @param queueid   queueid of the network card
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_create_queue(const char *cardname, uint64_t* queueid,
                                 struct devq **retqueue)
{
    struct net_state *st = get_default_net_state();

    return net_create_queue(st, cardname, queueid, retqueue);
}



static errval_t networking_poll_st(struct net_state *st)
{
    event_dispatch_non_block(get_default_waitset());
    if (st->flags & NET_FLAGS_POLLING) {
        return net_if_poll(&st->netif);
    } else {
        return event_dispatch_non_block(get_default_waitset());
    }
}




/**
 * @brief initializes the networking library with a given device queue
 *
 * @param st        the networking state to initialize
 * @param q         the device queue to initialize the networking on
 * @param flags     supplied initialization flags
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
static errval_t networking_init_with_queue_st(struct net_state *st,struct devq *q,
                                              net_flags_t flags)
{
    errval_t err;

    NETDEBUG("initializing networking with devq=%p, flags=%" PRIx32 "...\n", q,
             flags);

    if(st->initialized) {
        debug_printf("WARNING. initialize called twice. Ignoring\n");
        return SYS_ERR_OK;
    }

    /* set the variables */
    st->flags = flags;
    st->queue = q;
    st->initialized = true;
    st->waitset = get_default_waitset();

    /* associate the net state with the device queue */
    devq_set_state(st->queue, st);


    /* initialize the device queue */
    NETDEBUG("initializing LWIP...\n");
    lwip_init();

    /* create the LWIP network interface and initialize it */
    NETDEBUG("creating netif for LWIP...\n");
    err = net_if_init_devq(&st->netif, st->queue);
    if (err_is_fail(err)) {
        goto out_err1;
    }

    err = net_if_add(&st->netif, st);
    if (err_is_fail(err)) {
        goto out_err1;
    }


    NETDEBUG("initializing hw filter...\n");

    err = net_filter_init(&st->filter, st->cardname);
    if (err_is_fail(err)) {
        USER_PANIC("Init filter infrastructure failed: %s \n", err_getstring(err));
    }

    NETDEBUG("setting default netif...\n");
   // netif_set_default(&st->netif);

    /* create buffers and add them to the interface*/
    err = net_buf_pool_alloc(st->queue, NETWORKING_BUFFER_COUNT,
                             NETWORKING_BUFFER_SIZE, &st->pool);
    if (err_is_fail(err)) {
        //net_if_destroy(&st->netif);
        goto out_err1;
    }

    NETDEBUG("adding RX buffers\n");
    for (int i = 0; i < NETWORKING_BUFFER_RX_POPULATE; i++) {
        struct pbuf *p = net_buf_alloc(st->pool);
        if (p == NULL) {
            NETDEBUG("net: WARNING there was no buffer\n");
            break;
        }
        err = net_if_add_rx_buf(&st->netif, p);
        if (err_is_fail(err)) {
            break;
        }
    }

    if (flags & NET_FLAGS_DO_DHCP) {
        err = dhcpd_start(flags);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to start DHCP.\n");
        }

        err = arp_service_start();
        if (err_is_fail(err)) {
            DEBUG_ERR(err,  "failed to start the ARP service\n");
        }
    } else {
        /* get IP from dhcpd */
        err = dhcpd_query(flags);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to start DHCP.\n");
        }

        err = arp_service_subscribe();
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to subscribte the ARP service\n");
        }
    }

    NETDEBUG("initialization complete.\n");

    return SYS_ERR_OK;

    out_err1:
    st->initialized = false;

    return err;

}

/**
 * @brief initializes the networking library
 *
 * @param st        the networking state to be initalized
 * @param nic       the nic to use with the networking library
 * @param flags     flags to use to initialize the networking library
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
static errval_t networking_init_st(struct net_state *st, const char *nic,
                                   net_flags_t flags)
{
    errval_t err;

    NETDEBUG("initializing networking with nic=%s, flags=%" PRIx32 "...\n", nic,
             flags);

    if(st->initialized) {
        NETDEBUG("WARNING. initialize called twice. Ignoring\n");
        return SYS_ERR_OK;
    }

    st->cardname = nic;
    st->flags = flags;

    /* create the queue wit the given nic and card name */
    err = networking_create_queue(st->cardname, &st->queueid, &st->queue);
    if (err_is_fail(err)) {
        return err;
    }

    err = networking_init_with_queue_st(st, st->queue, flags);
    if (err_is_fail(err)) {
       // devq_destroy(st->queue);
    }

    return err;
}

/**
 * @brief initializes the networking with the defaults
 *
 * @param st    the networking state to be initialized
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
static errval_t networking_init_default_st(struct net_state *st)
{
    errval_t err;

    NETDEBUG("initializing networking with default options...\n");

    if(st->initialized) {
        NETDEBUG("WARNING. initialize called twice. Ignoring\n");
        return SYS_ERR_OK;
    }

    // obtain the settings to create the queue
    err = networking_get_defaults(&st->queueid, &st->cardname, &st->flags);
    if (err_is_fail(err)) {
        return err;
    }

    return networking_init_st(st, st->cardname, st->flags);
}



/*
 * ===========================================================================
 * Public interface
 * ===========================================================================
 */

/**
 * @brief initializes the networking library with a given device queue
 *
 * @param q         the device queue to initialize the networking on
 * @param flags     supplied initialization flags
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_init_with_queue(struct devq *q, net_flags_t flags)
{
    struct net_state *st = get_default_net_state();
    return networking_init_with_queue_st(st, q, flags);
}

/**
 * @brief initializes the networking library
 *
 * @param nic       the nic to use with the networking library
 * @param flags     flags to use to initialize the networking library
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_init(const char *nic, net_flags_t flags)
{
    struct net_state *st = get_default_net_state();
    return networking_init_st(st, nic, flags);
}


/**
 * @brief initializes the networking with the defaults
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_init_default(void)
{
    struct net_state *st = get_default_net_state();
    return networking_init_default_st(st);
}


/**
 * @brief polls the network for new packets
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_poll(void)
{
    struct net_state *st = &state;
    return networking_poll_st(st);
}


/**
 * @brief Install L3/L4 filter
 *
 * @param tcp       should TCP packets be filtered or UPD
 * @param src_ip    source ip of the filter, 0 for wildcard
 * @param src_port  source port of the filter, 0 for wildcard
 * @param dst_port  destination port fo the filter       
 *
 * @return SYS_ERR_OK on success, NET_FILTER_ERR_* on failure
 */
errval_t networking_install_ip_filter(bool tcp, ip_addr_t* src, 
                                      uint16_t src_port, uint16_t dst_port)
{
    errval_t err;
    if (state.filter == NULL) {
        return NET_FILTER_ERR_NOT_INITIALIZED;
    }

    struct net_filter_state *st = state.filter;

    // get current config
    ip_addr_t dst_ip;
    err = dhcpd_get_ipconfig(&dst_ip, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
    
    struct net_filter_ip ip = {
        .qid = state.queueid,
        .ip_src = (uint32_t) src->addr,
        .ip_dst = (uint32_t) dst_ip.addr,
        .port_dst = dst_port,
        .port_src = src_port,
    };

    if (tcp) {
        ip.type = NET_FILTER_TCP;
    } else {
        ip.type = NET_FILTER_UDP;
    }
    
    return net_filter_ip_install(st, &ip);
}

/**
 * @brief Remove L3/L4 filter
 *
 * @param tcp       should TCP packets be filtered or UPD
 * @param src_ip    source ip of the filter, 0 for wildcard
 * @param src_port  source port of the filter, 0 for wildcard
 * @param dst_port  destination port fo the filter       
 *
 * @return SYS_ERR_OK on success, NET_FILTER_ERR_* on failure
 */
errval_t networking_remove_ip_filter(bool tcp, ip_addr_t* src, 
                                     uint16_t src_port, uint16_t dst_port)
{

    errval_t err;
    if (state.filter == NULL) {
        return NET_FILTER_ERR_NOT_INITIALIZED;
    }

    struct net_filter_state *st = state.filter;

    // get current config
    ip_addr_t dst_ip;
    err = dhcpd_get_ipconfig(&dst_ip, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
    
    struct net_filter_ip ip = {
        .qid = state.queueid,
        .ip_src = (uint32_t) src->addr,
        .ip_dst = (uint32_t) dst_ip.addr,
        .port_dst = dst_port,
        .port_src = src_port,
    };

    if (tcp) {
        ip.type = NET_FILTER_TCP;
    } else {
        ip.type = NET_FILTER_UDP;
    }
    
    return net_filter_ip_remove(st, &ip);
}
