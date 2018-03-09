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
#include "lwip/timeouts.h"

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/waitset.h>
#include <barrelfish/waitset_chan.h>

#include <net/net_filter.h>
#include <net_interfaces/flags.h>
#include "networking_internal.h"
#include "net_queue_internal.h"

struct net_state state = {0};
struct waitset_chanstate net_loopback_poll_channel;
struct deferred_event net_lwip_timer;

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
    NETDEBUG("Enter int_handler!\n");
    struct net_state *st = devq_get_state(args);

    if (st) {
        net_if_poll(&st->netif);
        net_lwip_timeout();
    }
}

static void net_loopback_poll(void *arg)
{
    netif_poll_all();
    net_lwip_timeout();
}

void net_if_trigger_loopback(void)
{
    errval_t err;
    
    err = waitset_chan_trigger(&net_loopback_poll_channel);
    assert(err_is_ok(err));
}

void net_lwip_timeout(void)
{
    errval_t err;

    sys_check_timeouts();
    deferred_event_cancel(&net_lwip_timer);
    uint32_t delay = sys_timeouts_sleeptime();
    if (delay != 0xffffffff) {
        err = deferred_event_register(&net_lwip_timer, get_default_waitset(),
            delay * 1000, MKCLOSURE((void (*)(void *))net_lwip_timeout, NULL));
        assert(err_is_ok(err));
    }
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
    bool poll = st->flags & NET_FLAGS_POLLING;
    bool default_q = st->flags & NET_FLAGS_DEFAULT_QUEUE;
    return net_queue_internal_create(int_handler, cardname, queueid, default_q,
                                     poll, retqueue);
}


static errval_t networking_poll_st(struct net_state *st)
{
    event_dispatch_non_block(get_default_waitset());
    if (st->flags & NET_FLAGS_POLLING) {
        return net_if_poll(&st->netif);
    } else {
        return event_dispatch(get_default_waitset());
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
static errval_t networking_init_with_queue_st(struct net_state *st, struct devq *q,
                                              net_flags_t flags)
{
    errval_t err;

    NETDEBUG("initializing networking with devq=%p, flags=%" PRIx32 "...\n", q,
             flags);

    if (st->initialized) {
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

    /* create buffers and add them to the interface*/
    err = net_buf_pool_alloc(st->queue, NETWORKING_BUFFER_COUNT,
                             NETWORKING_BUFFER_SIZE, &st->pool);
    if (err_is_fail(err)) {
        //net_if_destroy(&st->netif);
        goto out_err1;
    }

    deferred_event_init(&net_lwip_timer);
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

    if (!(flags & NET_FLAGS_NO_NET_FILTER) && st->hw_filter) {
        NETDEBUG("initializing hw filter...\n");

        err = net_filter_init(&st->filter, st->cardname);
        if (err_is_fail(err)) {
            USER_PANIC("Init filter infrastructure failed: %s \n", err_getstring(err));
        }
    }

    NETDEBUG("setting default netif...\n");
    netif_set_default(&st->netif);

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
        /* get static IP config */
        err = net_config_static_ip_query(flags);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to set IP.\n");
        }

        err = arp_service_subscribe();
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to subscribte the ARP service\n");
        }
    }

    waitset_chanstate_init(&net_loopback_poll_channel, CHANTYPE_OTHER);
    net_loopback_poll_channel.persistent = true;
    err = waitset_chan_register(get_default_waitset(), &net_loopback_poll_channel,
                               MKCLOSURE(net_loopback_poll, NULL));

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
    // default no hw filters
    st->hw_filter = false;

    /* create the queue wit the given nic and card name */
    err = networking_create_queue(nic, &st->queueid, &st->queue);
    if (err_is_fail(err)) {
        return err;
    }

    assert(st->queue != NULL);

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
errval_t networking_install_ip_filter(bool tcp, struct in_addr *src,
                                      uint16_t src_port, uint16_t dst_port)
{
    errval_t err;
    if (state.filter == NULL) {
        return NET_FILTER_ERR_NOT_INITIALIZED;
    }

    struct net_filter_state *st = state.filter;

    // get current config
    struct in_addr dst_ip;
    err = netif_get_ipconfig(&dst_ip, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
    
    struct net_filter_ip ip = {
        .qid = state.queueid,
        .ip_src = (uint32_t) src->s_addr,
        .ip_dst = (uint32_t) dst_ip.s_addr,
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
errval_t networking_remove_ip_filter(bool tcp, struct in_addr *src,
                                     uint16_t src_port, uint16_t dst_port)
{

    errval_t err;
    if (state.filter == NULL) {
        return NET_FILTER_ERR_NOT_INITIALIZED;
    }

    struct net_filter_state *st = state.filter;

    // get current config
    struct in_addr dst_ip;
    err = netif_get_ipconfig(&dst_ip, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
    
    struct net_filter_ip ip = {
        .qid = state.queueid,
        .ip_src = (uint32_t) src->s_addr,
        .ip_dst = (uint32_t) dst_ip.s_addr,
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
