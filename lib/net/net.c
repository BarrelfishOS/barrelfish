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

#include <barrelfish/deferred.h>


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
    *cardname = "sfn5122f";
    *flags = NET_FLAGS_POLLING | NET_FLAGS_DO_DHCP | NET_FLAGS_BLOCKING_INIT;

    return SYS_ERR_OK;
}

static void int_handler(void* args)
{
    struct net_state *st = devq_get_state(args);

    net_if_poll(&st->netif);
}

static errval_t create_loopback_queue (struct net_state *st, uint64_t queueid,
                                       struct devq **retqueue)
{
    errval_t err;

    debug_printf("net: creating loopback queue.\n");

    err = loopback_queue_create((struct loopback_queue **)retqueue);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

static errval_t create_driver_queue (struct net_state *st, uint64_t queueid,
                                     struct devq **retqueue)
{
    return SYS_ERR_OK;
}


static errval_t create_e10k_queue (struct net_state *st, uint64_t queueid,
                                   struct devq **retqueue)
{
    return SYS_ERR_OK;
}

static errval_t create_sfn5122f_queue (struct net_state *st, uint64_t queueid, struct devq **retqueue)
{

    return sfn5122f_queue_create((struct sfn5122f_queue**)retqueue, int_handler,
                                false /*userlevel network feature*/,
                                !(st->flags & NET_FLAGS_POLLING) /* user interrupts*/);
}


typedef errval_t (*queue_create_fn)(struct net_state *, uint64_t, struct devq **);
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
                                        uint64_t queueid, struct devq **retqueue)
{
    debug_printf("net: creating queue for card='%s', queueid=%" PRIu64 "...\n",
                  cardname, queueid);

    struct networking_card *nc = networking_cards;
    while(nc->cardname != NULL) {
        if (strncmp(cardname, nc->cardname, strlen(nc->cardname)) == 0) {
            return nc->createfn(st, queueid, retqueue);
        }
        nc++;
    }

    debug_printf("net: ERROR unknown queue. card='%s', queueid=%" PRIu64 "\n",
                  cardname, queueid);

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
errval_t networking_create_queue(const char *cardname, uint64_t queueid,
                                 struct devq **retqueue)
{
    struct net_state *st = get_default_net_state();

    return net_create_queue(st, cardname, queueid, retqueue);
}

errval_t networking_get_mac(struct devq *q, uint8_t *hwaddr, uint8_t hwaddrlen) {
    debug_printf("net: obtaining MAC address for card.\n");
    errval_t err;

    uint64_t card_mac;
    err = devq_control(q, 0, 0, &card_mac);
    if (err_is_fail(err)) {
        return err;
    }

    memcpy(hwaddr, &card_mac, hwaddrlen);

    SMEMCPY(hwaddr, &card_mac, hwaddrlen);

    debug_printf("got mac: %x:%x:%x:%x:%x:%x\n",
                 hwaddr[0], hwaddr[1],hwaddr[2], hwaddr[3], hwaddr[4], hwaddr[5]);

    return SYS_ERR_OK;
}

static errval_t networking_poll_st(struct net_state *st)
{
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
    } else {
        /* get IP from dhcpd */
        err = dhcpd_query(flags);
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
    err = networking_create_queue(st->cardname, st->queueid, &st->queue);
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
