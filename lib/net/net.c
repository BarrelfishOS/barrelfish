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


#include <net_interfaces/flags.h>
#include "networking_internal.h"

struct net_state state = {0};

#define NETWORKING_DEFAULT_QUEUE_ID 0
#define NETWORKING_BUFFER_COUNT 1024
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
errval_t networking_get_defaults(uint64_t *queue, char **cardname)
{
    /* TODO: get the values from the SKB */

    *queue = NETWORKING_DEFAULT_QUEUE_ID;
    *cardname = "loopback";

    return SYS_ERR_OK;
}



static void int_handler(void* args)
{
    struct net_state *st = devq_get_state(args);

    net_if_poll(&st->netif);
}

static errval_t create_loopback_queue (uint64_t queueid, struct devq **retqueue)
{
    errval_t err;

    debug_printf("net: creating loopback queue.\n");

    err = loopback_queue_create((struct loopback_queue **)retqueue);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

static errval_t create_driver_queue (uint64_t queueid, struct devq **retqueue)
{
    return SYS_ERR_OK;
}


static errval_t create_e10k_queue (uint64_t queueid, struct devq **retqueue)
{
    return SYS_ERR_OK;
}

static errval_t create_sfn5122f_queue (uint64_t queueid, struct devq **retqueue)
{

    return sfn5122f_queue_create((struct sfn5122f_queue**)retqueue, int_handler,
                                false /*userlevel network feature*/,
                                true /* user interrupts*/);
}


typedef errval_t (*queue_create_fn)(uint64_t queueid, struct devq **retqueue);
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
errval_t networking_create_queue(const char *cardname, uint64_t queueid,
                                 struct devq **retqueue)
{
    debug_printf("net: creating queue for card='%s', queueid=%" PRIu64 "...\n",
                  cardname, queueid);

    struct networking_card *nc = networking_cards;
    while(nc->cardname != NULL) {
        if (strncmp(cardname, nc->cardname, strlen(nc->cardname)) == 0) {
            return nc->createfn(queueid, retqueue);
        }
        nc++;
    }

    debug_printf("net: ERROR unknown queue. card='%s', queueid=%" PRIu64 "\n",
                  cardname, queueid);

    return -1;
}

errval_t networking_get_mac(struct devq *q, uint8_t *hwaddr, uint8_t hwaddrlen) {
    debug_printf("net: obtaining MAC address for card.\n");
    return SYS_ERR_OK;
}

#define NETWORKING_POLL_MAX 100
#include <lwip/pbuf.h>
#include <lwip/prot/ethernet.h>
#include <lwip/prot/ip4.h>
#include <lwip/prot/udp.h>

errval_t networking_poll(void)
{
    struct net_state *st = &state;

    return net_if_poll(&st->netif);
}

/**
 * @brief
 * @return
 */
errval_t networking_init_default(void) {
    errval_t err;

    struct net_state *st = &state;

    if(st->initialized) {
        debug_printf("WARNING. initialize called twice. Ignoring\n");
        return SYS_ERR_OK;
    }

    NETDEBUG("initializing networking...\n");

    // obtain the settings to create the queue
    err = networking_get_defaults(&st->queueid, &st->cardname);
    if (err_is_fail(err)) {
        return err;
    }

    // create the queue
    err = networking_create_queue(st->cardname, st->queueid, &st->queue);
    if (err_is_fail(err)) {
        return err;
    }

    devq_set_state(st->queue, st);

    // initialize LWIP
    NETDEBUG("initializing LWIP...\n");
    lwip_init();

    /* create buffers */
    err = net_buf_init(st->queue, NETWORKING_BUFFER_COUNT,
                                 NETWORKING_BUFFER_SIZE, &st->pool);
    if (err_is_fail(err)) {
        goto out_err1;
    }

    NETDEBUG("creating netif for LWIP...\n");
    err = net_if_init_devq(&st->netif, st->queue);
    if (err_is_fail(err)) {
        goto out_err2;
    }

    err = net_if_add(&st->netif, st);
    if (err_is_fail(err)) {
        goto out_err2;
    }

    NETDEBUG("setting default netif...\n");
    netif_set_default(&st->netif);


    NETDEBUG("adding RX buffers\n");

    for (int i = 0; i < 10; i++) {
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


    NETDEBUG("starting DHCP...\n");
    err_t lwip_err = dhcp_start(&st->netif);
    if(lwip_err != ERR_OK) {

    }


    NETDEBUG("initialization complete.\n");

    return SYS_ERR_OK;

    out_err2:
        // TODO: clear buffers

    out_err1:
        // TODO: cleanup queue



    return err;
}

