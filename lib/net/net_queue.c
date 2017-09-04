/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <net/net_queue.h>
#include "networking_internal.h"
#include "net_queue_internal.h"

static errval_t create_loopback_queue(const char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                      bool default_q, bool poll, struct devq **retqueue)
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

static errval_t create_driver_queue(const char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                    bool default_q, bool poll, struct devq **retqueue)
{
    *queueid = 0;
    return SYS_ERR_OK;
}

// cardname - "e1000:vendor:deviceid:bus:device:function"
static errval_t create_e1000_queue(const char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                   bool default_q, bool poll, struct devq **retqueue)
{
    if (cardname[5] != ':') {
        return SYS_ERR_OK;
    }
    uint32_t vendor, deviceid, bus, device, function;
    unsigned parsed = sscanf(cardname + 6, "%x:%x:%x:%x:%x", &vendor,
                             &deviceid, &bus, &device, &function);
    if (parsed != 5) {
        return SYS_ERR_OK;
    }

    struct net_state* st = get_default_net_state();
    // disable HW filter since the card does not have them
    st->hw_filter = false;

    return e1000_queue_create((struct e1000_queue**)retqueue, vendor, deviceid,
                              bus, device, function, 1, interrupt);
}

// cardname - "mlx4:vendor:deviceid:bus:device:function"
static errval_t create_mlx4_queue(const char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                   bool default_q, bool poll, struct devq **retqueue)
{
    if (cardname[4] != ':') {
        return SYS_ERR_OK;
    }
    uint32_t vendor, deviceid, bus, device, function;
    unsigned parsed = sscanf(cardname + 5, "%x:%x:%x:%x:%x", &vendor,
                             &deviceid, &bus, &device, &function);
    if (parsed != 5) {
        return SYS_ERR_OK;
    }

    struct net_state* st = get_default_net_state();
    // disable HW filter since the card does not have them
    st->hw_filter = false;

    return mlx4_queue_create((struct mlx4_queue**)retqueue, vendor, deviceid,
                              bus, device, function, 1, interrupt);
}

static errval_t create_e10k_queue(const char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                  bool default_q, bool poll, struct devq **retqueue)
{
    errval_t err;
    struct net_state* st = get_default_net_state();
    // enable HW filter since they are enabled by default by the driver
    st->hw_filter = true;
    err = e10k_queue_create((struct e10k_queue**)retqueue, interrupt,
                            false /*virtual functions*/,
                            !poll, /* user interrupts*/
                            default_q);
    *queueid = e10k_queue_get_id((struct e10k_queue*)*retqueue);
    assert(retqueue != NULL);
    return err;
}

static errval_t create_sfn5122f_queue(const char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                      bool default_q, bool poll, struct devq **retqueue)
{
    errval_t err;
    struct net_state* st = get_default_net_state();
    // enable HW filter since they are enabled by default by the driver
    st->hw_filter = true;
    err = sfn5122f_queue_create((struct sfn5122f_queue**)retqueue, interrupt,
                                false /*userlevel network feature*/,
                                !poll /* user interrupts*/,
                                default_q);
    *queueid = sfn5122f_queue_get_id((struct sfn5122f_queue*)*retqueue);
    return err;
}


typedef errval_t (*queue_create_fn)(const char*, inthandler_t, uint64_t*, bool, bool, struct devq **);
struct networking_card
{
    char *cardname;
    queue_create_fn createfn;
} networking_cards [] = {
    { "loopback", create_loopback_queue},
    { "driver", create_driver_queue},
    { "e1000", create_e1000_queue},
    { "mlx4", create_mlx4_queue},
    { "e10k", create_e10k_queue},
    { "sfn5122f", create_sfn5122f_queue},
    { NULL, NULL}
};


/**
 * @brief creates a queue to the given card and the queueid
 *
 * @param interrupt interrupt handler
 * @param cardname  network card to create the queue for
 * @param queueid   queueid of the network card
 * @param default_q get the default queue (most of the time queue 0)
 * @param poll      Is the queue polled or are interrupts used
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_queue_internal_create(inthandler_t interrupt, const char *cardname,
                                   uint64_t* queueid, bool default_q, bool poll,
                                   struct devq **retqueue)
{
    struct networking_card *nc = networking_cards;
    while(nc->cardname != NULL) {
        if (strncmp(cardname, nc->cardname, strlen(nc->cardname)) == 0) {
            return nc->createfn(cardname, interrupt, queueid, default_q,
                                poll, retqueue);
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
 * @param interrupt interrupt handler
 * @param cardname  network card to create the queue for
 * @param queueid   queueid of the network card
 * @param poll      Is the queue polled or are interrupts used
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_queue_create(inthandler_t interrupt, const char *cardname,
                          uint64_t* queueid, bool poll, struct devq **retqueue)
{
    return net_queue_internal_create(interrupt, cardname, queueid, false, poll, retqueue);
}
