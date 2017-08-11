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

// cardname - "e1000:vendor:deviceid:bus:device:function"
static errval_t create_e1000_queue(char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                   bool poll, struct devq **retqueue)
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

    return e1000_queue_create((struct e1000_queue**)retqueue, vendor, deviceid,
                              bus, device, function, 1, interrupt);
}

static errval_t create_e10k_queue(char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                  bool poll, struct devq **retqueue)
{
    errval_t err;
    err = e10k_queue_create((struct e10k_queue**)retqueue, interrupt,
                            false /*virtual functions*/,
                            poll, /* user interrupts*/
                            false);
    *queueid = e10k_queue_get_id((struct e10k_queue*)*retqueue);
    return err;
}

static errval_t create_sfn5122f_queue(char* cardname, inthandler_t interrupt, uint64_t *queueid,
                                      bool poll, struct devq **retqueue)
{
    errval_t err;
    err = sfn5122f_queue_create((struct sfn5122f_queue**)retqueue, interrupt,
                                false /*userlevel network feature*/,
                                poll /* user interrupts*/,
                                false);
    *queueid = sfn5122f_queue_get_id((struct sfn5122f_queue*)*retqueue);
    return err;
}


typedef errval_t (*queue_create_fn)(char*, inthandler_t, uint64_t*, bool, struct devq **);
struct networking_card
{
    char *cardname;
    queue_create_fn createfn;
} networking_cards [] = {
    { "e1000", create_e1000_queue},
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
 * @param poll      Is the queue polled or are interrupts used
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_queue_create(inthandler_t interrupt, const char *cardname,
                          uint64_t* queueid, bool poll, struct devq **retqueue)
{
    struct networking_card *nc = networking_cards;
    while(nc->cardname != NULL) {
        if (strncmp(cardname, nc->cardname, strlen(nc->cardname)) == 0) {
            return nc->createfn(nc->cardname, interrupt, queueid, poll, retqueue);
        }
        nc++;
    }

    debug_printf("net: ERROR unknown queue. card='%s', queueid=%" PRIu64 "\n",
                  cardname, *queueid);

    return -1;
}
