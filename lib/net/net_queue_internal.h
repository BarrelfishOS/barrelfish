/**
 * @brief
 *  net_queue_internal.h
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef LIB_NETQUEUE_INTERNAL_INCLUDE_NETWORKING_H_
#define LIB_NETQUEUE_INTERNAL_INCLUDE_NETWORKING_H_

#include <errors/errno.h>

struct devq;

typedef void (*inthandler_t)(void*);

/**
 * @brief creates a queue to the given card
 *
 * @param interrupt interrupt handler 
 * @param cardname  network card to create the queue for
 * @param ep        endpoint to NIC driver, possibly NULL
 * @param queueid   queueid of the network card
 * @param default_q Request access to default queue (only used by driver)
 * @param poll      Is the queue polled or are interrupts used
 * @param filter_ep returns the endpoint to the netfilter interface of this queue
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_queue_internal_create(inthandler_t interrupt, const char *cardname,
                                   struct capref* ep, uint64_t* queueid, bool default_q, 
                                   bool poll, struct capref* filter_ep, struct devq **retqueue);
#endif /* LIB_NETQUEUE_INTERNAL_INCLUDE_NETWORKING_H_ */
