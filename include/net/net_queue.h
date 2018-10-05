/**
 * @brief
 *  net_queue.h
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef LIB_NETQUEUE_INCLUDE_NETWORKING_H_
#define LIB_NETQUEUE_INCLUDE_NETWORKING_H_

#include <errors/errno.h>

#define NETIF_TXFLAG (1UL << 31)
#define NETIF_TXFLAG_LAST (1UL << 30)
#define NETIF_TXFLAG_FIRST (1UL << 29)


#define NETIF_RXFLAG (1UL << 28)

struct devq;

typedef void (*inthandler_t)(void*);

/**
 * @brief creates a queue to the given card
 *
 * @param interrupt interrupt handler 
 * @param cardname  network card to create the queue for
 * @param ep        endpoint to NIC driver, possibly NULL
 * @param queueid   queueid of the network card
 * @param poll      Is the queue polled or are interrupts used
 * @param retqueue  returns endpoint to netfilter interface of this queue
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_queue_create(inthandler_t interrupt, const char *cardname, struct capref* ep,
                          uint64_t* queueid, bool poll, struct capref* filter_ep,
                          struct devq **retqueue);

struct bench_ctl* net_queue_get_bench_data(struct devq* q, const char* name, uint8_t type);
#endif /* LIB_NETQUEUE_INCLUDE_NETWORKING_H_ */
