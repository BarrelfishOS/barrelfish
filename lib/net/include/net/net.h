/**
 * @brief 
 *  net.h
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef LIB_NET_INCLUDE_NETWORKING_H_
#define LIB_NET_INCLUDE_NETWORKING_H_

// forward declarations
struct devq;
struct eth_addr;

errval_t networking_init_default(void);

static inline errval_t networking_init_with_queue(void *q) {return SYS_ERR_OK;}

errval_t networking_poll(void);



errval_t networking_get_mac(struct devq *q, uint8_t *hwaddr, uint8_t hwaddrlen);



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
                                 struct devq **retqueue);

/**
 * @brief obtains the default setting for initializaion of the driver
 *
 * @param queue     returns the queue to be used
 * @param cardname  returns the card name to be used
 *
 * @return SYS_ERR_OK on success, SKB_ERR_* on failure
 */
errval_t networking_get_defaults(uint64_t *queue, char **cardname);


#endif /* LIB_NET_INCLUDE_NETWORKING_H_ */
