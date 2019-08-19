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

#include <netinet/in.h>
#include <errors/errno.h>

// forward declarations
struct devq;
struct eth_addr;
struct capref;

/*
 * ==============================================================================
 * Networking Flags
 * ==============================================================================
 */

///< default flags to initialize the networking library
#define NET_FLAGS_DEFAULTS               (0)

///< use polling instead of interrupt driven messaging
#define NET_FLAGS_POLLING                (1 << 0)

///< enable DHCP functionality on this queue
#define NET_FLAGS_DO_DHCP                (1 << 1)

#define NET_FLAGS_BLOCKING_INIT          (1 << 2)

///< initalize with default queue
#define NET_FLAGS_DEFAULT_QUEUE          (1 << 3)

///< do not initalize the net filter
#define NET_FLAGS_NO_NET_FILTER          (1 << 4)

///< networking flags
typedef uint32_t net_flags_t;



/* network interfaces */

///< the default network interface
#define NET_DEFAULT_NIC NULL

/*
 * ==============================================================================
 * Library Initialization
 * ==============================================================================
 */

/**
 * @brief initializes the networking with the defaults
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t networking_init_default(void);


/**
 * @brief initializes the networking library using a certain nic
 *
 * @param nic       the nic to use with the networking library
 * @param flags     flags to use to initialize the networking library
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_init_with_nic(const char *nic, net_flags_t flags);

/**
 * @brief initializes the networking library
 *
 * @param nic       the nic to use with the networking library
 * @param ep        endpoint to the nic
 * @param flags     flags to use to initialize the networking library
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_init_with_ep(const char *nic, struct capref ep,
                                 net_flags_t flags);

/**
 * @brief initializes the networking library with a given device queue
 *
 * @param q         the device queue to initialize the networking on
 * @param flags     supplied initialization flags
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_init_with_queue(struct devq *q, net_flags_t flags);

/**
 * @brief polls the network for new packets
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_poll(void);




/**
 * @brief creates a queue to the given card and the queueid
 *
 * @param cardname  network card to create the queue for
 * @param queueid   queueid of the network card
 * @param filter_ep returns and endpoint to the net filter interface of this queue
 * @param retqueue  returns the pointer to the queue
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t networking_create_queue(const char *cardname, struct capref* ep,
                                 uint64_t* queueid, struct capref* filter_ep,
                                 struct devq **retqueue);

/**
 * @brief obtains the default setting for initialization of the driver
 *
 * @param queue     returns the queue to be used
 * @param cardname  returns the card name to be used
 *
 * @return SYS_ERR_OK on success, SKB_ERR_* on failure
 */
errval_t networking_get_defaults(uint64_t *queue, const char **cardname, uint32_t *flags);

/*
 * ==============================================================================
 * Installing filters
 * ==============================================================================
 */

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
                                      uint16_t src_port, uint16_t dst_port);

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
                                     uint16_t src_port, uint16_t dst_port);


/**
 * @brief Trigger a poll of the loopback interface
 */
void net_if_trigger_loopback(void);

/**
 * @brief Process LWIP timeouts
 */
void net_lwip_timeout(void);

#endif /* LIB_NET_INCLUDE_NETWORKING_H_ */
