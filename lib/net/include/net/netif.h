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


#ifndef LIB_NET_INCLUDE_NET_NETIF_H_
#define LIB_NET_INCLUDE_NET_NETIF_H_

// forward declarations
struct netif;
struct devq;


/*
 * ===============================================================================
 * Network Interface Management
 * ===============================================================================
 */


/**
 * @brief initializes a netif structure for LWIP with a device queue
 *
 * @param netif     the netif to be initialized
 * @param devq      the device queue to be used
 *
 * @return SYS_ERR_OK on success, errva on failure
 */
errval_t net_if_init_devq(struct netif *netif, struct devq *devq);


/**
 * @brief adds the netif to the LWIP
 *
 * @param netif  the netif ot be added
 * @param state  state to be passed
 *
 * @return
 */
errval_t net_if_add(struct netif *netif, void *state);


/**
 * @brief removes a network interface
 *
 * @param netif     the LWIP netif
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_remove(struct netif *netif);

/*
 * ===============================================================================
 * Buffer Management
 * ===============================================================================
 */


/**
 * @brief adds a new receive buffer to the interface
 *
 * @param netif     the LWIP netif
 * @param pbuf      packet buffer to be added
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_add_rx_buf(struct netif *netif, struct pbuf *pbuf);

/**
 * @brief adds a new transmit buffer to the interface
 *
 * @param netif     the LWIP netif
 * @param pbuf      packt boffer to be transmitted
 *
 * @return  SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_add_tx_buf(struct netif *netif, struct pbuf *pbuf);


/*
 * ===============================================================================
 * Polling the interfaces
 * ===============================================================================
 */


/**
 * @brief polls then network interface for new incoming packets
 *
 * @param netif     the LWIP netif to be polled
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_poll(struct netif *netif);

/**
 * @brief polls all added network interfaces
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_if_poll_all(void);

#endif /* LIB_NET_INCLUDE_NET_NETIF_H_ */
