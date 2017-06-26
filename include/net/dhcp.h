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


#ifndef LIB_NET_INCLUDE_NETWORKING_DHCP_H_
#define LIB_NET_INCLUDE_NETWORKING_DHCP_H_

#include <net/net.h>
#include <lwip/ip_addr.h>

/**
 * @brief starts the dhcpd service on the interface
 *
 * @param flags flags to provide
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t dhcpd_start(net_flags_t flags);


/**
 * @brief stops the dhcpd service
 */
errval_t dhcpd_stop(void);


/* functions for querying the current settings */

/**
 * @brief queries the DHCPD settings of the machine
 *
 * @param flags flags to provide
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t dhcpd_query(net_flags_t flags);


/**
 * @brief returns the IP configuration
 *
 * @param ip    return the IP address
 * @param gw    returns the gateway
 * @param nm    returns the netmask
 *
 * @return
 */
errval_t dhcpd_get_ipconfig(struct in_addr *ip, struct in_addr *gw, struct in_addr *nm);

/**
 * @brief sets the IP configuration
 *
 * @param ip    the IP address
 * @param gw    the Gateway
 * @param nm    the Netmask
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t dhcpd_set_ipconfig(struct in_addr *ip, struct in_addr *gw, struct in_addr *nm);

#endif /* LIB_NET_INCLUDE_NETWORKING_DHCP_H_ */
