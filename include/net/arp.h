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


#ifndef LIB_NET_INCLUDE_NETWORKING_ARP_H_
#define LIB_NET_INCLUDE_NETWORKING_ARP_H_


errval_t arp_service_start(void);

errval_t arp_service_subscribe(void);

errval_t arp_service_get_mac(uint32_t ip, uint64_t* mac);

#endif /* LIB_NET_INCLUDE_NETWORKING_ARP_H_ */
