/**
 * \file
 * \brief Header file for the interfaceing part to the network driver
 *
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IDC_NET_CONTROL_H_
#define IDC_NET_CONTROL_H_

#include "netg.h"
#include <if/net_queue_manager_defs.h>
#include "netg_common.h"

void idc_connect_port_manager_service(struct netg_interface* netif, char *service_name);

errval_t idc_tcp_new_port(struct netg_interface* netif, uint16_t * port_no, uint64_t queue_id);
errval_t idc_udp_new_port(struct netg_interface* netif, uint16_t * port_no, uint64_t queue_id);
errval_t idc_close_udp_port(struct netg_interface* netif, uint16_t port, uint64_t queue_id);
errval_t idc_close_tcp_port(struct netg_interface* netif, uint16_t port, uint64_t queue_id);
errval_t idc_bind_udp_port(struct netg_interface* netif, uint16_t port, uint64_t queue_id);
errval_t idc_bind_tcp_port(struct netg_interface* netif, uint16_t port, uint64_t queue_id);


#endif
