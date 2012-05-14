/** \file
 *  \brief idc ARP client
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef _ARP_LOOKUP_CLIENT_H_
#define _ARP_LOOKUP_CLIENT_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "netg.h"
#include "netg_common.h"


void idc_connect_ARP_lookup_service(struct netg_interface* netif, char *service_name);
errval_t idc_get_ip_from_ARP_lookup(struct netg_interface* netif, ip_addr_t *ip,
															ip_addr_t *gw,
															ip_addr_t *nm);
uint64_t idc_ARP_lookup(struct netg_interface* netif, uint32_t ip);

#endif
