/**
 * \file
 * \brief LWIP test/demo code
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <netif/etharp.h>
#include <lwip/init.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>


#include "netd_debug.h"
#include "idc_barrelfish.h"
#include "netd.h"


void startlwip(char *card_name)
{

    NETD_DEBUG("NETD is taking control of the LWIP for card[%s]\n", card_name);
    perform_ownership_housekeeping(alloc_tcp_port, alloc_udp_port,
                                   alloc_specific_port, free_port);

    /* take ownership of lwip */
    owner_lwip_init(card_name);
}
