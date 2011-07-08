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

#include "echoserver.h"

void startlwip(char *card_name)
{
    lwip_init(card_name);

    printf("ECHOSERVER: starting TCP server on port 7\n");
    int r = tcp_echo_server_init();
    assert(r == 0);

    printf("ECHOSERVER: starting UDP server on port 7\n");
    r = udp_echo_server_init();
    assert(r == 0);
}
