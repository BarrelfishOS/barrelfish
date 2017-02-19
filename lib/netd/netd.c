/**
 * \file
 * \brief Echo server main
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define NETD_SERVICE_DEBUG

#include <barrelfish/barrelfish.h>
#include <barrelfish/net_constants.h>

// For event loops
#include <barrelfish/dispatch.h>

// standard include files
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <netd/netd.h>
#include <netd/netd_debug.h>
#include "netd_private.h"

static char * duplicate_string(const char *string)
{
    if (!string)
        return NULL;
    char *new_string = strdup(string);
    assert(new_string);
    return new_string;
}

/****************************************************************************
 * netd initialization function
 ***************************************************************************/
 
errval_t netd_init(struct netd_state **state, char *card_name, uint64_t queueid,
    bool do_dhcp, char *ip_addr_str, char *netmask_str, char *gateway_str)
{
    NETD_DEBUG("###################################################\n");
    NETD_DEBUG("Initialising netd library\n");
    
    *state = malloc(sizeof(struct netd_state));
    (*state)->do_dhcp = do_dhcp;
    (*state)->dhcp_completed = false;
    
    (*state)->ip_addr_str = duplicate_string(ip_addr_str);
    (*state)->netmask_str = duplicate_string(netmask_str);
    (*state)->gateway_str = duplicate_string(gateway_str);
    
    NETD_DEBUG("running on core %d\n", disp_get_core_id());
    NETD_DEBUG("###################################################\n");


    NETD_DEBUG("card name = %s\n", card_name);
    if (!do_dhcp)
        NETD_DEBUG("using static IP address\n");

    assert(card_name);

    if (!do_dhcp) {
        // Making sure that we have enough info for static configuration
        if ((ip_addr_str == NULL) || (netmask_str == NULL)
                || (gateway_str == NULL)) {
            USER_PANIC("Error, not enough information provided for static "
                    "IP configuration IP[%s], NM[%s], GW[%s]",
                    ip_addr_str, netmask_str, gateway_str);
            return 1;
        }
    }
    // FIXME: This has to be done for every card
    // Connect to the driver for given card
    NETD_DEBUG("trying to connect to the %s:%"PRIu64" driver...\n",
            card_name, queueid);
    startlwip(*state, card_name, queueid);

    NETD_DEBUG("registering net_ARP service\n");
    // register ARP service
    init_ARP_lookup_service(*state, card_name);

    return SYS_ERR_OK;
}
