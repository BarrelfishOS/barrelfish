/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <net_device_manager/net_ports_service.h>
#include <barrelfish/nameservice_client.h>
#include "E1K_mng_debug.h"

/*
#include <barrelfish/net_constants.h>
#include <barrelfish/dispatch.h>
*/


int main(int argc, char **argv)
{
    E1KDM_DEBUG("Started the e1k_dev_manager\n");
    init_ports_service();
    E1KDM_DEBUG("done with most of things\n");
    // network_service_loop
} // end function: main


