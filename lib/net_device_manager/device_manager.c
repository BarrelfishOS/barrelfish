/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
//#include <barrelfish/nameservice_client.h>
//#include <barrelfish/net_constants.h>
#include <stdio.h>
#include <string.h>
#include <net_device_manager/net_ports_service.h>
#include <net_device_manager/net_device_manager.h>

#include "device_manager_debug.h"


/****************************************************************
* Global datastructure
*****************************************************************/

/*****************************************************************
* Prototypes
*****************************************************************/

// initializes the hardware independent part of device manager
int init_device_manager(void)
{
    NDM_DEBUG("init_device_manager called\n");

    return 0;
} // end function: init_ports_service

