/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/ump_chan.h>
#include <bench/bench.h>
#include <barrelfish/sys_debug.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>

#include <dma/xeon_phi/xeon_phi_dma.h>
#include <dma/dma_request.h>
#include <dma/client/dma_client_device.h>
#include <dma/dma_manager_client.h>

#include "benchmark.h"


#define HLINE debug_printf("========================================================\n");
#define hline debug_printf("--------------------------------------------------------\n");
#define PRINTF(x...) debug_printf("[HW Models] " x)
#define TODO(x...) debug_printf("[HW Models] TODO: " x)

int main(int argc,  char **argv)
{
    //errval_t err;

    HLINE
    PRINTF("Offload Scenario started.\n");
    HLINE


    PRINTF("Allocating memory for data processing\n");
    TODO("allocate RAM reachable by self, KNC-DMA [, Network]\n");

    hline

    PRINTF("Populating memory region with data\n");
    TODO("do some stuff\n");

    hline

    PRINTF("Allocating memory for message passing\n");
    TODO("allocate RAM reachable by self, KNC\n");

    hline

    PRINTF("Allocating memory on the co-processor\n");
    TODO("allocate RAM reachable by co-processor and KNC-DMA\n");

    hline

    PRINTF("Perform DMA from system RAM to co-processor GDDR\n");
    TODO("ask the DMA driver to do the copy\n");

    hline

    PRINTF("Spawning process on co-processor with 2x mem");
    TODO("load elf...\n");

    hline

    PRINTF("Wait for co-processor to finish\n");
    TODO("wait for message\n");

    hline

    PRINTF("Perform DMA from co-processor GDDR to system RAM\n");
    TODO("ask the DMA driver to do the copy\n");
}

