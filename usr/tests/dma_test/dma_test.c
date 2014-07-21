/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 *
 *
 */
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <dma/dma.h>
#include <dma/dma_client.h>



int main(int argc,
         char *argv[])
{

    debug_printf("DMA Test domain started\n");

    dma_client_get_connection_by_addr(0x100000, 0x200000, 0x100000);

    while(1) {
        messages_wait_and_handle_next();
    }

    debug_printf("I/O AT DMA driver terminated\n");

    return 0;
}

