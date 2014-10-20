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

#include "dma_mgr.h"
#include "debug.h"

#define BUFFER_SIZE (1<<22)


int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("DMA Manager started\n");

    err = driver_store_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the driver store\n");
    }

    err = dma_mgr_svc_start();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the service\n");
    }

    while(1) {
        messages_wait_and_handle_next();
    }

    debug_printf("DMA Manager terminated\n");

    return 0;
}

