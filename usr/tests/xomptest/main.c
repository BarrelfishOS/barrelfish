/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>
#include <omp.h>
#include <xomp/xomp.h>

#include <flounder/flounder_support_ump.h>

#include "xomptest.h"

void do_process(uint32_t *src,
                uint32_t *dst)
{

    if (src == NULL || dst == 0) {
        return;
    }
#pragma omp parallel for
    for (int j = 0; j < IT; j++) {
        for (int i = 0; i < MAX; i += IT) {
            dst[i + j] = src[i + j];
        }
    }
}

int main(int argc,
         char *argv[])
{
    errval_t err;

    xomp_wid_t wid;
    err = xomp_worker_parse_cmdline(argc, argv, &wid);
    switch (err_no(err)) {
        case SYS_ERR_OK:
            debug_printf("XOMP Test started. (WORKER) %u\n", argc);
            err = xomp_worker_init(wid);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not initialize the worker\n");
            }
            break;
        case XOMP_ERR_BAD_INVOCATION:
            debug_printf("XOMP Test started. (MASTER) %u\n", argc);
            err = start_master(argc, argv);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "start_master");
            }
            break;
        default:
            break;
    }

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "during initialization");
    }

    while (1) {
        messages_wait_and_handle_next();
    }

    return 0;
}

