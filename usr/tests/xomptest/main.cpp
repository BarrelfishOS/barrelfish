/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
extern "C" {
    #include <barrelfish_kpi/types.h>
    #include <errors/errno.h>
    #include <xomp/xomp.h>
    #include "xomptest.h"
}

#include <iostream>

#include <omp.h>

using namespace std;

void do_process(uint32_t *src,
                uint32_t *dst)
{
    cout << "do_process C++ called " << endl;
    if (src == NULL || dst == 0) {
        return;
    }
#pragma omp parallel for
    for (unsigned int j = 0; j < IT; j++) {
        for (unsigned int i = 0; i < MAX; i += IT) {
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
            std::cout << "XOMP Test started. (WORKER) #args:" << argc << std::endl;
            err = xomp_worker_init(wid);
            if (err_is_fail(err)) {
                std::cout <<  "could not initialize the worker: " << err_getstring(err) << std::endl;
            }
            break;
        case XOMP_ERR_BAD_INVOCATION:
            std::cout << "XOMP Test started. (MASTER) #args:" << argc << std::endl;
            err = start_master(argc, argv);
            if (err_is_fail(err)) {
                std::cout <<  "could not start the master: " << err_getstring(err) << std::endl;
            }
            break;
        default:
            break;
    }

    if (err_is_fail(err)) {
        std::cout <<  "error while initializing" << err_getstring(err) << std::endl;
    }

    handle_messages();

    return 0;
}

