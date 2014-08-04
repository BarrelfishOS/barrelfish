/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <omp.h>
#include <xomp/xomp.h>

#include <flounder/flounder_support_ump.h>

#include "xomptest.h"

#define NTHREADS 3
#define STACKSIZE 0

#define NUM_WORKERS 2
#define WORK_SIZE   (4 * BASE_PAGE_SIZE)

static uint32_t *arr = NULL;

#ifndef __k1om__

static errval_t initialize_master(int argc,
                char *argv[])
{
    errval_t err;
    debug_printf("Initializing Master\n");

    struct capref frame;
    err = frame_alloc(&frame, WORK_SIZE, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    void *addr;
    err = vspace_map_one_frame(&addr, WORK_SIZE, frame, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    err = xomp_master_add_memory(frame, (lvaddr_t) addr, XOMP_FRAME_TYPE_SHARED_RW);
    if (err_is_fail(err)) {
        return err;
    }

    uint32_t *data = addr;
    *data = (WORK_SIZE / sizeof(uint32_t)) - 2;
    data += 2;
    for (uint32_t i = 0; i < (WORK_SIZE / sizeof(uint32_t)) - 2; ++i) {
        data[i] = i;
    }

    arr = data;

    return SYS_ERR_OK;
#if 0
    debug_printf("=========================================================\n");
    debug_printf("Distributing work:\n");

    err = xomp_master_process_array((lvaddr_t) do_process, (lvaddr_t) addr, NUM_WORKERS);
    if (err_is_fail(err)) {
        return err;
    }
    debug_printf("=========================================================\n");
    debug_printf("Verification:\n");

    for (uint32_t i = 0; i < (WORK_SIZE / sizeof(uint32_t)) - 2; ++i) {
        if (data[i] != i + 1) {
            USER_PANIC("test failed: data[%u]=%u, expected %u\n", i, data[i], i + 1);
        }
    }
    debug_printf("=========================================================\n");
    debug_printf("SUCCESS!:\n");
    debug_printf("=========================================================\n");

    return SYS_ERR_OK;
#endif
}
#endif

static void process_array(uint32_t *a)
{
    if (a == NULL) {
        return;
    }
#pragma omp parallel for
    for (uint32_t i = 0; i < (WORK_SIZE / sizeof(uint32_t)) - 2; ++i) {
        a[i]++;
    }
}

#ifdef __k1om__
int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("XOMP worker started.\n");

    xomp_wid_t wid;
    err = xomp_worker_parse_cmdline(argc, argv, &wid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not parse the command line");
    }

    err = xomp_worker_init(wid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the worker\n");
    }

    process_array(arr);

    while (1) {
        messages_wait_and_handle_next();
    }

}
#else
int main(int argc,
                char *argv[])
{
    errval_t err;

    debug_printf("XOMP Test started. (MASTER) %u\n", argc);

    struct xomp_master_args args = {
        .num_phi = 2,
        .path = "/k1om/sbin/xomp_test",
        .argc = argc,
        .argv = argv
    };

    bomp_custom_init(&args);

    backend_span_domain(NTHREADS, 0);

    omp_set_num_threads(NTHREADS);

    err = initialize_master(argc, argv);

    debug_printf("#elements: %lu", (WORK_SIZE / sizeof(uint32_t)) - 2);

    debug_printf("omp parallel start>>> \n");
    debug_printf("=========================================================\n");

    process_array(arr);

    debug_printf("=========================================================\n");
    debug_printf("<<< omp parallel end\n");

    debug_printf("Verifying");

    for (uint32_t i = 0; i < (WORK_SIZE / sizeof(uint32_t)) - 2; ++i) {
        if (arr[i] != i + 1) {
            USER_PANIC("test failed: data[%u]=%u, expected %u\n", i, arr[i], i + 1);
        }
    }

    debug_printf("SUCCESSS!!!!!\n");
    while (1) {
        messages_wait_and_handle_next();
    }

    return 0;
}
#endif
