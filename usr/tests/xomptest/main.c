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
#include <xomp/xomp.h>

#include <flounder/flounder_support_ump.h>

#include "xomptest.h"

static uint8_t is_master = 0x1;

xomp_wid_t worker_id = 0x0;


#define NUM_WORKERS 2
#define WORK_SIZE   (4 * BASE_PAGE_SIZE)


static errval_t initialize_master(int argc,
                                  char *argv[])
{
    errval_t err;
    debug_printf("Initializing Master\n");


#ifdef __k1om__
    err = xomp_master_init(1, "/k1om/sbin/xomp_test", argc, argv);
#else
    err = xomp_master_init(2, "/k1om/sbin/xomp_test", argc, argv);
#endif
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Master initialization\n");
    }
    err = xomp_master_spawn_workers(NUM_WORKERS);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Master spawning workers\n");
    }

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

    uint32_t *data = addr;
    *data = (WORK_SIZE / sizeof(uint32_t))-2;
    data += 2;
    for (uint32_t i = 0; i < (WORK_SIZE / sizeof(uint32_t))-2; ++i) {
        data[i] = i;
    }

    err = xomp_master_add_memory(frame, (lvaddr_t)addr, XOMP_FRAME_TYPE_SHARED_RW);
    if (err_is_fail(err)) {
        return err;
    }

    debug_printf("=========================================================\n");
    debug_printf("Distributing work:\n");

    err = xomp_master_do_work((lvaddr_t)do_process, (lvaddr_t)addr, NUM_WORKERS);
    if (err_is_fail(err)) {
        return err;
    }
    debug_printf("=========================================================\n");
    debug_printf("Verification:\n");

    for (uint32_t i = 0; i < (WORK_SIZE / sizeof(uint32_t))-2; ++i) {
        if(data[i] != i+1) {
            USER_PANIC("test failed: data[%u]=%u, expected %u\n", i, data[i], i+1);
        }
    }
    debug_printf("=========================================================\n");
    debug_printf("SUCCESS!:\n");
    debug_printf("=========================================================\n");

    return SYS_ERR_OK;
}

int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("XOMP Test started. %u\n", argc);

    for (uint32_t i = 0; i < argc; ++i) {
        debug_printf("argv[%u]=%s\n", i, argv[i]);
    }

    debug_printf("##### do_process() is located at %016lx\n", (lvaddr_t) do_process);

    debug_printf("parsing arguments\n");

    for (uint32_t i = 1; i < argc; ++i) {
        if (strcmp("-xompworker", argv[i]) == 0) {
            is_master = 0x0;
        } else {
            debug_printf("unknown parameter: %s\n", argv[i]);
        }
    }

    xomp_wid_t wid;
    err = xomp_worker_parse_cmdline(argc, argv, &wid);
    switch(err_no(err)) {
        case SYS_ERR_OK:
            assert(!is_master);
            debug_printf("Initializing Worker: %"PRIu64"\n", wid);

            err = xomp_worker_init(wid);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not initialize the worker\n");
            }
            break;
        default:
            assert(is_master);
            err = initialize_master(argc, argv);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "initializing master");
            }
            break;
    }

    while (1) {
        messages_wait_and_handle_next();
    }

    return 0;
}
