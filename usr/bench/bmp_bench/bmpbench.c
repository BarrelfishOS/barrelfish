/** \file
 *  \brief IDC system test code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN /* for strdup() */
#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <bench/bench.h>
#ifdef __BEEHIVE__
// Why is this depended in other cases??
#include <dcache.h>
#endif
#include <if/test_defs.h>
#include <if/ping_pong_defs.h>

#include <trace/trace.h>
#include "bmpbench.h"

/* ------------------------------ MAIN ------------------------------ */

#ifndef __BEEHIVE__
STATIC_ASSERT(!"BEEHIVE assumptions here");
#endif

static void *icacheaddr;

cycles_t icachetest_run(int count)
{
    void (*x)(void) = (void(*)(void)) (((uint32_t)icacheaddr) >> 2);
    cycles_t t1 = bench_tsc();
    while(count--) {
	x();
    }
    cycles_t t2 = bench_tsc();
    return t2 - t1;
}
    
void icachetest_setup(void)
{
    const uint32_t jplus8 = 0xf800220c;
    const uint32_t jlink = 0xf000030c;

    uint32_t *ptr = (uint32_t*)malloc(8192);
    for(int i=0; i<2040; i+=8)
	ptr[i] = jplus8;
    ptr[2040] = jlink;

    bee_dcache_flush_all();
    icacheaddr = ptr;
}


int main(int argc, char *argv[])
{
    errval_t err;

    icachetest_setup();

    err = trace_control(
	TRACE_EVENT(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 1),
	TRACE_EVENT(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0),
	100 * 1000000ULL * 60);
    if (err_is_fail(err)) {
	DEBUG_ERR(err, "trace_control");
	return EXIT_FAILURE;
    }

    // "boot" time start domains
    if (argc >= 2 && strcmp(argv[1], "boot") == 0) {
	argv[1] = argv[0];
	argv++;
	argc--;
    }

    if (argc == 2 && strcmp(argv[1], "client") == 0) {
        bb_pingpong_start_client();
    } else if (argc == 2 && strcmp(argv[1], "server") == 0) {
        bb_pingpong_start_server();
    } else {
        printf("Usage: %s client|server\n", argv[0]);
        return EXIT_FAILURE;
    }

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    return EXIT_FAILURE;
}
