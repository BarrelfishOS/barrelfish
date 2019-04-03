/** \file
 *  \brief Memory server benchmark application, testing alloc and free
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Spawn benchmark on given # of cores. 
 * Request set amount of memory on each core.
 * This benchmark program waits on barriers to ensure that the mem server is 
 * running and to synchronise the starting and stopping of the benchmarks on 
 * different dispatchers.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

#include "memtest_trace.h"

#include "sleep.h"

#define MAX_REQUESTS 10
#define MAX_REQUESTS_0 10
#define MEM_BITS 20

#define MINSIZEBITS 9 // from mem_serv.c

static void run_benchmark(coreid_t core, int requests)
{
    errval_t err;
    struct capref ramcap;

    
    int i = -1;
    int bits = MEM_BITS;

    debug_printf("starting benchmark. allocating mem of size: %d\n", bits);
    //debug_printf("starting benchmark. allocating mem of size: %d to %d\n", 
    //             MINSIZEBITS, MINSIZEBITS+requests-1);


    sleep_init();

    // NOTE: because this is allocating and freeing, it should never stop.
    // it should also never have to steal

    do {
        i++;
        // bits =  MINSIZEBITS+i;
        trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_ALLOC, i);
        err = ram_alloc(&ramcap, bits);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "ram_alloc failed");
        }
        milli_sleep(1);
        err = ram_free(ramcap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "ram_free failed");
        }
        if ((i % 500 == 0) && (i > 0)) {
            debug_printf("allocated %d caps\n", i);
        }
    } while (err_is_ok(err)); // && (i < requests));

    debug_printf("done benchmark. allocated %d caps (%lu bytes)\n", 
                 i, i * (1UL << bits));

}



static int run_worker(coreid_t mycore)
{
    errval_t err;

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_WAIT, 0);

    err = ns_barrier_worker((int)mycore, "mem_bench_ready");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "barrier_worker failed");
    }

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_RUN, 0);
    
    run_benchmark(mycore, MAX_REQUESTS);

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_WAIT, 0);

    err = ns_barrier_worker((int)mycore, "mem_bench_finished");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "barrier_worker failed");
    }

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_DONE, 0);
    
    return EXIT_SUCCESS;
}

static int run_master(coreid_t mycore, int argc, char *argv[])
{
    //    assert(mycore == 0);

    errval_t err;
    int num_spawn = strtol(argv[1], NULL, 10);
    int first_core = mycore + 1;

    debug_printf("spawning on %d cores\n", num_spawn);

    err = init_tracing();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "initialising tracing");
        return EXIT_FAILURE;
    }
    prepare_dump();

    // start_tracing();

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_STARTED, 0);

    // spawn some dispatchers

    char *path = argv[0];      // reuse argv and path
    argv[1] = NULL;

    for (int i = first_core; i <= num_spawn; i++) {
        err = spawn_program(i, path, argv, NULL, 0, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "spawning on core %d", i);
        } else {
            //debug_printf("dispatcher %d on core %d spawned\n", i, i);
        }

    }

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_WAIT, 0);

    //    debug_printf("waiting for all spawns to start\n");
    err = ns_barrier_master(first_core, num_spawn, "mem_bench_ready");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed barrier_master");
        return EXIT_FAILURE;
    }

    start_tracing();

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_RUN, 0);

    //    run_benchmark(mycore, MAX_REQUESTS_0);

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_WAIT, 0);

    //    debug_printf("waiting for all spawns to complete\n");
    err = ns_barrier_master(first_core, num_spawn, "mem_bench_finished");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed barrier_master");
        return EXIT_FAILURE;
    }

    trace_event(TRACE_SUBSYS_MEMTEST, TRACE_EVENT_MEMTEST_DONE, 0);

    debug_printf("all benchmarks completed\n");

    stop_tracing();
    // dump_trace();

    return EXIT_SUCCESS;
}


int main(int argc, char *argv[]) 
{
    coreid_t mycore = disp_get_core_id();

    debug_printf("This is mem_bench_5\n");

    if (argc < 2) {
        return run_worker(mycore);
    } else {
        return run_master(mycore, argc, argv);
    }
}
