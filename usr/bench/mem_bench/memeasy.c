/**
 * \file
 * \brief Simple Memory Benchmark to test kernel infrastructure
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <bench/bench.h>
#include <trace/trace.h>

#define MAX_ITERATION 1000

static volatile bool finished;

static void set_true(void* arg) {
    *(bool*)arg = true;
}

static void sleep_until(delayus_t delay) {
    struct deferred_event de;
    deferred_event_init(&de);

    bool can_continue = false;

    struct event_closure ec;
    ec.handler = set_true;
    ec.arg = &can_continue;

    errval_t err = deferred_event_register(&de, get_default_waitset(),
                                           delay, ec);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "deferred event register failed.");
    }

    //printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
    while(!can_continue) {
        messages_wait_and_handle_next();
    }
    //printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
}

static void after_prepare(void *arg)
{
    debug_printf("after_prepare starts");
    finished = true;
}

static errval_t init_tracing(void)
{
    trace_reset_all();
    debug_printf("after trace reset\n");

    // Tell the trace system when to start and stop.  We can also
    // provide an overriding maximum duration (in cycles) as the last parameter.
    return trace_control(TRACE_EVENT(TRACE_SUBSYS_BENCH,
                                    TRACE_EVENT_BENCH_START, 0),
                        TRACE_EVENT(TRACE_SUBSYS_BENCH,
                                    TRACE_EVENT_BENCH_STOP, 0),
                        0);
}

static void start_tracing(void)
{
    // start the trace going by providing the start event
    TRACE(BENCH, START, 0);
}

static void stop_tracing(void)
{
    // stop the trace by providing the stop event
    TRACE(BENCH, STOP, 0);
}

static void callback(void *arg)
{
    debug_printf("callback invoked\n");

    finished = true;
}

static void dump_trace(void)
{
    // dump the trace on the output.  We can copy and paste it
    // to use in Aquarium.

    debug_printf("the trace dump\n");

    // Let the trace framework decide where to flush to
    trace_flush(MKCLOSURE(callback, NULL));

    debug_printf("finished trace dump\n");

}


int main(int argc, char** argv)
{
    uint64_t sleep = 0;
    uint64_t ram_bits = 20;
    errval_t err;

    printf("%s:%s:%d: argc = %d\n", __FILE__, __FUNCTION__, __LINE__, argc);
    if (argc > 3) {
        printf("%s:%s:%d: Usage: %s <ram bits> <sleep ms>\n",
               __FILE__, __FUNCTION__, __LINE__, argv[0]);
    } else if (argc == 3) {
        sleep = atoll(argv[2]);
        ram_bits = atoll(argv[1]);
    } else if (argc == 2) {
        ram_bits = atoll(argv[1]);
    }
    printf("%s:%s:%d: Use ram_bits = %"PRIu64"\n",
       __FILE__, __FUNCTION__, __LINE__, ram_bits);
    printf("%s:%s:%d: Use sleep = %"PRIu64"\n",
        __FILE__, __FUNCTION__, __LINE__, sleep);


    finished = false;

    err = init_tracing();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "initialising tracing");
        return EXIT_FAILURE;
    }

    // Make sure all subsystems get logged.
    trace_set_all_subsys_enabled(true);

    debug_printf("after init tracing\n");

    // Prepare the tracing framework. This is optional.
    trace_prepare(MKCLOSURE(after_prepare, NULL));

    while(!finished) {
        // Make sure this program is not exited before everything
        // is completed.
        event_dispatch_non_block(get_default_waitset());
        thread_yield_dispatcher(NULL_CAP);
    }


    bench_init();
    cycles_t runs[MAX_ITERATION];

    start_tracing();

    uint64_t start, end;

    struct capref ram;
    err = ram_alloc(&ram, ram_bits);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ram_alloc failed.");
    }

    struct capref frame;
    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "slot_alloc failed.");
    }

    for (size_t i=0; i<MAX_ITERATION; i++) {
        TRACE(BENCH, ROUND_START, 0);
        //printf("%s:%s:%d: i=%"PRIu64"\n",
        //       __FILE__, __FUNCTION__, __LINE__, i);
        start = bench_tsc();
        //printf("%s:%s:%d: \n", __FILE__, __FUNCTION__, __LINE__);
        err = cap_retype(frame, ram, ObjType_Frame, ram_bits);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_retype failed.");
        }
        end = bench_tsc();

        err = cap_delete(frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_delete failed.");
        }

        if (sleep > 0) {
            sleep_until(sleep);
        }
        TRACE(BENCH, ROUND_END, 0);

        runs[i] = end - start;
    }

    runs[0] = BENCH_IGNORE_WATERMARK;

    printf("Average cycles %"PRIuCYCLES", Variance %"PRIuCYCLES"\n" \
           "Average ms %"PRIu64" Variance ms %"PRIu64"\n",
            bench_avg(runs, MAX_ITERATION),
            bench_variance(runs, MAX_ITERATION),
            bench_tsc_to_ms(bench_avg(runs, MAX_ITERATION)),
            bench_tsc_to_ms(bench_variance(runs, MAX_ITERATION)));

    finished = false;
    stop_tracing();
    // flush the trace buffer
    dump_trace();

    while(!finished) {
        // Make sure this program is not exited before everything
        // is completed.
        event_dispatch_non_block(get_default_waitset());
        thread_yield_dispatcher(NULL_CAP);
    }


    return 0;
}
