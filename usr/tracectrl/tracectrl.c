/**
 * \file
 * \brief tracectrl application
 *
 * Starts and stops the tracing.
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
#include <trace/trace.h>

static volatile bool finished;

static void after_prepare(void *arg)
{
    finished = true;
}

static errval_t init_tracing(void)
{
    printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
    trace_reset_all();
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
    printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
    finished = true;
}

static void trace_setup(void)
{
    finished = false;

    errval_t err = init_tracing();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "initialising tracing");
    }

    // Make sure all subsystems get logged.
    trace_set_all_subsys_enabled(true);
    // Prepare the tracing framework. This is optional.
    trace_prepare(MKCLOSURE(after_prepare, NULL));

    while(!finished) {
        // Make sure this program is not exited before everything
        // is completed.
        event_dispatch_non_block(get_default_waitset());
        thread_yield_dispatcher(NULL_CAP);
    }
}

static void trace_flush_sync(void) {

    finished = false;
    trace_flush(MKCLOSURE(callback, NULL));
    while(!finished) {
        // Make sure this program is not exited before everything
        // is completed.
        event_dispatch_non_block(get_default_waitset());
        thread_yield_dispatcher(NULL_CAP);
    }
}

int main(int argc, char **argv)
{

    if (argc < 2) {
        printf("Usage: %s start|stop\n", argv[0]);
        return 1;
    }

    if (strcmp(argv[1], "start") == 0) {
        trace_setup();
        start_tracing();
    } else if (strcmp(argv[1], "stop") == 0) {
        stop_tracing();
        trace_flush_sync();
    } else {
        printf("%s:%s:%d: Unknown command: %s\n",
               __FILE__, __FUNCTION__, __LINE__, argv[1]);
    }

    return 0;
}