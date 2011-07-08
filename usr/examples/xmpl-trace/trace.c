/** \file
 *  \brief Tracing example application
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <trace/trace.h>

/*
 * Trace identifiers.  Typically these are defined in trace/trace.h
 * have a look in there to see which ones are used already.
 */
#define TRACE_SUBSYS_XMPL 		0x7337
#define TRACE_EVENT_XMPL_START          0x0001
#define TRACE_EVENT_XMPL_STOP           0x0002
#define TRACE_EVENT_XMPL_EV1           	0x0003
#define TRACE_EVENT_XMPL_EV2           	0x0004

static errval_t init_tracing(void)
{
    trace_reset_all();

    // Tell the trace system when to start and stop.  We can also 
    // provide an overriding maximum duration (in cycles) as the last parameter.
    return trace_control(TRACE_EVENT(TRACE_SUBSYS_XMPL,
                                    TRACE_EVENT_XMPL_START, 0),
                        TRACE_EVENT(TRACE_SUBSYS_XMPL,
                                    TRACE_EVENT_XMPL_STOP, 0), 
                        0);
}

static void start_tracing(void)
{
    // start the trace going by providing the start event
    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_START, 0);
}

static void stop_tracing(void)
{
    // stop the trace by providing the stop event
    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_STOP, 0);
}

static void dump_trace(void)
{
    // dump the trace on the output.  We can copy and paste it
    // to use in Aquarium.

    debug_printf("the trace dump\n");

    char *buf = malloc(4096*4096);
    trace_dump(buf, 4096*4096);
    printf("%s\n", buf);

    debug_printf("finished trace dump\n");
}

static void do_stuff(void)
{
    // generate our own traces

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, 0);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV2, 1);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, 2);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV2, 3);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, 10);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV2, 11);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV1, 12);

    trace_event(TRACE_SUBSYS_XMPL, TRACE_EVENT_XMPL_EV2, 13);
}

int main(int argc, char *argv[]) 
{
    errval_t err;

    debug_printf("starting\n");

    err = init_tracing();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "initialising tracing");
        return EXIT_FAILURE;
    }
    
    start_tracing();
    
    debug_printf("we are tracing now\n");

    // do stuff to generate traces
    do_stuff();

    stop_tracing();

    dump_trace();
    
    return EXIT_SUCCESS;
}
