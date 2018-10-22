/**
 * \file
 * \brief Tracing helpers for benchmarks
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>
#include <barrelfish/barrelfish.h>
#include <if/bench_distops_defs.h>

#include <trace/trace.h>

#include <barrelfish/nameservice_client.h>

#include "benchapi.h"

//{{{1 Tracing init
#ifdef CONFIG_TRACE
static void after_prepare(void *arg)
{
    bool *done = arg;
    *done = true;
}

errval_t mgmt_init_tracing(void)
{
    errval_t err;
    // set NUM_COPIES_{START,END} for tracing
    bench_enable_tracing();
    // Initialize tracing
    printf("# mgmt node: initializing tracing\n");
    trace_reset_all();
    trace_set_autoflush(true);
    // Trace everything
    trace_set_all_subsys_enabled(false);
    trace_set_subsys_enabled(TRACE_SUBSYS_CAPOPS, true);
    trace_set_subsys_enabled(TRACE_SUBSYS_KERNEL_CAPOPS, true);
    //trace_set_subsys_enabled(TRACE_SUBSYS_MDB, true);
    err = trace_control(TRACE_EVENT(TRACE_SUBSYS_CAPOPS,
                                    TRACE_EVENT_CAPOPS_START, 0),
                        TRACE_EVENT(TRACE_SUBSYS_CAPOPS,
                                    TRACE_EVENT_CAPOPS_STOP, 0),
                        0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "unable to enable capops tracing");
        return err;
    }
    bool trace_prepare_done = false;
    trace_prepare(MKCLOSURE(after_prepare, &trace_prepare_done));

    while (!trace_prepare_done) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "event_dispatch while waiting for trace_prepare");
        }
    }

    printf("# mgmt node: waiting for bfscope\n");
    iref_t bfscope;
    return nameservice_blocking_lookup("bfscope", &bfscope);
}

void mgmt_trace_flush(struct event_closure cont)
{
    trace_flush(cont);
}
#else
errval_t mgmt_init_tracing(void)
{
    return SYS_ERR_OK;
}
void mgmt_trace_flush(struct event_closure cont)
{
    return;
}
#endif
