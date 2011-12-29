/**
 * \file
 * \brief System-wide tracing
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <trace/trace.h>
#include <inttypes.h>
#include <stdio.h>

/**
 * \brief Reset the trace buffer on the current core.
 *
 * Reset head and tail pointers.
 */
void trace_reset_buffer(void)
{
    uintptr_t i, new;

    struct trace_buffer *buf = (struct trace_buffer *)trace_buffer_va;

    //buf->master = (struct trace_buffer *)trace_buffer_master;
    do {
        i = buf->head_index;
        new = 0;
    } while (!trace_cas(&buf->head_index, i, new));
    buf->tail_index = 0;
}

/**
 * \brief Reset all trace buffers discarding the current trace
 *
 * Reset head and tail pointers.
 */
void trace_reset_all(void)
{
    for (coreid_t core = 0; core < TRACE_COREID_LIMIT; core++) {
        struct trace_buffer *tbuf = (struct trace_buffer *)compute_trace_buf_addr(core);
        tbuf->head_index = 0;
        tbuf->tail_index = 0;
	tbuf->done_rundown = false;
    }
}

/**
 * \brief Specify the trigger events which start and stop tracing
 * \param start_trigger - Raw event value which starts the trace
 * \param stop_trigger - Raw event value which stops the trace
 * \param duration - Maximum trace duration in cycles (0 is infinite)
 */
errval_t trace_control(uint64_t start_trigger,
                       uint64_t stop_trigger,
                       uint64_t duration)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    struct trace_buffer *buf = disp->trace_buf;

    if (buf == NULL) return TRACE_ERR_NO_BUFFER;

    struct trace_buffer *master = (struct trace_buffer*)trace_buffer_master;
    //struct trace_buffer *master = buf->master;

    master->running = false;
    master->stop_trigger = stop_trigger;
    master->duration = duration;
    master->stop_time = 0xFFFFFFFFFFFFFFFFULL; // Updated on start trigger
    master->start_trigger = start_trigger;

    return SYS_ERR_OK;
}

/**
 * \brief Wait for a trace to complete
 */
errval_t trace_wait(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    struct trace_buffer *buf = disp->trace_buf;

    if (buf == NULL) return TRACE_ERR_NO_BUFFER;

    struct trace_buffer *master = (struct trace_buffer*)trace_buffer_master;
    //struct trace_buffer *master = buf->master;

    while (master->start_trigger != 0) thread_yield_dispatcher(NULL_CAP);
    while (master->stop_trigger != 0) thread_yield_dispatcher(NULL_CAP);

    return SYS_ERR_OK;
}

/**
 * \brief Dump the contents of the trace buffers
 *
 */
size_t trace_dump(char *buf, size_t buflen)
{
    if (buf == NULL) return TRACE_ERR_NO_BUFFER;

    struct trace_buffer *master = (struct trace_buffer*)trace_buffer_master;
    //struct trace_buffer *master = trace_buf->master;

    char *ptr = buf;
    size_t totlen = 0;
    size_t len;

    uint64_t t0 = master->t0;

    /* Ensure tracing is stopped */
    master->start_trigger = 0;
    master->stop_trigger = 0;
    master->running = false;

    len = snprintf(ptr, buflen-totlen,
                   "# Start %" PRIu64 " Duration %" PRIu64 " Stop %" PRIu64
                   "\n",
                   master->t0, master->duration, master->stop_time);
    ptr += len; totlen += len;

    for (coreid_t core = 0; core < TRACE_COREID_LIMIT; core++) {
        struct trace_buffer *tbuf = (struct trace_buffer *)compute_trace_buf_addr(core);
        if (tbuf->head_index == 0) continue;

        len = snprintf(ptr, buflen-totlen,
                       "# Core %d LOG DUMP ==================================================\n", core);
        ptr += len; totlen += len;


        for (int e = 0; e < tbuf->head_index; e++) {

            if (tbuf->events[e].timestamp < t0) continue;

            if (tbuf->events[e].timestamp >> 63) {
                /* Top bit set means it's a DCB rundown event,
                   timestamp is the DCB pointer and event data is first 8 chars
                   of the domain name. */
                len = snprintf(ptr, buflen-totlen,
                               "# DCB %d %" PRIx64 " %.*s\n",
                               core, tbuf->events[e].timestamp,
                               8, (char*)&tbuf->events[e].u.raw);
            }
            else {
                len = snprintf(ptr, buflen-totlen,
                               "%d %" PRIu64 " %" PRIx64 "\n",
                               core, tbuf->events[e].timestamp - t0,
                               tbuf->events[e].u.raw);
            }
            ptr += len; totlen += len;
        }
    }

    return totlen;
}

