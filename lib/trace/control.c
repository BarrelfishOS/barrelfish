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
#include <barrelfish/nameservice_client.h>
#include <barrelfish/event_queue.h>
#include <flounder/flounder.h>
#include <if/monitor_defs.h>
#include <trace/trace.h>
#include <inttypes.h>
#include <stdio.h>

#define CONSOLE_DUMP_BUFLEN (2<<20)

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
        new = 1;
    } while (!trace_cas(&buf->head_index, i, new));
    buf->tail_index = 0;

    buf->num_applications = 0;
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
        tbuf->head_index = 1;
        tbuf->tail_index = 0;
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
 * buf : The buffer to write the trace log into.
 * buflen : Length of buf.
 * number_of_events_dumped : (optional) Returns how many events have been
 * 	written into the buffer.
 *
 */
size_t trace_dump(char *buf, size_t buflen, int *number_of_events_dumped)
{
    if (buf == NULL) return TRACE_ERR_NO_BUFFER;

    struct trace_buffer *master = (struct trace_buffer*)trace_buffer_master;
    //struct trace_buffer *master = trace_buf->master;

    char *ptr = buf;
    size_t totlen = 0;
    size_t len;

    len = snprintf(ptr, buflen-totlen,
                   "# Start %" PRIu64 " Duration %" PRIu64 " Stop %" PRIu64
                   "\n",
                   master->t0, master->duration, master->stop_time);
    ptr += len; totlen += len;

    if (number_of_events_dumped != NULL) {
        *number_of_events_dumped = 0;
    }

    // Determine the minimum timestamp for which an event has been recorded.
    uint64_t min_timestamp = 0xFFFFFFFFFFFFFFFFULL;
    for (coreid_t core = 0; core < TRACE_COREID_LIMIT; core++) {
        struct trace_buffer *tbuf = (struct trace_buffer *)compute_trace_buf_addr(core);

        int num_events;
        if (tbuf->head_index > tbuf->tail_index) {
            num_events = tbuf->head_index - tbuf->tail_index - 1;
        } else {
            num_events = (TRACE_MAX_EVENTS - tbuf->tail_index) + tbuf->head_index - 1;
        }

        if (num_events == 0) {
            // Ringbuffer is empty.
            continue;
        }

        // Get the first event
        int idx = tbuf->tail_index + 1;
        if (idx == TRACE_MAX_EVENTS) {
            idx = 0;
        }

        uint64_t timestamp = tbuf->events[idx].timestamp;
        if (timestamp <= min_timestamp) {
            min_timestamp = timestamp;
        }

    }

    len = snprintf(ptr, buflen-totlen,
                                       "# Min_timestamp %" PRIu64 "\n",
                                       min_timestamp);
    ptr += len; totlen += len;

    // Create dumps for each core.
    for (coreid_t core = 0; core < TRACE_COREID_LIMIT; core++) {
        struct trace_buffer *tbuf = (struct trace_buffer *)compute_trace_buf_addr(core);

        int num_events;
        if (tbuf->head_index > tbuf->tail_index) {
            num_events = tbuf->head_index - tbuf->tail_index - 1;
            //printf("> , head %ld tail %ld num_events %d\n", tbuf->head_index, tbuf->tail_index, num_events);
        } else {
            num_events = (TRACE_MAX_EVENTS - tbuf->tail_index) + tbuf->head_index - 1;
            //printf("< , head %ld tail %ld num_events %d\n", tbuf->head_index, tbuf->tail_index, num_events);
        }

        if (num_events == 0) {
            // Ringbuffer is empty.
            continue;
        }

        len = snprintf(ptr, buflen-totlen,
                       "# Core %d LOG DUMP ==================================================\n", core);
        ptr += len; totlen += len;

        // Print the core time offset relative to core 0
        len = snprintf(ptr, buflen-totlen,
                                   "# Offset %d %" PRIi64 "\n",
                                   core, tbuf->t_offset);

        ptr += len; totlen += len;

        // Print all application names
        for(int app_index = 0; app_index < tbuf->num_applications; app_index++ ) {

            len = snprintf(ptr, buflen-totlen,
                           "# DCB %d %" PRIx64 " %.*s\n",
                           core, tbuf->applications[app_index].dcb,
                           8, (char*)&tbuf->applications[app_index].name);

            ptr += len; totlen += len;
        }

        for (int i = 0; i < num_events; i++) {

            tbuf->tail_index++;
            if (tbuf->tail_index == TRACE_MAX_EVENTS) {
                tbuf->tail_index = 0;
            }

            len = snprintf(ptr, buflen-totlen,
                           "%d %" PRIu64 " %" PRIx64 "\n",
                           core, tbuf->events[tbuf->tail_index].timestamp,
                           //core, tbuf->events[tbuf->tail_index].timestamp - t0,
                           tbuf->events[tbuf->tail_index].u.raw);

            ptr += len; totlen += len;

            if(number_of_events_dumped != NULL) {
                (*number_of_events_dumped)++;
            }

        }
    }

    return totlen;
}

//------------------------------------------------------------------------------
// Flushing functionality
//------------------------------------------------------------------------------

struct trace_flush_state
{
	struct event_queue_node qnode;
	struct monitor_binding *monitor_binding;
	iref_t iref;
};

// ------- Global Variables

static struct event_closure flush_callback;

// -------

static void trace_notify_bfscope_cont(void* arg)
{
	errval_t err;

	struct trace_flush_state *state = (struct trace_flush_state*) arg;
	struct monitor_binding *monitor_binding = state->monitor_binding;

	err = monitor_binding->tx_vtbl.bfscope_flush_send(monitor_binding, MKCONT(free, state), state->iref);

	if (err_is_ok(err)) {
		event_mutex_unlock(&monitor_binding->mutex);
	} else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		err = monitor_binding->register_send(monitor_binding, monitor_binding->waitset, MKCONT(&trace_notify_bfscope_cont, state));
		assert(err_is_ok(err));
	} else {
		event_mutex_unlock(&monitor_binding->mutex);
		//TODO: Error handling
		USER_PANIC_ERR(err, "Could not send flush message to bfscope");
	}
}

static void trace_notify_bfscope_finished(struct monitor_binding *mb)
{
	printf("bfscope flush returned!\n");

	struct event_closure callback = flush_callback;

	if (callback.handler != NULL) {
		callback.handler(callback.arg);
	}
}

/*
 * Send a message to bfscope, notifying it that it should dump the trace buffer.
 */
static void trace_notify_bfscope(struct event_closure callback, iref_t iref)
{

	flush_callback = callback;

	struct trace_flush_state *state = malloc(sizeof(struct trace_flush_state));
	//memset(state, 0, sizeof(struct trace_broadcast_start_state));

	state->monitor_binding = get_monitor_binding();
	state->iref = iref;

	state->monitor_binding->rx_vtbl.bfscope_flush_ack = &trace_notify_bfscope_finished;

	event_mutex_enqueue_lock(&state->monitor_binding->mutex, &state->qnode, MKCLOSURE(&trace_notify_bfscope_cont, state));
}

/*
 * Dump the current content of the trace buffer to the console and reset the trace
 * buffer.
 */
static void trace_flush_to_console(void)
{
	char *trace_buf = malloc(CONSOLE_DUMP_BUFLEN);
	assert(trace_buf);

	trace_dump(trace_buf, CONSOLE_DUMP_BUFLEN, NULL);

	printf("%s\n", trace_buf);

	trace_reset_all();
}

/**
 * \brief Flush the trace buffer to the "best" destination.
 *
 * This function automatically determines if bfscope is running and if someone
 * is connected over the network. If so, the buffer is flushed over the network.
 * If nobody is connected or bfscope is not running, the buffer is flushed onto
 * the console.
 *
 */
void trace_flush(struct event_closure callback)
{
	// Check if bfscope is currently running
	iref_t iref;
	errval_t err = nameservice_lookup("bfscope", &iref);

	if (err_no(err) == LIB_ERR_NAMESERVICE_UNKNOWN_NAME) {
		// Bfscope is not running
		trace_flush_to_console();

		if (callback.handler != NULL) {
			callback.handler(callback.arg);
		}

	} else if (err_is_fail(err)) {
		USER_PANIC_ERR(err, "nameservice_lookup failed");
	} else {
		// Lookup was successful, bfscope is running

		// Send a message to bfscope, so that it flushes
		trace_notify_bfscope(callback, iref);
	}
}

/**
 * \brief Enable/Disable the autoflush mechanism of the tracing framework.
 *
 * If autoflush is enabled, the tracing framework will automatically flush
 * the content of the buffer periodically.
 *
 * NOTE: This only works when bfscope is running!
 *
 * NOTE: If you enable autoflush, it will affect your performance during the
 * time period you are flushing, as the buffer might be flushed at any given
 * time.
 */
void trace_set_autoflush(bool enabled)
{
	struct trace_buffer *master = (struct trace_buffer*) trace_buffer_master;
	master->autoflush = enabled;
}

//------------------------------------------------------------------------------
// Trace subsystem enabiling/disabling functionality
//------------------------------------------------------------------------------

/**
 * \brief Enable/Disable the logging of a given subsytem.
 *
 * subsys: A subsystem, i.e. the macro generated by the DSL.
 * enabled: True iff the events should be logged.
 */
errval_t trace_set_subsys_enabled(uint16_t subsys, bool enabled)
{
	bool* subsystem_states = (bool*) (((uint8_t*) trace_buffer_master) + TRACE_BUF_SIZE);

	subsystem_states[subsys] = enabled;

	return SYS_ERR_OK;
}

/**
 * \brief Enable/Disable all subsystems.
 */
errval_t trace_set_all_subsys_enabled(bool enabled)
{
	bool* subsystem_states = (bool*) (((uint8_t*) trace_buffer_master) + TRACE_BUF_SIZE);
	int i = 0;
	for (i = 0; i < TRACE_NUM_SUBSYSTEMS; i++) {
		subsystem_states[i] = enabled;
	}

	return SYS_ERR_OK;
}

//------------------------------------------------------------------------------
// Trace preparation functionality
//------------------------------------------------------------------------------

struct trace_prepare_state
{
	struct event_queue_node qnode;
	struct monitor_binding *monitor_binding;
};

// ------- Global Variables

/// Callback that will be invoked at the end of preparing the tracing environment.
static struct event_closure prepare_callback;

// -------

/*
 * Continuation function for sending the initial trace_prepare message.
 */
static void trace_prepare_cont(void *arg)
{
	errval_t err;

	struct trace_prepare_state *state = (struct trace_prepare_state*) arg;
	struct monitor_binding *mb = state->monitor_binding;

	dispatcher_handle_t handle = curdispatcher();
	struct dispatcher_generic *disp = get_dispatcher_generic(handle);
	coreid_t my_coreid = disp->core_id;

	err = mb->tx_vtbl.trace_prepare(mb, MKCONT(free, state), my_coreid);

	if (err_is_ok(err)) {
		event_mutex_unlock(&mb->mutex);
	} else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		err = mb->register_send(mb, mb->waitset, MKCONT(&trace_prepare_cont, state));
		assert(err_is_ok(err));
	} else {
		event_mutex_unlock(&mb->mutex);
		// TODO: error handling
		USER_PANIC_ERR(err, "Could not send trace_prepare message");
	}
}

/*
 * Function that is called when we receive a message that the preparation is
 * finished.
 */
static void trace_prepare_finished(struct monitor_binding *mb)
{
	printf("Trace prepare finished!\n");

	prepare_callback.handler(prepare_callback.arg);
}

/**
 * \brief Optional call to do "extra" preparation of the tracing framework.
 *
 * Call this method to prepare for tracing. This is not a preparation in a strict
 * sense, i.e. tracing will also work when you do not call this method, but it
 * provides some benefits.
 *
 * Currently it provides a mechanism for clock synchronization between cores.
 */
errval_t trace_prepare(struct event_closure callback)
{

	prepare_callback = callback;

	struct trace_prepare_state *state = malloc(sizeof(struct trace_prepare_state));

	state->monitor_binding = get_monitor_binding();

	state->monitor_binding->rx_vtbl.trace_prepare_finished = &trace_prepare_finished;

	return event_mutex_enqueue_lock(&state->monitor_binding->mutex, &state->qnode, MKCLOSURE(&trace_prepare_cont, state));
}
