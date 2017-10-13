/**
 * \file
 * \brief Barrelfish trace server, Version 2, NFS variant
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/lmp_endpoints.h>
#include <barrelfish/event_queue.h>
#include <barrelfish/nameservice_client.h>
#include <trace/trace.h>
#include <vfs/vfs.h>

#include <flounder/flounder.h>
#include <if/monitor_defs.h>

#include <if/empty_defs.h>

#define DEBUG if (0) printf

/// Buffer size for temp buffer during dumping: 128MB
#define BFSCOPE_BUFLEN (128UL * 1024 * 1024)

/// Use /bfscope as mount point for NFS share
#define MOUNT_DIR "/bfscope"
/// try to make NFS writes fit in ethernet frame, copied from lib/vfs/vfs_nfs.c
//#define MAX_NFS_CHUNK 1420
static vfs_handle_t dump_file_vh;

/// If we are in autoflush is enabled, bfscope can itself determine to flush. In
/// that case, we don't want to notify anyone after doing a locally initiated flush.
static bool local_flush = false;

static char *trace_buf = NULL;
static bool dump_in_progress = false;

struct bfscope_ack_send_state {
    struct event_queue_node qnode;
    struct monitor_binding *monitor_binding;
};

static void bfscope_send_flush_ack_cont(void* arg)
{
    errval_t err;

    struct bfscope_ack_send_state *state = (struct bfscope_ack_send_state*) arg;
    struct monitor_binding *monitor_binding = state->monitor_binding;

    err = monitor_binding->tx_vtbl.bfscope_flush_ack(monitor_binding, MKCONT(free, state));

    if (err_is_ok(err)) {
        event_mutex_unlock(&monitor_binding->mutex);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = monitor_binding->register_send(monitor_binding,
                monitor_binding->waitset,
                MKCONT(&bfscope_send_flush_ack_cont, state));
        assert(err_is_ok(err));
    } else {
        event_mutex_unlock(&monitor_binding->mutex);
        //TODO: Error handling
        USER_PANIC_ERR(err, "Could not send flush ack message to monitor of bfscope");
    }
}

static void bfscope_send_flush_ack_to_monitor(void) 
{
    struct bfscope_ack_send_state *state =
        malloc(sizeof(struct bfscope_ack_send_state));

    state->monitor_binding = get_monitor_binding();

    event_mutex_enqueue_lock(&state->monitor_binding->mutex, &state->qnode,
            MKCLOSURE(&bfscope_send_flush_ack_cont, state));
}

static void bfscope_trace_dump_finished(void)
{
    dump_in_progress = false;

    if (!local_flush) {
        bfscope_send_flush_ack_to_monitor();
    } else {
        // Locally initiated flush is finished.
        local_flush = false;
    }
}

static void bfscope_trace_dump(void)
{
    errval_t err;
    int number_of_events = 0;
    size_t trace_length = 0;

    if(dump_in_progress) {
        // Currently there is already a dump in progress, do nothing.
        return;
    }

    // Acquire the trace buffer
    trace_length = trace_dump(trace_buf, BFSCOPE_BUFLEN, &number_of_events);

    DEBUG("bfscope: trace length %zu, nr. of events %d\n", trace_length, number_of_events);

    if (trace_length <= 0 || number_of_events <= 0) {
        DEBUG("bfscope: trace length too small, not dumping.\n");
        goto finish;
    }

    dump_in_progress = true;

    DEBUG("dumping %zu bytes to NFS share\n", trace_length);

    size_t total_written = 0;
    while (total_written < trace_length) {
        size_t written = 0;
        char *bufptr = trace_buf + total_written;
        size_t bytes = trace_length - total_written;
        // artificially limit nfs writes
        //bytes = bytes > MAX_NFS_CHUNK ? MAX_NFS_CHUNK : bytes;
        DEBUG("dumping to NFS share: trying to write %zu bytes\n", bytes);
        err = vfs_write(dump_file_vh, bufptr, bytes, &written);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vfs_write while dumping trace");
        }
        total_written += written;
        DEBUG("dumping to NFS share: %zu/%zu bytes written\n", total_written, trace_length);
    }
    DEBUG("dump to NFS share done!\n");

finish:
    bfscope_trace_dump_finished();
}

static void bfscope_handle_flush_msg(struct monitor_binding *mb, iref_t iref)
{
    printf("bfscope flush request message received!\n");

    bfscope_trace_dump();
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    printf("bfscope: exported at iref %"PRIuIREF"\n", iref);

    // register this iref with the name service
    err = nameservice_register("bfscope", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct empty_binding *b)
{
    USER_PANIC("bfscope: connect_cb got called");
}

int main(int argc, char**argv)
{
#ifndef CONFIG_TRACE
    // bail - no tracing support
    printf("%*s: Error, no tracing support, cannot start bfscope\n",
           DISP_NAME_LEN, disp_name());
    printf("%.*s: recompile with trace = TRUE in build/hake/Config.hs\n",
           DISP_NAME_LEN, disp_name());
    return -1;
#endif

    errval_t err;
    /* connect to nfs server according to command line arguments */
    vfs_init();

    if(argc < 3) {
        printf("Usage: %s mount-URL filepath\n", argv[0]);
        printf("Example: %s nfs://10.110.4.4/mnt/local/nfs/gerbesim  /bfscope/trace.data\n\n"
               "    This example mounts the nfs share /mnt/local/nfs/gerbesim\n"
               "    on 10.110.4.4 (emmentaler1) in the hardcoded directory /bfscope\n"
               "    in the barrelfish VFS, and then writes the trace data into the\n"
               "    file trace.data in the NFS share.\n",
                argv[0]);
        exit(EXIT_FAILURE);
    }

    err = vfs_mount(MOUNT_DIR, argv[1]);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_mount");
    }
    assert(err_is_ok(err));
    debug_printf("NFS mount done\n");

    // open/create file
    err = vfs_create(argv[2], &dump_file_vh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "creating dump file");
        return 1;
    }

    // do a write to the file, to check that we're able to write
    char header[] = "# bfscope trace dump\n";
    size_t written = 0;
    // take sizeof(header)-1 to not write null byte to file
    err = vfs_write(dump_file_vh, header, sizeof(header)-1, &written);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "writing header to trace dump file");
        return 1;
    }

    // Allocate the outgoing buffer
    if (trace_buf == NULL) {
        trace_buf = malloc(BFSCOPE_BUFLEN);
    }
    assert(trace_buf);

    // Disable tracing for bfscope
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    disp->trace_buf = NULL;

    printf("%.*s running on core %d\n", DISP_NAME_LEN, disp_name(),
           disp_get_core_id());


    // Export our empty interface
    err = empty_export(NULL /* state pointer for connect/export callbacks */,
            export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // Register our message handlers with the monitor
    struct monitor_binding *monitor_binding;
    monitor_binding = get_monitor_binding();
    monitor_binding->rx_vtbl.bfscope_flush_send = &bfscope_handle_flush_msg;

    // dispatch events, and check whether to autoflush periodically
    while (1) {
        err = event_dispatch_non_block(get_default_waitset());
        if (err == LIB_ERR_NO_EVENT) {
            // It is ok that no event is dispatched, we want to check whether
            // to autoflush, anyway
            err = SYS_ERR_OK;
        }
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }

        DEBUG("bfscope: dispatched event, autoflush: %d\n",
                ((struct trace_buffer*) trace_buffer_master)->autoflush);

        // Check if we are in autoflush mode
        if(((struct trace_buffer*) trace_buffer_master)->autoflush) {
            local_flush = true;
            bfscope_trace_dump();
        }

        thread_yield_dispatcher(NULL_CAP);
    }
}
