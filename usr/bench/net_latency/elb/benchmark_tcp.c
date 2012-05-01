/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "elb.h"

#include <barrelfish/sys_debug.h>
#include <string.h>

#include <bench/bench.h>

#include <lwip/init.h>

#include "tcp_server_bm.h"

static void start_next_iteration(void);

static uint64_t tscperms;

//static size_t   buf_cur = 0;
static size_t   buf_count;

bool is_server = false;
static char *app_type = "client";

static uint64_t sent_at;
static uint64_t started_at;
static uint64_t minbase = -1ULL;
static uint64_t maxbase = -1ULL;
static bool affinity_set = false;

#define MAX_PAYLOAD 1500
/** Size of payload for ethernet packets in benchmark */
static size_t payload_size = 64;

/** Specifies whether the data should be read by the client */
static bool read_incoming = false;

// the cardname provided on commandline
static char *cardname = "e10k";

// the queueid asked by the application
static uint64_t qi = 0;

/** Number of runs to run */
static size_t total_runs = 10000;

/** Number of dry runs before we start benchmarking */
static size_t dry_runs = 100;

/** Specifies whether the time for each run should be dumped */
static bool dump_each_run = false;

/** Specifies if NOCACHE should be used for mapping the buffers */
static bool use_nocache = false;

/** Prefix for outputting the results */
static const char *out_prefix = "";

// port no of server on which it will listen
static uint16_t server_port = 7;

// IP address of server
static char *server_ip_addr = NULL;

static uint8_t *data_to_send = NULL;

/** Benchmark control handle */
bench_ctl_t *bench_ctl = NULL;

// to avoid compiler optimizations of packet read path
static size_t acc = 0;

// send one message to server
static void start_next_iteration(void)
{

    sent_at = rdtsc();
    // send single byte of TCP data
    int ret = send_message_client(data_to_send, payload_size);
    assert(ret == 0);
} // end function: start_next_iteration


void benchmark_init(size_t buffers)
{
    errval_t err;

    // Getting CPU frequency
    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    buffer_count = buffers;

    printf("tcp benchmark: init started\n");
    bool ret = lwip_init(cardname, qi);
    if (!ret) {
        USER_PANIC("lwip_init failed!");
        return;
    }
    printf("tcp benchmark: lwip init done\n");


    buf_count = buffers;

    // Initialize benchmark control
    bench_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, total_runs);
    bench_ctl_dry_runs(bench_ctl, dry_runs);


    if (is_server) {
        printf("elb_tcp: Starting benchmark server...\n");
        // listen on port
        if (tcp_server_bm_init(server_port) != 0) {
            USER_PANIC("tcp_server_bm_init failed");
            return;
        }
    } else { // is client
        printf("elb_tcp: Starting benchmark client...\n");
        // onnect on ip/port
        if (tcp_client_bm_init(server_ip_addr, server_port) != 0) {
            USER_PANIC("tcp_server_bm_init failed");
            return;
        }
        data_to_send = malloc(MAX_PAYLOAD);
        assert(data_to_send != NULL);
        // FIXME: make it cache aligned
        memset(data_to_send, 1, payload_size);
        started_at = rdtsc();
        start_next_iteration();
    } // end else: is_client
} // end function: benchmark_init

// This function is called whenever new data arrives for client
void handle_data_arrived(char *payload, size_t data_len)
{

    volatile uint8_t *b =(uint8_t *)payload;
    if (read_incoming) {
        for (int i = 0; i< data_len; i++) {
            acc += (b[i]);
        }
    }

    if (is_server) {
        return;
    }

    // record completion time
    cycles_t tsc = rdtsc();
    cycles_t result[1] = {
        tsc - sent_at,
    };

    if (bench_ctl_add_run(bench_ctl, result)) {
        uint64_t tscperus = tscperms / 1000;
        printf("cycles per us %"PRIu64"\n", tscperus);

        // Output our results
       bench_ctl_dump_csv_bincounting(bench_ctl, 0, 100, 9 * tscperus,
                    25 * tscperus, out_prefix, tscperus);

       bench_ctl_dump_analysis(bench_ctl, 0,  out_prefix, tscperus);

       // bench_ctl_dump_csv(bench_ctl, out_prefix, tscperus);

        bench_ctl_destroy(bench_ctl);
        terminate_benchmark();
        printf("pkt content some is %zd\n", acc);
        return;
    }

    start_next_iteration();
} // end function: handle_data_arrived

void benchmark_argument(char *arg)
{
    if (!strcmp(arg, "elb_server=1")) {
        is_server = true;
        app_type = "server";
    } else if (!strncmp(arg, "runs=", strlen("runs="))) {
        total_runs = atol(arg + strlen("runs="));
    } else if (!strncmp(arg, "dry_runs=", strlen("dry_runs="))) {
        dry_runs = atol(arg + strlen("dry_runs="));
    } else if (!strncmp(arg, "payload_size=", strlen("payload_size="))) {
        payload_size = atol(arg + strlen("payload_size="));
        if (payload_size < 46) {
            printf("elb: Payload size too small (must be at least 46), has "
                    "been extended to 46!\n");
            payload_size = 46;
        } else if (payload_size > MAX_PAYLOAD) {
            printf("elb: Payload size too big (must be at most 1500), has "
                    "been limited to 1500!\n");
            payload_size = 1500;
        }
    } else if (!strncmp(arg, "elp_outprefix=", strlen("elp_outprefix="))) {
        out_prefix = arg + strlen("elp_outprefix=");
    } else if (!strncmp(arg, "elb_nocache=", strlen("elb_nocache="))) {
        use_nocache = !!atol(arg + strlen("elb_nocache="));
    } else if (!strncmp(arg, "read_incoming=", strlen("read_incoming="))) {
        read_incoming = !!atol(arg + strlen("read_incoming="));
    } else if (!strncmp(arg, "dump_each=", strlen("dump_each="))) {
        dump_each_run = !!atol(arg + strlen("dump_each="));
    } else if (!strncmp(arg, "affinitymin=", strlen("affinitymin="))) {
        minbase = atol(arg + strlen("affinitymin="));
    } else if(!strncmp(arg, "affinitymax=", strlen("affinitymax="))) {
        maxbase = atol(arg + strlen("affinitymax="));
    } else if(!strncmp(arg, "cardname=", strlen("cardname="))) {
        cardname = arg + strlen("cardname=");
    } else if(!strncmp(arg, "queue=", strlen("queue="))) {
        qi = atol(arg + strlen("queue="));
    } else if(!strncmp(arg, "server_ip=", strlen("server_ip="))) {
        server_ip_addr = arg + strlen("server_ip=");
    } else {
        printf("Invalid command line argument [%s]\n", arg);
        abort();
    }

    if (!affinity_set && minbase != -1ULL && maxbase != -1ULL) {
        ram_set_affinity(minbase, maxbase);
        affinity_set = true;
    }

}

// Returns the card-name provided by command line parameters
char *get_cardname(void)
{
    return cardname;
}

// Returns the queue-id provided by command line parameters
uint64_t get_cmdline_queueid(void)
{
    return qi;
}


