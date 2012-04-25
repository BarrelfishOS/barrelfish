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
#include <bench/bench.h>

static void client_send_packet(void);
static void start_run(void);
static void respond_buffer(size_t i, size_t len);


struct ethernet_frame {
    uint8_t dst_mac[6];
    uint8_t src_mac[6];
    uint16_t ethertype;
    uint8_t payload[];
} __attribute__((packed));


static uint64_t tscperms;

static size_t   buf_cur = 0;
static size_t   buf_count;

static bool is_server = false;
static uint64_t sent_at;
static uint64_t started_at;
static bool got_response = false;
static uint64_t minbase = -1ULL;
static uint64_t maxbase = -1ULL;
static bool affinity_set = false;

#define MAX_PAYLOAD 1500
/** Size of payload for ethernet packets in benchmark */
static size_t payload_size = 64;

/** Specifies whether the data should be read by the client */
static bool read_incoming = false;

/** Specifies whether a permutation should be used or just a linear scan */
static bool read_linear = false;

/** Will be initialized with a permutation for touching the packet content */
static uint16_t read_permutation[MAX_PAYLOAD];


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


/** Benchmark control handle */
bench_ctl_t *bench_ctl = NULL;



/** Generate a permutation for touching the packet contents */
static void create_read_permutation(void)
{
    uint16_t i;
    uint16_t j;
    uint16_t tmp;

    for (i = 0; i < payload_size; i++) {
        read_permutation[i] = i;
    }

    srand(rdtsc());

    // Use fisher-yates shuffle
    for (i = payload_size - 1; i >= 1; i--) {
        j = rand() % (i + 1);

        tmp = read_permutation[i];
        read_permutation[i] = read_permutation[j];
        read_permutation[j] = tmp;
    }
}




void benchmark_init(size_t buffers)
{
    errval_t err;

    buf_count = buffers;

    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));


    // If desired, create permutation for accessing incoming data
    if (read_incoming && !read_linear) {
        create_read_permutation();
    }

    // Initialize benchmark control
    bench_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, total_runs);
    bench_ctl_dry_runs(bench_ctl, dry_runs);

    if (is_server) {
        printf("elb: Starting benchmark server...\n");
        buffer_rx_add(buf_cur);
    } else {
        printf("elb: Starting benchmark client...\n");

        started_at = rdtsc();
        start_run();
    }

}

void benchmark_argument(const char *arg)
{
    if (!strcmp(arg, "elb_server=1")) {
        is_server = true;
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
    }

    if (!affinity_set && minbase != -1ULL && maxbase != -1ULL) {
        ram_set_affinity(minbase, maxbase);
        affinity_set = true;
    }
}

void benchmark_do_pending_work(void)
{
    if (is_server) {
        return;
    }

    if (!got_response && (sent_at + 1000*tscperms) < rdtsc()) {
        client_send_packet();
    }
}

void benchmark_rx_done(size_t idx, size_t pkt_len)
{
    if (is_server) {
        buf_cur = (buf_cur + 1) % buf_count;
        buffer_rx_add(buf_cur);

        respond_buffer(idx, pkt_len);
    } else {
        // Touch data if desired
        if (read_incoming) {
            struct ethernet_frame* frame = buffer_address(buf_cur);
            volatile uint8_t* b = frame->payload;
            size_t i;
            size_t acc = 0;
            if (read_linear) {
                for (i = 0; i< payload_size; i++) {
                    acc += b[i];
                }
            } else {
                for (i = 0; i < payload_size; i++) {
                    acc += b[read_permutation[i]];
                }
            }
        }

        cycles_t tsc = rdtsc();
        cycles_t result[1] = {
            tsc - sent_at,
        };

        got_response = true;

        if (bench_ctl_add_run(bench_ctl, result)) {
            uint64_t tscperus = tscperms / 1000;

            // Output our results
            bench_ctl_dump_csv_bincounting(bench_ctl, 0, 100, 9 * tscperus,
                    25 * tscperus, out_prefix);
            //bench_ctl_dump_csv(bench_ctl, out_prefix);

            bench_ctl_destroy(bench_ctl);
            terminate_benchmark();
        } else {
            start_run();
        }
    }

}

void benchmark_tx_done(size_t idx)
{

}

static void start_run(void)
{
    // Register receive buffer
    buffer_rx_add(buf_cur);
    client_send_packet();
}

static void client_send_packet(void)
{
    struct ethernet_frame *frame;
    const char bcast[6] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};
    size_t len = sizeof(*frame) + payload_size;
    size_t idx = (buf_cur + 1) % buf_count;

    //printf("elb: Sending packet...\n");
    frame = buffer_address(idx);
    //memcpy(frame->src_mac, our_mac, 6);
    memcpy(frame->src_mac, bcast, 6);
    memcpy(frame->dst_mac, bcast, 6);
    frame->ethertype = 0x0608;
    sent_at = rdtsc();
    buffer_tx_add(idx, len);
}

static void respond_buffer(size_t i, size_t len)
{
    /*struct ethernet_frame *frame = buf_virt[i];
    const char bcast[6] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};

    memcpy(frame->src_mac, our_mac, 6);
    memcpy(frame->dst_mac, bcast, 6);*/

    buffer_tx_add(i, len);
}

