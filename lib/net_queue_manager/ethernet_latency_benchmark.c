/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/net_constants.h>
#include <bench/bench.h>
#include <stdio.h>
#include <string.h>

#include "queue_manager_local.h"

#define TSCPERMS 2511862ULL

static void client_send_packet(void);
static void start_run(void);

static void register_rx_buffer(size_t i);
static void register_tx_buffer(size_t i, size_t len);
static void respond_buffer(size_t i, size_t len);

static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;
static ether_terminate_queue terminate_queue_fn_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;
static ether_handle_free_TX_slot handle_free_tx_slot_fn_ptr = NULL;
ether_rx_register_buffer rx_register_buffer_fn_ptr = NULL;
ether_rx_get_free_slots rx_get_free_slots_fn_ptr = NULL;

struct ethernet_frame {
    uint8_t dst_mac[6];
    uint8_t src_mac[6];
    uint16_t ethertype;
    uint8_t payload[];
} __attribute__((packed));


static size_t rx_ring_bufsz;
static uint8_t our_mac[8];

#define BUF_COUNT 2
static uint64_t buf_phys[BUF_COUNT];
static void    *buf_virt[BUF_COUNT];
static size_t   buf_cur = 0;

static bool initialized = false;
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
static size_t total_runs = 1024;

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



static void alloc_mem(uint64_t* phys, void** virt, size_t size)
{
    struct capref frame;
    errval_t r;
    vregion_flags_t flags;
    struct frame_identity frameid = { .base = 0, .bits = 0 };

    r = frame_alloc(&frame, size, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Allocating memory region frame failed!");
    }

    r = invoke_frame_identify(frame, &frameid);
    if (!err_is_ok(r)) {
        USER_PANIC("Identifying memory region frame failed!");
    }
    *phys = frameid.base;

    flags = (use_nocache ? VREGION_FLAGS_READ_WRITE_NOCACHE :
                           VREGION_FLAGS_READ_WRITE);
    r = vspace_map_one_frame_attr(virt, size, frame, flags, NULL, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Mapping memory region frame failed!");
    }
    memset(*virt, 0, size);
}

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

void ethersrv_init(char *service_name, uint64_t queueid,
                   ether_get_mac_address_t get_mac_ptr,
                   ether_terminate_queue terminate_queue_ptr,
                   ether_transmit_pbuf_list_t transmit_ptr,
                   ether_get_tx_free_slots tx_free_slots_ptr,
                   ether_handle_free_TX_slot handle_free_tx_slot_ptr,
                   size_t rx_bufsz,
                   ether_rx_register_buffer rx_register_buffer_ptr,
                   ether_rx_get_free_slots rx_get_free_slots_ptr)
{
    uint64_t phys;
    void *virt;
    size_t i;

    ether_get_mac_address_ptr = get_mac_ptr;
    terminate_queue_fn_ptr = terminate_queue_ptr;
    ether_transmit_pbuf_list_ptr = transmit_ptr;
    tx_free_slots_fn_ptr = tx_free_slots_ptr;
    handle_free_tx_slot_fn_ptr = handle_free_tx_slot_ptr;
    rx_register_buffer_fn_ptr = rx_register_buffer_ptr;
    rx_get_free_slots_fn_ptr = rx_get_free_slots_ptr;

    rx_ring_bufsz = rx_bufsz;
    ether_get_mac_address_ptr(our_mac);

    // Allocate packet buffers
    alloc_mem(&phys, &virt, BUF_COUNT * rx_bufsz);
    for (i = 0; i < BUF_COUNT; i++) {
        buf_phys[i] = phys + i * rx_bufsz;
        buf_virt[i] = (void*) ((uintptr_t) virt + i * rx_bufsz);
    }

    // If desired, create permutation for accessing incoming data
    if (read_incoming && !read_linear) {
        create_read_permutation();
    }

    // Initialize benchmark control
    bench_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 2, total_runs);
    bench_ctl_dry_runs(bench_ctl, dry_runs);

    initialized = true;

    if (is_server) {
        printf("elb: Starting benchmark server...\n");
        register_rx_buffer(buf_cur);
    } else {
        printf("elb: Starting benchmark client...\n");

        started_at = rdtsc();
        start_run();
    }
}

void ethersrv_argument(const char* arg)
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

void do_pending_work_for_all(void)
{
    if (!initialized || is_server) {
        return;
    }

    if (!got_response && (sent_at + 1000*TSCPERMS) < rdtsc()) {
        client_send_packet();
    }
}

void process_received_packet(void *opaque, size_t pkt_len, bool is_last)
{
    size_t idx = buf_cur;
    assert(is_last);

    if (is_server) {
        buf_cur = (buf_cur + 1) % BUF_COUNT;
        register_rx_buffer(buf_cur);

        respond_buffer(idx, pkt_len);
        //iprintf("elb: sent response...\n");
    } else {
        // Touch data if desired
        if (read_incoming) {
            struct ethernet_frame* frame = buf_virt[buf_cur];
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
        cycles_t result[2] = {
            tsc - sent_at,
            tsc - started_at,
        };

        got_response = true;

        if (bench_ctl_add_run(bench_ctl, result)) {
            bench_ctl_dump_csv(bench_ctl, out_prefix);
            bench_ctl_destroy(bench_ctl);
            terminate_queue_fn_ptr();
        } else {
            start_run();
        }
    }
}

bool handle_tx_done(void *opaque)
{
    //printf("elb: Transmitted a packet...\n");
    return true;
}

static void start_run(void)
{
    // Register receive buffer
    register_rx_buffer(buf_cur);
    client_send_packet();
}

static void client_send_packet(void)
{
    struct ethernet_frame *frame;
    const char bcast[6] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};
    size_t len = sizeof(*frame) + payload_size;
    size_t idx = (buf_cur + 1) % BUF_COUNT;

    //printf("elb: Sending packet...\n");
    frame = buf_virt[idx];
    //memcpy(frame->src_mac, our_mac, 6);
    memcpy(frame->src_mac, bcast, 6);
    memcpy(frame->dst_mac, bcast, 6);
    frame->ethertype = 0x0608;
    sent_at = rdtsc();
    register_tx_buffer(idx, len);
}

static void register_rx_buffer(size_t i)
{
    rx_register_buffer_fn_ptr(buf_phys[i], buf_virt[i], NULL);
}

static void register_tx_buffer(size_t i, size_t len)
{
    struct driver_buffer buffer = {
        .va = buf_virt[i],
        .pa = buf_phys[i],
        .len = len,
    };
    ether_transmit_pbuf_list_ptr(&buffer, 1, NULL);
}

static void respond_buffer(size_t i, size_t len)
{
    /*struct ethernet_frame *frame = buf_virt[i];
    const char bcast[6] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};

    memcpy(frame->src_mac, our_mac, 6);
    memcpy(frame->dst_mac, bcast, 6);*/

    register_tx_buffer(i, len);
}

