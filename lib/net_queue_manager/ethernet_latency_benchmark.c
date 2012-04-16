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
#include <stdio.h>
#include <string.h>

#include "queue_manager_local.h"

#define TSCPERMS 2511862ULL

static void client_send_packet(void);

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
static bool got_response = false;



static void alloc_mem(uint64_t* phys, void** virt, size_t size)
{
    struct capref frame;
    errval_t r;
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

    r = vspace_map_one_frame_attr(virt, size, frame,
            VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Mapping memory region frame failed!");
    }
    memset(*virt, 0, size);
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

    alloc_mem(&phys, &virt, BUF_COUNT * rx_bufsz);

    for (i = 0; i < BUF_COUNT; i++) {
        buf_phys[i] = phys + rx_bufsz;
        buf_virt[i] = (void*) ((uintptr_t) virt + rx_bufsz);
    }

    initialized = true;

    if (is_server) {
        printf("elb: Starting benchmark server...\n");
        register_rx_buffer(buf_cur);
    } else {
        printf("elb: Starting benchmark client...\n");

        // Register receive buffer
        register_rx_buffer(buf_cur);
        client_send_packet();
    }
}

void ethersrv_argument(const char* arg)
{
    if (!strcmp(arg, "elb_server=1")) {
        is_server = true;
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
        printf("elb: sent response...\n");
    } else {
        uint64_t diff = rdtsc() - sent_at;
        printf("elb: Got response: %"PRIu64"!\n", diff);
        got_response = true;
    }
}

bool handle_tx_done(void *opaque)
{
    printf("elb: Transmitted a packet...\n");
    return true;
}

static void client_send_packet(void)
{
    struct ethernet_frame *frame;
    const char bcast[6] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};
    size_t len = sizeof(*frame) + 64;
    size_t idx = (buf_cur + 1) % BUF_COUNT;

    printf("elb: Sending packet...\n");
    frame = buf_virt[idx];
    memcpy(frame->src_mac, our_mac, 6);
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
    struct ethernet_frame *frame = buf_virt[i];
    const char bcast[6] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};

    memcpy(frame->src_mac, our_mac, 6);
    memcpy(frame->dst_mac, bcast, 6);

    register_tx_buffer(i, len);
}

