/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/waitset_chan.h>
#include <barrelfish/deferred.h>
#include <barrelfish/sys_debug.h>
#include <devif/queue_interface.h>
#include <devif/backends/net/udp.h>
#include <bench/bench.h>
#include <net_interfaces/flags.h>


#define BENCH
//#define DEBUG(x...) printf("devif_test: " x)
#define DEBUG(x...) do {} while (0)

#define BUF_SIZE 2048
#define NUM_BUF 1024
#define MEMORY_SIZE BUF_SIZE*NUM_BUF


static struct devq* udp_q;
static struct capref memory_rx;
static regionid_t regid_rx;
static struct frame_identity id;
static lpaddr_t phys_rx;
static void* va_rx;


static uint16_t len;
static uint32_t ip_dst;
static uint64_t mac_dst;
static uint16_t port_src;
static uint16_t port_dst;
static const char* cardname;

static uint64_t tsc_per_ms;

static void event_cb(void* q)
{
    return;
}

int main(int argc, char *argv[])
{
    if (argc > 6) {
        char* stop;
        ip_dst = atoi(argv[1]);
        mac_dst = strtoull(argv[2], &stop, 10);
        port_src = atoi(argv[3]);
        port_dst = atoi(argv[4]);
        cardname = argv[5];
        len = atoi(argv[6]);
    } else {
        USER_PANIC("NO src or dst IP given \n");
    }

    errval_t err;
    // Allocate memory
    err = frame_alloc(&memory_rx, MEMORY_SIZE, NULL);
    if (err_is_fail(err)){
        USER_PANIC("Allocating cap failed \n");
    }
    
    // RX frame
    err = invoke_frame_identify(memory_rx, &id);
    if (err_is_fail(err)) {
        USER_PANIC("Frame identify failed \n");
    }

    err = vspace_map_one_frame_attr(&va_rx, id.bytes, memory_rx,
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC("Frame mapping failed \n");
    }

    phys_rx = id.base;

    err = udp_create((struct udp_q**) &udp_q, cardname, port_src, port_dst,
                     ip_dst, event_cb, true);
    if (err_is_fail(err)) {
        USER_PANIC("Queue creation failed \n");
    }

    err = devq_register(udp_q, memory_rx, &regid_rx);
    if (err_is_fail(err)) {
        USER_PANIC("Register failed \n");
    }

    err = sys_debug_get_tsc_per_ms(&tsc_per_ms);
    assert(err_is_ok(err));

    barrelfish_usleep(1000*1000*15);

    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    err = SYS_ERR_OK;

    assert(len < 1500);

    int j = 0;
    while (err == SYS_ERR_OK) {
        devq_enqueue((struct devq*) udp_q, regid_rx, j*BUF_SIZE, BUF_SIZE, 0, len, NETIF_TXFLAG | NETIF_TXFLAG_LAST);
        devq_dequeue((struct devq*) udp_q, &rid, &offset, &length, &valid_data,
                     &valid_length, &flags);
        j = (j+1) % NUM_BUF;
    }

}

