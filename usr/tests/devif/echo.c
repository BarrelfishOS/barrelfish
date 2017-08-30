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

#define TX_BUF_SIZE 2048
#define RX_BUF_SIZE 2048
#define NUM_BUF 1024
#define MEMORY_SIZE RX_BUF_SIZE*NUM_BUF



static struct devq* udp_q;
static struct capref memory_rx;
static regionid_t regid_rx;
static struct frame_identity id;
static lpaddr_t phys_rx;
static void* va_rx;

static uint32_t ip_dst;
static uint64_t mac_dst;
static uint16_t port_src;
static uint16_t port_dst;
static const char* cardname;


static uint64_t bytes = 0;
#ifdef BENCH
static uint64_t num_dequeue_tx = 0;
static uint64_t num_dequeue_rx = 0;
static uint64_t num_enqueue_tx = 0;
static uint64_t num_enqueue_rx = 0;
static uint64_t enq_s, enq_e;
static uint64_t deq_s, deq_e;
static uint64_t tot_enq_rx, tot_deq_rx;
static uint64_t tot_enq_tx, tot_deq_tx;
#endif
static uint64_t num_rx = 0;
static uint64_t start;
static uint64_t end;
static uint64_t tsc_per_ms;

static bool use_irq = false;

static void event_cb(void* queue)
{
    struct devq* q = (struct devq*) udp_q;

    errval_t err;

    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    err = SYS_ERR_OK;

    while (err == SYS_ERR_OK) {
#ifdef BENCH
        deq_s = rdtsc();
#endif
        err = devq_dequeue(q, &rid, &offset, &length, &valid_data,
                           &valid_length, &flags);

        if (err_is_fail(err)) {
            break;
        }

        if (flags & NETIF_TXFLAG) {
#ifdef BENCH
            deq_e = rdtsc();
            num_dequeue_tx++;
            tot_deq_tx += deq_e - deq_s;
#endif
            DEBUG("Received TX buffer back \n");
#ifdef BENCH
            enq_s = rdtsc();
#endif
            err = devq_enqueue(q, rid, offset, length, 0,
                               0, NETIF_RXFLAG);
            if (err_is_fail(err)) {
                break;
            }
#ifdef BENCH
            enq_e = rdtsc();
            num_enqueue_rx++;
            tot_enq_rx += enq_e - enq_s;
#endif
        } else if (flags & NETIF_RXFLAG) {
#ifdef BENCH
            deq_e = rdtsc();
            num_dequeue_rx++;
            tot_deq_rx += deq_e - deq_s;
#endif
            num_rx++;
            bytes += valid_length;
            DEBUG("Received RX buffer \n");
#ifdef BENCH
            enq_s = rdtsc();
#endif
            // TODO change to TX flag
            //printf("offset %lu lenght %lu valid_length %lu \n", offset, length, valid_length);
            err = devq_enqueue(q, rid, offset, length, 0,
                               valid_length, NETIF_TXFLAG | NETIF_TXFLAG_LAST);
            if (err_is_fail(err)) {
                break;
            }
#ifdef BENCH
            enq_e = rdtsc();
            tot_enq_tx += enq_e - enq_s;
            num_enqueue_tx++;
#endif
            if ((num_rx % 1000000) == 0) {
                end = rdtsc();
                double time = ((double) end-start)/(tsc_per_ms*1000);
                printf("Mbit/s %f during %f seconds \n", 
                      ((double)bytes*8)/(1000*1000*time), time);
                printf("Num packets/s %f \n", (double) 1000000/time);
#ifdef BENCH
                printf("AVG deq_rx %f micro seconds\n", ((double) (tot_deq_rx/num_dequeue_rx)/(tsc_per_ms/1000)));
                printf("AVG enq_rx %f micro seconds\n", ((double) (tot_enq_rx/num_enqueue_rx)/(tsc_per_ms/1000)));
                printf("AVG deq_tx %f micro seconds\n", ((double) (tot_deq_tx/num_dequeue_tx)/(tsc_per_ms/1000)));
                printf("AVG enq_tx %f micro seconds\n", ((double) (tot_enq_tx/num_enqueue_tx)/(tsc_per_ms/1000)));

                tot_deq_rx = 0;
                tot_deq_tx = 0;
                tot_enq_rx = 0;
                tot_enq_tx = 0;
                num_enqueue_rx = 0;
                num_enqueue_tx = 0;
                num_dequeue_rx = 0;
                num_dequeue_tx = 0;
#endif              
                num_rx = 0;     
                bytes = 0;
                start = rdtsc();
            }
        } else {
            printf("Unknown flags %lx \n", flags);
        }

    }
}

int main(int argc, char *argv[])
{
    if (argc > 5) {
        char* stop;
        ip_dst = atoi(argv[1]);
        mac_dst = strtoull(argv[2], &stop, 10);

        port_src = atoi(argv[3]);
        port_dst = atoi(argv[4]);
        cardname = argv[5];
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

    for (int j = 0; j < NUM_BUF; j++) {
        err = devq_enqueue(udp_q, regid_rx, j*RX_BUF_SIZE, RX_BUF_SIZE, 0, RX_BUF_SIZE,
                           NETIF_RXFLAG);
        if (err_is_fail(err)) {
            USER_PANIC("Err %s \n", err_getstring(err));
        }
    }

    err = sys_debug_get_tsc_per_ms(&tsc_per_ms);
    assert(err_is_ok(err));

    barrelfish_usleep(1000*1000*15);

    if (use_irq) {
        while (true) {
            event_dispatch(get_default_waitset());
        }
    } else {
        printf("Testing receiving UDP packets \n");
        start = rdtsc();
        while(true) {
            event_cb(udp_q);
        }
    }

}

