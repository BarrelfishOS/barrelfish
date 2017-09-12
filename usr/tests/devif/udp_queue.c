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
#include <devif/queue_interface.h>
#include <devif/backends/net/udp.h>
#include <bench/bench.h>
#include <net_interfaces/flags.h>
#include <net/net_filter.h>

//#define DEBUG(x...) printf("devif_test: " x)
#define DEBUG(x...) do {} while (0)


#define NUM_ENQ 512
#define MEMORY_SIZE BASE_PAGE_SIZE*NUM_ENQ
#define TX_BUF_SIZE 2048
#define RX_BUF_SIZE 2048

static uint32_t ip_dst;
static uint64_t mac_dst;
static uint16_t port_src;
static uint16_t port_dst;

static struct capref memory_rx;
static struct capref memory_tx;
static regionid_t regid_rx;
static regionid_t regid_tx;
static struct frame_identity id;
static void* va_rx;
static void* va_tx;

static uint32_t num_tx = 0;
static uint32_t num_rx = 0;
static struct udp_q* udp_q;
static const char* cardname;

/*
static void wait_for_interrupt(void)
{
    uint32_t tx = num_tx;

    while(tx == num_tx) {
        errval_t err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch for wait_for_interrupt");
        }
    }
}
*/

static uint64_t total_rx = 0;
static bool reg_done = false;

static bool use_interrupts = false;


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

    uint64_t start = 0, end = 0;

    if (!reg_done) {
        return;
    }

    while (err == SYS_ERR_OK) {
        start = rdtsc();
        err = devq_dequeue(q, &rid, &offset, &length, &valid_data,
                           &valid_length, &flags);
        if (err_is_fail(err)) {
            break;
        }

        if (flags & NETIF_TXFLAG) {
            DEBUG("Received TX buffer back \n");
            num_tx++;
        } else if (flags & NETIF_RXFLAG) {
            num_rx++;
            DEBUG("Received RX buffer \n");
            err = devq_enqueue(q, rid, offset, length, 0,
                               0, NETIF_RXFLAG);
            end = rdtsc();
            total_rx += end - start;
        } else {
            printf("Unknown flags %lx \n", flags);
        }
    }
}

static void test_udp(void)
{
    errval_t err;
    struct devq* q;
   
    // create queue with interrupts
    udp_create(&udp_q, cardname, port_src, port_dst, 
               ip_dst, event_cb, !use_interrupts);

    q = (struct devq*) udp_q;

    assert(q != NULL);

    num_tx = 0;
    num_rx = 0;
    err = devq_register(q, memory_rx, &regid_rx);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    // inesrt buffers
    for (int i = 0; i < NUM_ENQ; i++) {
        err = devq_enqueue(q, regid_rx, i*(RX_BUF_SIZE), RX_BUF_SIZE,
                           0, RX_BUF_SIZE,
                           NETIF_RXFLAG);
        if (err_is_fail(err)){
            USER_PANIC("Devq enqueue RX buffer failed \n");
        }
    }

    
    err = devq_register(q, memory_tx, &regid_tx);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    reg_done = true;
    // write something into the buffers
    char* data = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    printf("Data length %zu \n", strlen(data));

    for (int i = 0; i < NUM_ENQ; i++) {
        udp_write_buffer(udp_q, regid_tx, i*(TX_BUF_SIZE), 
                         data, strlen(data));
    }

    uint64_t total = 0, start = 0, end = 0;
    for (int z = 0; z < NUM_ENQ; z++) {

        start = rdtsc();
        for (int i = 0; i < NUM_ENQ; i++) {
            err = devq_enqueue(q, regid_tx, i*(TX_BUF_SIZE), TX_BUF_SIZE,
                               0, strlen(data), NETIF_TXFLAG | NETIF_TXFLAG_LAST);
            if (err_is_fail(err)){
                USER_PANIC("Devq enqueue failed \n");
            }
        }

        while (num_tx < NUM_ENQ*z) {
            if (use_interrupts) {
                event_dispatch(get_default_waitset());
            } else {
                event_cb(q);
            }
        }

        end = rdtsc();
        total += end - start;
    }
    printf("Average %f cycles TX\n", (double)total/(NUM_ENQ*NUM_ENQ));
    barrelfish_usleep(1000*1000);
    printf("Testing receiving UDP packets \n");

    for (int z = 0; z < NUM_ENQ; z++) {
        while(num_rx < NUM_ENQ*z) {
            if (use_interrupts) {
                event_dispatch(get_default_waitset());
            } else {
                event_cb(q);
            }
        }
    }

    printf("Average %f cycles RX\n", (double)total_rx/(NUM_ENQ*NUM_ENQ));

    err = devq_deregister(q, regid_rx, &memory_rx);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq deregister tx failed \n");
    }
 
    err = devq_deregister(q, regid_tx, &memory_tx);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq deregister tx failed \n");
    }
 
    printf("Receiving UDP packets done \n");
    printf("SUCCESS: udp test ended \n");
}

int main(int argc, char *argv[])
{
    errval_t err;
    // Allocate memory
    err = frame_alloc(&memory_tx, MEMORY_SIZE, NULL);
    if (err_is_fail(err)){
        USER_PANIC("Allocating cap failed \n");
    }

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
                                    VREGION_FLAGS_READ, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC("Frame mapping failed \n");
    }

    // RX frame
    err = invoke_frame_identify(memory_tx, &id);
    if (err_is_fail(err)) {
        USER_PANIC("Frame identify failed \n");
    }

    err = vspace_map_one_frame_attr(&va_tx, id.bytes, memory_tx,
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC("Frame mapping failed \n");
    }

    if (argc > 5) {
        char* end;
        ip_dst = atoi(argv[1]);
        mac_dst = strtoull(argv[2], &end, 10);
        port_src = atoi(argv[3]);
        port_dst = atoi(argv[4]);
        cardname = argv[5];
    } else {
        USER_PANIC("NO src or dst IP given \n");
    }

    barrelfish_usleep(1000*1000*15);

    test_udp();
}

