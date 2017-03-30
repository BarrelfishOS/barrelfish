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
#include <devif/backends/net/sfn5122f_devif.h>
#include <devif/backends/net/e10k_devif.h>
#include <devif/backends/descq.h>
#include <bench/bench.h>
#include <net_interfaces/flags.h>


//#define DEBUG(x...) printf("devif_test: " x)
#define DEBUG(x...) do {} while (0)

#define TX_BUF_SIZE 2048
#define RX_BUF_SIZE 2048
#define NUM_ENQ 512
#define NUM_RX_BUF 1024
#define NUM_ROUNDS_TX 16384
#define NUM_ROUNDS_RX 128
#define MEMORY_SIZE BASE_PAGE_SIZE*512

static char* card;

static struct capref memory_rx;
static struct capref memory_tx;
static regionid_t regid_rx;
static regionid_t regid_tx;
static struct frame_identity id;
static lpaddr_t phys_rx;
static lpaddr_t phys_tx;

static volatile uint32_t num_tx = 0;
static volatile uint32_t num_rx = 0;

static void* va_rx;
static void* va_tx;

struct direct_state {
    struct list_ele* first;
    struct list_ele* last;
};

struct list_ele{
    regionid_t rid;
    bufferid_t bid;
    lpaddr_t addr;
    size_t len;
    uint64_t flags;
   
    struct list_ele* next;
};


static struct waitset_chanstate *chan = NULL;
static struct waitset card_ws;


static uint8_t udp_header[8] = {
    0x07, 0xD0, 0x07, 0xD0,
    0x00, 0x80, 0x00, 0x00,
};

static void print_buffer(size_t len, bufferid_t bid)
{
   /*
    uint8_t* buf = (uint8_t*) va_rx+bid;
    printf("Packet in region %p at address %p len %zu \n",
           va_rx, buf, len);
    for (int i = 0; i < len; i++) {
        if (((i % 10) == 0) && i > 0) {
            printf("\n");
        }
        printf("%2X ", buf[i]);
    }
    printf("\n");
    */
}

static void wait_for_interrupt(void)
{
    errval_t err = event_dispatch(&card_ws);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in event_dispatch for wait_for_interrupt");
    }
}

static void event_cb(void* queue)
{
    struct devq* q = (struct devq*) queue;

    errval_t err;

    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    err = SYS_ERR_OK;

    while (err == SYS_ERR_OK) {
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
            print_buffer(valid_length, offset);
        } else {
            printf("Unknown flags %lx \n", flags);
        }
    }

    // MSIX is not working on so we have to "simulate interrupts"
    err = waitset_chan_register(&card_ws, chan,
                                MKCLOSURE(event_cb, queue));
    if (err_is_fail(err) && err_no(err) == LIB_ERR_CHAN_ALREADY_REGISTERED) {
        printf("Got actual interrupt?\n");
    }
    else if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can't register our dummy channel.");
    }
    err = waitset_chan_trigger(chan);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trigger failed.");
    }
}

static struct devq* create_net_queue(char* card_name)
{
    errval_t err;
    if (strcmp(card_name, "sfn5122f") == 0) {
        struct sfn5122f_queue* q;
        
        err = sfn5122f_queue_create(&q, event_cb, /* userlevel*/ true,
                                    /*MSIX interrupts*/ false);
        if (err_is_fail(err)){
            USER_PANIC("Allocating devq failed \n");
        }
        return (struct devq*) q;
    }

    if (strcmp(card_name, "e10k") == 0) {
        struct e10k_queue* q;
        
        err = e10k_queue_create(&q, event_cb, /*VFs */ false,
                                /*MSIX interrupts*/ false);
        if (err_is_fail(err)){
            USER_PANIC("Allocating devq failed \n");
        }
        return (struct devq*) q;
    }

    USER_PANIC("Unknown card name\n");

    return NULL;
}

static errval_t destroy_net_queue(struct devq* q, char* card_name)
{
    errval_t err;
    if (strcmp(card_name, "sfn5122f") == 0) {
        err = sfn5122f_queue_destroy((struct sfn5122f_queue*)q);
        if (err_is_fail(err)){
            USER_PANIC("Destroying devq failed \n");
        }
        return err;
    }

    if (strcmp(card_name, "e10k") == 0) {
        err = e10k_queue_destroy((struct e10k_queue*)q);
        if (err_is_fail(err)){
            USER_PANIC("Destroying devq failed \n");
        }
        return err;
    }

    USER_PANIC("Unknown card name\n");

    return SYS_ERR_OK;
}

static void test_net_tx(void)
{
    num_tx = 0;
    num_rx = 0;

    errval_t err;
    struct devq* q;
    

    q = create_net_queue(card);
    assert(q != NULL);

    waitset_init(&card_ws);

    // MSIX is not working on sfn5122f yet so we have to "simulate interrupts"
    chan = malloc(sizeof(struct waitset_chanstate));
    waitset_chanstate_init(chan, CHANTYPE_AHCI);

    err = waitset_chan_register(&card_ws, chan, MKCLOSURE(event_cb, q));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "waitset_chan_regster failed.");
    }

    err = waitset_chan_trigger(chan);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trigger failed.");
    }

    err = devq_register(q, memory_tx, &regid_tx);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }
    

    // write something into the buffers
    char* write = NULL;

    for (int i = 0; i < NUM_ENQ; i++) {
        write = va_tx + i*(TX_BUF_SIZE);
        for (int j = 0; j < 8; j++) {
            write[j] = udp_header[j];
        }
        for (int j = 8; j < TX_BUF_SIZE; j++) {
            write[j] = 'a';
        }
    }

    // Send something
    cycles_t t1 = bench_tsc();

    for (int z = 0; z < NUM_ROUNDS_TX; z++) {
        for (int i = 0; i < NUM_ENQ; i++) {
            err = devq_enqueue(q, regid_tx, i*(TX_BUF_SIZE), TX_BUF_SIZE,
                               0, TX_BUF_SIZE,
                               NETIF_TXFLAG | NETIF_TXFLAG_LAST);
            if (err_is_fail(err)){
                USER_PANIC("Devq enqueue failed \n");
            }
        }

        while(true) {
            if ((num_tx < NUM_ENQ)) {
                wait_for_interrupt();
            } else {
                break;
            }
        }
        num_tx = 0;
    }

    cycles_t t2 = bench_tsc();
    cycles_t result = (t2 -t1 - bench_tscoverhead());
 
    uint64_t sent_bytes = (uint64_t) TX_BUF_SIZE*NUM_ENQ*NUM_ROUNDS_TX;
    double result_ms = (double) bench_tsc_to_ms(result);
    double bw = sent_bytes / result_ms / 1000;
    
    printf("Write throughput %.2f [MB/s] for %.2f ms \n", bw, result_ms);

    
    err = devq_control(q, 1, 1, &sent_bytes);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq control failed \n");
    }

    err = devq_deregister(q, regid_tx, &memory_tx);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq deregister tx failed \n");
    }
    
    err = destroy_net_queue(q, card);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Destroying %s queue failed \n", card);
    }

    printf("SUCCESS: %s tx test ended\n", card);
}


static void test_net_rx(void)
{

    num_tx = 0;
    num_rx = 0;

    errval_t err;
    struct devq* q;
   
    q = create_net_queue(card);
    assert(q != NULL);

    waitset_init(&card_ws);

    // MSIX is not working on sfn5122f yet so we have to "simulate interrupts"
    chan = malloc(sizeof(struct waitset_chanstate));
    waitset_chanstate_init(chan, CHANTYPE_AHCI);

    err = waitset_chan_register(&card_ws, chan, MKCLOSURE(event_cb, q));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "waitset_chan_regster failed.");
    }

    err = waitset_chan_trigger(chan);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trigger failed.");
    }

    err = devq_register(q, memory_rx, &regid_rx);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }
    
    // Enqueue RX buffers to receive into
    for (int i = 0; i < NUM_ROUNDS_RX; i++){
        err = devq_enqueue(q, regid_rx, i*RX_BUF_SIZE, RX_BUF_SIZE,
                           0, RX_BUF_SIZE,
                           NETIF_RXFLAG);
        if (err_is_fail(err)){
            USER_PANIC("Devq enqueue failed: %s\n", err_getstring(err));
        }

    }

    while (true) {
        if ((num_rx < NUM_ROUNDS_RX)) {
            wait_for_interrupt();
        } else {
            break;
        }
    }


    err = devq_control(q, 1, 1, NULL);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq control failed \n");
    }

    err = devq_deregister(q, regid_rx, &memory_rx);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq deregister rx failed \n");
    }
   
    err = destroy_net_queue(q, card);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Destroying %s queue failed \n", card);
    }

    printf("SUCCESS: %s rx test ended\n", card);
}


static errval_t descq_notify(struct descq* q)
{
    errval_t err = SYS_ERR_OK;
    struct devq* queue = (struct devq*) q;
    
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    while(err_is_ok(err)) {
        err = devq_dequeue(queue, &rid, &offset, &length, &valid_data,
                           &valid_length, &flags);
        if (err_is_ok(err)){
            num_rx++;
        }
    }
    return SYS_ERR_OK;
}

static void test_idc_queue(void)
{
    num_tx = 0;
    num_rx = 0;

    errval_t err;
    struct devq* q;
    struct descq* queue;
    struct descq_func_pointer f;
    f.notify = descq_notify;
    
    debug_printf("Descriptor queue test started \n");
    err = descq_create(&queue, DESCQ_DEFAULT_SIZE, "test_queue",
                       false, true, true, NULL, &f);
    if (err_is_fail(err)){
        USER_PANIC("Allocating devq failed \n");
    }
   
    q = (struct devq*) queue;

    err = devq_register(q, memory_rx, &regid_rx);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }
  
    err = devq_register(q, memory_tx, &regid_tx);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }
 
    // Enqueue RX buffers to receive into
    for (int j = 0; j < 1000000; j++){
        for (int i = 0; i < 32; i++){
            err = devq_enqueue(q, regid_rx, i*2048, 2048, 
                               0, 2048, 0);
            if (err_is_fail(err)){
                // retry
                i--;
            } else {
                num_tx++;
            }
        }

        err = devq_notify(q);
        if (err_is_fail(err)) {
                USER_PANIC("Devq notify failed: %s\n", err_getstring(err));
        }
        event_dispatch(get_default_waitset());
    }    

    while(num_tx != num_rx) {
        event_dispatch(get_default_waitset());
    }

    err = devq_control(q, 1, 1, NULL);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq control failed \n");
    }

    err = devq_deregister(q, regid_rx, &memory_rx);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq deregister rx failed \n");
    }

    err = devq_deregister(q, regid_tx, &memory_tx);
    if (err_is_fail(err)){
        printf("%s \n", err_getstring(err));
        USER_PANIC("Devq deregister tx failed \n");
    }

    printf("SUCCESS: IDC queue\n");
}

int main(int argc, char *argv[])
{
    errval_t err;
    // Allocate memory
    err = frame_alloc(&memory_rx, MEMORY_SIZE, NULL);
    if (err_is_fail(err)){
        USER_PANIC("Allocating cap failed \n");
    }

    err = frame_alloc(&memory_tx, MEMORY_SIZE, NULL);
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

    phys_rx = id.base;

    // TX Frame
    err = invoke_frame_identify(memory_tx, &id);
    if (err_is_fail(err)) {
        USER_PANIC("Frame identify failed \n");
    }
   
    err = vspace_map_one_frame_attr(&va_tx, id.bytes, memory_tx,
                                    VREGION_FLAGS_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC("Frame mapping failed \n");
    }

    phys_tx = id.base;

    if (argc > 2) {
        card = argv[2];
        printf("Card =%s \n", card);
    } else {
        card = "e10k";
    }

    if (strcmp(argv[1], "net_tx") == 0) {
        test_net_tx();
    }

    if (strcmp(argv[1], "net_rx") == 0) {
        test_net_rx();
    }

    if (strcmp(argv[1], "idc") == 0) {
        test_idc_queue();
    }
   
    barrelfish_usleep(1000*1000*5);
}

