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
#include <devif/backends/debug.h>
#include <devif/backends/descq.h>
#include <devif/backends/null.h>
#include <bench/bench.h>
#include <net_interfaces/flags.h>

//#define BENCH

//#define DEBUG(x...) printf("devif_test: " x)
#define DEBUG(x...) do {} while (0)

#define BUF_SIZE 2048
#define NUM_BUFS 128
#define MEMORY_SIZE BUF_SIZE*NUM_BUFS

static struct capref memory;
static regionid_t regid;
static struct frame_identity id;
static lpaddr_t phys;

static volatile uint32_t num_tx = 0;
static volatile uint32_t num_rx = 0;

static void* va;

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

static struct descq* queue;
static struct debug_q* debug_q;
static struct null_q* null_q;
static struct devq* que;

static volatile bool enq[NUM_BUFS];

#ifdef BENCH
static uint64_t tot_deq = 0;
static uint64_t tot_enq = 0;
static uint64_t tot_notify = 0;
static uint64_t start_enq = 0, end_enq = 0;
static uint64_t start_deq = 0, end_deq = 0;
static uint64_t start_not = 0, end_not = 0;

static double avg_deq, avg_enq, avg_not;
static double avg_deq_d, avg_enq_d, avg_not_d;
static double avg_deq_n, avg_enq_n, avg_not_n;
#endif


static errval_t descq_notify(struct descq* q)
{
    errval_t err = SYS_ERR_OK;
    
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    while(err_is_ok(err)) {

#ifdef BENCH
        start_deq = rdtsc();
#endif
        err = devq_dequeue(que, &rid, &offset, &length, &valid_data,
                           &valid_length, &flags);
        if (err_is_ok(err)){
#ifdef BENCH
            end_deq = rdtsc();
            tot_deq += end_deq - start_deq;
#endif
            num_rx++;
            enq[offset/BUF_SIZE] = false;
        } 
    }
    return SYS_ERR_OK;
}

#define NUM_REGIONS 128

#define NUM_ROUNDS 1000000

static void test_register(void)
{
    errval_t err;
    struct capref regions[NUM_REGIONS];
    regionid_t rids[NUM_REGIONS];
    bool is_reg[NUM_REGIONS];

    for (int i = 0; i < NUM_REGIONS; i++) {
        err = frame_alloc(&regions[i], BASE_PAGE_SIZE, NULL);
        if (err_is_fail(err)){
            USER_PANIC("Allocating cap failed \n");
        }
        is_reg[i] = false;
    } 

    srand(rdtsc());
    int idx = 0;
    struct capref ret;
    for (int i = 0; i < NUM_ROUNDS/10; i++) {
        idx = rand() % NUM_REGIONS;   
        if (is_reg[idx]) {
            err = devq_deregister(que, rids[idx], &ret);
            if (err_is_fail(err)){
                USER_PANIC("Registering memory to devq failed: %s\n",
                           err_getstring(err));
            }
            is_reg[idx] = false;
        } else {
            err = devq_register(que, regions[idx], &rids[idx]);
            if (err_is_fail(err)){
                USER_PANIC("Registering memory to devq failed: %s \n",
                            err_getstring(err));
            }
            is_reg[idx] = true;
        }
    }
    
    for (int i = 0; i < NUM_REGIONS; i++) {
        if (is_reg[i]) {
            err = devq_deregister(que, rids[i], &ret);
            if (err_is_fail(err)){
                USER_PANIC("Registering memory to devq failed: %s\n",
                           err_getstring(err));
            }
        }
    }
}

static void test_enqueue_dequeue(void)
{
    errval_t err;
    num_tx = 0;
    num_rx = 0;
    // enqueue from the beginning of the region
    for (int i = 0; i < NUM_BUFS/8; i++) {
        err = devq_enqueue(que, regid, i*BUF_SIZE, BUF_SIZE, 
                           0, BUF_SIZE, 0);
        if (err_is_fail(err)){
            USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
        } else {
            num_tx++;
        }
    }


    // enqueue from the end of the region
    for (int i = 0; i < NUM_BUFS/8; i++) {
        err = devq_enqueue(que, regid, MEMORY_SIZE-((i+1)*BUF_SIZE), 
                           BUF_SIZE, 0, BUF_SIZE, 0);
        if (err_is_fail(err)){
            USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
        } else {
            num_tx++;
        }
    }
 
    err = devq_notify(que);
    if (err_is_fail(err)) {
        USER_PANIC("Devq notify failed: %s\n", err_getstring(err));
    }

    while(num_rx < (NUM_BUFS/4)) {
        event_dispatch(get_default_waitset());
    }
}


static void test_failures(void)
{
    errval_t err;
    num_tx = 0;
    num_rx = 0;


    err = devq_enqueue(que, regid, 0, BUF_SIZE, 
                       0, BUF_SIZE, 0);
    if (err_is_fail(err)) {
        USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
    }

    // do the same enqueue again

    err = devq_enqueue(que, regid, 0, BUF_SIZE, 
                       0, BUF_SIZE, 0);
    if (err_is_ok(err)) {
        USER_PANIC("Enqueue should fail! \n");
    }

    // revert to original state
    err = devq_notify(que);
    if (err_is_fail(err)) {
        USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
    }

    while(num_rx < 1) {
        event_dispatch(get_default_waitset());
    }

    err = devq_enqueue(que, regid, 4096, BUF_SIZE, 
                       0, BUF_SIZE, 0);
    if (err_is_fail(err)) {
        USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
    }

    // do an overlapping enqueue
    err = devq_enqueue(que, regid, 5120, BUF_SIZE, 
                       0, BUF_SIZE, 0);
    if (err_is_ok(err)) {
        USER_PANIC("Enqueue should fail! \n");
    }

    // revert to original state
    err = devq_notify(que);
    if (err_is_fail(err)) {
        USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
    }

    while(num_rx < 2) {
        event_dispatch(get_default_waitset());
    }

    // enqueue buffer not in region
    err = devq_enqueue(que, regid, MEMORY_SIZE+BUF_SIZE, BUF_SIZE, 
                       0, BUF_SIZE, 0);
    if (err_is_ok(err)) {
        USER_PANIC("Enqueue should fail! \n");
    }

}


static void test_randomized_test(void)
{
    errval_t err;
    num_tx = 0;
    num_rx = 0;
    memset((void*)enq, 0, sizeof(bool)*NUM_BUFS);

#ifdef BENCH
    tot_enq = 0;
    tot_deq = 0;
    tot_notify = 0;
#endif    

    for (int i = 0; i < NUM_BUFS; i++) {
        enq[i] = false;
    }

    srand(rdtsc());
    int idx = 0;
    // enqueue from the beginning of the region
    for (int i = 0; i < NUM_ROUNDS; i++) {
        for (int j = 0; j < NUM_BUFS/2; j++) {
            idx = rand() % NUM_BUFS;
            while (enq[idx]) {
                idx = rand() % NUM_BUFS;
            }            

#ifdef BENCH
            start_enq = rdtsc();
#endif
            err = devq_enqueue(que, regid, idx*BUF_SIZE, BUF_SIZE, 
                               0, BUF_SIZE, 0);
            if (err_is_fail(err)){
                USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
            } else {
#ifdef BENCH
                end_enq = rdtsc();
                tot_enq += end_enq - start_enq;
#endif
                enq[idx] = true;
                num_tx++;
            }
        }

        if ((i % 100000) == 0) {
            printf("Round %d \n", i);
        }

#ifdef BENCH
        start_not = rdtsc();
#endif
        err = devq_notify(que);
        if (err_is_fail(err)) {
            USER_PANIC("Devq notify failed: %s\n", err_getstring(err));
        }
#ifdef BENCH
        end_not = rdtsc();
        tot_notify += end_not - start_not;
#endif
        while(num_rx < ((i+1)*NUM_BUFS/2)) {
            event_dispatch(get_default_waitset());
        }
    }

}

int main(int argc, char *argv[])
{
    errval_t err;
    // Allocate memory
    err = frame_alloc(&memory, MEMORY_SIZE, NULL);
    if (err_is_fail(err)){
        USER_PANIC("Allocating cap failed \n");
    }
    
    // RX frame
    err = invoke_frame_identify(memory, &id);
    if (err_is_fail(err)) {
        USER_PANIC("Frame identify failed \n");
    }

    err = vspace_map_one_frame_attr(&va, id.bytes, memory,
                                    VREGION_FLAGS_READ, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC("Frame mapping failed \n");
    }

    phys = id.base;

    struct descq_func_pointer f;
    f.notify = descq_notify;
    
    debug_printf("Descriptor queue test started \n");
    err = descq_create(&queue, DESCQ_DEFAULT_SIZE, "test_queue",
                       false, true, true, NULL, &f);
    if (err_is_fail(err)){
        USER_PANIC("Allocating devq failed \n");
    }
 
    // stack debug queue on top
    err = debug_create(&debug_q, (struct devq*) queue);
    if (err_is_fail(err)) {
        USER_PANIC("Allocating debug q failed \n");
    }
  
    // stack null queue on top
    err = null_create(&null_q, (struct devq*) debug_q);
    if (err_is_fail(err)) {
        USER_PANIC("Allocating null q failed \n");
    }

    que = (struct devq*) debug_q;

    err = devq_register(que, memory, &regid);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    printf("Starting failure handling test \n");
    test_failures();

    printf("Starting register/deregister test \n");  
    test_register();

    printf("Starting enqueue/dequeue test \n");
    test_enqueue_dequeue();

    printf("Starting randomized test debug\n");
    que = (struct devq*) debug_q;
    test_randomized_test();

#ifdef BENCH
    avg_enq_d = ((double) tot_deq)/(NUM_ROUNDS*NUM_BUFS/2);
    avg_deq_d = ((double) tot_enq)/(NUM_ROUNDS*NUM_BUFS/2);
    avg_not_d = ((double) tot_notify)/NUM_ROUNDS;

    printf("AVG deq debug %f \n", avg_enq_d);
    printf("AVG enq debug %f \n", avg_deq_d);
    printf("AVG notify debug %f \n", avg_not_d);
    printf("############################################################ \n");


    err = devq_deregister(que, regid, &memory);
    if (err_is_fail(err)){
        USER_PANIC("Deregistering memory from devq failed: %s \n",
                   err_getstring(err));
    }

    printf("Starting randomized test non debug\n");
    que = (struct devq*) queue;
    
    err = devq_register(que, memory, &regid);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    test_randomized_test();

    avg_enq = ((double) tot_deq)/(NUM_ROUNDS*NUM_BUFS/2);
    avg_deq = ((double) tot_enq)/(NUM_ROUNDS*NUM_BUFS/2);
    avg_not = ((double) tot_notify)/NUM_ROUNDS;

    printf("AVG deq %f \n", avg_enq);
    printf("AVG enq %f \n", avg_deq);
    printf("AVG notify %f \n", avg_not);
    printf("############################################################ \n");

    err = devq_deregister(que, regid, &memory);
    if (err_is_fail(err)){
        USER_PANIC("Deregistering memory from devq failed: %s \n",
                   err_getstring(err));
    }

    printf("Starting randomized test debug + null\n");
    que = (struct devq*) null_q;

    err = devq_register(que, memory, &regid);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    test_randomized_test();

    avg_enq_n = ((double) tot_deq)/(NUM_ROUNDS*NUM_BUFS/2);
    avg_deq_n = ((double) tot_enq)/(NUM_ROUNDS*NUM_BUFS/2);
    avg_not_n = ((double) tot_notify)/NUM_ROUNDS;

    printf("AVG deq debug + null %f \n", avg_enq_n);
    printf("AVG enq debug + null %f \n", avg_deq_n);
    printf("AVG notify debug + null %f \n", avg_not_n);

    printf("############################################################ \n");

    printf("AVG enq overhead null %f \n", avg_enq_n - avg_enq_d);
    printf("AVG deq overhead null %f \n", avg_deq_n - avg_deq_d);
    printf("AVG notify overhead null %f \n", avg_not_n - avg_not_d);

    printf("############################################################ \n");

    printf("AVG enq overhead debug %f \n", avg_enq_d-avg_enq);
    printf("AVG deq overhead debug %f \n", avg_deq_d - avg_deq);
    printf("AVG notify overhead debug %f \n", avg_not_d - avg_not);
#endif

    err = devq_deregister(que, regid, &memory);
    if (err_is_fail(err)){
        USER_PANIC("Deregistering memory from devq failed: %s \n",
                   err_getstring(err));
    }

    printf("SUCCESS \n");
}

