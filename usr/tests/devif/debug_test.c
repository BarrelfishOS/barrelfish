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
#include <bench/bench.h>
#include <net_interfaces/flags.h>


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

struct devq* que;

static volatile bool enq[NUM_BUFS];

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
        err = devq_dequeue(que, &rid, &offset, &length, &valid_data,
                           &valid_length, &flags);
        if (err_is_ok(err)){
            num_rx++;
            enq[offset/BUF_SIZE] = false;
        } 
    }
    return SYS_ERR_OK;
}

#define NUM_REGIONS 32

#define NUM_ROUNDS 10000

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
    for (int i = 0; i < NUM_ROUNDS; i++) {
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

    while(num_rx < 1) {
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
    
    srand(rdtsc());
    int idx = 0;
    // enqueue from the beginning of the region
    for (int i = 0; i < 1000000; i++) {
        for (int j = 0; j < NUM_BUFS/2; j++) {
            idx = rand() % NUM_BUFS;
            while (enq[idx]) {
                idx = rand() % NUM_BUFS;
            }            

            err = devq_enqueue(que, regid, idx*BUF_SIZE, BUF_SIZE, 
                               0, BUF_SIZE, 0);
            if (err_is_fail(err)){
                USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
            } else {
                enq[idx] = true;
                num_tx++;
            }
        }

        if ((i % 100000) == 0) {
            printf("Round %d \n", i);
        }

        err = devq_notify(que);
        if (err_is_fail(err)) {
            USER_PANIC("Devq notify failed: %s\n", err_getstring(err));
        }

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

    struct descq* queue;
    struct debug_q* debug_q;
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

    printf("Starting randomized test \n");
    test_randomized_test();

    err = devq_deregister(que, regid, &memory);
    if (err_is_fail(err)){
        USER_PANIC("Deregistering memory from devq failed: %s \n",
                   err_getstring(err));
    }

    printf("SUCCESS \n");
}

