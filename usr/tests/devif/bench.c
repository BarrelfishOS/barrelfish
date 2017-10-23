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
//#include <time.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/deferred.h>
#include <devif/queue_interface.h>
#include <devif/backends/loopback_devif.h>
#include <devif/backends/debug.h>
#include <devif/backends/null.h>
#include <bench/bench.h>
#include <vfs/vfs.h>

//#define DEBUG(x...) printf("devif_test: " x)
#define DEBUG(x...) do {} while (0)

#define BUF_SIZE 2048
#define NUM_BUFS 128
#define MEMORY_SIZE BUF_SIZE*NUM_BUFS

#define NUM_REGIONS 128
#define NUM_ROUNDS 100000

static struct capref memory;
static regionid_t regid;
static struct frame_identity id;
static lpaddr_t phys;

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

static struct loopback_queue* queue;
static struct debug_q* debug_q;
static struct null_q* null_q;
static struct null_q* all_q;
static struct devq* que;

static cycles_t tot_deq = 0;
static cycles_t tot_enq = 0;
static cycles_t tot_reg = 0;
static cycles_t tot_dereg = 0;
static cycles_t start_enq = 0, end_enq = 0;
static cycles_t start_deq = 0, end_deq = 0;
static cycles_t start_reg = 0, end_reg = 0;
static cycles_t start_dereg = 0, end_dereg = 0;

static double avg_deq, avg_enq;
static double avg_reg, avg_dereg;
static double avg_deq_d, avg_enq_d;
static double avg_reg_d, avg_dereg_d;
static double avg_deq_n, avg_enq_n;
static double avg_reg_n, avg_dereg_n;
static double avg_deq_tot, avg_enq_tot;
static double avg_reg_tot, avg_dereg_tot;

static bench_ctl_t *ctl_tmp_en;
static bench_ctl_t *ctl_tmp_de;
static bench_ctl_t *ctl_tmp_reg;
static bench_ctl_t *ctl_tmp_dereg;

static uint64_t tscperus;
static char* machine_name;

static void dump_results_nfs(char* filename)
{
    errval_t err;
    char buffer[256];
    vfs_handle_t handle;
    sprintf(buffer, "/nfs/%s/%s.csv", machine_name, filename);
    printf("%s \n", buffer);
    err = vfs_open(buffer, &handle);
    assert(err_is_ok(err));

    size_t bytes;
    // first 10 % is warmup
    for (int i = NUM_ROUNDS/10 ; i < NUM_ROUNDS; i++) {
        sprintf(buffer, "enqueue,%lu \ndequeue,%lu \nregister,%lu \nderegister,%lu \n", 
                ctl_tmp_en->data[i], ctl_tmp_de->data[i], ctl_tmp_reg->data[i], 
                ctl_tmp_dereg->data[i]);
        err = vfs_write(handle, buffer, strlen(buffer), &bytes);
        assert(err_is_ok(err));
        assert(bytes == strlen(buffer));
    }

    err = vfs_close(handle);
    assert(err_is_ok(err));

    bench_ctl_destroy(ctl_tmp_dereg);
    bench_ctl_destroy(ctl_tmp_reg);
    bench_ctl_destroy(ctl_tmp_de);
    bench_ctl_destroy(ctl_tmp_en);
}

static void test_register(void)
{
    errval_t err;
    struct capref regions[NUM_REGIONS];
    regionid_t rids[NUM_REGIONS];

    tot_reg = 0;
    tot_dereg = 0;

    ctl_tmp_reg = calloc(1, sizeof(*ctl_tmp_reg));
    ctl_tmp_reg->mode = BENCH_MODE_FIXEDRUNS;
    ctl_tmp_reg->result_dimensions = 1;
    ctl_tmp_reg->min_runs = NUM_ROUNDS;
    ctl_tmp_reg->data = calloc(ctl_tmp_reg->min_runs * ctl_tmp_reg->result_dimensions,
                       sizeof(*ctl_tmp_de->data));

    ctl_tmp_dereg = calloc(1, sizeof(*ctl_tmp_dereg));
    ctl_tmp_dereg->mode = BENCH_MODE_FIXEDRUNS;
    ctl_tmp_dereg->result_dimensions = 1;
    ctl_tmp_dereg->min_runs = NUM_ROUNDS;
    ctl_tmp_dereg->data = calloc(ctl_tmp_dereg->min_runs * ctl_tmp_dereg->result_dimensions,
                       sizeof(*ctl_tmp_dereg->data));

    for (int i = 0; i < NUM_REGIONS; i++) {
        err = frame_alloc(&regions[i], BASE_PAGE_SIZE, NULL);
        if (err_is_fail(err)){
            USER_PANIC("Allocating cap failed \n");
        }
    } 

    srand(rdtsc());
    int idx = 0;
    struct capref ret;
    cycles_t res1;
    cycles_t res2;
    for (int i = 0; i < NUM_ROUNDS; i++) {
        idx = i % NUM_REGIONS;
        start_reg = bench_tsc();
        err = devq_register(que, regions[idx], &rids[idx]);
        if (err_is_fail(err)){
            USER_PANIC("Registering memory to devq failed: %s \n",
                        err_getstring(err));
        }
        end_reg = bench_tsc();
        tot_reg += end_reg - start_reg;
        res1 = end_reg - start_reg;

        start_dereg = bench_tsc();
        err = devq_deregister(que, rids[idx], &ret);
        if (err_is_fail(err)){
            USER_PANIC("Registering memory to devq failed: %s\n",
                       err_getstring(err));
        }

        end_dereg = bench_tsc();
        tot_dereg += end_dereg - start_dereg;
        res2 = end_dereg - start_dereg;
        bench_ctl_add_run(ctl_tmp_reg, &res1);
        bench_ctl_add_run(ctl_tmp_dereg, &res2);
    }

    for (int i = 0; i < NUM_REGIONS; i++) {
        err = cap_destroy(regions[i]);
        if (err_is_fail(err)){
            USER_PANIC("Destroy region failed: %s\n",
                       err_getstring(err));
        }
    }

    bench_ctl_dump_analysis(ctl_tmp_dereg, 0, "deregister", tscperus);
    bench_ctl_dump_analysis(ctl_tmp_reg, 0, "register", tscperus);

}

static void test_randomized_test(void)
{
    errval_t err;
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    tot_enq = 0;
    tot_deq = 0;

    ctl_tmp_en = calloc(1, sizeof(*ctl_tmp_en));
    ctl_tmp_en->mode = BENCH_MODE_FIXEDRUNS;
    ctl_tmp_en->result_dimensions = 1;
    ctl_tmp_en->min_runs = NUM_ROUNDS;
    ctl_tmp_en->data = calloc(ctl_tmp_en->min_runs * ctl_tmp_en->result_dimensions,
                       sizeof(*ctl_tmp_de->data));

    ctl_tmp_de = calloc(1, sizeof(*ctl_tmp_en));
    ctl_tmp_de->mode = BENCH_MODE_FIXEDRUNS;
    ctl_tmp_de->result_dimensions = 1;
    ctl_tmp_de->min_runs = NUM_ROUNDS;
    ctl_tmp_de->data = calloc(ctl_tmp_de->min_runs * ctl_tmp_de->result_dimensions,
                       sizeof(*ctl_tmp_de->data));

    srand(rdtsc());
    int idx = 0;
    cycles_t res;
    // enqueue from the beginning of the region
    for (int i = 0; i < NUM_ROUNDS; i++) {

        idx = i % NUM_BUFS;
        start_enq = rdtsc();
        
        err = devq_enqueue(que, regid, idx*BUF_SIZE, BUF_SIZE, 
                           0, BUF_SIZE, 0);
        if (err_is_fail(err)){
            USER_PANIC("Enqueue failed: %s \n", err_getstring(err));
        } else {
            end_enq = rdtsc();
            tot_enq += end_enq - start_enq;
            res = end_enq - start_enq;
            bench_ctl_add_run(ctl_tmp_de, &res);
        }

        start_deq = rdtsc();
        err = devq_dequeue(que, &rid, &offset, &length, &valid_data,
                           &valid_length, &flags);
        if (err_is_ok(err)){
            end_deq = rdtsc();
            tot_deq += end_deq - start_deq;
            res = end_deq - start_deq;
            bench_ctl_add_run(ctl_tmp_en, &res);
        } else {
            USER_PANIC("Dequeue failed: %s \n", err_getstring(err));
        } 
    }

    bench_ctl_dump_analysis(ctl_tmp_de, 0, "enqueue", tscperus);
    bench_ctl_dump_analysis(ctl_tmp_en, 0, "dequeue", tscperus);
}

int main(int argc, char *argv[])
{
    
    if (argc > 1) {
        machine_name = argv[1];
    } else {
        machine_name = "default";
    }

    errval_t err;
    
    // mount_vfs
    vfs_init();
   
    char fname[256];
    err = vfs_mount("/nfs", "nfs://10.110.4.4/mnt/local/nfs/haeckir");
    if(err_is_fail(err)) {
        USER_PANIC("vfs_mount: %s \n", err_getstring(err));
    }

    vfs_handle_t handle;
    sprintf(fname, "/nfs/%s", machine_name);
    err = vfs_mkdir(fname);
    if (err_is_fail(err)) {
        printf("Folder %s already exists \n", fname);
    } else {
        printf("Creating folder %s \n", fname);
    }

    sprintf(fname, "/nfs/%s/debug.csv", machine_name);
    err = vfs_create(fname, &handle);
    assert(err_is_ok(err));

    sprintf(fname, "/nfs/%s/loopback.csv", machine_name);
    err = vfs_create(fname, &handle);
    assert(err_is_ok(err));


    sprintf(fname, "/nfs/%s/null.csv", machine_name);
    err = vfs_create(fname, &handle);
    assert(err_is_ok(err));


    sprintf(fname, "/nfs/%s/null_debug.csv", machine_name);
    err = vfs_create(fname, &handle);
    assert(err_is_ok(err));

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
    
    debug_printf("Descriptor queue test started \n");
    err = loopback_queue_create(&queue);
    if (err_is_fail(err)){
        USER_PANIC("Allocating devq failed \n");
    }
 
    // stack debug queue on top
    err = debug_create(&debug_q, (struct devq*) queue);
    if (err_is_fail(err)) {
        USER_PANIC("Allocating debug q failed \n");
    }
  
    // stack null queue on top
    err = null_create(&null_q, (struct devq*) queue);
    if (err_is_fail(err)) {
        USER_PANIC("Allocating null q failed \n");
    }

    // stack null queue on top of debug queue
    err = null_create(&all_q, (struct devq*) debug_q);
    if (err_is_fail(err)) {
        USER_PANIC("Allocating null q failed \n");
    }


    que = (struct devq*) queue;

    err = devq_register(que, memory, &regid);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    printf("Starting randomized queue\n");
    que = (struct devq*) queue;

    test_register();

    test_randomized_test();

    dump_results_nfs("loopback");
    avg_enq = ((double) tot_deq)/NUM_ROUNDS;
    avg_deq = ((double) tot_enq)/NUM_ROUNDS;
    avg_dereg = ((double) tot_dereg)/NUM_ROUNDS;
    avg_reg = ((double) tot_reg)/NUM_ROUNDS;
/*
    printf("AVG enq %f \n", avg_enq);
    printf("AVG deq %f \n", avg_deq);
    printf("AVG reg %f \n", avg_reg);
    printf("AVG dereg %f \n", avg_dereg);
*/
    printf("############################################################ \n");

    err = devq_deregister(que, regid, &memory);
    if (err_is_fail(err)){
        USER_PANIC("Deregistering memory from devq failed: %s \n",
                   err_getstring(err));
    }

    printf("Starting randomized test debug\n");
    que = (struct devq*) debug_q;
    
    test_register();

    err = devq_register(que, memory, &regid);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    test_randomized_test();

    dump_results_nfs("debug");
    avg_enq_d = ((double) tot_deq)/NUM_ROUNDS;
    avg_deq_d = ((double) tot_enq)/NUM_ROUNDS;
    avg_dereg_d  = ((double) tot_dereg)/NUM_ROUNDS;
    avg_reg_d = ((double) tot_reg)/NUM_ROUNDS;
/*
    printf("AVG enq debug %f \n", avg_enq_d);
    printf("AVG deq debug %f \n", avg_deq_d);
    printf("AVG reg debug %f \n", avg_reg_d);
    printf("AVG dereg debug %f \n", avg_dereg_d);
*/
    printf("############################################################ \n");

    err = devq_deregister(que, regid, &memory);
    if (err_is_fail(err)){
        USER_PANIC("Deregistering memory from devq failed: %s \n",
                   err_getstring(err));
    }

    printf("Starting randomized test null\n");
    que = (struct devq*) null_q;

    err = devq_register(que, memory, &regid);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    test_register();

    test_randomized_test();

    dump_results_nfs("null");
    avg_enq_n = ((double) tot_enq)/NUM_ROUNDS;
    avg_deq_n = ((double) tot_deq)/NUM_ROUNDS;
    avg_reg_n = ((double) tot_reg)/NUM_ROUNDS;
    avg_dereg_n = ((double) tot_dereg)/NUM_ROUNDS;

    printf("############################################################ \n");

    err = devq_deregister(que, regid, &memory);
    if (err_is_fail(err)){
        USER_PANIC("Deregistering memory from devq failed: %s \n",
                   err_getstring(err));
    }

    printf("Starting randomized test all queues\n");
    que = (struct devq*) all_q;

    test_register();

    err = devq_register(que, memory, &regid);
    if (err_is_fail(err)){
        USER_PANIC("Registering memory to devq failed \n");
    }

    test_randomized_test();

    dump_results_nfs("null_debug");
    avg_enq_tot = ((double) tot_enq)/NUM_ROUNDS;
    avg_deq_tot = ((double) tot_deq)/NUM_ROUNDS;
    avg_reg_tot = ((double) tot_reg)/NUM_ROUNDS;
    avg_dereg_tot = ((double) tot_dereg)/NUM_ROUNDS;

/*
    printf("AVG enq null %f \n", avg_enq_n);
    printf("AVG deq null %f \n", avg_deq_n);
    printf("AVG reg null %f \n", avg_reg_n);
    printf("AVG dereg null %f \n", avg_dereg_n);
    printf("############################################################ \n");

    printf("AVG enq overhead null %f \n", avg_enq_n - avg_enq);
    printf("AVG deq overhead null %f \n", avg_deq_n - avg_deq);
    printf("AVG reg overhead null %f \n", avg_reg_n - avg_reg);
    printf("AVG dereg overhead null %f \n", avg_dereg_n - avg_dereg);

    printf("############################################################ \n");

    printf("AVG enq overhead debug %f \n", avg_enq_d - avg_enq);
    printf("AVG deq overhead debug %f \n", avg_deq_d - avg_deq);
    printf("AVG reg overhead debug %f \n", avg_reg_d - avg_reg);
    printf("AVG dereg overhead debug %f \n", avg_dereg_d - avg_dereg);
*/
    err = devq_deregister(que, regid, &memory);
    if (err_is_fail(err)){
        USER_PANIC("Deregistering memory from devq failed: %s \n",
                   err_getstring(err));
    }
    printf("SUCCESS! \n");;

}

