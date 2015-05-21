/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <sys/time.h>

#include <sched.h> /* CPU_SET et. al. */
#include <pthread.h>

#ifdef BARRELFISH
#include <barrelfish/barrelfish.h>
#include <octopus/octopus.h>
#else
#define MAX_COREID 1024
#define USER_PANIC_ERR(x...)
#define USER_PANIC(x...)
#define err_is_fail(x) false
typedef size_t errval_t;
#endif

#define NUM_INCR 1000000
static size_t sanity = 0;
static size_t cpu_count = 4;

static void* prj_thread(void* param)
{
#if BARRELFISH
    printf("Hello from thread %"PRIu64" on core %"PRIuCOREID"\n",
           (uint64_t)param, disp_get_core_id());
#endif
    sanity = 1;
    //messages_handler_loop();
    return 0;
}

static int pthread_create_join_test(void) {
    pthread_t tid;
    pthread_attr_t attr;
    pthread_attr_init(&attr);

    for (size_t rep = 0; rep < 20; rep++) {
        printf("Create a new thread rep=%zu\n", rep);
        int rv = pthread_create(&tid, &attr, prj_thread, NULL);
        if (rv){
            printf("[ERROR] return code from pthread_create() is %d\n", rv);
            return 1;
        }

        /* wait for threads to finish */
        pthread_join(tid, NULL);

        assert(sanity == 1);
        sanity = 0;
        printf("Joined thread.\n");
    }

    printf("%s PASS\n", __FUNCTION__);
    return 0;
}

static int pthread_setaffinity_test(void) {

    pthread_t tid;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    cpu_set_t set;

    for (size_t rep = 0; rep < cpu_count; rep++) {
        printf("Create a new thread on core %zu\n", rep);
        sanity = 0;

        CPU_ZERO(&set);
        CPU_SET(rep, &set);
        pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &set);
        int rv = pthread_create(&tid, &attr, prj_thread, NULL);
        if (rv){
            printf("[ERROR] return code from pthread_create() is %d\n", rv);
            return 1;
        }

        /* wait for threads to finish */
        pthread_join(tid, NULL);
        //assert(sanity == 1);
    }

    printf("%s PASS\n", __FUNCTION__);
    return 0;
}

static pthread_mutex_t lock;

static size_t atomic_counter;
static void* mutex_increment(void* param)
{
    struct timeval start;
    struct timeval end;

    gettimeofday(&start, NULL);
    for (size_t i=0; i < NUM_INCR; i++) {
        pthread_mutex_lock(&lock);
        if (atomic_counter % 10000 == 0) {
            debug_printf("progress\n");
        }
        atomic_counter++;
        pthread_mutex_unlock(&lock);
    }
    gettimeofday(&end, NULL);

    assert(atomic_counter >= NUM_INCR);
    double diff_usec = (((end).tv_sec*1000000L + (end).tv_usec) - ((start).tv_sec*1000000L+(start).tv_usec));

    debug_printf("%s:%s:%d: end-start [usec] = %.4lf\n",
                 __FILE__, __FUNCTION__, __LINE__, diff_usec);

    return 0;
}

static int pthread_mutex_performance(void) {
    pthread_t tid[cpu_count];
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_mutex_init(&lock, NULL);
    cpu_set_t set;

    for (size_t rep = 0; rep < cpu_count; rep++) {
        printf("Create a new thread on core %zu\n", rep);
        CPU_ZERO(&set);
        CPU_SET(rep, &set);
        pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &set);
        int rv = pthread_create(&tid[rep], &attr, mutex_increment, NULL);
        if (rv){
            printf("[ERROR] return code from pthread_create() is %d\n", rv);
            return 1;
        }
    }
    for (size_t rep = 0; rep < cpu_count; rep++) {
        printf("%s:%s:%d: waiting for %zu\n", __FILE__, __FUNCTION__, __LINE__, rep);
        pthread_join(tid[rep], NULL);
    }
    assert(atomic_counter == cpu_count*NUM_INCR);


    printf("%s PASS\n", __FUNCTION__);

    return 0;
}

static bool is_spanned[MAX_COREID] = { false };

static void domain_spanned_callback(void *arg, errval_t err)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "domain spanning failed.");
    }
    uintptr_t i = (uintptr_t) arg;
    is_spanned[i] = true;
}

int main(int argc, char** argv)
{
    int r = 0;
#ifdef BARRELFISH
    oct_init();
    static char* local_apics = "r'hw\\.processor\\.[0-9]+' { enabled: 1 }";
    char** names;
    errval_t err = oct_get_names(&names, &cpu_count, local_apics);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not get core count");
        return 1;
    }
    oct_free_names(names, cpu_count);

    // Spawn to other cores:
    for (size_t rep = 0; rep < cpu_count; rep++) {
        if (rep != disp_get_core_id()) {
            err = domain_new_dispatcher(rep, domain_spanned_callback, (void*)(uintptr_t)rep);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "domain_new_dispatcher failed");
            }

            while (!is_spanned[rep]) {
                thread_yield();
            }
        }
    }
#endif

    r = pthread_mutex_performance();
    if (r != 0) {
        USER_PANIC("pthread_mutex_performance failed");
    }

    r = pthread_setaffinity_test();
    if (r != 0) {
        USER_PANIC("pthread_setaffinity_test failed");
    }

    r = pthread_create_join_test();
    if (r != 0) {
        USER_PANIC("pthread_create_join_test failed");
    }

    printf("TESTS PASSED\n");

#ifdef BARRELFISH
    // XXX: threads on remote cores page-fault if parent exits :(
    messages_handler_loop();
#endif
    return 0;
}