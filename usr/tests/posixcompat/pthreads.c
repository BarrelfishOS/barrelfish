/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <octopus/octopus.h>

#include <sched.h> /* CPU_SET et. al. */
#include <pthread.h>

static size_t sanity = 0;

static void* prj_thread(void* param)
{
    printf("Hello from thread %"PRIu64" on core %"PRIuCOREID"\n",
           (uint64_t)param, disp_get_core_id());
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

    static char* local_apics = "r'hw\\.processor\\.[0-9]+' { enabled: 1 }";
    char** names;
    size_t count;
    errval_t err = oct_get_names(&names, &count, local_apics);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not get core count");
        return 1;
    }
    oct_free_names(names, count);

    for (size_t rep = 0; rep < count; rep++) {
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
    oct_init();

    static char* local_apics = "r'hw\\.processor\\.[0-9]+' { enabled: 1 }";
    char** names;
    size_t count;
    errval_t err = oct_get_names(&names, &count, local_apics);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not get core count");
        return 1;
    }
    oct_free_names(names, count);

    // Spawn to other cores:
    for (size_t rep = 0; rep < count; rep++) {
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


    r = pthread_setaffinity_test();
    if (r != 0) {
        USER_PANIC("pthread_setaffinity_test failed");
    }

    r = pthread_create_join_test();
    if (r != 0) {
        USER_PANIC("pthread_create_join_test failed");
    }

    printf("TESTS PASSED\n");
    // XXX: threads on remote cores page-fault if parent exits :(
    messages_handler_loop();
    return 0;
}