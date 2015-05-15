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

#include <pthread.h>

static size_t sanity = 0;

static void* prj_thread(void* param)
{
    printf("%s:%s:%d: Hello from thread param=%"PRIu64"\n",
           __FILE__, __FUNCTION__, __LINE__, (uint64_t)param);
    sanity = 1;
    return 0;
}


int main(int argc, char** argv)
{
    pthread_t tid;
    pthread_attr_t attr;
    pthread_attr_init(&attr);

    for (size_t rep = 0; rep < 100; rep++) {
        printf("Create a new thread rep=%zu\n", rep);
        int rv = pthread_create(&tid, &attr, prj_thread, NULL);
        if (rv){
            printf("[ERROR] return code from pthread_create() is %d\n", rv);
            exit(-1);
        }

        /* wait for threads to finish */
        pthread_join(tid, NULL);

        assert(sanity == 1);
        sanity = 0;
        printf("Joined thread.\n");
    }

    printf("TEST PASS\n");
    return 0;
}