/**
 * \file
 * \brief FPU context switch test program.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/systime.h>

static const char *progname = NULL;

static int fpu_thread(void *arg)
{
    double save;
    volatile double lastsave;
    int n = (uintptr_t)arg;
    int j = 0;
    save = systime_now();
    lastsave = sin(3.0 * save + 3.0);
    for (uint64_t i = 0;; i++) {
        save = sin(3.0 * save + 3.0);
        if (save != lastsave) {
            printf("ERROR %s(%d): %.15g != %.15g at iteration %" PRIu64 "\n",
                   progname, n, save, lastsave, i);
            abort();
        }
        lastsave = sin(3.0 * lastsave + 3.0);
        if (i % 10000000 == 0) {
            printf("%s(%d): iteration %" PRIu64 "\n", progname, n, i);
            j++;
            if (j == 6) {
                printf("fputest passed successfully!\n");
                thread_exit(0);
            }
        }
    }
    return 0;
}

int main(int argc, char *argv[])
{
    if(argc != 3) {
        printf("Usage: %s <identifier> <number of threads>\n", argv[0]);
        exit(1);
    }

    int nthreads = atoi(argv[2]);
    progname = argv[1];

    for(int i = 0; i < nthreads; i++) {
        thread_create(fpu_thread, (void *)(uintptr_t)i);
    }

    thread_exit(0);
    return 0;
}
