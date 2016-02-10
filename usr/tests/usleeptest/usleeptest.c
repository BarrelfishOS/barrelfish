/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstr. 6, CH-8092 Zurich. Attn: Systems Group.
*/

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/debug.h>
#include <barrelfish/deferred.h>

#define USEC_PER_SEC 1000000
#define DEFAULT_DELAY (1 * USEC_PER_SEC)

int main(int argc, char **argv)
{
    errval_t err;

    delayus_t delay = DEFAULT_DELAY;

    if (argc > 1) {
        delay = (delayus_t) strtof(argv[1], NULL) * USEC_PER_SEC;
    }

    err = barrelfish_usleep(delay);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "barrelfish_usleep(%" PRIuDELAYUS ") failed", delay);
    }

    printf("usleeptest_done\n");

    return 0;
}
