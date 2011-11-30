/**
 * \file
 * \brief start for libdist2 tests
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <dist2/dist2.h>

#include "common.h"
int id = 0;

static errval_t lock_test(void)
{

    char* lock = NULL;

    debug_printf("lock!\n");

    errval_t err = dist_lock("lockX", &lock);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_lock failed!");
        abort();
    }

    err = dist_unlock(lock);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_lock failed!");
        abort();
    }


    free(lock);

    return err;
}


int main(int argc, char *argv[])
{
    dist_init();

    lock_test();

    return EXIT_SUCCESS;
}
