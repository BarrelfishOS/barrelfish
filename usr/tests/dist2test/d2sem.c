/**
 * \file
 * \brief Tests for dist2 get/set/del API
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
#include <octopus/dist2.h>

#include "common.h"

int main(int argc, char *argv[])
{
    dist_init();

    uint32_t id;

    errval_t err = dist_sem_new(&id, 1);
    ASSERT_ERR_OK(err);
    debug_printf("Semaphore created with id=%d\n", id);

    err = dist_sem_trywait(id);
    ASSERT_ERR_OK(err);

    err = dist_sem_trywait(id);
    ASSERT_ERR(err, DIST2_ERR_NO_RECORD);

    err = dist_sem_post(id);
    ASSERT_ERR_OK(err);

    err = dist_sem_post(id);
    ASSERT_ERR_OK(err);

    err = dist_sem_trywait(id);
    ASSERT_ERR_OK(err);

    err = dist_sem_trywait(id);
    ASSERT_ERR_OK(err);

    err = dist_sem_trywait(id);
    ASSERT_ERR(err, DIST2_ERR_NO_RECORD);

    printf("d2sem SUCCESS!\n");
    return EXIT_SUCCESS;
}
