/**
 * \file
 * \brief Test RPC calls with triggers in dist2
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


int main(int argc, char *argv[])
{
    dist_init();
    errval_t err = SYS_ERR_OK;

    while(1) {
        err = dist_set("obj1 { attr: 1 }");
        ASSERT_ERR_OK(err);

        err = dist_del("obj1");
        ASSERT_ERR_OK(err);
    }

    return EXIT_FAILURE;
}
