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
#include <barrelfish/deferred.h>

#include <skb/skb.h>
#include <dist2/dist2.h>

#include "common.h"
int id = 0;

int main(int argc, char *argv[])
{
    errval_t err = SYS_ERR_OK;
    dist_init();

    char* name = NULL;
    char* attr1 = NULL;

    printf("1\n");
    err = dist_read("record {}", "record {}");
    ASSERT_ERR_OK(err);

    printf("2\n");
    err = dist_read("record", "record");
    ASSERT_ERR_OK(err);

    printf("3\n");
    err = dist_read("record", "%s", &name);
    printf("name: %s\n", name);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(name, "record");
    free(name);

    printf("4\n");
    err = dist_read("record {}", "%s {}", &name);
    printf("name: %s\n", name);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(name, "record");
    free(name);

    printf("5\n");
    err = dist_read("record { attr1: 'Test String 123' }", "%s { attr1: %s }", &name, &attr1);
    printf("name: %s\n", name);
    printf("attr: %s\n", attr1);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(name, "record");
    ASSERT_STRING(attr1, "Test String 123");
    free(name);
    free(attr1);



    return EXIT_SUCCESS;
}
