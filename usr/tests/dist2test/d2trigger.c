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
#include <barrelfish/deferred.h>

#include <skb/skb.h>
#include <dist2/dist2.h>

#include "common.h"

static void trigger_handler(char* object, void* state)
{
    size_t* received = (size_t*) state;
    *received = *received + 1;

    free(object);
}

int main(int argc, char *argv[])
{
    dist_init();
    errval_t err = SYS_ERR_OK;
    size_t received = 0;

    err = dist_set("obj1 { attr: 1 }");
    ASSERT_ERR_OK(err);
    err = dist_set("obj2 { attr: 2 }");
    ASSERT_ERR_OK(err);
    err = dist_set("obj3 { attr: 3 }");
    ASSERT_ERR_OK(err);

    struct dist2_rpc_client* c = get_dist_rpc_client();

    dist2_trigger_t record_deleted = dist_mktrigger(SYS_ERR_OK, DIST_ON_DEL,
            trigger_handler, &received);

    errval_t error_code = SYS_ERR_OK;
    char* output = NULL;
    cycles_t time;
    uint8_t busy;
    err = c->vtbl.get(c, "r'^obj.$' { attr: 3 } ", record_deleted, &output,
            &error_code, &time, &busy);
    ASSERT_ERR_OK(err);
    ASSERT_ERR_OK(error_code);
    ASSERT_STRING(output, "obj3 { attr: 3 }");

    dist_del("obj3");
    while (received != 1) {
        // busy waiting
    }

    printf("d2trigger SUCCESS!\n");
    return EXIT_SUCCESS;
}
