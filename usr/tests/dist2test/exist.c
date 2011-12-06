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

#define ASSERT_STRING(a, b) (assert(strcmp((a), (b)) == 0))
int id = 0;

int main(int argc, char *argv[])
{
    errval_t err = SYS_ERR_OK;
    skb_client_connect();
    dist_init();

    char* obj = NULL;

    err = dist_wait_for(DIST_ON_SET | DIST_ON_DEL, &obj, "object3");
    DEBUG_ERR(err, "dist_wait_for returned");
    assert(err_is_ok(err));
    debug_printf("exist test got obj: %s\n", obj);
    free(obj);

    err = dist_exists_not(true, "unexistingObjectname");
    assert(err_is_ok(err));
    printf("dist_exists_not done\n");

    err = dist_exists(false, &obj, "unexistingObjectname");
    assert(err_no(err) == DIST2_ERR_NO_RECORD);
    // free(obj); TODO?

    err = dist_exists(true, &obj, "object1");
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_exists");
        abort();
    }
    ASSERT_STRING(obj, "object1 { weight: 20 }");
    free(obj);

    err = dist_exists(true, &obj, "object3 { attr: 'A text string.' }");
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_exists");
        abort();
    }
    ASSERT_STRING(obj, "object3 { attr: A text string., weight: 9, fl: 12.0 }");
    free(obj);

    err = dist_exists_not(true, "object1");
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_exists");
        abort();
    }
    debug_printf("dist_exists_not: object1\n");


    debug_printf("dist2exist test complete\n");
    messages_handler_loop();
    return EXIT_FAILURE;
}
