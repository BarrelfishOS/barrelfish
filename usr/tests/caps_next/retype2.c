/**
 * \file
 * \brief New retype test
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>

static struct capref bunch_o_ram;
static struct frame_identity bor_id;

static void setup(size_t bytes)
{
    errval_t err;
    err = ram_alloc(&bunch_o_ram, log2ceil(bytes));
    assert(err_is_ok(err));
    err = invoke_frame_identify(bunch_o_ram, &bor_id);
    assert(err_is_ok(err));
}

static void cleanup(void)
{
    errval_t err;
    err = cap_revoke(bunch_o_ram);
    assert(err_is_ok(err));
    err = cap_destroy(bunch_o_ram);
    assert(err_is_ok(err));
}

static int test_retype_single(void)
{
    setup(LARGE_PAGE_SIZE);

    int result = 1;
    errval_t err;

    struct capref cap;
    err = slot_alloc(&cap);
    assert(err_is_ok(err));

    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_Frame, BASE_PAGE_SIZE, 1);
    if (err_is_fail(err)) {
        printf("%s: %s\n", __FUNCTION__, err_getstring(err));
        goto out;
    }

    struct frame_identity fi;
    err = invoke_frame_identify(cap, &fi);
    assert(err_is_ok(err));

    if (bor_id.base == fi.base && fi.bytes == BASE_PAGE_SIZE) {
        result = 0;
    }

out:
    slot_free(cap);
    /* this also cleans up any descendants of bunch_o_ram */
    cleanup();
    return result;
}
static int test_retype_multi(void)
{
    setup(LARGE_PAGE_SIZE);
    cleanup();
    return 0;
}
static int test_retype_repeat(void)
{
    setup(LARGE_PAGE_SIZE);
    cleanup();
    return 0;
}

int main(void)
{
    int result = 0;
    printf("0: Single retype test\n");
    result |= test_retype_single() << 0;
    printf("1: Multiple non-overlapping retype test\n");
    result |= test_retype_multi() << 1;
    printf("2: Repeated retype test\n");
    result |= test_retype_repeat() << 2;

    printf("retype2: result: %x\n", result);

    return result;
}
