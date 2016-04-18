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

    int result = 0;
    errval_t err;
    struct frame_identity fi;
    struct capref cap, cap2;

    /* get slots for results */
    err = slot_alloc(&cap);
    assert(err_is_ok(err));
    err = slot_alloc(&cap2);
    assert(err_is_ok(err));

    /* allocate 4kB Frame at offset 0 of 2MB region */
    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_Frame, BASE_PAGE_SIZE, 1);
    if (err_is_fail(err)) {
        printf("%s: %s\n", __FUNCTION__, err_getstring(err));
        result = 1;
        goto out;
    }
    err = invoke_frame_identify(cap, &fi);
    assert(err_is_ok(err));

    if (bor_id.base != fi.base || fi.bytes != BASE_PAGE_SIZE) {
        printf("%s: expected %#"PRIxGENPADDR", 4kB; got %#"PRIxGENPADDR", %zu bytes\n",
                __FUNCTION__, bor_id.base, fi.base, fi.bytes);
        result = 1;
        goto out;
    }

    /* allocate 16kB RAM at offset 4kB of 2MB region */
    err = cap_retype2(cap2, bunch_o_ram, BASE_PAGE_SIZE, ObjType_RAM, BASE_PAGE_SIZE * 4, 1);
    if (err_is_fail(err)) {
        printf("%s: %s\n", __FUNCTION__, err_getstring(err));
        result = 1;
        goto out;
    }
    err = invoke_frame_identify(cap2, &fi);
    assert(err_is_ok(err));

    if (bor_id.base + BASE_PAGE_SIZE != fi.base || fi.bytes != 4*BASE_PAGE_SIZE) {
        printf("%s: expected %#"PRIxGENPADDR", 16kB; got %#"PRIxGENPADDR", %zu bytes\n",
                 __FUNCTION__, bor_id.base + BASE_PAGE_SIZE, fi.base, fi.bytes);
        result = 1;
        goto out;
    }

out:
    slot_free(cap);
    slot_free(cap2);
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
static int test_retype_overlap(void)
{
    setup(LARGE_PAGE_SIZE);
    cleanup();
    return 0;
}

int main(void)
{
    int result = 0;
    printf("0: Non-overlapping retype test, no deletion\n");
    result |= test_retype_single() << 0;
    printf("1: Non-overlapping retype test, with deletions\n");
    result |= test_retype_multi() << 1;
    printf("2: Overlapping retype test\n");
    result |= test_retype_overlap() << 2;

    printf("retype2: result: %x\n", result);

    return result;
}
