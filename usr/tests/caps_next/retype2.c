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

// adapted from usr/monitor/capops/internal.h
#define GOTO_IF_ERR(err, label) do { \
    if (err_is_fail(err)) { \
        printf("...fail: %s\n", err_getstring(err)); \
        result = 1; \
        goto label; \
    } \
} while (0)

//{{{1 setup & helpers
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

static void print_unexpected(char *info, genpaddr_t expected_base,
                            char *expected_size, genpaddr_t actual_base,
                            size_t actual_size)
{
    printf("...fail: %sexpected %#"PRIxGENPADDR", %s; got %#"PRIxGENPADDR", %zu bytes\n",
            info, expected_base, expected_size, actual_base, actual_size);
}

//{{{1 test_retype_single
static int test_retype_single(void)
{
    printf("%s:\n", __FUNCTION__);
    setup(LARGE_PAGE_SIZE);

    int result = 0;
    errval_t err;
    struct frame_identity fi;
    struct capref cap, cap2, cnram, cncap, tmp;

    /* get slots for results */
    err = slot_alloc(&cap);
    assert(err_is_ok(err));
    err = slot_alloc(&cap2);
    assert(err_is_ok(err));
    err = slot_alloc(&cnram);
    assert(err_is_ok(err));
    err = slot_alloc(&cncap);
    assert(err_is_ok(err));

    /* allocate 4kB Frame at offset 0 of 2MB region */
    printf("  allocate 4kB Frame at offset 0 of 2MB region: ");
    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_Frame, BASE_PAGE_SIZE, 1);
    GOTO_IF_ERR(err, out);
    err = invoke_frame_identify(cap, &fi);
    assert(err_is_ok(err));

    if (bor_id.base != fi.base || fi.bytes != BASE_PAGE_SIZE) {
        print_unexpected("", bor_id.base, "4kB", fi.base, fi.bytes);
        result = 1;
        goto out;
    }
    printf("...ok\n");

    /* allocate 16kB RAM at offset 4kB of 2MB region */
    printf("  allocate 16kB RAM at offset 4kB of 2MB region: ");
    err = cap_retype2(cap2, bunch_o_ram, BASE_PAGE_SIZE, ObjType_RAM, BASE_PAGE_SIZE * 4, 1);
    GOTO_IF_ERR(err, out);
    err = invoke_frame_identify(cap2, &fi);
    assert(err_is_ok(err));

    if (bor_id.base + BASE_PAGE_SIZE != fi.base || fi.bytes != 4*BASE_PAGE_SIZE) {
        print_unexpected("", bor_id.base + BASE_PAGE_SIZE, "16kB", fi.base, fi.bytes);
        result = 1;
        goto out;
    }
    printf("...ok\n");

    /* split 16kB into 4kB CNode, and 3x4kB Frame */
    printf("  split 16kB into 4kB CNode, and 3x4kB Frame: ");
    err = cap_retype2(cnram, cap2, 0, ObjType_RAM, BASE_PAGE_SIZE, 1);
    GOTO_IF_ERR(err, out);
    err = cnode_create_from_mem(cncap, cnram, &tmp.cnode, DEFAULT_CNODE_BITS);
    GOTO_IF_ERR(err, out);
    tmp.slot = 0;

    err = cap_retype2(tmp, cap2, BASE_PAGE_SIZE, ObjType_Frame, BASE_PAGE_SIZE, 3);
    GOTO_IF_ERR(err, out);
    // offset of slot 0 is 8kB --> addrs should be (2+slot) * BASE_PAGE_SIZE
    for (tmp.slot = 0; tmp.slot <= 2; tmp.slot++) {
        err = invoke_frame_identify(tmp, &fi);
        assert(err_is_ok(err));
        if (bor_id.base + (2+tmp.slot)*BASE_PAGE_SIZE != fi.base ||
            fi.bytes != BASE_PAGE_SIZE)
        {
            char buf[16];
            snprintf(buf, 16, "slot %d: ", tmp.slot);
            print_unexpected(buf, bor_id.base + (2+tmp.slot) * BASE_PAGE_SIZE,
                    "4kB", fi.base, fi.bytes);
            result = 1;
            goto out;
        }
    }

    printf("...ok\n");


out:
    slot_free(cap);
    slot_free(cap2);
    slot_free(cnram);
    slot_free(cncap);
    /* this also cleans up any descendants of bunch_o_ram */
    cleanup();
    return result;
}

//{{{1 test_retype_multi
static int test_retype_multi(void)
{
    printf("%s:\n", __FUNCTION__);
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
    printf("  allocate 4kB Frame at offset 0 of 2MB region: ");
    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_Frame, BASE_PAGE_SIZE, 1);
    GOTO_IF_ERR(err, out);
    err = invoke_frame_identify(cap, &fi);
    assert(err_is_ok(err));

    if (bor_id.base != fi.base || fi.bytes != BASE_PAGE_SIZE) {
        print_unexpected("", bor_id.base, "4kB", fi.base, fi.bytes);
        result = 1;
        goto out;
    }
    printf("...ok\n");

    /* allocate 16kB RAM at offset 4kB of 2MB region */
    printf("  allocate 16kB RAM at offset 4kB of 2MB region: ");
    err = cap_retype2(cap2, bunch_o_ram, BASE_PAGE_SIZE, ObjType_RAM, BASE_PAGE_SIZE * 4, 1);
    GOTO_IF_ERR(err, out);
    err = invoke_frame_identify(cap2, &fi);
    assert(err_is_ok(err));

    if (bor_id.base + BASE_PAGE_SIZE != fi.base || fi.bytes != 4*BASE_PAGE_SIZE) {
        print_unexpected("", bor_id.base + BASE_PAGE_SIZE, "16kB", fi.base, fi.bytes);
        result = 1;
        goto out;
    }
    printf("...ok\n");

    /* delete first cap and retype first 4k again */
    printf("  deleting first 4kB descendant\n");
    err = cap_delete(cap);
    assert(err_is_ok(err));

    printf("  allocating first 4kB again: ");
    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_RAM, BASE_PAGE_SIZE, 1);
    GOTO_IF_ERR(err, out);
    err = invoke_frame_identify(cap, &fi);
    assert(err_is_ok(err));

    if (bor_id.base != fi.base || fi.bytes != BASE_PAGE_SIZE) {
        print_unexpected("", bor_id.base, "4kB", fi.base, fi.bytes);
        result = 1;
        goto out;
    }
    printf("...ok\n");

out:
    slot_free(cap);
    slot_free(cap2);
    /* this also cleans up any descendants of bunch_o_ram */
    cleanup();
    return result;
}

//{{{1 test_retype_overlap
static int test_retype_overlap(void)
{
    errval_t err;
    int result = 0;
    struct capref cap, cap2;
    //struct frame_identity fi;

    err = slot_alloc(&cap);
    assert(err_is_ok(err));
    err = slot_alloc(&cap2);
    assert(err_is_ok(err));

    setup(LARGE_PAGE_SIZE);
    printf("overlap testing with 32 pages allocated at offset 0:\n");
    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_Frame, 32*BASE_PAGE_SIZE, 1);
    assert(err_is_ok(err));

    printf("  allocating 16 pages at offset 16 pages: ");
    err = cap_retype2(cap2, bunch_o_ram, 16*BASE_PAGE_SIZE,
            ObjType_Frame, 16*BASE_PAGE_SIZE, 1);
    if (err_no(err) != SYS_ERR_REVOKE_FIRST) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));

    printf("  allocating 4kB at offset 31 pages: ");
    err = cap_retype2(cap2, bunch_o_ram, BASE_PAGE_SIZE,
            ObjType_Frame, 31*BASE_PAGE_SIZE, 1);
    if (err_no(err) != SYS_ERR_REVOKE_FIRST) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));

    printf("  allocating 16kB at offset 31 pages: ");
    err = cap_retype2(cap2, bunch_o_ram, 4*BASE_PAGE_SIZE,
            ObjType_Frame, 31*BASE_PAGE_SIZE, 1);
    if (err_no(err) != SYS_ERR_REVOKE_FIRST) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));

    printf("  allocating 32 pages at offset 0: ");
    err = cap_retype2(cap2, bunch_o_ram, 0,
            ObjType_Frame, 32*BASE_PAGE_SIZE, 1);
    if (err_no(err) != SYS_ERR_REVOKE_FIRST) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));

out:
    slot_free(cap);
    slot_free(cap2);
    cleanup();
    return result;
}

//{{{1 test_non_aligned
static int test_non_aligned(void)
{
    errval_t err;
    int result = 0;
    struct capref cap;

    err = slot_alloc(&cap);
    assert(err_is_ok(err));

    setup(LARGE_PAGE_SIZE);
    printf("  offset 1024: ");
    err = cap_retype2(cap, bunch_o_ram, 1024, ObjType_Frame, 32*BASE_PAGE_SIZE, 1);
    if (err_no(err) != SYS_ERR_RETYPE_INVALID_OFFSET) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));
    printf("  offset >= object size: ");
    err = cap_retype2(cap, bunch_o_ram, LARGE_PAGE_SIZE, ObjType_Frame,
            32*BASE_PAGE_SIZE, 1);
    if (err_no(err) != SYS_ERR_RETYPE_INVALID_OFFSET) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));
    printf("  offset + objects >= object size: ");
    err = cap_retype2(cap, bunch_o_ram, LARGE_PAGE_SIZE - 31*BASE_PAGE_SIZE, ObjType_Frame,
            32*BASE_PAGE_SIZE, 1);
    if (err_no(err) != SYS_ERR_RETYPE_INVALID_OFFSET) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));
    printf("  object size 6kB: ");
    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_Frame, 6144, 1);
    if (err_no(err) != SYS_ERR_INVALID_SIZE) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));

    printf("  objects do not fit into cap: ");
    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_Frame, BASE_PAGE_SIZE, 513);
    if (err_no(err) != SYS_ERR_RETYPE_INVALID_COUNT) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));

    printf("  object larger than cap: ");
    err = cap_retype2(cap, bunch_o_ram, 0, ObjType_Frame, 513*BASE_PAGE_SIZE, 1);
    if (err_no(err) != SYS_ERR_INVALID_SIZE) {
        printf("...fail: %s\n", err_getstring(err));
        result = 1;
        goto out;
    }
    printf("...ok: retype fails with '%s'\n", err_getstring(err));

out:
    slot_free(cap);
    cleanup();
    return result;
}

//{{{1 main
int main(void)
{
    int result = 0;
    printf("0: Non-overlapping retype test, no deletion\n");
    result |= test_retype_single() << 0;
    printf("1: Non-overlapping retype test, with deletions\n");
    result |= test_retype_multi() << 1;
    printf("2: Overlapping retype test\n");
    result |= test_retype_overlap() << 2;
    printf("3: Non-aligned retype test\n");
    result |= test_non_aligned() << 3;

    printf("retype2: result: %x\n", result);

    return result;
}
