/**
 * \file
 * \brief two-level cspace L1/L2 cnode creation test
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

#if 0
static void print_unexpected(char *info, genpaddr_t expected_base,
                            char *expected_size, genpaddr_t actual_base,
                            size_t actual_size)
{
    printf("...fail: %sexpected %#"PRIxGENPADDR", %s; got %#"PRIxGENPADDR", %zu bytes\n",
            info, expected_base, expected_size, actual_base, actual_size);
}
#endif

//{{{1 test create L1
static int test_l1_create(void)
{
    int result = 0;
    errval_t err;

    printf("Test L1 Creation:\n");

    // get 2MB RAM cap
    setup(LARGE_PAGE_SIZE);

    struct capref cnode1, cnode2;

    err = slot_alloc(&cnode1);
    assert(err_is_ok(err));
    err = slot_alloc(&cnode2);
    assert(err_is_ok(err));

    printf("  Allocate 16kB L1 CNode at offset 0: ");
    err = cap_retype(cnode1, bunch_o_ram, 0, ObjType_L1CNode, OBJSIZE_L2CNODE, 1);
    GOTO_IF_ERR(err, out);
    printf("...ok\n");

    printf("  Allocate 32kB L1 CNode at offset 16kB: ");
    err = cap_retype(cnode2, bunch_o_ram, OBJSIZE_L2CNODE, ObjType_L1CNode, 2*OBJSIZE_L2CNODE, 1);
    GOTO_IF_ERR(err, out);
    printf("...ok\n");
out:
    //slot_free(cnode1);
    //slot_free(cnode2);
    /* this also cleans up any descendants of bunch_o_ram */
    cleanup();
    return result;
}

static int test_l2_create(void)
{
    int result = 0;
    errval_t err;

    // get 2MB RAM cap
    setup(LARGE_PAGE_SIZE);

    struct capref l1_cnode, l2_cnode;

    // get L1 CNode
    printf("  setup: getting L1 CNode ");
    err = slot_alloc(&l1_cnode);
    assert(err_is_ok(err));
    err = cap_retype(l1_cnode, bunch_o_ram, 0, ObjType_L1CNode, OBJSIZE_L2CNODE, 1);
    GOTO_IF_ERR(err, out);
    printf("...ok\n");

    // create old-style CNodeRef
    printf("  setup: building old-style CNref for L1 CNode and setting L2 capref ");
    err = cnode_build_cnoderef(&l2_cnode.cnode, l1_cnode);
    l2_cnode.slot = 0;
    GOTO_IF_ERR(err, out);
    printf("...ok\n");

    printf("  allocating L2 CNode at offset 16kB: ");
    err = cap_retype(l2_cnode, bunch_o_ram, OBJSIZE_L2CNODE, ObjType_L2CNode,
                     OBJSIZE_L2CNODE, 1);
    GOTO_IF_ERR(err, out);
    printf("...ok\n");

out:
    slot_free(l1_cnode);
    cleanup();
    return result;
}

//{{{1 main
int main(void)
{
    int result = 0;
    printf("0: L1 CNode creation\n");
    result |= test_l1_create() << 0;
    printf("1: L2 CNode creation\n");
    result |= test_l2_create() << 1;

    printf("L1/L2 CNode creation: result: %x\n", result);

    return result;
}
