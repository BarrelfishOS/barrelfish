/**
 * \file
 * \brief remote revoke test
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/capbits.h>
#include <if/test_defs.h>

#include "test.h"

__attribute__((unused))
static void debug_capref(const char *prefix, struct capref cap)
{
    char buf[128];
    debug_print_capref(buf, 128, cap);
    printf("%s capref = %s\n", prefix, buf);
}

enum server_op {
    // Try to retype ram (should fail) then revoke RAM cap
    SERVER_OP_RETYPE_THEN_REVOKE,
    // After checking that caps were deleted on client do another retype on
    // server that should succeed
    SERVER_OP_RETYPE,
};

enum client_op {
    // Check that caps that we were holding were deleted by revoke() on server
    // side
    CLIENT_OP_CHECK_DELETED,
};

//{{{1 Server-side cap operations

struct server_state {
    struct capref ram;
};

void init_server(struct test_binding *b)
{
    b->st = malloc(sizeof(struct server_state));
    assert(b->st);
    if (!b->st) {
        USER_PANIC("state malloc() in server");
    }
}

void server_do_test(struct test_binding *b, uint32_t test, struct capref cap)
{
    errval_t err;
    struct server_state *st = b->st;

    switch(test) {
        case SERVER_OP_RETYPE_THEN_REVOKE:
            // Remember RAM capref
            st->ram = cap;

            // Try retype; expect SYS_ERR_REVOKE_FIRST
            printf("server: try retype on already retyped RAM cap\n");
            server_test_retype(st->ram, NULL, 0, BASE_PAGE_SIZE, 1, SYS_ERR_REVOKE_FIRST);

            printf("server: revoke RAM cap\n");
            err = cap_revoke(st->ram);
            if (err_is_fail(err)) {
                printf("distops_revoke: revoke: expected OK, got %s\n", err_getcode(err));
            }

            err = test_basic__tx(b, NOP_CONT, CLIENT_OP_CHECK_DELETED);
            PANIC_IF_ERR(err, "server: triggering check_deleted on client");
            break;

        case SERVER_OP_RETYPE:
            printf("server: try retype on revoked cap\n");
            server_test_retype(st->ram, NULL, 0, BASE_PAGE_SIZE, 1, SYS_ERR_OK);

            printf("distops_revoke: test done\n");
            exit(0);

        default:
            USER_PANIC("server: Unknown test %"PRIu32"\n", test);
    }
}

//{{{1 Client-side cap operations

struct client_state {
    struct capref ram;   ///< Large RAM cap
    struct capref cnode; ///< CNode that contains all the small Frames
    struct capref frame; ///< First slot in cnode
};

void init_client(struct test_binding *b)
{
    errval_t err;

    // Allocate client state struct
    b->st = malloc(sizeof(struct client_state));
    assert(b->st);
    if (!b->st) {
        USER_PANIC("state malloc() in client");
    }
    struct client_state *cst = b->st;

    // Allocate RAM cap
    err = ram_alloc(&cst->ram, RAM_BITS);
    PANIC_IF_ERR(err, "in client: ram_alloc()");

    err = cnode_create_l2(&cst->cnode, &cst->frame.cnode);
    PANIC_IF_ERR(err, "in client: cnode_create_l2");

    err = cap_retype(cst->frame, cst->ram, 0, ObjType_Frame, BASE_PAGE_SIZE, L2_CNODE_SLOTS);
    PANIC_IF_ERR(err, "in client: cap_retype");

    // Trigger Server
    err = test_caps__tx(b, NOP_CONT, SERVER_OP_RETYPE_THEN_REVOKE, cst->ram, NULL_CAP);
    PANIC_IF_ERR(err, "in client: sending ram to server");
}

void client_do_test(struct test_binding *b, uint32_t test, struct capref cap)
{
    errval_t err;
    struct capability c;
    struct client_state *st = b->st;
    assert(st);

    switch(test) {
        // Check that revoke on server deleted our Frame caps and our RAM cap
        case CLIENT_OP_CHECK_DELETED:
            err = debug_cap_identify(st->ram, &c);
            if (err_no(err) == SYS_ERR_LMP_CAPTRANSFER_SRC_LOOKUP) {
                // RAM is empty -> cap got deleted by revoke
            } else {
                PANIC_IF_ERR(err, "debug_cap_identify(RAM)");
                if (c.type != ObjType_Null) {
                    printf("client: RAM: expected ObjType_Null, got %d\n", c.type);
                }
            }

            cap = st->frame;
            for (cap.slot = 0; cap.slot < L2_CNODE_SLOTS; cap.slot++) {
                err = debug_cap_identify(cap, &c);
                if (err_no(err) == SYS_ERR_LMP_CAPTRANSFER_SRC_LOOKUP) {
                    // slot is empty -> cap got deleted by revoke
                    continue;
                }
                PANIC_IF_ERR(err, "debug_cap_identify(Frame %"PRIuCSLOT")", cap.slot);
                if (c.type != ObjType_Null) {
                    printf("client: Frame %"PRIuCSLOT": expected ObjType_Null, got %d\n",
                            cap.slot, c.type);
                }
            }

            err = test_basic__tx(b, NOP_CONT, SERVER_OP_RETYPE);
            PANIC_IF_ERR(err, "in client: triggering server retype #2");
            printf("client: exit\n");
            exit(0);
            break;

        default:
            USER_PANIC("client: Unknown test %"PRIu32"\n", test);
    }
}
