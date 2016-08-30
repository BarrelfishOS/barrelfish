/**
 * \file
 * \brief remote delete test
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
    // Setup CNode in server's cspace
    SERVER_OP_CREATE_COPY,
    // Check that CNode was deleted in server's cspace
    SERVER_OP_CHECK_CNODE_DELETED,
    // Exit server
    SERVER_OP_EXIT,
};

enum client_op {
    // Access cap in CNode when CNode exists on server, then delete our copy
    // of CNode -> ownership should transfer to server core
    CLIENT_OP_ACCESS_1,
    // Exit client
    CLIENT_OP_EXIT,
};

//{{{1 Server-side cap operations

struct server_state {
    struct capref cnode;
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
        case SERVER_OP_CREATE_COPY:
            // First call: store copy of received cnode
            err = slot_alloc(&st->cnode);
            PANIC_IF_ERR(err, "slot alloc in root cnode in server");
            err = cap_copy(st->cnode, cap);
            PANIC_IF_ERR(err, "cap copy to root cnode");

            printf("distops_delete: deleting one cnode copy on foreign core\n");
            err = cap_destroy(cap);
            if (err_is_fail(err)) {
                printf("distops_delete: delete failed: %s\n", err_getcode(err));
            }
            PANIC_IF_ERR(err, "cap destroy of received cap");

            err = test_basic__tx(b, NOP_CONT, CLIENT_OP_ACCESS_1);
            PANIC_IF_ERR(err, "server: triggering access on client");
            break;

        case SERVER_OP_CHECK_CNODE_DELETED:
            printf("server: check that cnode deleted (CNodes are not moveable)\n");

            err = cap_destroy(st->cnode);
            if (err_no(err) != SYS_ERR_CAP_NOT_FOUND) {
                printf("distops_delete: delete failed: %s\n", err_getcode(err));
            }

            err = test_basic__tx(b, NOP_CONT, CLIENT_OP_EXIT);
            PANIC_IF_ERR(err, "sending exit to client");
            printf("distops_delete: test done\n");
            exit(0);

        default:
            USER_PANIC("server: Unknown test %"PRIu32"\n", test);
    }
}

//{{{1 Client-side cap operations

struct client_state {
    struct capref cnode;
    struct capref cap;
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
    err = cnode_create_l2(&cst->cnode, &cst->cap.cnode);
    PANIC_IF_ERR(err, "in client: cnode_create_l2");

    cst->cap.slot = 0;

    struct capref cap = cst->cap;

    for (; cap.slot < 4; cap.slot ++) {
        err = cap_copy(cap, cap_vroot);
        PANIC_IF_ERR(err, "in client: copying into our cnode, slot=%"PRIuCSLOT, cap.slot);
    }

    // Trigger Server
    err = test_caps__tx(b, NOP_CONT, SERVER_OP_CREATE_COPY, cst->cnode, NULL_CAP);
    PANIC_IF_ERR(err, "in client: sending cnode to server");
}

void client_do_test(struct test_binding *b, uint32_t test, struct capref cap)
{
    errval_t err;
    struct vnode_identity vi;
    struct client_state *st = b->st;
    assert(st);

    switch(test) {
        // Do retype while copies exist on other core
        case CLIENT_OP_ACCESS_1:
            printf("client: access cap in cnode\n");
            err = invoke_vnode_identify(st->cap, &vi);
            if (err_is_fail(err)) {
                printf("distops_delete: invoke failed: %s\n", err_getcode(err));
            }
            PANIC_IF_ERR(err, "cannot identify vnode in slot 0 of cnode");

            printf("client: delete cnode\n");
            err = cap_destroy(st->cnode);
            if (err_is_fail(err)) {
                printf("distops_delete: delete failed: %s\n", err_getcode(err));
            }
            PANIC_IF_ERR(err, "client: deleting cnode");

            err = test_basic__tx(b, NOP_CONT, SERVER_OP_CHECK_CNODE_DELETED);
            PANIC_IF_ERR(err, "client: trigger first retype on server");
            break;

        case CLIENT_OP_EXIT:
            printf("client: exit\n");
            exit(0);

        default:
            USER_PANIC("client: Unknown test %"PRIu32"\n", test);
    }
}
