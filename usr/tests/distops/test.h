/**
 * \file
 * \brief remote retype test
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TEST_DISTOPS_TEST_H
#define TEST_DISTOPS_TEST_H

#define PANIC_IF_ERR(err, msg...) \
do { \
    if (err_is_fail(err)) { \
        printf("distops_retype: got %s, expected SYS_ERR_OK\n", err_getcode(err)); \
        USER_PANIC_ERR(err, msg); \
    } \
} while(0)

#define RAM_BITS LARGE_PAGE_BITS
#define RAM_SIZE (1UL << RAM_BITS)

void init_server(struct test_binding *b);
void server_do_test(struct test_binding *b, uint32_t test, struct capref cap);

void init_client(struct test_binding *b);
void client_do_test(struct test_binding *b, uint32_t test, struct capref cap);

#endif // TEST_DISTOPS_TEST_H
