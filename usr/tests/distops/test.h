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
        USER_PANIC_ERR(err, msg); \
    } \
} while(0)

#define RAM_BITS LARGE_PAGE_BITS
#define RAM_SIZE (1UL << RAM_BITS)

void init_server(struct test_binding *b);
void server_do_test(struct test_binding *b, uint32_t test, struct capref cap);

void init_client(struct test_binding *b);
void client_do_test(struct test_binding *b, uint32_t test, struct capref cap);

void test_retype(struct capref src, struct capref *dest, gensize_t offset,
                 gensize_t size, size_t count, errval_t expected_err,
                 const char *role);

static inline void client_test_retype(struct capref src, struct capref *dest,
                                      gensize_t offset, gensize_t size,
                                      size_t count, errval_t expected_err)
{
    return test_retype(src, dest, offset, size, count, expected_err, "client");
}

static inline void server_test_retype(struct capref src, struct capref *dest,
                                      gensize_t offset, gensize_t size,
                                      size_t count, errval_t expected_err)
{
    return test_retype(src, dest, offset, size, count, expected_err, "server");
}


#endif // TEST_DISTOPS_TEST_H
