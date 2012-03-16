/**
 * \file
 * \brief Tests for dist2 publish/subscribe API
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <octopus/dist2.h>

#include "common.h"

static const char* barrier_name = "d2pubsub_test";
static struct thread_sem ts;

static void message_handler(dist_mode_t mode, char* record, void* st)
{
    size_t* received = (size_t*) st;

    if (mode & DIST_ON_PUBLISH) {
        static const char* receive_order[] =
        { "msg_2", "msg_4", "msg_5", "msg_5", "msg_6", "msg_7" };
        char* name = NULL;

        debug_printf("Message: %s received: %lu\n", record, *received);

        errval_t err = dist_read(record, "%s", &name);
        ASSERT_ERR_OK(err);
        ASSERT_STRING(receive_order[*received], name);


        free(name);
        free(record);
    }
    else if (mode & DIST_REMOVED) {
        debug_printf("DIST_REMOVED set...\n");
    }

    (*received)++;
}

static void subscriber(void)
{
    errval_t err;
    subscription_t id1 = 0;
    subscription_t id2 = 0;
    subscription_t id3 = 0;
    subscription_t id4 = 0;
    size_t received = 0;
    char* barrier_record = NULL;

    thread_sem_init(&ts, 0);

    err = dist_subscribe(message_handler, &received, &id1, "111 [] attr: 10 }");
    ASSERT_ERR(err, DIST2_ERR_PARSER_FAIL);

    err = dist_subscribe(message_handler, &received, &id1,
            "_ { fl: 1.01, attr: 10 }");
    ASSERT_ERR_OK(err);
    debug_printf("id is: %lu\n", id1);

    char* str = "test.txt";
    err = dist_subscribe(message_handler, &received, &id2, "_ { str: r'%s' }",
            str);
    ASSERT_ERR_OK(err);
    debug_printf("id is: %lu\n", id2);

    err = dist_subscribe(message_handler, &received, &id3, "_ { age > %d }",
            9);
    ASSERT_ERR_OK(err);
    debug_printf("id is: %lu\n", id3);

    err = dist_subscribe(message_handler, &received, &id4,
            "r'^msg_(6|7)$'");
    ASSERT_ERR_OK(err);
    debug_printf("id is: %lu\n", id4);

    // Synchronize with publisher
    err = dist_barrier_enter(barrier_name, &barrier_record, 2);
    if (err_is_fail(err)) DEBUG_ERR(err, "barrier enter");
    assert(err_is_ok(err));

    // Wait until all messages received
    while(received != 6) {
        messages_wait_and_handle_next();
    }

    // Unsubscribe message handlers
    err = dist_unsubscribe(id1);
    ASSERT_ERR_OK(err);
    err = dist_unsubscribe(id2);
    ASSERT_ERR_OK(err);
    err = dist_unsubscribe(id3);
    ASSERT_ERR_OK(err);
    err = dist_unsubscribe(id4);
    ASSERT_ERR_OK(err);

    while(received != 10) {
        messages_wait_and_handle_next();
    }

    dist_barrier_leave(barrier_record);
    free(barrier_record);

    printf("Subscriber all done.\n");
}

static void publisher(void)
{
    errval_t err;
    char* barrier_record = NULL;

    // Synchronize with subscriber
    err = dist_barrier_enter(barrier_name, &barrier_record, 2);
    if (err_is_fail(err)) DEBUG_ERR(err, "barrier enter");
    assert(err_is_ok(err));

    err = dist_publish("msg_1 { age: %d }", 9);
    ASSERT_ERR_OK(err);

    err = dist_publish("msg_2 { age: %d }", 10);
    ASSERT_ERR_OK(err);

    err = dist_publish("msg_3 { str: %d, age: '%d' }", 123, 8);
    ASSERT_ERR_OK(err);

    err = dist_publish("msg_4 { str: 'test.txt' }");
    ASSERT_ERR_OK(err);

    err = dist_publish("msg_5 { str: 'test.txt', attr: 10, fl: 1.01 }");
    ASSERT_ERR_OK(err);

    err = dist_publish("msg_6 { type: 'test', pattern: '123123' }");
    ASSERT_ERR_OK(err);

    err = dist_publish("msg_7 { type: 'test' }");
    ASSERT_ERR_OK(err);

    dist_barrier_leave(barrier_record);
    free(barrier_record);

    printf("Publisher all done.\n");
}

int main(int argc, char** argv)
{
    dist_init();
    assert(argc >= 2);

    if (strcmp(argv[1], "subscriber") == 0) {
        subscriber();
    } else if (strcmp(argv[1], "publisher") == 0) {
        publisher();
    } else {
        printf("Bad arguments (Valid choices are subscriber/publisher).");
    }

    return EXIT_SUCCESS;
}

