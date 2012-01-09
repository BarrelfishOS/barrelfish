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

#include <dist2/dist2.h>

#include "common.h"

static const char* barrier_name = "d2pubsub_test";
static struct thread_sem ts;

static void message_handler(subscription_t id, char* record, void* st)
{
    static const char* receive_order[] = { "msg_2", "msg_4", "msg_5", "msg_5" };
    static size_t to_receive = sizeof(receive_order) / sizeof(char*);
    char* name = NULL;
    size_t* received = (size_t*) st;

    debug_printf("Message: %s received: %lu id: %lu\n", record, *received, id);

    errval_t err = dist_read(record, "%s", &name);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(receive_order[*received], name);
    *received = *received + 1;

    if(*received == to_receive) {
        thread_sem_post(&ts);
    }

    free(name);
    free(record);
}

static void subscriber(void)
{
    errval_t err;
    subscription_t id1 = 0;
    subscription_t id2 = 0;
    subscription_t id3 = 0;
    size_t received = 0;
    char* barrier_record = NULL;

    thread_sem_init(&ts, 0);

    err = dist_subscribe(message_handler, &received, &id1, "111 [] attr: 10 }");
    ASSERT_ERR(err, DIST2_ERR_PARSER_FAIL);

    err = dist_subscribe(message_handler, &received, &id1,
            "_ { fl: 1.01, attr: 10 }");
    ASSERT_ERR_OK(err);

    char* str = "test.txt";
    err = dist_subscribe(message_handler, &received, &id2, "_ { str: r'%s' }",
            str);
    ASSERT_ERR_OK(err);

    err = dist_subscribe(message_handler, &received, &id3, "_ { age: > %d }",
            9);
    ASSERT_ERR_OK(err);

    // Synchronize with publisher
    dist_barrier_enter(barrier_name, &barrier_record, 2);

    // Wait until all messages received
    thread_sem_wait(&ts);

    // Unsubscribe message handlers
    err = dist_unsubscribe(id1);
    ASSERT_ERR_OK(err);
    err = dist_unsubscribe(id2);
    ASSERT_ERR_OK(err);
    err = dist_unsubscribe(id3);
    ASSERT_ERR_OK(err);

    // TODO check what happens:
    err = dist_unsubscribe(id1);
    ASSERT_ERR_OK(err);

    dist_barrier_leave(barrier_record);
    free(barrier_record);

    printf("Subscriber all done.\n");
}

static void publisher(void)
{
    errval_t err;
    char* barrier_record = NULL;

    // Synchronize with subscriber
    dist_barrier_enter(barrier_name, &barrier_record, 2);

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
        printf("Wrong arguments (Valid choices are subscriber/publisher).");
    }

    return EXIT_SUCCESS;
}

