/**
 * \file
 * \brief Testing shared memory bulk_transfer backend
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>
#include <bulk_transfer/bulk_sm.h>

#define TEST_NAME "usr/tests/bulk_transfer/bulk_shm"
#define SERVICE_NAME "tests__bulk_transfer__bulk_shm"
#define TESTDATA "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

enum agent_state {
    A1_STARTED,
    A1_WAIT_PASS_BUFFER,
    A1_BUFFER_PASSED,
    A1_BUFFER_MOVED,
    A1_POOL_ACCEPTED,
    A1_BUFFER_COPIED,
    A1_BUFFER_RELEASED,
    A1_DONE,

    A2_STARTED,
    A2_BOUND,
    A2_WAIT_POOL_ACCEPTED,
    A2_POOL_ACCEPTED,
    A2_BUFFER_PASSED,
    A2_BUFFER_MOVED,
    A2_BUFFER_COPIED,
    A2_BUFFER_RELEASED,
    A2_DONE
};
volatile enum agent_state state;

static void agent_wait_state(enum agent_state desired_state)
{
    struct waitset *ws = get_default_waitset();
    while (state != desired_state) {
        errval_t err = event_dispatch(ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch");
        }
    }
}

// Agent 1 ----------------------------------------------------------------

struct bulk_buffer *a1_buffer;

static void a1_copy_released(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer)
{
    debug_printf("a1_copy_released\n");
    state = A1_BUFFER_RELEASED;
}

static void a1_buffer_copied_cont(void *arg, errval_t err,
                                  struct bulk_channel *channel)
{
    debug_printf("a1_buffer_copied_cont: %s\n", err_getstring(err));
    state = A1_BUFFER_COPIED;
}

static void a1_pool_assign_cont(void *arg, errval_t err,
                                struct bulk_channel *channel)
{
    debug_printf("a1_pool_assign_cont: %s\n", err_getstring(err));
    state = A1_POOL_ACCEPTED;
}

static void a1_buffer_moved_cont(void *arg, errval_t err,
                                 struct bulk_channel *channel)
{
    debug_printf("a1_buffer_moved_cont: %s\n", err_getstring(err));
    state = A1_BUFFER_MOVED;
}

static void a1_buffer_received(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta)
{
    debug_printf("a1_buffer_received\n");
    a1_buffer = buffer;
    state = A1_BUFFER_PASSED;
}

static errval_t a1_pool_assigned(struct bulk_channel *channel,
                                 struct bulk_pool    *pool)
{
    debug_printf("a1_pool_assigned.\n");
    return SYS_ERR_OK;
}

static void run_agent_1(int test)
{
    errval_t err;

    debug_printf("Agent 1 [PID=%u]: starting. Running test %d.\n",
            disp_get_domain_id(), test);
    state = A1_STARTED;

    // Create channel and register with nameservice -----------------------
    debug_printf("Agent 1: creating channel.\n");

    // Endpoint
    struct bulk_sm_endpoint_descriptor ep;
    err = bulk_sm_ep_create(&ep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A1: bulk_sm_ep_create");
    }

    // Callbacks
    struct bulk_channel_callbacks callbacks = {
        .bind_received     = NULL,
        .teardown_received = NULL,
        .pool_assigned     = a1_pool_assigned,
        .pool_removed      = NULL,
        .move_received     = NULL,
        .buffer_received   = a1_buffer_received,
        .copy_received     = NULL,
        .copy_released     = a1_copy_released,
    };

    // Channel creation
    struct bulk_channel_setup setup = {
        .direction   = BULK_DIRECTION_TX,
        .role        = BULK_ROLE_SLAVE,
        .trust       = ((test == 0) ? BULK_TRUST_FULL : BULK_TRUST_NONE),
        // .constraints =
        // .meta_size   =
        .waitset     = get_default_waitset(),
        // .user_state  =
    };

    struct bulk_endpoint_descriptor *ep_generic = (void*)&ep;
    struct bulk_channel channel;
    err = bulk_channel_create(&channel, ep_generic, &callbacks, &setup);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A1: bulk_channel_create");
    }

    // Register channel
    err = nameservice_register(SERVICE_NAME, ep.iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A1: nameservice_register");
    }

    // Wait for passed buffer, fill with data and move back ---------------
    state = A1_WAIT_PASS_BUFFER;
    debug_printf("Agent 1: channel created. waiting for buffer.\n");
    agent_wait_state(A1_BUFFER_PASSED);

    // Additional Test 2: done by Agent 2.
    if (test == 2) {
        debug_printf("Agent 1: expecting Agent 2 to fail. entering loop.\n");
        while(1) {
            //we need to make sure the reply on our backend channel gets sent
            event_dispatch(channel.waitset);
        }
        return ;
    }

    debug_printf("Agent 1: buffer received. writing testdata.\n");
    memcpy(a1_buffer->address, TESTDATA, sizeof(TESTDATA));
    debug_printf("Agent 1: buffer reads: %s\n", (char*)a1_buffer->address);

    // need to wait a bit before releasing buffer s.t. Agent 2 can run tests
    // in "buffer passed" state.
    debug_printf("Agent 1: some delay.\n");
    for (volatile int i=0; i<10000; ++i) {
        thread_yield();
    }

    debug_printf("Agent 1: moving buffer to agent 2.\n");
    err = bulk_channel_move(&channel, a1_buffer, NULL,
            MK_BULK_CONT(a1_buffer_moved_cont, NULL));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A1: bulk_channel_move");
    }
    agent_wait_state(A1_BUFFER_MOVED);

    // Additional Test 3: Verify buffer inaccessible
    if (test == 3) {
        debug_printf("Agent 1: Test 3: This should abort the program.\n");
        debug_printf("%.30s\n", (char*)a1_buffer->address);
        debug_printf("Agent 1: failed. aborting manually\n");
        abort();
    }

    // Test if we can also add pools. Write buffer and copy to agent 2-----
    debug_printf("Agent 1: Allocating buffers.\n");

    struct bulk_allocator allocator;
    //set the trust level we want in our pool from the start
    struct bulk_pool_constraints pool_constraints = {
        .range_min = 0,
        .range_max = 0,
        .alignment = 0,
        .trust = channel.trust,
    };
    err = bulk_alloc_init(&allocator, 5, 4096, &pool_constraints);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A1: bulk_alloc_init");
    }

    // assign pool
    debug_printf("Agent 1: assigning pool\n");
    err = bulk_channel_assign_pool(&channel, allocator.pool,
            MK_BULK_CONT(a1_pool_assign_cont, NULL));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A1: bulk_channel_assign_pool");
    }

    agent_wait_state(A1_POOL_ACCEPTED);

    // fill buffer with data
    debug_printf("Agent 1: copy buffer to agent 2.\n");
    struct bulk_buffer *buffer = bulk_alloc_new_buffer(&allocator);
    assert(buffer);

    memset(buffer->address, 0x21, buffer->pool->buffer_size);

    err = bulk_channel_copy(&channel, buffer, NULL,
            MK_BULK_CONT(a1_buffer_copied_cont, NULL));

    agent_wait_state(A1_BUFFER_COPIED);

    // verify data still readable
    memcmp(buffer->address, TESTDATA, 3); // dummy function call
    debug_printf("Agent 1: buffer still readable.\n");

    // Additional Test 4: Verify buffer not writeable
    if (test == 4) {
        debug_printf("Agent 1: Test 4: This should abort the program.\n");
        *((char*)buffer->address) = 'A';
        debug_printf("Agent 1: failed: buffer was written, now reads %s\n",
                        (char*) buffer->address);
        abort();
    }

    // Additional Test 5: done by Agent 2.
    if (test == 5) {
        debug_printf("Agent 1: expecting Agent 2 to fail. entering loop.\n");
        while(1) {
            //we need to make sure the reply on our backend channel gets sent
            event_dispatch(channel.waitset);
        }
        return ;
    }

    // verify can write after release
    agent_wait_state(A1_BUFFER_RELEASED);
    debug_printf("Agent 1: buffer released by agent 2. "
                 "Check if writeable again.\n");

    memset(buffer->address, 0x22, buffer->pool->buffer_size);
    debug_printf("Agent 1: sucessful.\n");

    // XXX channel teardown? pool removal? buffer deallocation?

    state = A1_DONE;
    debug_printf("Agent 1: DONE.\n");
}

// Agent 2 ----------------------------------------------------------------

struct bulk_buffer *a2_buffer1;
struct bulk_buffer *a2_buffer2;

static errval_t a2_pool_assigned(struct bulk_channel *channel,
                                 struct bulk_pool    *pool)
{
    debug_printf("a2_pool_assigned.\n");
    return SYS_ERR_OK;
}

static void a2_copy_received(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{
    debug_printf("a2_copy_received.\n");
    a2_buffer2 = buffer;
    state = A2_BUFFER_COPIED;
}

static void a2_move_received(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{
    debug_printf("a2_move_received.\n");
    a2_buffer1 = buffer;
    state = A2_BUFFER_MOVED;
}

static void a2_buffer_release_cont(void *arg, errval_t err,
        struct bulk_channel *channel)
{
    debug_printf("a2_buffer_release_cont: %s\n", err_getstring(err));
    state = A2_BUFFER_RELEASED;
}

static void a2_buffer_passed_cont(void *arg, errval_t err,
        struct bulk_channel *channel)
{
    debug_printf("a2_buffer_passed_cont: %s\n", err_getstring(err));
    state = A2_BUFFER_PASSED;
}

static void a2_pool_assign_cont(void *arg, errval_t err,
        struct bulk_channel *channel)
{
    debug_printf("a2_pool_assign_cont: %s\n", err_getstring(err));
    state = A2_POOL_ACCEPTED;
}

static void a2_bound_cont(void *arg, errval_t err,
        struct bulk_channel *channel)
{
    debug_printf("a2_bound_cont: %s\n", err_getstring(err));
    state = A2_BOUND;
}

static void run_agent_2(int test)
{
    errval_t err;

    debug_printf("Agent 2 [PID=%u]: starting. Running test %d.\n",
            disp_get_domain_id(), test);
    state = A2_STARTED;

    // Wait for channel, bind, create pool, pass a buffer -----------------
    debug_printf("Agent 2: waiting for channel.\n");

    // look for service
    iref_t iref;
    err = nameservice_blocking_lookup(SERVICE_NAME, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A2: nameservice_blocking_lookup");
    }

    // create endpoint
    debug_printf("Agent 2: binding to channel.\n");
    struct bulk_sm_endpoint_descriptor ep;
    err = bulk_sm_ep_create_remote(&ep, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A2: bulk_sm_ep_create_remote");
    }

    // bind channel
    struct bulk_endpoint_descriptor *ep_generic = (void*)&ep;
    struct bulk_channel channel;
    //make sure we don't have initialization artefacts
    memset(&channel, 0, sizeof(struct bulk_channel));
    struct bulk_channel_callbacks callbacks = {
        .bind_received     = NULL,
        .teardown_received = NULL,
        .pool_assigned     = a2_pool_assigned,
        .pool_removed      = NULL,
        .move_received     = a2_move_received,
        .buffer_received   = NULL,
        .copy_received     = a2_copy_received,
        .copy_released     = NULL,
    };
    struct bulk_channel_bind_params params = {
        .role        = BULK_ROLE_MASTER,
        .trust       = ((test == 0) ? BULK_TRUST_FULL : BULK_TRUST_NONE),
        // .constraints =
        .waitset     = get_default_waitset(),
        // .user_state  =
    };
    struct bulk_continuation bind_cont = {
        .handler = a2_bound_cont,
        .arg     = NULL,
    };

    err = bulk_channel_bind(&channel, ep_generic, &callbacks, &params,
            bind_cont);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A2: bulk_channel_bind");
    }

    agent_wait_state(A2_BOUND);

    // Allocating buffers.
    debug_printf("Agent 2: bound. Allocating buffers.\n");

    struct bulk_allocator allocator;
    //set the trust level we want in our pool from the start
    struct bulk_pool_constraints pool_constraints = {
        .range_min = 0,
        .range_max = 0,
        .alignment = 0,
        .trust = channel.trust,
    };
    err = bulk_alloc_init(&allocator, 5, 4096, &pool_constraints);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A2: bulk_alloc_init");
    }

    // assign pool
    debug_printf("Agent 2: assigning pool\n");
    err = bulk_channel_assign_pool(&channel, allocator.pool,
            MK_BULK_CONT(a2_pool_assign_cont, NULL));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A2: bulk_channel_assign_pool");
    }

    agent_wait_state(A2_POOL_ACCEPTED);

    // pass buffer
    debug_printf("Agent 2: passing buffer\n");
    struct bulk_buffer *buffer = bulk_alloc_new_buffer(&allocator);
    assert(buffer);

    err = bulk_channel_pass(&channel, buffer, NULL,
            MK_BULK_CONT(a2_buffer_passed_cont, NULL));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A2: bulk_channel_pass");
    }
    
    agent_wait_state(A2_BUFFER_PASSED);

    // Additional Test 2: Verify buffer inaccessible
    if (test == 2) {
        debug_printf("Agent 2: Test 2: This should abort the program.\n");
        debug_printf("%.30s\n", (char*)buffer->address);
        debug_printf("Agent 2: failed. aborting manually\n");
        abort();
    }

    agent_wait_state(A2_BUFFER_MOVED);

    // Additional Test 3: done by Agent 1.
    if (test == 3) {
        debug_printf("Agent 2: expecting Agent 1 to fail. Terminating.\n");
        return ;
    }

    // Receive buffer. Verify data. Check can modify. ---------------------
    debug_printf("Agent 2: received buffer. verifying contents.\n");

    char *buffer_data = a2_buffer1->address;
    debug_printf("Agent 2: buffer reads: %s\n", buffer_data);

    // verify contents
    int compare = memcmp(buffer_data, TESTDATA, sizeof(TESTDATA));
    if (compare == 0) {
        debug_printf("Agent 2: match.\n");
    } else {
        debug_printf("Agent 2: failed.\n");
        abort();
    }

    // replace contents
    debug_printf("Agent 2: check if data can be modified.\n");
    memset(buffer_data, 0, 4096);
    debug_printf("Agent 2: success.\n");

    // Wait for copy. Check read/no-write access, release -----------------
    agent_wait_state(A2_BUFFER_COPIED);
    debug_printf("Agent 2: buffer_copy received. Data: %.30s\n",
            (char*) a2_buffer2->address);

    // Additional Test 4: done by Agent 1.
    if (test == 4) {
        debug_printf("Agent 2: expecting Agent 1 to fail. entering loop.\n");
        while(1) {
            //we need to make sure the reply on our backend channel gets sent
            event_dispatch(channel.waitset);
        }
        return ;
    }

    // verify contents
    debug_printf("Agent 2: verifying buffer contents.\n");
    int valid = 1;
    buffer_data = a2_buffer2->address;
    for (int i=0; i < a2_buffer2->pool->buffer_size; ++i) {
        valid = valid && (buffer_data[i] == 0x21);
    }
    if (valid) {
        debug_printf("Agent 2: match.\n");
    } else {
        debug_printf("Agent 2: failed.\n");
        abort();
    }

    // Additional Test 5: Verify buffer not writeable
    if (test == 5) {
        debug_printf("Agent 2: Test 5: This should abort the program.\n");
        * (char*)a2_buffer2->address = 'B';
        debug_printf("Agent 2: failed. buffer was written, now reads %.30s\n",
                        (char*)a2_buffer2->address);
        abort();
    }

    // need to wait a bit before releasing buffer s.t. Agent 1 can run tests
    // in "buffer copied" state.
    debug_printf("Agent 2: some delay.\n");
    for (volatile int i=0; i<10000; ++i) {
        thread_yield();
    }

    debug_printf("Agent 2: releasing buffer.\n");
    err = bulk_channel_release(&channel, a2_buffer2,
            MK_BULK_CONT(a2_buffer_release_cont, NULL));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "A2: bulk_channel_release");
    }

    agent_wait_state(A2_BUFFER_RELEASED);

    // Additional Test 6: Verify buffer inaccessible
    if (test == 6) {
        debug_printf("Agent 2: Test 6: This should abort the program.\n");
        debug_printf("%.30s\n", (char*)a2_buffer2->address);
        debug_printf("Agent 2: failed. aborting manually\n");
        abort();
    }
    
    debug_printf("Agent 2: DONE.\n");
    state = A2_DONE;
}

// Main -------------------------------------------------------------------

static void usage(char *argv[])
{
    debug_printf("%s:\n", TEST_NAME);
    debug_printf("  Usage: %s AgentNr [TestNr]\n", argv[0]);
    debug_printf("    AgentNr : 1 | 2\n");
    debug_printf("    TestNr  : 0 - 6\n");
    debug_printf("      Test 0 runs a trusted channel scenario.\n");
    debug_printf("      Test 1 runs a non-trusted channel scenario.\n");
    debug_printf("      Test 2-6 run on an untrusted channel and provokes "
                           "program abortions.\n");
    debug_printf("        Test 2: no buffer access after pass.\n");
    debug_printf("        Test 3: no buffer access after move.\n");
    debug_printf("        Test 4: no write to buffer shared by copy.\n");
    debug_printf("        Test 5: no write to buffer received from copy.\n");
    debug_printf("        Test 6: no buffer access after release.\n");
}

int main(int argc, char *argv[])
{
    debug_printf("%s: started.\n", TEST_NAME);

    if (argc == 1) {
        usage(argv);
        return 0;
    }

    int test = 0;
    if (argc == 3) {
        test = atoi(argv[2]);
        if (test > 6) {
            usage(argv);
            return 0;
        }
    } else {
        debug_printf("No test specified. Defaulting to %d.\n", test);
    }

    int agent = atoi(argv[1]);

    if (agent == 1) {
        run_agent_1(test);
    } else if(agent == 2) {
        run_agent_2(test);
    } else {
        usage(argv);
        abort();
    }
    debug_printf("agent %d exiting\n", agent);
    return 0;
}
