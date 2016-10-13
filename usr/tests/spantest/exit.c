/**
 * \file
 * \brief Test termination of spannign
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitatsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>

/**
 * @brief callback when domain is spanned.
 *
 * @param arg supplied arguemtn
 * @param err outcome of the error
 */
static void domain_init_done(void *arg, errval_t err)
{
    debug_printf("domain_init_done t:%p\n", thread_self());
}

static int start_thread(void *a)
{
    debug_printf("start_thread t:%p\n", thread_self());
    return 0;
}

int main(int argc, char **argv)
{
    errval_t err;
    debug_printf("program started. Main thread is t:%p\n", thread_self());

    uint64_t num_threads = 2;
    if (argc == 2) {
        num_threads = strtol(argv[1], NULL, 10);
    }
    debug_printf("Spanning domain to %" PRIu64 " cores\n", num_threads);

    for (uint64_t i=1; i<num_threads; i++) {
        coreid_t core = i;
        debug_printf("Spanning domain (%03" PRIuCOREID "/%03" PRIu64 ")\n", core,
                     num_threads);
        err = domain_new_dispatcher(core, domain_init_done, NULL);
        if (err_is_fail(err)) {
            USER_PANIC("FAILURE: Domain span\n");
        }
    }

    for (uint64_t i=1; i<num_threads; i++) {
        coreid_t core = i;
        debug_printf("Starting thread on dispatcher (%03" PRIuCOREID "/%03" PRIu64 ")\n",
                     core, num_threads);
        err = domain_thread_create_on(core, start_thread, NULL, NULL);
        if (err_is_fail(err)) {
            USER_PANIC("FAILURE: Thread Create\n");
        }
        assert(err_is_ok(err));
    }

    printf("\nSPAN_TEST_DONE.\n");
}
