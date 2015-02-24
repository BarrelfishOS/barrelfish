/**
 * \file
 * \brief Test spanning of domains across cores
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/waitset.h>


static int remote(void *dummy)
{
    uintptr_t v = (uintptr_t)dummy;
    debug_printf("v = %"PRIuPTR"\n", v);
    return v;
}

int ndispatchers = 1;

static void domain_spanned_callback(void *arg, errval_t err)
{
    ndispatchers++;
}

int main(int argc, char *argv[])
{
    errval_t err;
    if (argc != 2) {
        printf("Usage %s: <Num additional threads>\n", argv[0]);
        exit(-1);
    }


    //printf("main running on %d\n", disp_get_core_id());

    int cores = strtol(argv[1], NULL, 10) + 1;

    for(int i = 1; i < cores; i++) {
        err = domain_new_dispatcher(i + disp_get_core_id(),
                                    domain_spanned_callback,
                                    (void*)(uintptr_t)i);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "domain_new_dispatcher failed");
        }
    }

    while (ndispatchers < cores) {
        thread_yield();
    }

    struct thread *threads[cores];
    for(int i = 1; i < cores; i++) {
        err = domain_thread_create_on(i, remote, (void*)(uintptr_t)i, &threads[i]);
        assert(err_is_ok(err));
        debug_printf("created %p\n", threads[i]);
    }

    for(int i = 1; i < cores; i++) {
        int retval;
        debug_printf("joining %p\n", threads[i]);
        err = domain_thread_join(threads[i], &retval);
        assert(err_is_ok(err));
        debug_printf("retval = %d\n", retval);
        assert(retval == i);
    }
    return 0;
}
