/**
 * \file
 * \brief Some tests to see if it works with THC
 *
 * TODO There seems to be a problems with running thc
 * in a multithreaded environment.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>

//#include <octopus/getset.h>

#include <thc/thc.h>
#include <if/octopus_defs.h>
#include <if/octopus_thc.h>



static int th(void* st)
{
    struct octopus_thc_client_binding_t cl;
    struct octopus_binding* b = NULL;


    printf("running event loop?\n");
    errval_t err;
    assert(get_default_waitset() != NULL);
    err = octopus_thc_connect_by_name("octopus_rpc",
                                   get_default_waitset(),
                                   IDC_BIND_FLAGS_DEFAULT,
                                   &b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connect failed!");
        abort();
    }
    printf("octopus_thc_connect_by_name done...\n");


    err = octopus_thc_init_client(&cl, b, b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "init failed");
        abort();
    }
    printf("octopus_thc_init_client done...\n");


    //struct octopus_thc_client_binding_t* cl = (struct octopus_thc_client_binding_t*)st;
    debug_printf("before do finish\n");

    DO_FINISH({
        debug_printf("in do finish\n");

        bool stop = false;
        while (!stop) {
            octopus_client_msg_t m;
            debug_printf("before recv any\n");
            cl.recv_any(&cl, &m, (struct octopus_client_selector) { .trigger=1 });
            debug_printf("after recv any\n");

            switch (m.msg) {
            case octopus_trigger:
                //ASYNC({trigger_handler(m.args.trigger.mode, m.args.trigger.record, m.args.trigger.error_code);});
                break;

            default:
                assert(!"Unexpected message");
                break;
            }
        }
    });


    assert(!"event loop finished, bad :-(");
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    printf("hello world!\n");


    struct thread* t = thread_create(th, NULL);
    assert(t != 0);

    printf("now do some rpc calls...\n");

    /*
    char* record;
    errval_t error_code;
    while(1) {
        printf("Doing set call...\n");
        cl.call_seq.set(&cl, "record1 { attr: 'bla' }", 0, 1, &record, &error_code);
        free(record);
    }*/

    while(1)
        thread_yield();

    return EXIT_SUCCESS;
}
