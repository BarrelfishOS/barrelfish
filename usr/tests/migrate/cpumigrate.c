/**
 * \file
 * \brief Tests for octopus get/set/del API
 */

/*
 * Copyright (c) 2013, ETH Zurich.
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
#include <barrelfish/domain.h>
#include <if/monitor_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>

static void domain_spanned(void *arg, errval_t err) {
    printf("%s:%d: domain spanned! err=%s\n", 
           __FILE__, __LINE__, err_getstring(err));

    err = domain_thread_move_to(thread_self(), 0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "domain_thread_move_to failed!");
    }
    assert(err_is_ok(err));

    printf("%s:%d: migrated away from disp_get_core_id=%d\n", 
           __FILE__, __LINE__, disp_get_core_id());

}

int main(int argc, char *argv[])
{
    errval_t err;
    printf("%s:%d start cpumigrate test program.\n", __FILE__, __LINE__);

    //struct monitor_binding *mb = get_monitor_binding();

    printf("%s:%d We are on core=%"PRIuCOREID"\n",
           __FILE__, __LINE__, disp_get_core_id());
    for (volatile int i=0; i<0xFFFFFF; i++);

    printf("%s:%d: spawn domain to core 0\n", __FILE__, __LINE__);
    err = domain_new_dispatcher(0, domain_spanned, NULL);
    assert(err_is_ok(err));

    //assert(disp_get_core_id() == 1);
    //err = mb->tx_vtbl.migrate_dispatcher_request(mb, NOP_CONT, 0, NULL_CAP, NULL_CAP);
    //assert(err_is_ok(err));

    printf("%s:%d: do some waiting...\n", __FILE__, __LINE__);
    for (volatile int i=0; i<0xFFFFFF; i++);

    messages_handler_loop();

    return EXIT_SUCCESS;
}