/**
 * \file
 * \brief A simple test for checking if lpc_timer works
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <timer/timer.h>

static void callback(struct timer *timer, void *data)
{
    printf("client done\n");
}

int main(int argc, char *argv[])
{
    errval_t err;
    printf("timer_test running\n");

    err = timer_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "timer_init failed");
    }

    struct timer * timer = timer_create(1000, false, callback, NULL);
    timer_start(timer);

    messages_handler_loop();
    return 0;
}
