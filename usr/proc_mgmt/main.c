/**
 * \file
 * \brief Process management server.
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>

#include "internal.h"

coreid_t my_core_id;

int main(int argc, const char *argv[])
{
    errval_t err;

    my_core_id = disp_get_core_id();

    printf("proc_mgmt.%u up.\n", my_core_id);

    err = start_service();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to start proc_mgmt service loop");
    }

    messages_handler_loop();
}
