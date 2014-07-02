/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>


#include "service.h"

int main(int argc, char **argv)
{
    errval_t err;

    debug_printf("Xeon Phi Manager started.\n");

    err = service_start();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the service\n");
    }
}


