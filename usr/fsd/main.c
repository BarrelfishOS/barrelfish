/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/trivfs_defs.h>
#include <vfs/vfs.h>

#include "ramfs.h"

int main(int argc, char *argv[])
{
    errval_t err;

    // Mount NFS from argv[1]
    assert(argc > 1);

    err = vfs_mkdir("/fsd.fs");
    assert(err_is_ok(err));

    err = vfs_mount("/fsd.fs", argv[1]);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_mount");
    }

    struct dirent *root = ramfs_init();

    // Start the service
    err = start_service(root);
    assert(err_is_ok(err));

    messages_handler_loop();
    return 0;
}
