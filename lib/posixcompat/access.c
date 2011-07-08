/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include "posixcompat.h"

//XXX: this is not as it should be.
int access(const char*pathname,int mode)
{
    vfs_handle_t vh;
    errval_t err;

    if (_posixcompat_cwd == NULL) {
        _posixcompat_cwd_init();
    }

    char *path = vfs_path_mkabsolute(_posixcompat_cwd, pathname);
    assert(path != NULL);

    err = vfs_open(path, &vh);
    free(path);
    if (err_is_fail(err)) {
        POSIXCOMPAT_DEBUG("access(%s) failed\n", pathname);
        return -1;
    } else {
        vfs_close(vh);
        POSIXCOMPAT_DEBUG("access(%s): OK\n", pathname);
        return 0;
    }
}
