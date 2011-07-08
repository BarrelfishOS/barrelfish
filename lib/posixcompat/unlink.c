/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <unistd.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include "posixcompat.h"

int unlink(const char *pathname)
{
    errval_t err;

    if (_posixcompat_cwd == NULL) {
        _posixcompat_cwd_init();
    }

    char *path = vfs_path_mkabsolute(_posixcompat_cwd, pathname);
    assert(path != NULL);

    POSIXCOMPAT_DEBUG("unlink('%s')\n", pathname);

    err = vfs_remove(path);
    free(path);
    if (err_is_fail(err)) {
        return -1;
    } else {
        return 0;
    }
}
