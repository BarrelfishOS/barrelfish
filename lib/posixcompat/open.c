/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include "fdtab.h"
#include "posixcompat.h"

//XXX: flags are ignored...
int open(const char*pathname, int flags, ...)
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
        POSIXCOMPAT_DEBUG("open('%s') failed\n", pathname);
        return -1;
    }

    int fd = fdtab_alloc(vh);
    POSIXCOMPAT_DEBUG("open(%s) as fd %d\n", pathname, fd);
    if (fd < 0) {
        vfs_close(vh);
        return -1;
    } else {
        return fd;
    }
}

