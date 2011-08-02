/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
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
#include <errno.h>
#include "fdtab.h"
#include "posixcompat.h"

//XXX: flags are ignored...
int open(const char *pathname, int flags, ...)
{
    vfs_handle_t vh;
    errval_t err;

    char *path = vfs_path_mkabs(pathname);
    assert(path != NULL);

    // If O_CREAT was given, we use vfs_create()
    if(flags & O_CREAT) {
        // If O_EXCL was also given, we check whether we can open() first
        if(flags & O_EXCL) {
            err = vfs_open(path, &vh);
            if(err_is_ok(err)) {
                vfs_close(vh);
                errno = EEXIST;
                return -1;
            }
            assert(err_no(err) == FS_ERR_NOTFOUND);
        }

        err = vfs_create(path, &vh);
    } else {
        // Regular open()
        err = vfs_open(path, &vh);
    }

    free(path);
    if (err_is_fail(err)) {
        POSIXCOMPAT_DEBUG("open('%s') failed\n", pathname);

        switch(err_no(err)) {
        case FS_ERR_NOTFOUND:
            errno = ENOENT;
            break;

        default:
#ifdef POSIXCOMPAT_DEBUG_ENABLED
            DEBUG_ERR(err, "vfs_open");
#endif
            break;
        }

        return -1;
    }

    struct fdtab_entry e = {
        .type = FDTAB_TYPE_FILE,
        .handle = vh,
    };
    int fd = fdtab_alloc(&e);
    POSIXCOMPAT_DEBUG("open(%s) as fd %d\n", pathname, fd);
    if (fd < 0) {
        vfs_close(vh);
        return -1;
    } else {
        return fd;
    }
}
