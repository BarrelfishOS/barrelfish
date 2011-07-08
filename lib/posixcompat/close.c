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
#include "fdtab.h"
#include "posixcompat.h"

int close(int fd)
{
    errval_t err;

    if (fd >= 0 && fd <= 2) { // unsupported closing standard IO streams
        return -1;
    }

    vfs_handle_t vh = fdtab_get(fd);
    if (vh == NULL_VFS_HANDLE) {
        return -1;
    }

    POSIXCOMPAT_DEBUG("close(%d)\n", fd);

    fdtab_free(fd);

    err = vfs_close(vh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error in vfs_close");
        return -1;
    }

    return 0;
}
