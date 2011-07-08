/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include "fdtab.h"
#include "posixcompat.h"

int read(int fd, void *buf, int len)
{
    if (fd == 0) {
        return fread(buf, 1, len, stdin);
    } else if (fd == 1 || fd == 2) {
        return -1;
    }

    vfs_handle_t vh = fdtab_get(fd);
    if (vh == NULL_VFS_HANDLE) {
        return -1;
    }

    size_t retlen = 0;
    errval_t err = vfs_read(vh, buf, len, &retlen);
    POSIXCOMPAT_DEBUG("read(%d, %d) = %zu\n", fd, len, retlen);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error in vfs_read");
        return -1;
    }

    return retlen;
}

