/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
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
#include <lwip/sys.h>
#include <lwip/sockets.h>
#include <posixcompat/fdtab.h>
#include "posixcompat.h"

int read(int fd, void *buf, size_t len)
{
    struct fdtab_entry *e = fdtab_get(fd);
    if (e->type == FDTAB_TYPE_AVAILABLE) {
        return -1;
    }

    size_t retlen = 0;

    switch(e->type) {
    case FDTAB_TYPE_FILE:
        {
            errval_t err = vfs_read((vfs_handle_t)e->handle, buf, len, &retlen);
            POSIXCOMPAT_DEBUG("%d : read(%d, %d) = %zu\n", disp_get_domain_id(), fd, len, retlen);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "error in vfs_read");
                return -1;
            }
        }
        break;

    case FDTAB_TYPE_STDIN:
        retlen = fread(buf, 1, len, stdin);
        break;

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        int ret = lwip_read(e->fd, buf, len);
        lwip_mutex_unlock();
        return ret;

    case FDTAB_TYPE_STDOUT:
    case FDTAB_TYPE_STDERR:
    default:
        return -1;
    }

    return retlen;
}

