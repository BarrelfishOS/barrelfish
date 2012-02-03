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
#include <lwip/sys.h>
#include <lwip/sockets.h>
#include <vfs/vfs_fd.h>
#include <vfs/fdtab.h>
#include "posixcompat.h"

int write(int fd, const void *buf, size_t len)
{
    int ret;
    struct fdtab_entry *e = fdtab_get(fd);

    if (e->type == FDTAB_TYPE_LWIP_SOCKET) {
        lwip_mutex_lock();
        ret = lwip_write(e->fd, buf, len);
        lwip_mutex_unlock();
        return ret;
    } else {
        ret = vfsfd_write(fd, buf, len);
    }

    return ret;
}
