/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <unistd.h>
#include <stdio.h>
#include <lwip/sys.h>
#include <lwip/sockets.h>
#include <vfs/vfs_fd.h>
#include <vfs/fdtab.h>
#include "posixcompat.h"
#include "pty.h"

__weak_reference(write, _write);
ssize_t write(int fd, const void *buf, size_t len)
{
    int ret;
    struct fdtab_entry *e = fdtab_get(fd);

    switch(e->type) {
    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        ret = lwip_write(e->fd, buf, len);
        lwip_mutex_unlock();
        break;

    case FDTAB_TYPE_UNIX_SOCKET:
        ret = send(fd, buf, len, 0);
        break;

    case FDTAB_TYPE_PTM:
        ret = ptm_write(fd, buf, len);
        break;

    case FDTAB_TYPE_PTS:
        ret = pts_write(fd, buf, len);
        break;

    default:
        ret = vfsfd_write(fd, buf, len);
        break;
    }

    return ret;
}
