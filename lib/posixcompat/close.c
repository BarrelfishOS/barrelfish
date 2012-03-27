/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs_fd.h>
#include <stdio.h>
#include <lwip/sockets.h>
#include <vfs/fdtab.h>
#include "posixcompat.h"

int close(int fd)
{
    int ret;
    struct fdtab_entry *e = fdtab_get(fd);
    if (e->type == FDTAB_TYPE_AVAILABLE) {
        return -1;
    }

    if (e->type == FDTAB_TYPE_LWIP_SOCKET) {
        if(e->inherited) {
            // Perform shallow close on lwip so that it will not terminate
            // the TCP session
            printf("close: Inherited socket, not closing completely\n");
            ret = 0;
        } else {
            ret = lwip_close(e->fd);
            if(ret < 0) {
                POSIXCOMPAT_DEBUG("[%d]error in lwip_close\n",
                        disp_get_domain_id());
                return -1;
            }
        }
        fdtab_free(fd);
    } else {
        ret = vfsfd_close(fd);
    }

    return ret;
}
