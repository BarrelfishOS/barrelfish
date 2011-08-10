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
#include <vfs/vfs.h>
#include "fdtab.h"
#include "posixcompat.h"

int close(int fd)
{
    errval_t err;
    int ret = 0;

    struct fdtab_entry *e = fdtab_get(fd);
    if (e->type == FDTAB_TYPE_AVAILABLE) {
        return -1;
    }

    POSIXCOMPAT_DEBUG("close(%d)\n", fd);

    switch(e->type) {
    case FDTAB_TYPE_FILE:
        err = vfs_close((vfs_handle_t)e->handle);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error in vfs_close");
            return -1;
        }
        break;

    case FDTAB_TYPE_STDIN:
    case FDTAB_TYPE_STDOUT:
    case FDTAB_TYPE_STDERR:
        // XXX: Should call fclose() when closing last FD
        break;

    case FDTAB_TYPE_LWIP_SOCKET:
        if(e->inherited) {
            // Perform shallow close on lwip so that it will not terminate
            // the TCP session
            printf("close: Inherited socket, not closing completely\n");
        } else {
            ret = lwip_close(e->fd);
            if(ret < 0) {
                POSIXCOMPAT_DEBUG("[%d]error in lwip_close\n",
                        disp_get_domain_id());
                return -1;
            }
        }
        break;

    default:
        return -1;
    } // end switch

    fdtab_free(fd);
    return 0;
}
