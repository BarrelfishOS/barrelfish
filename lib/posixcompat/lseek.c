/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <unistd.h>
#include <vfs/vfs.h>
#include "fdtab.h"
#include "posixcompat.h"

off_t lseek(int fd, off_t offset, int whence)
{
    struct fdtab_entry *e = fdtab_get(fd);
    switch(e->type) {
    case FDTAB_TYPE_FILE:
        {
            enum vfs_seekpos vfs_whence;
            errval_t err;
            size_t retpos;

            switch(whence) {
            case SEEK_SET:
                vfs_whence = VFS_SEEK_SET;
                break;

            case SEEK_CUR:
                vfs_whence = VFS_SEEK_CUR;
                break;

            case SEEK_END:
                vfs_whence = VFS_SEEK_END;
                break;

            default:
                return -1;
            }

            err = vfs_seek((vfs_handle_t)e->handle, vfs_whence, offset);
            if(err_is_fail(err)) {
                DEBUG_ERR(err, "vfs_seek");
                return -1;
            }

            err = vfs_tell((vfs_handle_t)e->handle, &retpos);
            if(err_is_fail(err)) {
                DEBUG_ERR(err, "vfs_tell");
                return -1;
            }

            POSIXCOMPAT_DEBUG("lseek(%d, %lld, %d) = %zu\n",
                              fd, offset, whence, retpos);

            return retpos;
        }
        break;

    default:
        return -1;
    }
}
