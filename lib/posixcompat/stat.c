/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include "posixcompat.h"

void _posixcompat_vfs_info_to_stat(struct vfs_fileinfo *info, struct stat *buf)
{
    memset(buf, 0, sizeof(struct stat));
    buf->st_size = info->size;
    buf->st_mode = 0777;
    if (info->type == VFS_DIRECTORY) {
        buf->st_mode |= S_IFDIR;
    } else if (info->type == VFS_FILE) {
        buf->st_mode |= S_IFREG;
    }        
}

int stat(const char *pathname, struct stat *buf)
{
    struct vfs_fileinfo info;
    vfs_handle_t vh;
    int ret;
    errval_t err;

    if (_posixcompat_cwd == NULL) {
        _posixcompat_cwd_init();
    }

    char *path = vfs_path_mkabsolute(_posixcompat_cwd, pathname);
    assert(path != NULL);

    bool isdir = false;
    err = vfs_open(path, &vh);
    if (err_is_fail(err) && err_no(err) == FS_ERR_NOTFILE) {
        // try to open it as a directory
        err = vfs_opendir(path, &vh);
        isdir = true;
    }
    free(path);
    if (err_is_fail(err)) {
        POSIXCOMPAT_DEBUG("stat('%s') failed\n", pathname);
        return -1;
    }

    POSIXCOMPAT_DEBUG("stat('%s')\n", pathname);

    err = vfs_stat(vh, &info);
    if (err_is_ok(err)) {
        ret = 0;
        _posixcompat_vfs_info_to_stat(&info, buf);
    } else {
        ret = -1;
        DEBUG_ERR(err, "stat('%s') failed\n", pathname);
    }

    if (isdir) {
        vfs_closedir(vh);
    } else {
        vfs_close(vh);
    }

    return ret;
}
