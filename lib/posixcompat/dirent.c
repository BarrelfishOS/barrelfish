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
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include "posixcompat.h"

DIR *opendir(const char *pathname)
{
    vfs_handle_t vh;
    errval_t err;

    if (_posixcompat_cwd == NULL) {
        _posixcompat_cwd_init();
    }

    char *path = vfs_path_mkabsolute(_posixcompat_cwd, pathname);
    assert(path != NULL);

    err = vfs_opendir(path, &vh);
    free(path);
    if (err_is_fail(err)) {
        POSIXCOMPAT_DEBUG("opendir(%s) not found\n", pathname);
        return NULL;
    }

    POSIXCOMPAT_DEBUG("opendir(%s)\n", pathname);

    DIR *ret = malloc(sizeof(DIR));
    assert(ret != NULL);

    ret->vh = vh;
    return ret;
}

struct dirent *readdir(DIR* dir)
{
    char *name;
    errval_t err;

    err = vfs_dir_read_next(dir->vh, &name, NULL);
    if (err_is_fail(err)) {
        return NULL;
    }

    strncpy(dir->dirent.d_name, name, sizeof(dir->dirent.d_name));
    free(name);
    dir->dirent.d_name[sizeof(dir->dirent.d_name) - 1] = '\0';
    return &dir->dirent;
}

int closedir(DIR *dir)
{
    vfs_closedir(dir->vh);
    free(dir);
    return(0);
}
