/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // for strdup()
#include <string.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include "posixcompat.h"

char *_posixcompat_cwd;

void _posixcompat_cwd_init(void)
{
    if (_posixcompat_cwd == NULL) {
        _posixcompat_cwd = strdup("/");
        assert(_posixcompat_cwd != NULL);
    }
}

int chdir(const char *pathname)
{
    errval_t err;

    if (_posixcompat_cwd == NULL) {
        _posixcompat_cwd_init();
    }

    char *newcwd = vfs_path_mkabsolute(_posixcompat_cwd, pathname);
    assert(newcwd != NULL);

    // ensure directory exists, by attempting to open it
    vfs_handle_t dh;
    err = vfs_opendir(newcwd, &dh);
    if (err_is_fail(err)) {
        POSIXCOMPAT_DEBUG("chdir('%s') -> '%s' FAILED\n", pathname, newcwd);
        free(newcwd);
        return -1;
    }
    vfs_closedir(dh);

    POSIXCOMPAT_DEBUG("chdir('%s') -> '%s'\n", pathname, newcwd);

    // ok!
    free(_posixcompat_cwd);
    _posixcompat_cwd = newcwd;

    return 0;
}
