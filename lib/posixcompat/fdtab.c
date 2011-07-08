/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include "fdtab.h"

#define MIN_FD  3 // avoid using stdin/out/err FDs
#define MAX_FD  32

static vfs_handle_t fdtab[MAX_FD];

int fdtab_alloc(vfs_handle_t h)
{
    assert(h != NULL_VFS_HANDLE);
    for (int fd = MIN_FD; fd < MAX_FD; fd++) {
        if (fdtab[fd] == NULL_VFS_HANDLE) {
            fdtab[fd] = h;
            return fd;
        }
    }
    fprintf(stderr, "Warning: out of FDs in posixcompat lib\n");
    return -1; // table full
}

int fdtab_search_alloc(vfs_handle_t h)
{
    assert(h != NULL_VFS_HANDLE);
    for (int fd = MIN_FD; fd < MAX_FD; fd++) {
        if (fdtab[fd] == h) {
            return fd;
        }
    }
    return fdtab_alloc(h);
}

vfs_handle_t fdtab_get(int fd)
{
    if (fd < MIN_FD || fd >= MAX_FD) {
        return NULL_VFS_HANDLE;
    } else {
        return fdtab[fd];
    }
}

void fdtab_free(int fd)
{
    assert(fd >= MIN_FD && fd < MAX_FD);
    assert(fdtab[fd] != NULL_VFS_HANDLE);
    fdtab[fd] = NULL_VFS_HANDLE;
}
