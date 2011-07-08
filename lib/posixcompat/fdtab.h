/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _FDTAB_H
#define _FDTAB_H

#include <vfs/vfs.h>

int fdtab_alloc(vfs_handle_t h);
int fdtab_search_alloc(vfs_handle_t h);
vfs_handle_t fdtab_get(int fd);
void fdtab_free(int fd);

#endif // _FDTAB_H
