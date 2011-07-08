/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef POSIXCOMPAT_INTERNAL_H
#define POSIXCOMPAT_INTERNAL_H

#include <barrelfish/barrelfish.h>

struct vfs_fileinfo;
struct stat;

extern char *_posixcompat_cwd;
void _posixcompat_cwd_init(void);
void _posixcompat_vfs_info_to_stat(struct vfs_fileinfo *info, struct stat *buf);

#if 0
#define POSIXCOMPAT_DEBUG(x...) debug_printf(x)
#else
#define POSIXCOMPAT_DEBUG(x...) ((void)0)
#endif

#endif
