/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef POSIXCOMPAT_INTERNAL_H
#define POSIXCOMPAT_INTERNAL_H

#include <barrelfish/barrelfish.h>

struct vfs_fileinfo;
struct stat;

void _posixcompat_vfs_info_to_stat(struct vfs_fileinfo *info, struct stat *buf);

// Internal use only, set error which can be retrieved through
// posixcompat_get_bf_error() declared in public header include/posixcompat.h
void posixcompat_set_bf_error(errval_t err);

#if 0
#define POSIXCOMPAT_DEBUG(x...) debug_printf(x)
#define POSIXCOMPAT_DEBUG_ENABLED
#else
#define POSIXCOMPAT_DEBUG(x...) ((void)0)
#endif

#endif
