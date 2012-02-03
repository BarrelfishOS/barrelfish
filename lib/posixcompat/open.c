/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <fcntl.h>
#include <vfs/vfs_fd.h>
#include "posixcompat.h"

//XXX: flags are ignored...
int open(const char *pathname, int flags, ...)
{
    return vfsfd_open(pathname, flags);
}
