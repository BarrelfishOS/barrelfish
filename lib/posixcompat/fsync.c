/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include "posixcompat.h"

int fsync(int fd)
{
    POSIXCOMPAT_DEBUG("Warning: fsync(%d) ignored!\n", fd);
    return 0;
}
