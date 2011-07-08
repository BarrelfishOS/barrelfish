/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "posixcompat.h"

pid_t getpid(void)
{
    POSIXCOMPAT_DEBUG("getpid() = 3\n");
    return(3);
}
