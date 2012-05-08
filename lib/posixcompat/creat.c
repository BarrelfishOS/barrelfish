/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <barrelfish/barrelfish.h>
#include "posixcompat.h"

int creat (char * file, int mode)
{
    USER_PANIC("creat() NYI");
    return -1;
}
