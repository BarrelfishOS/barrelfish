/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/stat.h>
#include <barrelfish/barrelfish.h>

mode_t umask(mode_t mask)
{
    USER_PANIC("umask() NYI");
    return 0;
}
