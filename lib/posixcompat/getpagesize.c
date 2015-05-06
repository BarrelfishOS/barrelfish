/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/paging_arch.h>

#include "posixcompat.h"

// this is rather UNIX than POSIX.
int getpagesize(void)
{
    return BASE_PAGE_SIZE;
}
