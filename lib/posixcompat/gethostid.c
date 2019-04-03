/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <stdio.h>
#include <assert.h>

long gethostid(void)
{
    fprintf(stderr, "XXX: gethostid() returns a constant\n");
    return(0xba33e1f1);
}
