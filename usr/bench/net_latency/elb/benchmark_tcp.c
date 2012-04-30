/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "elb.h"

#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

#include <lwip/init.h>


void benchmark_init(size_t buffers)
{
    buffer_count = buffers;

    printf("tcp benchmark: init started\n");
    lwip_init("e10k", 0);
    printf("tcp benchmark: lwip init done\n");
}

void benchmark_argument(const char *arg)
{
}


