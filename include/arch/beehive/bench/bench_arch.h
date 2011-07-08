/**
 * \file
 * \brief Arch specific bench include.
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_BENCH_H
#define ARCH_BEEHIVE_BARRELFISH_BENCH_H

#include <bench/bench.h>

void bench_arch_init(void);

/**
 * \brief Take a timestamp
 */
static inline cycles_t bench_tsc(void)
{
    return *(volatile size_t *)(0x22);
}

#endif // ARCH_BEEHIVE_BARRELFISH_BENCH_H
