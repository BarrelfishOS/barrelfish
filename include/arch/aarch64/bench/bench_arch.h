/**
 * \file
 * \brief Arch specific bench include.
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_BENCH_H
#define ARCH_AARCH64_BARRELFISH_BENCH_H

#include <barrelfish/sys_debug.h>
#include <bench/bench.h>
#include <stdio.h>

extern uint64_t tsc_hz;
void bench_arch_init(void);


/**
 * \brief Take a timestamp
 */
static inline cycles_t bench_tsc(void)
{
    STATIC_ASSERT_SIZEOF(cycles_t, sizeof(uintptr_t));
    cycles_t tsc;
    sys_debug_hardware_timer_read((uintptr_t *)&tsc);
    return tsc;
}

uint64_t bench_tsc_to_ms(cycles_t tsc);
uint64_t bench_tsc_to_us(cycles_t tsc);
uint64_t bench_tsc_per_us(void);
uint64_t bench_tsc_per_ms(void);
#endif // ARCH_AARCH64_BARRELFISH_BENCH_H
