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

#ifndef ARCH_X86_BARRELFISH_BENCH_H
#define ARCH_X86_BARRELFISH_BENCH_H

#include <bench/bench.h>
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>

extern bool rdtscp_flag;

void bench_arch_init(void);

/**
 * \brief Take a timestamp
 */
static inline cycles_t bench_tsc(void)
{
    if (rdtscp_flag) {
        return rdtscp();
    }
    return rdtsc();
}

uint64_t bench_tsc_to_ms(cycles_t tsc);

#endif // ARCH_X86_64_BARRELFISH_BENCH_H
