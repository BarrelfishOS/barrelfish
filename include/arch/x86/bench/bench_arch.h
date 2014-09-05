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

#include <stdbool.h>
#include <sys/cdefs.h>

#if defined(__k1om__)
#include <barrelfish_kpi/asm_inlines_arch.h>
#else
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>
#endif
#include <bench/bench.h>

__BEGIN_DECLS

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
uint64_t bench_tsc_to_us(cycles_t tsc);
uint64_t bench_tsc_per_us(void);
uint64_t bench_tsc_per_ms(void);
__END_DECLS

#endif // ARCH_X86_64_BARRELFISH_BENCH_H
