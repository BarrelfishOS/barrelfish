/**
 * \file
 * \brief Some arch specific asm inlines
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_KPI_ASM_INLINES_H
#define ARCH_AARCH64_BARRELFISH_KPI_ASM_INLINES_H

#ifndef __ASSEMBLER__

#include <stdint.h>

static inline uint64_t rdtsc(void)
{  
    uint64_t ccnt = 0;
    __asm__ volatile("mrs %[ccnt], CNTPCT_EL0;" : [ccnt] "=r" (ccnt));
    return ccnt;
}

static inline uint64_t rdtscp(void)
{
    uint64_t ccnt;
    /* An ISB flushes the pipeline, and re-fetches the instructions from the
     * cache or memory and ensures that the effects of any completed
     * context-changing operation before the ISB are visible to any instruction
     * after the ISB
     */
    __asm__ volatile("isb" : : : "memory");
    __asm__ volatile("mrs %[ccnt], CNTVCT_EL0;" : [ccnt] "=r" (ccnt));
    return ccnt;
}


static inline void dmb(void)
{
	__asm volatile ("dmb sy" : : : "memory");
}

static inline uint8_t is_cycle_counter_overflow(void)
{
	//NYI
	return 0;
}

static inline uint32_t get_cycle_count(void)
{
	//NYI
	return 0;
}


static inline void reset_cycle_counter(void)
{
	//NYI
}

#endif // __ASSEMBLER__

#endif // ARCH_AARCH64_BARRELFISH_KPI_ASM_INLINES_H
