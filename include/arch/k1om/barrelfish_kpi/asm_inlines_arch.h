/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_KPI_ASM_INLINES_H
#define ARCH_X86_64_BARRELFISH_KPI_ASM_INLINES_H

#ifndef __ASSEMBLER__

#include <target/k1om/barrelfish_kpi/registers_target.h>

#define CLOCK_PER_MS 1046008

static inline void
delay_ms(uint32_t ms)
{
    if (ms == 0) {
        return;
    }
    uint64_t num_cpu_clks = ms * CLOCK_PER_MS;

    if (num_cpu_clks <= 1023) {
        /* since delay can only go upto 1023 clocks */
        __asm __volatile("delay %0"::"r"(num_cpu_clks));
    } else {
        /* break it up into 1000 clock chunks */
        for (uint32_t tick = 1000; num_cpu_clks >= 1000; num_cpu_clks -= 1000) {
            __asm __volatile("delay %0"::"r"(tick));
        }

        /* the remaining */
        __asm __volatile("delay %0"::"r"(num_cpu_clks));
    }
    return;
}

/*
 * some functions require to have the register ECX set to a specific value
 * and will produce wrong results if the value in register ECX is out of range.
 */
static inline void
cpuid_ext(uint32_t function,
          uint32_t ecx_in,
          uint32_t *eax,
          uint32_t *ebx,
          uint32_t *ecx,
          uint32_t *edx)
{
    // make it possible to omit certain return registers
    uint32_t a, b, c, d;
    if (eax == NULL) {
        eax = &a;
    }
    if (ebx == NULL) {
        ebx = &b;
    }
    if (ecx == NULL) {
        ecx = &c;
    }
    if (edx == NULL) {
        edx = &d;
    }
    __asm volatile("cpuid"
            : "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)
            : "a" (function), "c" (ecx_in)
    );
}

static inline void
cpuid(uint32_t function,
      uint32_t *eax,
      uint32_t *ebx,
      uint32_t *ecx,
      uint32_t *edx)
{
    cpuid_ext(function, 0, eax, ebx, ecx, edx);
}

/** \brief Atomic compare-and-swap on 128 bits
 *
 * If *dst == old then *dst = new, returns 0 on failure
 *
 * Note, dest should point to a 128bit structure that is to be overwritten
 */
static inline int
cmpxchg128(volatile uint64_t dest[2],
           uint64_t old_top,
           uint64_t old_bot,
           uint64_t new_top,
           uint64_t new_bot)
{
    uint8_t ret;

    __asm volatile (
            "lock cmpxchg16b %1\n\t"
            "setz %0\n\t"
            : "=a"(ret), "=m"(*dest)  //, "=d"(old_top), "=a"(old_bot)
            : "a"(old_top), "d"(old_bot), "b"(new_top), "c"(new_bot), "m"(*dest)
            : "memory");

    return ret;
}

static inline void
fpu_init(void)
{
    __asm volatile ("fninit");
}

static inline void
fpu_save(struct registers_fpu_x86_64 *fpustate)
{
    uint8_t *regs = fpustate->registers;
    regs += 16 - ((uintptr_t) regs % 16);

    __asm volatile("fxsaveq %0" : "=m" (*regs));
}

static inline void
fpu_restore(struct registers_fpu_x86_64 *fpustate)
{
    uint8_t *regs = fpustate->registers;
    regs += 16 - ((uintptr_t) regs % 16);

    __asm volatile ("fxrstorq %0" :: "m" (*regs));
}

/** \brief This code reads the cycle counter */
static inline uint64_t
rdtsc(void)
{

    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t) edx << 32) | eax;

    return 0;
}

/** \brief This code reads the cycle counter -- flushing the
 instruction pipeline first. Throws away the processor ID information. */
static inline uint64_t
rdtscp(void)
{
    /*
     uint32_t eax, edx;
     __asm volatile ("rdtscp" : "=a" (eax), "=d" (edx) :: "ecx");
     return ((uint64_t)edx << 32) | eax;
     */
    return 0;
}

static inline uint64_t
rdpmc(uint32_t counter)
{
    /*
    uint32_t eax, edx;

     __asm volatile("rdpmc"
     : "=a" (eax), "=d" (edx)
     : "c" (counter)
     );

    return ((uint64_t) edx << 32) | eax;
     */
    return 0;
}

static inline void
mfence(void)
{
    // __asm volatile("mfence");
}

static inline void
sfence(void)
{
    //  __asm volatile("sfence");
}

static inline void
lfence(void)
{
    // __asm volatile("lfence");
}

static inline void
clflush(void *line)
{
    __asm volatile("clflush %0" :: "m" (line));
}

#ifndef __scc__
#       define CACHE_LINE_SIZE 64 /* bytes */
#else
#       define CACHE_LINE_SIZE 32 /* bytes */
#endif

#ifndef __cplusplus
/* flush a range of memory from the cache */
static inline void cache_flush_range(void *base, size_t len)
{
    //mfence();

    uint8_t *line = (uint8_t *)((uintptr_t)base & ~(CACHE_LINE_SIZE-1UL));
    do {
        clflush(line);
        line += CACHE_LINE_SIZE;
    }while (line < (uint8_t *)base + len);
}
#endif

#endif // __ASSEMBLER__

#endif // ARCH_X86_64_BARRELFISH_KPI_ASM_INLINES_H
