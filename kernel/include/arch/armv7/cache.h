/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARMV7_CACHE_H
#define __ARMV7_CACHE_H

#include <bitmacros.h>
#include <cp15.h>
#include <stdio.h>

/* The cache level at which the instruction cache, data cache and table walks
 * are unified, from a single processor. */
static inline size_t
cache_get_louu(void) {
    uint32_t clidr= cp15_read_clidr();
    return (clidr >> 27) & MASK(3);
}

enum armv7_cache_type {
    armv7_cache_none    = 0x0,
    armv7_cache_i       = 0x1,
    armv7_cache_d       = 0x2,
    armv7_cache_id      = 0x3,
    armv7_cache_unified = 0x4,
};

static inline enum armv7_cache_type
cache_get_type(size_t level) {
    assert(level >= 1 && level <= 7);

    uint32_t clidr= cp15_read_clidr();

    /* Each cache level has a 3-bit field with LSB at 3*(n-1). */
    return (clidr >> (3 * (level-1))) & MASK(3);
}

static inline uint32_t
cache_get_ccsidr(size_t level, int icache) {
    assert(level >= 1 && level <= 7);

    /* Select the cache level and type. */
    cp15_write_csselr((((level - 1) & MASK(3)) << 1) | (icache & MASK(1)));

    /* Read the size register. */
    return cp15_read_ccsidr();
}

/* Count set bits. */
static inline size_t
csb(uint32_t x) {
    return 32 - __builtin_clz(x);
}

/* Invalidate an entire data cache. */
static inline void
invalidate_data_cache(size_t level, bool clean) {
    assert(level >= 1 && level <= 7);

    enum armv7_cache_type type= cache_get_type(level);

    /* If there's no data cache at this level, there's nothing to do. */
    if(type == armv7_cache_none || type == armv7_cache_i) return;

    /* Get the details of the data cache at this level. See TRM B4.1.19. */
    uint32_t ccsidr= cache_get_ccsidr(level, 0);
    size_t L= (ccsidr & MASK(3)) + 2;       /* log2(LINELEN) */

    size_t x=    (ccsidr >>  3) & MASK(10); /* WAYS - 1 */
    size_t A=    csb(x);                    /* ceil(log2(WAYS)) */
    size_t ways= x + 1;

    size_t y=    (ccsidr >> 13) & MASK(15); /* SETS - 1 */
    size_t S=    csb(x);                    /* ceil(log2(SETS)) */
    size_t sets= y + 1;

    /* Calculate the set/way register format. See TRM B4.2.1. */
    size_t w_shift= 32 - A;
    size_t s_shift= L + S;

    for(size_t w= 0; w < ways; w++) {
        for(size_t s= 0; s < sets; s++) {
            uint32_t wsl= (w << w_shift) |
                          (s << s_shift) |
                          ((level - 1) << 1);
            if(clean) cp15_write_dccisw(wsl);
            else      cp15_write_dcisw(wsl);
        }
    }
}

/* Invalidate (and possibly clean) all data caches to point of unification. */
static inline void
invalidate_data_caches_pouu(bool clean) {
    size_t louu= cache_get_louu();

    for(size_t level= 1; level <= louu; level++)
        invalidate_data_cache(level, clean);
}

/* Invalidate this core's instruction cache. */
static inline void
invalidate_instruction_cache(void) {
    cp15_write_iciallu(0); /* The argument is ignored. */
}

/* Invalidate all TLBs on this core. */
static inline void
invalidate_tlb(void) {
    cp15_write_tlbiall(0); /* The argument is ignored. */

    /* Ensure the invalidate has completed. */
    dsb();

    /* Ensure that the invalidate completes in program order. */
    isb();
}

/* Clean a cache line to point of unification - for table walks and
 * instruction fetches.  Takes a virtual address. */
static inline void
clean_to_pou(void *addr) {
    cp15_write_dccmvau((uint32_t)addr);
}

#endif /* __ARMV7_CACHE_H */
