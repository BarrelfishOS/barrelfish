/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __CP15_H__
#define __CP15_H__

/**
 * \brief Read domain access control register
 */
static inline uint32_t cp15_read_dacr(void)
{
    panic("NYI");
}

/**
 * \brief Read instruction fault status register.
 */
static inline uint32_t cp15_read_ifsr(void)
{
    panic("NYI");
}

/**
 * \brief Read data fault status register.
 */
static inline uint32_t cp15_read_dfsr(void)
{
    panic("NYI");
}

/**
 * \brief Read fault address register.
 */
static inline uint32_t cp15_read_far(void)
{
    panic("NYI");
}

static inline lpaddr_t cp15_read_ttbr0(void)
{
    panic("NYI");
}

static inline lpaddr_t cp15_read_ttbr1(void)
{
    panic("NYI");
}

static inline void cp15_write_ttbr0(lpaddr_t ttbr)
{
    panic("NYI");
}

static inline void cp15_write_ttbr1(lpaddr_t ttbr)
{
    panic("NYI");
}

static inline uint32_t cp15_read_ttbcr(void)
{
    panic("NYI");
}

static inline void cp15_write_ttbcr(uint32_t ttbcr)
{
    panic("NYI");
}

extern void cp15_invalidate_d_cache(void);
extern void cp15_invalidate_i_and_d_caches(void);
extern void cp15_invalidate_i_and_d_caches_fast(void);
extern void cp15_invalidate_tlb_fn(void);
extern void cp15_enable_mmu(void);
extern void cp15_enable_alignment(void);

static inline uint32_t cp15_read_cache_status(void){
    panic("NYI");
}


static inline void cp15_disable_cache(void){

    cp15_invalidate_i_and_d_caches_fast();

    panic("NYI");

    printf("WARNING! Caching has been disabled, configuration is: %"PRIx32"\n", cp15_read_cache_status());

}

static inline void cp15_invalidate_tlb(void)
{
    panic("NYI");
}

static inline uint8_t cp15_get_cpu_id(void) {
    panic("NYI");
}

/*
 * Get the configuration base address
 * This is described in the Cortex A9 TRM, 4.2.32
 */
static inline uint32_t cp15_read_cbar(void)
{
    panic("NYI");
}

#endif // __CP15_H__
