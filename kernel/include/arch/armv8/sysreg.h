/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SYSREG_H__
#define __SYSREG_H__

/**
 * \brief Read domain access control register
 */
static inline uint32_t sysreg_read_dacr(void)
{
    panic("NYI");
}

/**
 * \brief Read instruction fault status register.
 */
static inline uint32_t sysreg_read_ifsr(void)
{
    panic("NYI");
}

/**
 * \brief Read data fault status register.
 */
static inline uint32_t sysreg_read_dfsr(void)
{
    panic("NYI");
}

/**
 * \brief Read fault address register.
 */
static inline uint32_t sysreg_read_far(void)
{
    panic("NYI");
}

static inline uintptr_t get_current_el(void)
{
    uintptr_t currentel;
    __asm volatile ("mrs %[curel], currentel" : [curel] "=r" (currentel));
    return (currentel >> 2) & 0x3;
}

static inline lpaddr_t sysreg_read_ttbr0_el1(void)
{
    lpaddr_t ttbr0;
    __asm volatile ("mrs %[ttbr], ttbr0_el1" : [ttbr] "=r" (ttbr0));
    return ttbr0;
}

static inline lpaddr_t sysreg_read_ttbr0_el2(void)
{
    lpaddr_t ttbr0;
    __asm volatile ("mrs %[ttbr], ttbr0_el2" : [ttbr] "=r" (ttbr0));
    return ttbr0;
}

static inline lpaddr_t sysreg_read_ttbr0(void)
{
    uintptr_t currentel = get_current_el();
    if (currentel == 1) {
        return sysreg_read_ttbr0_el1();
    } else if (currentel == 2) {
        return sysreg_read_ttbr0_el2();
    }
    panic("%s: Unsupported EL: %d\n", __FUNCTION__, currentel);
}

static inline void sysreg_write_ttbr0_el1(lpaddr_t ttbr)
{
    __asm volatile ("msr ttbr0_el1, %[ttbr]" : [ttbr] "=r" (ttbr));
}

static inline void sysreg_write_ttbr0_el2(lpaddr_t ttbr)
{
    __asm volatile ("msr ttbr0_el2, %[ttbr]" : [ttbr] "=r" (ttbr));
}

static inline void sysreg_write_ttbr0(lpaddr_t ttbr)
{
    uintptr_t currentel = get_current_el();
    if (currentel == 1) {
        sysreg_write_ttbr0_el1(ttbr);
    } else if (currentel == 2) {
        sysreg_write_ttbr0_el2(ttbr);
    }
    panic("%s: Unsupported EL: %d\n", __FUNCTION__, currentel);
}

static inline lpaddr_t sysreg_read_ttbr1_el1(void)
{
    lpaddr_t ttbr1;
    __asm volatile ("mrs %[ttbr], ttbr1_el1" : [ttbr] "=r" (ttbr1));
    return ttbr1;
}

static inline lpaddr_t sysreg_read_ttbr1_el2(void)
{
    lpaddr_t ttbr1;
    __asm volatile ("mrs %[ttbr], ttbr1_el2" : [ttbr] "=r" (ttbr1));
    return ttbr1;
}

static inline lpaddr_t sysreg_read_ttbr1(void)
{
    uintptr_t currentel = get_current_el();
    if (currentel == 1) {
        return sysreg_read_ttbr1_el1();
    } else if (currentel == 2) {
        return sysreg_read_ttbr1_el2();
    }
    panic("%s: Unsupported EL: %d\n", __FUNCTION__, currentel);
}

static inline void sysreg_write_ttbr1_el1(lpaddr_t ttbr)
{
    __asm volatile ("msr ttbr1_el1, %[ttbr]" : [ttbr] "=r" (ttbr));
}

static inline void sysreg_write_ttbr1_el2(lpaddr_t ttbr)
{
    __asm volatile ("msr ttbr1_el2, %[ttbr]" : [ttbr] "=r" (ttbr));
}

static inline lpaddr_t sysreg_write_ttbr1(lpaddr_t ttbr)
{
    uintptr_t currentel = get_current_el();
    if (currentel == 1) {
        sysreg_write_ttbr1_el1(ttbr);
    } else if (currentel == 2) {
        sysreg_write_ttbr1_el2(ttbr);
    }
    panic("%s: Unsupported EL: %d\n", __FUNCTION__, currentel);
}

static inline uint32_t sysreg_read_ttbcr(void)
{
    panic("NYI");
}

static inline void sysreg_write_ttbcr(uint32_t ttbcr)
{
    panic("NYI");
}

static inline uint32_t sysreg_read_cache_status(void){
    panic("NYI");
}


static inline void sysreg_disable_cache(void){

    panic("NYI");

    printf("WARNING! Caching has been disabled, configuration is: %"PRIx32"\n", sysreg_read_cache_status());

}

static inline void sysreg_invalidate_tlb(void)
{
    panic("NYI");
}

static inline uint8_t sysreg_get_cpu_id(void) {
    panic("NYI");
}

/*
 * Get the configuration base address
 * This is described in the Cortex A9 TRM, 4.2.32
 */
static inline uint32_t sysreg_read_cbar(void)
{
    panic("NYI");
}

#endif // __SYSREG_H__
