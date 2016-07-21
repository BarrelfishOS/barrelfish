/*
 * Copyright (c) 2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SYSREG_H__
#define __SYSREG_H__

#include <bitmacros.h>

void sysreg_invalidate_i_and_d_caches_fast(void);
void sysreg_invalidate_i_and_d_caches(void);
void sysreg_invalidate_tlb_fn(void);
void sysreg_enable_mmu(void);

/**
 * \brief Read instruction fault status register.
 */
static inline uint64_t
sysreg_read_ifsr(void) {
    uint64_t ifsr;
    __asm volatile("mrs %[ifsr], esr_el1" : [ifsr] "=r" (ifsr));
    return ifsr;
}

/**
 * \brief Read data fault status register.
 */
static inline uint64_t
sysreg_read_dfsr(void) {
    uint64_t dfsr;
    __asm volatile("mrs %[dfsr], esr_el1" : [dfsr] "=r" (dfsr));
    return dfsr;
}

/**
 * \brief Read fault address register.
 */
static inline uint64_t
sysreg_read_far(void) {
    uint64_t addr;
    __asm volatile("mrs %[addr], far_el1" : [addr] "=r" (addr));
    return addr;
}

/**
 * \brief Read Exception Syndrome Register EL1.
 */
static inline uint64_t
sysreg_read_esr_el1(void) {
    uint64_t addr;
    __asm volatile("mrs %[addr], esr_el1" : [addr] "=r" (addr));
    return addr;
}

static inline uintptr_t
get_current_el(void) {
    uintptr_t currentel;
    __asm volatile("mrs %[curel], currentel" : [curel] "=r" (currentel));
    return (currentel >> 2) & 0x3; /* bits [3:2] */
}

static inline lpaddr_t
sysreg_read_ttbr0_el1(void) {
    lpaddr_t ttbr0;
    __asm volatile("mrs %[ttbr], ttbr0_el1" : [ttbr] "=r" (ttbr0));
    return ttbr0;
}

static inline lpaddr_t
sysreg_read_ttbr0_el2(void) {
    lpaddr_t ttbr0;
    __asm volatile("mrs %[ttbr], ttbr0_el2" : [ttbr] "=r" (ttbr0));
    return ttbr0;
}

static inline lpaddr_t
sysreg_read_ttbr0_el3(void) {
    lpaddr_t ttbr0;
    __asm volatile ("mrs %[ttbr], ttbr0_el3" : [ttbr] "=r" (ttbr0));
    return ttbr0;
}

static inline void
sysreg_write_ttbr0_el1(lpaddr_t ttbr) {
    __asm volatile ("msr ttbr0_el1, %[ttbr]" : : [ttbr] "r" (ttbr));
}

static inline void
sysreg_write_ttbr0_el2(lpaddr_t ttbr) {
    __asm volatile ("msr ttbr0_el2, %[ttbr]" : : [ttbr] "r" (ttbr));
}

static inline lpaddr_t
sysreg_read_ttbr1_el1(void) {
    lpaddr_t ttbr1;
    __asm volatile ("mrs %[ttbr], ttbr1_el1" : [ttbr] "=r" (ttbr1));
    return ttbr1;
}

static inline lpaddr_t
sysreg_read_ttbr1_el2(void) {
    lpaddr_t ttbr1;
    __asm volatile ("mrs %[ttbr], ttbr1_el2" : [ttbr] "=r" (ttbr1));
    return ttbr1;
}

static inline void
sysreg_write_ttbr1_el1(lpaddr_t ttbr) {
    __asm volatile ("msr ttbr1_el1, %[ttbr]" : : [ttbr] "r" (ttbr));
}

static inline void
sysreg_write_ttbr1_el2(lpaddr_t ttbr) {
    __asm volatile ("msr ttbr1_el2, %[ttbr]" : : [ttbr] "r" (ttbr));
}

static inline uint64_t
sysreg_read_ttbcr(void) {
    uint64_t ttbcr;
    __asm volatile(" mrs %[ttbcr], tcr_el1" : [ttbcr] "=r" (ttbcr));
    return ttbcr;
}

static inline void
sysreg_write_ttbcr(uint64_t ttbcr) {
    __asm volatile("msr tcr_el1, %[ttbcr]" : : [ttbcr] "r" (ttbcr));
}

static inline uint64_t
sysreg_read_cache_status(void) {
    uint64_t cache;
    __asm volatile(" mrs %[cache], sctlr_el1" : [cache] "=r" (cache));
    return cache;
}

static inline void
sysreg_invalidate_tlb(void) {
    __asm volatile("tlbi vmalle1");
}

static inline uint8_t
sysreg_get_cpu_id(void) {
    uint8_t mpidr;
    __asm volatile("mrs %[mpidr], mpidr_el1" : [mpidr] "=r" (mpidr));
    return mpidr & 0x3;
}

/*
 * Get the address of the GIC CPU interface registers.
 * See Cortex-A57 TRM, S4.3.70.
 */
static inline uint64_t
sysreg_read_cbar(void) {
    uint64_t cbar;
    __asm volatile("mrs %[cbar], s3_1_c15_c3_0" : [cbar] "=r" (cbar));
    /*
     * bits 18..43 of this are PERIPHBASE[43:18].  we need to mask out bits
     * 44..63 and 0..17
     */
    return cbar & ((1UL << 44) - 1) & ~((1UL << 18) - 1);
}

static inline uint64_t
sysreg_read_sp_el0(void) {
    uint64_t sp_el0;
    __asm volatile("mrs %[sp_el0], sp_el0" : [sp_el0] "=r" (sp_el0));
    return sp_el0;
}

static inline uint64_t
sysreg_read_sp(void) {
    uint64_t sp;
    __asm volatile("mov %[sp], sp" : [sp] "=r" (sp));
    return sp;
}

static inline void
sysreg_write_sp(uint64_t sp) {
    __asm volatile("mov sp, %[sp]" : : [sp] "r" (sp));
}

static inline void
sysreg_write_vbar_el1(uint64_t vbar_el1) {
    __asm volatile("msr vbar_el1, %[vbar_el1]" : : [vbar_el1] "r" (vbar_el1));
}

static inline void
sysreg_write_sp_el1(uint64_t sp_el1) {
    __asm volatile("msr sp_el1, %[sp_el1]" : : [sp_el1] "r" (sp_el1));
}

static inline void
sysreg_write_tpidrro_el0(uint64_t x) {
    __asm volatile("msr tpidrro_el0, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_tpidr_el1(uint64_t x) {
    __asm volatile("msr tpidr_el1, %[x]" : : [x] "r" (x));
}

static inline uint64_t
sysreg_get_id_aa64pfr0_el1(void) {
    uint64_t pfr;
    __asm volatile("mrs %[pfr], id_aa64pfr0_el1" : [pfr] "=r" (pfr));
    return pfr;
}

static inline uint64_t
sysreg_get_id_aa64mmfr0_el1(void) {
    uint64_t pfr;
    __asm volatile("mrs %[pfr], id_aa64mmfr0_el1" : [pfr] "=r" (pfr));
    return pfr;
}

static inline void
sysreg_write_scr_el3(uint64_t x) {
    __asm volatile("msr scr_el3, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_mdcr_el3(uint64_t x) {
    __asm volatile("msr mdcr_el3, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_hcr_el2(uint64_t x) {
    __asm volatile("msr hcr_el2, %[x]" : : [x] "r" (x));
}

static inline uint64_t
sysreg_read_sctlr_el1(void) {
    uint64_t sctlr_el1;
    __asm volatile("mrs %[x], sctlr_el1" : [x] "=r" (sctlr_el1));
    return sctlr_el1;
}

static inline uint64_t
sysreg_read_sctlr_el2(void) {
    uint64_t sctlr_el2;
    __asm volatile("mrs %[x], sctlr_el2" : [x] "=r" (sctlr_el2));
    return sctlr_el2;
}

static inline void
sysreg_write_sctlr_el1(uint64_t x) {
    __asm volatile("msr sctlr_el1, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_sctlr_el2(uint64_t x) {
    __asm volatile("msr sctlr_el2, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_mair_el1(uint64_t x) {
    __asm volatile("msr mair_el1, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_spsr_el3(uint64_t x) {
    __asm volatile("msr spsr_el3, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_elr_el3(uint64_t x) {
    __asm volatile("msr elr_el3, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_spsr_el2(uint64_t x) {
    __asm volatile("msr spsr_el2, %[x]" : : [x] "r" (x));
}

static inline void
sysreg_write_elr_el2(uint64_t x) {
    __asm volatile("msr elr_el2, %[x]" : : [x] "r" (x));
}

static inline uint64_t
sysreg_read_par_el1(void) {
    uint64_t par_el1;
    __asm volatile("mrs %[x], par_el1" : [x] "=r" (par_el1));
    return par_el1;
}

#endif // __SYSREG_H__
