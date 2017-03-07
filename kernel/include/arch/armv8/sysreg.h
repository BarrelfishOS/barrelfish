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


/*
 * ============================================================================
 * System register from section C5.2
 * ============================================================================
 */

#define ARMV8_SYSREG_WRITE_FN(_name, _reg, _bits) \
        static inline void \
        armv8_sysreg_write_## _bits ## _ ## _name(uint## _bits ## _t val) { \
            __asm volatile ("msr "#_reg ", %[val]\n" \
                            "isb \n" : : [val] "r" (val)); \
        }

#define ARMV8_SYSREG_READ_FN(_name, _reg, _bits) \
        static inline uint## _bits ## _t \
        armv8_sysreg_read_## _bits ## _ ## _name(void) { \
            uint## _bits ## _t val; \
            __asm volatile("mrs %[val], "#_reg "\n" \
                           "isb \n" : [val] "=r" (val)); \
            return val; \
        }

#define ARMV8_SYSREG_WO(_name, _reg, _bits) \
    ARMV8_SYSREG_WRITE_FN(_name, _reg, _bits)

#define ARMV8_SYSREG_RO(_name, _reg, _bits) \
    ARMV8_SYSREG_READ_FN(_name, _reg, _bits)

#define ARMV8_SYSREG_RW(_name, _reg, _bits) \
    ARMV8_SYSREG_READ_FN(_name, _reg, _bits) \
    ARMV8_SYSREG_WRITE_FN(_name, _reg, _bits)


/*
 * System register from section C5.2
 */
ARMV8_SYSREG_RO(current_el, CurrentEL, 32)
ARMV8_SYSREG_RW(daif, DAIF, 32)
ARMV8_SYSREG_RW(dlr_el0, DLR_EL0, 64)
ARMV8_SYSREG_RW(dspsr_el0, DSPSR_EL0, 32)
ARMV8_SYSREG_RW(elr_el1, ELR_EL1, 64)
ARMV8_SYSREG_RW(elr_el2, ELR_EL2, 64)
ARMV8_SYSREG_RW(elr_el3, ELR_EL3, 64)
ARMV8_SYSREG_RW(fpcr, FCPR, 32)
ARMV8_SYSREG_RW(fpsr, FPSR, 32)
ARMV8_SYSREG_RW(nzcv, NZCV, 32)
ARMV8_SYSREG_RW(sp_el0, SP_EL0, 64)
ARMV8_SYSREG_RW(sp_el1, SP_EL1, 64)
ARMV8_SYSREG_RW(sp_el2, SP_EL2, 64)
ARMV8_SYSREG_RW(sp_el3, SP_EL3, 64)
ARMV8_SYSREG_RW(spsel, SPSel, 32)
ARMV8_SYSREG_RW(spsr_abt, SPSR_abt, 32)
ARMV8_SYSREG_RW(spsr_fiq, SPSR_fiq, 32)
ARMV8_SYSREG_RW(spsr_irq, SPSR_irq, 32)
ARMV8_SYSREG_RW(spsr_und, SPSR_und, 32)
ARMV8_SYSREG_RW(spsr_el1, SPSR_EL1, 32)
ARMV8_SYSREG_RW(spsr_el2, SPSR_EL2, 32)
ARMV8_SYSREG_RW(spsr_el3, SPSR_EL3, 32)

ARMV8_SYSREG_RO(id_aa64pfr0_el1, id_aa64pfr0_el1, 64)

ARMV8_SYSREG_RW(CPACR_EL1, CPACR_EL1, 32)
ARMV8_SYSREG_RW(esr_el1, esr_el1, 64)

ARMV8_SYSREG_RW(dfsr, dfsr, 64)
ARMV8_SYSREG_RW(ifsr, ifsr, 64)

ARMV8_SYSREG_RW(hcr_el2, hcr_el2, 64)
ARMV8_SYSREG_RW(scr_el3, scr_el3, 32)

ARMV8_SYSREG_RW(mdcr_el2, mdcr_el2, 32)
ARMV8_SYSREG_RW(mdcr_el3, mdcr_el3, 32)


ARMV8_SYSREG_RW(ttbr0_el1, ttbr0_el1, 64)
ARMV8_SYSREG_RW(ttbr0_el2, ttbr0_el2, 64)
ARMV8_SYSREG_RW(ttbr0_el3, ttbr0_el3, 64)
ARMV8_SYSREG_RW(ttbr1_el1, ttbr1_el1, 64)

ARMV8_SYSREG_RW(mair_el1, mair_el1, 64)
ARMV8_SYSREG_RW(mair_el2, mair_el2, 64)
ARMV8_SYSREG_RW(mair_el3, mair_el3, 64)


ARMV8_SYSREG_RW(ICC_AP0R0_EL1, S3_0_C12_C8_4, 32)
ARMV8_SYSREG_RW(ICC_AP0R1_EL1, S3_0_C12_C8_5, 32)
ARMV8_SYSREG_RW(ICC_AP0R2_EL1, S3_0_C12_C8_6, 32)
ARMV8_SYSREG_RW(ICC_AP0R3_EL1, S3_0_C12_C8_7, 32)
ARMV8_SYSREG_RW(ICC_AP1R0_EL1, S3_0_C12_C9_0, 32)
ARMV8_SYSREG_RW(ICC_AP1R1_EL1, S3_0_C12_C9_1, 32)
ARMV8_SYSREG_RW(ICC_AP1R2_EL1, S3_0_C12_C9_2, 32)
ARMV8_SYSREG_RW(ICC_AP1R3_EL1, S3_0_C12_C9_3, 32)
ARMV8_SYSREG_RW(ICC_ASGI1R_EL1, S3_0_C12_C11_6, 64)
ARMV8_SYSREG_RW(ICC_BPR0_EL1, S3_0_C12_C8_3, 32)
ARMV8_SYSREG_RW(ICC_BPR1_EL1, S3_0_C12_C12_3, 32)
ARMV8_SYSREG_RW(ICC_CTLR_EL1, S3_0_C12_C12_4, 32)
ARMV8_SYSREG_RW(ICC_DIR_EL1, S3_0_C12_C11_1, 32)
ARMV8_SYSREG_RW(ICC_EOI1_EL1, ICC_EOI1_EL1, 32)
ARMV8_SYSREG_RW(ICC_EOIR0_EL1, S3_0_C12_C8_1, 32)
ARMV8_SYSREG_RW(ICC_EOIR1_EL1, S3_0_C12_C12_1, 32)
ARMV8_SYSREG_RW(ICC_HPPIR0_EL1, S3_0_C12_C8_2, 32)
ARMV8_SYSREG_RW(ICC_HPPIR1_EL1, S3_0_C12_C12_2, 32)
ARMV8_SYSREG_RW(ICC_IAR0_EL1, S3_0_C12_C8_0, 32)
ARMV8_SYSREG_RW(ICC_IAR1_EL1, S3_0_C12_C12_0, 32)
ARMV8_SYSREG_RW(ICC_IGRPEN0_EL1, S3_0_C12_C12_6, 32)
ARMV8_SYSREG_RW(ICC_IGRPEN1_EL1, S3_0_C12_C12_7, 32)
ARMV8_SYSREG_RW(ICC_PMR_EL1, S3_0_C4_C6_0, 32)
ARMV8_SYSREG_RW(ICC_RPR_EL1,    S3_0_C12_C11_3, 32)
ARMV8_SYSREG_RW(ICC_SGI0R_EL1, S3_0_C12_C11_7, 64)
ARMV8_SYSREG_RW(ICC_SGI1R_EL1, S3_0_C12_C11_5, 64)
ARMV8_SYSREG_RW(ICC_SRE_EL1, S3_0_C12_C12_5, 32)

ARMV8_SYSREG_RW(SCTLR_EL1, SCTLR_EL1, 32)
ARMV8_SYSREG_RW(SCTLR_EL2, SCTLR_EL2, 32)
ARMV8_SYSREG_RW(SCTLR_EL3, SCTLR_EL3, 32)


ARMV8_SYSREG_RW(TCR_EL1, TCR_EL1, 64)
ARMV8_SYSREG_RW(TCR_EL2, TCR_EL2, 32)
ARMV8_SYSREG_RW(TTBCR, TTBCR, 32)



/*
 * ============================================================================
 * C5.3 A64 system instructions for cache maintenance
 * ============================================================================
 */

#define ARMV8_CACHE_CTRL_WRITE_FN(_name, _reg, _bits) \
        static inline void \
        armv8_cache_ctrl_write_## _bits ## _ ## _name(uint## _bits ## _t val) { \
            __asm volatile ("dc "#_reg ", %[val]\n" \
                            "isb \n" : : [val] "r" (val)); \
        }

#define ARMV8_CACHE_CTRL_READ_FN(_name, _reg, _bits) \
        static inline uint## _bits ## _t \
        armv8_cache_ctrl_read_## _bits ## _ ## _name(void) { \
            uint## _bits ## _t val; \
            __asm volatile("dc %[val], "#_reg "\n" \
                           "isb \n" : [val] "=r" (val)); \
            return val; \
        }

#define ARMV8_CACHE_CTRL_WO(_name, _reg, _bits) \
    ARMV8_CACHE_CTRL_WRITE_FN(_name, _reg, _bits)


ARMV8_CACHE_CTRL_WO(cisw,CISW,64)
ARMV8_CACHE_CTRL_WO(civac, CIVAC,64)
ARMV8_CACHE_CTRL_WO(csw, CSW, 64)
ARMV8_CACHE_CTRL_WO(cvac, CVAC, 64)
ARMV8_CACHE_CTRL_WO(cvau, CVAU, 64)
ARMV8_CACHE_CTRL_WO(isw, ISW, 64)
ARMV8_CACHE_CTRL_WO(ivac, IVAC, 64)
ARMV8_CACHE_CTRL_WO(zva, zva, 64)



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

/**
 * Read the counter-timer frequency register.
 * See AArch64 generic timer registers.
 */
static inline uint32_t
sysreg_read_cntfrq_el0(void) {
    uint32_t frq;
    __asm volatile("mrs %[frq], cntfrq_el0" : [frq] "=r" (frq));
    return frq;
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
sysreg_write_elr_el3(uint64_t x) {
    __asm volatile("msr elr_el3, %[x]" : : [x] "r" (x));
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

static inline uint32_t
sysreg_read_cnthctl_el2(void) {
    uint32_t cnthctl_el2;
    __asm volatile("mrs %[cnthctl_el2], CNTHCTL_EL2" : [cnthctl_el2] "=r" (cnthctl_el2));
    return cnthctl_el2;
}

static inline void
sysreg_write_cnthctl_el2(uint32_t cnthctl_el2) {
    __asm volatile("msr CNTHCTL_EL2, %[cnthctl_el2]" : : [cnthctl_el2] "r" (cnthctl_el2));
}

static inline uint32_t
sysreg_read_cntkctl_el1(void) {
    uint32_t cntkctl_el1;
    __asm volatile("mrs %[cntkctl_el1], CNTKCTL_EL1" : [cntkctl_el1] "=r" (cntkctl_el1));
    return cntkctl_el1;
}

static inline void
sysreg_write_cntkctl_el1(uint32_t cntkctl_el1) {
    __asm volatile("msr CNTKCTL_EL1, %[cntkctl_el1]" : : [cntkctl_el1] "r" (cntkctl_el1));
}

static inline uint32_t
sysreg_read_cntp_ctl_el0(void) {
    uint32_t cntp_ctl_el0;
    __asm volatile("mrs %[cntp_ctl_el0], cntp_ctl_el0" : [cntp_ctl_el0] "=r" (cntp_ctl_el0));
    return cntp_ctl_el0;
}

static inline void
sysreg_write_cntp_ctl_el0(uint32_t cntp_ctl_el0) {
    __asm volatile("msr cntp_ctl_el0, %[cntp_ctl_el0]" : : [cntp_ctl_el0] "r" (cntp_ctl_el0));
}

static inline uint64_t
sysreg_read_cntp_cval_el0(void) {
    uint64_t cntp_cval_el0;
    __asm volatile("mrs %[cntp_cval_el0], cntp_cval_el0" : [cntp_cval_el0] "=r" (cntp_cval_el0));
    return cntp_cval_el0;
}

static inline void
sysreg_write_cntp_cval_el0(uint64_t cntp_cval_el0) {
    __asm volatile("msr cntp_cval_el0, %[cntp_cval_el0]" : : [cntp_cval_el0] "r" (cntp_cval_el0));
}

static inline uint32_t
sysreg_read_cntp_tval_el0(void) {
    uint64_t cntp_tval_el0;
    __asm volatile("mrs %[cntp_tval_el0], cntp_tval_el0" : [cntp_tval_el0] "=r" (cntp_tval_el0));
    return cntp_tval_el0;
}

static inline void
sysreg_write_cntp_tval_el0(uint32_t cntp_tval_el0) {
    __asm volatile("msr cntp_tval_el0, %[cntp_tval_el0]" : : [cntp_tval_el0] "r" (cntp_tval_el0));
}

static inline uint64_t
sysreg_read_cntpct_el0(void) {
    uint64_t cntpct_el0;
    __asm volatile("mrs %[cntpct_el0], cntpct_el0" : [cntpct_el0] "=r" (cntpct_el0));
    return cntpct_el0;
}

static inline uint64_t
sysreg_read_id_aa64pfr0_el1(void) {
    uint64_t x;
    __asm volatile("mrs %[x], id_aa64pfr0_el1" : [x] "=r" (x));
    return x;
}

static inline uint64_t
sysreg_read_id_aa64dfr0_el1(void) {
    uint64_t x;
    __asm volatile("mrs %[x], id_aa64dfr0_el1" : [x] "=r" (x));
    return x;
}

static inline uint64_t
sysreg_read_id_aa64isar0_el1(void) {
    uint64_t x;
    __asm volatile("mrs %[x], id_aa64isar0_el1" : [x] "=r" (x));
    return x;
}

static inline uint64_t
sysreg_read_id_aa64mmfr0_el1(void) {
    uint64_t x;
    __asm volatile("mrs %[x], id_aa64mmfr0_el1" : [x] "=r" (x));
    return x;
}

static inline uint32_t
sysreg_read_isr_el1(void) {
    uint32_t x;
    __asm volatile("mrs %[x], isr_el1" : [x] "=r" (x));
    return x;
}

#endif // __SYSREG_H__
