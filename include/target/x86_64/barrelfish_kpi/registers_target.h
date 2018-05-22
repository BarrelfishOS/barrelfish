/**
 * \file
 * \brief Arch specific definition of the registers, can be included by anyone.
 * Definitions shared by kernel and user
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_64_BARRELFISH_KPI_REGISTERS_H
#define TARGET_X86_64_BARRELFISH_KPI_REGISTERS_H

#include <barrelfish_kpi/eflags_arch.h> // for USER_EFLAGS
#include <barrelfish_kpi/types.h>       // for lvaddr_t

/** \brief Number of registers to be saved for this architecture
 *
 * This is the same as ARCH_NUMREGS, but it is used by assembler stubs, so needs
 * to be defined here as a constant.
 */
#define X86_64_NUM_REGS                        20

#ifndef __ASSEMBLER__

// Warning: both the GDB stubs and any number of asm fragments depend
// on the order of these fields. Don't change them without due care!
struct registers_x86_64 {
    uint64_t rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp,
        r8, r9, r10, r11, r12, r13, r14, r15, rip, eflags;
    uint16_t fs, gs; ///< Only meaningful segment selectors in 64-bit mode
    struct {
        uint16_t fcw, fsw;
        uint8_t ftw, res1;
        uint16_t fop;
        uint32_t fpu_ip1;
        uint16_t fpu_ip2, res2;
        uint32_t fpu_dp1;
        uint16_t fpu_dp2, res3;
        uint32_t mxcsr, mxcsr_mask;
        uint64_t st[8][2];
        uint64_t xmm[16][2];
        uint64_t res4[12];
    } fxsave_area __attribute__ ((packed, aligned (16)));
};

static inline void
registers_x86_64_set_entry(struct registers_x86_64 *regs, lvaddr_t entry)
{
    regs->rip    = entry;
    regs->eflags = USER_EFLAGS;
    regs->fs = 0;
    regs->gs = 0;
}

static inline void
registers_x86_64_set_param(struct registers_x86_64 *regs, uint64_t param)
{
    regs->rax = param;
}

static inline void
registers_x86_64_get_param(struct registers_x86_64 *regs, uint64_t *param)
{
    *param = regs->rax;
}

static inline uint64_t
registers_x86_64_get_ip(struct registers_x86_64 *regs)
{
    return regs->rip;
}

static inline uint64_t
registers_x86_64_get_sp(struct registers_x86_64 *regs)
{
    return regs->rsp;
}

#endif // __ASSEMBLER__
#endif // TARGET_X86_64_BARRELFISH_KPI_REGISTERS_H
