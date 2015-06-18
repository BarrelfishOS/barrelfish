/**
 * \file
 * \brief architecture-specific registers code
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_KPI_REGISTERS_H
#define ARCH_AARCH64_BARRELFISH_KPI_REGISTERS_H

// TODO: update for aarch64

#ifndef __ASSEMBLER__
#include<stddef.h> // for offsetof
//#include <barrelfish/curdispatcher_arch.h> // XXX For curdispatcher()
#include <barrelfish_kpi/types.h> // for lvaddr_t
#endif

//
// Offsets of saved registers in save area.
//
#define CPSR_REG  0
#define X0_REG    1
#define X1_REG    2
#define X2_REG    3
#define X3_REG    4
#define X4_REG    5
#define X5_REG    6
#define X6_REG    7
#define X7_REG    8
#define X8_REG    9
#define X9_REG   10
#define X10_REG  11
#define X11_REG  12
#define X12_REG  13
#define X13_REG  14
#define X14_REG  15
#define X15_REG  16
#define X16_REG  17
#define X17_REG  18
#define X18_REG  19
#define X19_REG  20
#define X20_REG  21
#define X21_REG  22
#define X22_REG  23
#define X23_REG  24
#define X24_REG  25
#define X25_REG  26
#define X26_REG  27
#define X27_REG  28
#define X28_REG  29
#define X29_REG  30
#define SP_REG   31
#define LR_REG   32
#define PC_REG   33

#define NUM_REGS 34            /* cpsr, x0-x30, sp, pc */
#define NUM_FPU_REGS 0
#define ARCH_NUMREGS NUM_REGS

#define RIP_REG  PC_REG        /* pc == rip.x86_64 */
#define RSP_REG  SP_REG        /* sp == rsp.x86_64 */

/// Register used in system calls to encode function and arg count
#define SYSCALL_REG       0

//
// Helpers for pasting system reserved register names
//
#define REG_OFFSET_CONCAT(x)    x ## _REG
#define REG_OFFSET(name)        REG_OFFSET_CONCAT(name)

#define REG_NAME(ord)

#ifndef __ASSEMBLER__

union registers_aarch64 {
    struct registers_aarch64_named {
        uint64_t cpsr;
        uint64_t r0, r1, r2, r3;
        uint64_t r4, r5, r6, r7, r8;
        uint64_t rtls;  // r9 is thread local storage
        uint64_t r10;   // r10 is for global offset table base.
        uint64_t r11, r12, r13, r14;
        uint64_t r15, r16, r17, r18;
        uint64_t r19, r20, r21, r22;
        uint64_t r23, r24, r25, r26;
        uint64_t r27, r28, r29;
        uint64_t stack; // sp
        uint64_t link;  // x30
        uint64_t pc;    // pc
    } named;
    struct registers_aarch64_syscall_args {
        uint64_t cpsr;
        uint64_t arg0, arg1, arg2, arg3;
        uint64_t arg4, arg5, arg6, arg7, arg8;
        uint64_t arg9;
        uint64_t arg10;
        uint64_t fp;
        uint64_t arg11, arg12, arg13, arg14;
        uint64_t arg15, arg16, arg17, arg18;
        uint64_t arg19, arg20, arg21, arg22;
        uint64_t arg23, arg24, arg25, arg26;
        uint64_t arg27, arg28;
        uint64_t stack;
        uint64_t link;
        uint64_t pc;
    } syscall_args;
    uint64_t regs[sizeof(struct registers_aarch64_named) / sizeof(uint64_t)];
};

STATIC_ASSERT_SIZEOF(union registers_aarch64, NUM_REGS * sizeof(uint64_t));

STATIC_ASSERT((REG_OFFSET(THREAD_REGISTER) * sizeof(uint64_t)) == offsetof(struct registers_aarch64_named, rtls), "Thread register conflict");


///< Opaque handle for the register state
typedef union registers_aarch64 arch_registers_state_t;

///< Opaque handle for the FPU register state
typedef void *arch_registers_fpu_state_t;

static inline void
registers_set_entry(arch_registers_state_t *regs, lvaddr_t entry)
{
    regs->named.pc = (uint64_t)entry;
}

static inline void
registers_set_param(arch_registers_state_t *regs, uint64_t param)
{
    regs->named.r0 = param;
}

static inline void
registers_get_param(arch_registers_state_t *regs, uint64_t *param)
{
    *param = regs->named.r0;
}

static inline uint64_t
registers_get_ip(arch_registers_state_t *regs)
{
    return regs->named.pc;
}

static inline uint64_t
registers_get_sp(arch_registers_state_t *regs)
{
    return regs->named.stack;
}

#endif // __ASSEMBLER__

#endif // ARCH_AARCH64_BARRELFISH_KPI_REGISTERS_H
