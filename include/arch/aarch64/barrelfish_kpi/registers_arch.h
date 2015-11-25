/**
 * \file
 * \brief Architecture-specific registers code
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_KPI_REGISTERS_H
#define ARCH_AARCH64_BARRELFISH_KPI_REGISTERS_H

#ifndef __ASSEMBLER__
#include <stddef.h> // for offsetof
#include <barrelfish/curdispatcher_arch.h> // XXX For curdispatcher()
#include <barrelfish_kpi/types.h> // for lvaddr_t
#endif

//
// Offsets of saved registers in save area.
//
#define X0_REG    0
#define X1_REG    1
#define X2_REG    2
#define X3_REG    3
#define X4_REG    4
#define X5_REG    5
#define X6_REG    6
#define X7_REG    7
#define X8_REG    8
#define X9_REG    9
#define X10_REG  10
#define X11_REG  11
#define X12_REG  12
#define X13_REG  13
#define X14_REG  14
#define X15_REG  15
#define X16_REG  16
#define X17_REG  17
#define X18_REG  18
#define X19_REG  19
#define X20_REG  20
#define X21_REG  21
#define X22_REG  22
#define X23_REG  23
#define X24_REG  24
#define X25_REG  25
#define X26_REG  26
#define X27_REG  27
#define X28_REG  28
#define FP_REG   29
#define LR_REG   30
#define SP_REG   31
#define PC_REG   32
#define SPSR_REG 33

#define PIC_REGISTER X10

#define NUM_REGS 34     /* cpsr, x0-x30, sp, pc */
#define NUM_FPU_REGS 0
#define ARCH_NUMREGS NUM_REGS

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
        uint64_t x0,  x1,  x2,  x3,  x4,  x5,  x6,  x7;
        uint64_t x8,  x9,  x10, x11, x12, x13, x14, x15;
        uint64_t x16, x17, x18, x19, x20, x21, x22, x23;
        uint64_t x24, x25, x26, x27, x28, x29, x30;
        uint64_t stack, pc, spsr;
    } named;
    struct registers_aarch64_syscall_args {
        uint64_t arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7;
        uint64_t x8,   x9,   x10,  x11,  x12,  x13,  x14,  x15;
        uint64_t x16,  x17,  x18,  x19,  x20,  x21,  x22,  x23;
        uint64_t x24,  x25,  x26,  x27,  x28,  x29,  x30;
        uint64_t stack, pc, spsr;
    } syscall_args;
    uint64_t regs[sizeof(struct registers_aarch64_named) / sizeof(uint64_t)];
};

STATIC_ASSERT_SIZEOF(union registers_aarch64, NUM_REGS * sizeof(uint64_t));

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
    regs->named.x0 = param;
}

static inline void
registers_get_param(arch_registers_state_t *regs, uint64_t *param)
{
    *param = regs->named.x0;
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
