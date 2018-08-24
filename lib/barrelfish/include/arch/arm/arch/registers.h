/**
 * \file
 * \brief architecture-specific registers code
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_REGISTERS_H
#define ARCH_ARM_BARRELFISH_REGISTERS_H

#include <barrelfish/curdispatcher_arch.h> // XXX For curdispatcher()
#include "threads_priv.h"

static inline uint32_t
curgotbase(void)
{
    uint32_t ret;
    __asm (
        "mov %[ret], r9" : [ret] "=r" (ret)
          );
    return ret;

}

static inline void
registers_set_initial(arch_registers_state_t *regs, struct thread *thread,
                      lvaddr_t entry, lvaddr_t stack, uint32_t arg1,
                      uint32_t arg2, uint32_t arg3, uint32_t arg4)
{
    regs->named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
    regs->named.r0 = arg1;
    regs->named.r1 = arg2;
    regs->named.r2 = arg3;
    regs->named.r3 = arg4;
    regs->named.stack = stack;
    regs->named.r9 = (uintptr_t)curgotbase();
    regs->named.pc = entry;
    regs->named.fpscr = 0x01000000; // flush-to-zero
}

#endif // ARCH_ARM_BARRELFISH_REGISTERS_H
