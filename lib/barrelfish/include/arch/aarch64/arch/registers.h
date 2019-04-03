/**
 * \file
 * \brief architecture-specific registers code
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_REGISTERS_H
#define ARCH_AARCH64_BARRELFISH_REGISTERS_H

#include "threads_priv.h"

static inline uint64_t
curgotbase(void)
{
    uint64_t ret;
    __asm (
        "mov %[ret], x10" : [ret] "=r" (ret)
          );
    return ret;

}

static inline void
registers_set_initial(arch_registers_state_t *regs, struct thread *thread,
                      lvaddr_t entry, lvaddr_t stack, uint64_t arg1,
                      uint64_t arg2, uint64_t arg3, uint64_t arg4)
{
    regs->named.x0 = arg1;
    regs->named.x1 = arg2;
    regs->named.x2 = arg3;
    regs->named.x3 = arg4;
    regs->named.x10 = (uintptr_t)curgotbase();
    regs->named.stack = stack;
    regs->named.pc = entry;
    regs->named.spsr = AARCH64_MODE_USR | CPSR_F_MASK;
}

#endif // ARCH_AARCH64_BARRELFISH_REGISTERS_H
