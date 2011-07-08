/**
 * \file
 * \brief architecture-specific registers code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_REGISTERS_H
#define ARCH_BEEHIVE_BARRELFISH_REGISTERS_H

#include <barrelfish/curdispatcher_arch.h> // XXX For curdispatcher()
#include <threads.h>

static inline void
registers_set_initial(arch_registers_state_t *regs, struct thread *thread,
                      lvaddr_t entry, lvaddr_t stack, uint32_t arg1,
                      uint32_t arg2, uint32_t arg3, uint32_t arg4)
{
    regs->named.count = 0;
    regs->named.arg1 = arg1;
    regs->named.arg2 = arg2;
    regs->named.arg3 = arg3;
    regs->named.arg4 = arg4;
    regs->named.stack = stack;
    regs->named.frame = 0;
    regs->named.p1 = (uintptr_t)curdispatcher(); // XXX API bug means this must be run same-core
    regs->named.pc = entry;
}

static inline bool
registers_is_stack_invalid(struct dispatcher_generic *disp_gen,
                             arch_registers_state_t *archregs)
{
  return archregs->named.stack > (lvaddr_t)disp_gen->current->stack ||
    archregs->named.stack <= (lvaddr_t)disp_gen->current->stack_top;
}

#endif // ARCH_BEEHIVE_BARRELFISH_REGISTERS_H
