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

#ifndef ARCH_BEEHIVE_BARRELFISH_KPI_REGISTERS_H
#define ARCH_BEEHIVE_BARRELFISH_KPI_REGISTERS_H

// saved register 0 contains the count of words in RQ.  Currently
// if that is non-zero we panic.

union registers_beehive {
    struct registers_beehive_named {
	uint32_t count;
	uint32_t return1, return2;
	uint32_t arg1, arg2, arg3, arg4, arg5, arg6;
	uint32_t save1, save2, save3, save4, save5, save6, save7, save8;
	uint32_t save9, save10, save11, save12, save13, save14;
	uint32_t frame;
	uint32_t t1, t2, t3;
	uint32_t p1;
	uint32_t stack;
	uint32_t vb;
	uint32_t link, pc;
	uint32_t rqvals[64];
    } named;
    uint32_t regs[sizeof(struct registers_beehive_named) / sizeof(uint32_t)];
};

STATIC_ASSERT_SIZEOF(union registers_beehive, 96 * 4);

///< Opaque handle for the register state
typedef union registers_beehive arch_registers_state_t;

///< Opaque handle for the FPU register state
typedef void *arch_registers_fpu_state_t;

static inline void
registers_set_entry(arch_registers_state_t *regs, lvaddr_t entry)
{
    regs->named.pc = (uint32_t)entry;
}

static inline void
registers_set_param(arch_registers_state_t *regs, uint32_t param)
{
    regs->named.arg1 = param;
}

static inline void
registers_get_param(arch_registers_state_t *regs, uint32_t *param)
{
    *param = regs->named.arg1;
}

static inline uint32_t
registers_get_ip(arch_registers_state_t *regs)
{
    return regs->named.pc;
}

#endif // ARCH_BEEHIVE_BARRELFISH_KPI_REGISTERS_H
