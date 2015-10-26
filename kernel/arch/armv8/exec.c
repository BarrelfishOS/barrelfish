/**
 * \file
 * \brief AArch64 execution and miscellany
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <init.h>
#include <aarch64.h>
#include <arm_hal.h>
#include <exec.h>
#include <misc.h>
#include <sysreg.h>   // for invalidating tlb and cache

//static arch_registers_state_t upcall_state;

extern uint32_t ctr;
static inline __attribute__((noreturn))
void do_resume(uint32_t *regs)
{
    panic("NYI");
}

/// Ensure context is for user-mode with interrupts enabled.
static inline void
ensure_user_mode_policy(arch_registers_state_t *state)
{
    panic("NYI");
}

/**
 * \brief Go to user-space at entry point 'entry'.
 *
 * This function goes to user-space and starts executing the program at
 * its entry point at virtual address 'entry'.
 *
 * \param entry Entry point address of program to execute.
 */
void __attribute__ ((noreturn))
execute(lvaddr_t entry)
{
    panic("NYI");
}

/**
 * \brief Resume the given user-space snapshot.
 *
 * This function resumes user-space execution by restoring the CPU
 * registers with the ones given in the array, pointed to by 'state'.
 */
void __attribute__ ((noreturn)) resume(arch_registers_state_t *state)
{
    panic("NYI");
}

void wait_for_interrupt(void)
{
    panic("NYI");
}
