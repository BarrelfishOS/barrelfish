/**
 * \file
 * \brief ARM execution and miscellany
 */

/*
 * Copyright (c) 2007-2009, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <init.h>
#include <arch/arm/arm.h>
#include <arm_hal.h>
#include <exec.h>
#include <exceptions.h>
#include <misc.h>
#include <sysreg.h>   // for invalidating tlb and cache

static arch_registers_state_t upcall_state;

STATIC_ASSERT(X0_REG   ==  0, "");
STATIC_ASSERT(PC_REG   == 32, "");
STATIC_ASSERT(SPSR_REG == 33, "");

extern uint32_t ctr;

void __attribute__((noreturn)) do_resume(uint64_t *regs);

/// Ensure context is for user-mode with interrupts enabled.
static inline void
ensure_user_mode_policy(arch_registers_state_t *state)
{
    uintptr_t cpsr_if_mode = CPSR_F_MASK | AARCH64_MODE_USR;

    if ((state->named.spsr & (CPSR_IF_MASK | AARCH64_MODE_MASK)) != cpsr_if_mode) {
        assert(0 == (state->named.spsr & AARCH64_MODE_PRIV));
        state->named.spsr &= CPSR_IF_MASK | AARCH64_MODE_MASK;
        state->named.spsr |= cpsr_if_mode;
    }
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
    dispatcher_handle_t handle = dcb_current->disp;
    struct dispatcher_shared_aarch64 *disp_aarch64 =
        get_dispatcher_shared_aarch64(handle);

    arch_registers_state_t *state = &upcall_state;
    assert(0 != disp_aarch64->got_base);

    /* XXX - why is this here? */
    state->named.x10 = disp_aarch64->got_base;

    struct dispatcher_shared_generic *disp_gen
        = get_dispatcher_shared_generic(handle);

    state->named.x0 = disp_gen->udisp;

    state->named.pc = entry;
    ensure_user_mode_policy(state);
    do_resume(state->regs);
}

/**
 * \brief Resume the given user-space snapshot.
 *
 * This function resumes user-space execution by restoring the CPU
 * registers with the ones given in the array, pointed to by 'state'.
 */
uint32_t ctr=0;
void __attribute__ ((noreturn)) resume(arch_registers_state_t *state)
{
    ctr++;
    ensure_user_mode_policy(state);

	//printf("thread reg..%p\n",state->named.rtls);
    /*
      This function succeeds the first time executed, i.e.
      when init is started for the first time.
      If we hold the execution here after the first execption, we are still good
    */
    //    while(ctr>1);
    do_resume(state->regs);
}

void wait_for_interrupt(void)
{
    // Load magic and enable interrupts.
    __asm volatile(
        "mov    w0, #" XTR(WAIT_FOR_INTERRUPT_MAGIC) "              \n\t"
        "0:                                             \n\t"
#if defined(__ARM_ARCH_8A__)
        "msr daifclr, #2                  \n\t"
        "wfi                  \n\t"
#else
          // If no WFI functionality exists on system, just
          // spinning here is okay.
#error "Unknown platform for wait_for_interrupt"
#endif //
        "b      0b                                      \n\t" ::: "r0");

    panic("wfi returned");
}
