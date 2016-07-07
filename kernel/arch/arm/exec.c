/**
 * \file
 * \brief ARM execution and miscellany
 */

/*
 * Copyright (c) 2007-2009,2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <cache.h>
#include <dispatch.h>
#include <init.h>
#include <arm.h>
#include <platform.h>
#include <exec.h>
#include <exceptions.h>
#include <misc.h>
/* XXX - not AArch64-compatible. */
#include <cp15.h>   // for invalidating tlb and cache

static arch_registers_state_t upcall_state;

extern uint32_t ctr;
static inline __attribute__((noreturn))
/* XXX - not 64-bit clean, not AArch64-compatible. */
void do_resume(uint32_t *regs)
{
    STATIC_ASSERT(CPSR_REG ==  0, "");
    STATIC_ASSERT(R0_REG   ==  1, "");
    STATIC_ASSERT(PC_REG   == 16, "");

    __asm volatile(
        "clrex\n\t"
        // There is no SPSR in system mode, so switch to supervisor.
        "msr    CPSR_c, #(" XTR(CPSR_IF_MASK) "|" XTR(ARM_MODE_SVC)") \n\t"
        // Load cpsr into LR and move regs to next entry (postindex op)
        // LR = r14, used as scratch register.
        // LDR = read word from memory
        //        target register
        //        /   use register containing "regs" as base register
        //       /   /           post index: only base register is used for
        //      /   /           /   addressing and the offset added afterwards
        "ldr    lr, [%[regs]], #4                       \n\t"
        // set SPSR to value of lr == regs.cpsr
        // restore cpsr
        //        bits indicating SPSR
        //       /      read from register lr
        //      /      /
        "msr    spsr, lr                                 \n\t"
        // Restore register r0 to r15, "^" means: cpsr := spsr
        // Restore the non-banked registers.  Use LR as the index.
        "mov    lr, %[regs]                             \n\t"
        //          will increment the base pointer
        //         /
        "ldmia  lr!, {r0-r12}                           \n\t"
        // Restore the user stack pointer and link register.  n.b. LR is
        // banked in SVC mode, so *our* LR isn't affected.  Also, this can't
        // write back, so we've got to add the offset ourselves.
        "ldmia  lr, {r13,r14}^                          \n\t"
        // Load the (banked SVC) LR with the return address (add the offset
        // that the last ldmia couldn't).
        "ldr    lr, [lr, #8]                            \n\t"
        // Exception return - LR_svc -> PC_usr, SPSR_svc -> CPSR
        "movs pc, lr                                    \n\t"
        :: [regs] "r" (regs) : "lr");

    panic("do_resume returned.");
}

/// Ensure context is for user-mode with interrupts enabled.
static inline void
ensure_user_mode_policy(arch_registers_state_t *state)
{
    uintptr_t cpsr_if_mode = CPSR_F_MASK | ARM_MODE_USR;

    if ((state->named.cpsr & (CPSR_IF_MASK | ARM_MODE_MASK)) != cpsr_if_mode) {
        assert(0 == (state->named.cpsr & ARM_MODE_PRIV));
        state->named.cpsr &= CPSR_IF_MASK | ARM_MODE_MASK;
        state->named.cpsr |= cpsr_if_mode;
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
    struct dispatcher_shared_arm *disp_arm = get_dispatcher_shared_arm(handle);

    arch_registers_state_t *state = &upcall_state;
    assert(0 != disp_arm->got_base);

    /* XXX - not AArch64-compatible. */
    state->named.r9 = disp_arm->got_base;

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

    /*
      This function succeeds the first time executed, i.e.
      when init is started for the first time.
      If we hold the execution here after the first execption, we are still good
    */
    //    while(ctr>1);
    do_resume(state->regs);
}

bool waiting_for_interrupt= 0;

/* XXX - not AArch64-compatible. */
void wait_for_interrupt(void)
{
    // XXX - is this true?
    // REVIEW: Timer interrupt could be masked here.

    /* If we're waiting on an interrupt in the kernel, it must be because
     * there was no runnable dispatcher. */
    assert(dcb_current == NULL);

    /* Let the IRQ handler know that we expect an interrupt in kernel mode, so
     * it shouldn't panic. */
    waiting_for_interrupt= 1;

    /* Unmask IRQ and wait. */
    __asm volatile("cpsie i");
    while(1) __asm volatile("wfi");
}
