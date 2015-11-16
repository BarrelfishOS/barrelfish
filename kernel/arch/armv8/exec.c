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
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
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

extern uint32_t ctr;
static inline __attribute__((noreturn))
void do_resume(uint64_t *regs)
{
    STATIC_ASSERT(SPSR_REG ==  0, "");
    STATIC_ASSERT(X0_REG   ==  1, "");
    STATIC_ASSERT(PC_REG   == 33, "");

    // Flush cashes and tlb
    sysreg_invalidate_tlb();
    sysreg_invalidate_i_and_d_caches();

    __asm volatile(
        "clrex\n\t"
       /* Restore cpsr condition bits  */
	    "    mov     x30, %[regs]							             \n\t"
        "    ldr     x2, [x30, #(" XTR(SP_REG) "  * 8)]                  \n\t"
        "    mov     sp, x2                                              \n\t"
        "    ldr     x2, [x30, # (" XTR(PC_REG) "  * 8)]                 \n\t"
        "    msr     elr_el1, x2                                         \n\t"
        "    ldr     x2, [x30], #8                                       \n\t"
        /*"    msr     spsr_el1, x2                                        \n\t"*/
        /* Restore registers */
        "    ldp     x0, x1, [x30], #16                                  \n\t"
        "    ldp     x2, x3, [x30], #16                                  \n\t"
        "    ldp     x4, x5, [x30], #16                                  \n\t"
        "    ldp     x6, x7, [x30], #16                                  \n\t"
        "    ldp     x8, x9, [x30], #16                                  \n\t"
        "    ldp     x10, x11, [x30], #16                                \n\t"
        "    ldp     x12, x13, [x30], #16                                \n\t"
        "    ldp     x14, x15, [x30], #16                                \n\t"
        "    ldp     x16, x17, [x30], #16                                \n\t"
        "    ldp     x18, x19, [x30], #16                                \n\t"
        "    ldp     x20, x21, [x30], #16                                \n\t"
        "    ldp     x22, x23, [x30], #16                                \n\t"
        "    ldp     x24, x25, [x30], #16                                \n\t"
        "    ldp     x26, x27, [x30], #16                                \n\t"
        "    ldp     x28, x29, [x30], #16                                \n\t"
        "    ldr     x30, [x30], #8                                      \n\t"
		"    eret														\n\t"
        :: [regs] "r" (regs) : "x30");

    panic("do_resume returned.");
}

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

    state->named.x10 = disp_aarch64->got_base;

    struct dispatcher_shared_generic *disp_gen
        = get_dispatcher_shared_generic(handle);

    state->named.rtls = disp_gen->udisp;

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
    state->named.rtls = arch_get_thread_register();
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
    // REVIEW: Timer interrupt could be masked here.

    // Switch to system mode with interrupts enabled. -- OLD
    // Switch to priviledged mode with interrupts enabled.
    __asm volatile(
        "mov    x0, #" XTR(AARCH64_MODE_PRIV) "              \n\t"
        "0:                                             \n\t"
#if defined(__ARM_ARCH_8A__)
        "wfi                  \n\t"
#else
          // If no WFI functionality exists on system, just
          // spinning here is okay.
#error "Unknown platform for wait_for_interrupt"
#endif //
        "b      0b                                      \n\t" ::: "r0");

    panic("wfi returned");
}
