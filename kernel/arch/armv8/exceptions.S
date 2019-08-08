/*
 * Copyright (c) 2015,2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <asmoffsets.h>
#include <offsets.h>
#include <exceptions.h>

.global do_resume
.global vectors

.macro invalid_exception vector
    /* Just stick the trap frame on the kernel stack - we're about to panic
     * anyway.  */

    add sp, sp, #-(34 * 8)

    /* Spill the GPRs */
    stp  x0,  x1, [sp]
    stp  x2,  x3, [sp, #( 2 * 8)]
    stp  x4,  x5, [sp, #( 4 * 8)]
    stp  x6,  x7, [sp, #( 6 * 8)]
    stp  x8,  x9, [sp, #( 8 * 8)]
    stp x10, x11, [sp, #(10 * 8)]
    stp x12, x13, [sp, #(12 * 8)]
    stp x14, x15, [sp, #(14 * 8)]
    stp x16, x17, [sp, #(16 * 8)]
    stp x18, x19, [sp, #(18 * 8)]
    stp x20, x21, [sp, #(20 * 8)]
    stp x22, x23, [sp, #(22 * 8)]
    stp x24, x25, [sp, #(24 * 8)]
    stp x26, x27, [sp, #(26 * 8)]
    stp x28, x29, [sp, #(28 * 8)]

    /* Stack pointer */
    mrs x0, sp_el0
    stp x30, x0, [sp, #(30 * 8)]

    mrs x0, elr_el1
    mrs x1, spsr_el1
    stp x0, x1, [sp, #(32 * 8)]

    /* Exception Syndrome Register */
    mrs x2, esr_el1

    /* Exception vector */
    mov x3, \vector

    /* Base of the register save area. */
    mov x4, sp

    /* Arguments: x0 = EPC, x1 = SPSR, x2 = ESR, x3 = vector, x4 = save area. */
    b fatal_kernel_fault
.endm

/**********************************/
/*** Start of exception vectors ***/
/**********************************/

/* The AArch64 exception vectors occupy 2kiB */
.align 11
vectors:

/* Offset 0x000 */
/* Exceptions from the current EL, on the EL0 stack.  We never do this. */
/* Each table entry occupies 128B, which lets us put up to 32 instructions
 * here before we branch. */
.align 7 /* 0x000 */
el1_sp_el0_sync:
    invalid_exception AARCH64_EVECTOR_EL0_SYNC
.align 7 /* 0x080 */
el1_sp_el0_irq:
    invalid_exception AARCH64_EVECTOR_EL0_IRQ 
.align 7 /* 0x100 */
el1_sp_el0_fiq:
    invalid_exception AARCH64_EVECTOR_EL0_FIQ
.align 7 /* 0x180 */
el1_sp_el0_serror:
    invalid_exception AARCH64_EVECTOR_EL0_SERROR

/* Offset 0x200 */
/* Exceptions from the kernel itself, at EL1. */
.align 7 /* 0x200 */
el1_sync:
    invalid_exception AARCH64_EVECTOR_EL1_SYNC
.align 7 /* 0x280 */
el1_irq:
    /* This happens only in case the kernel runs out of work and
     * calls wait_for_interrupt. To make sure we come from this function
     * we store a magic value in a register */
    mov w1, #WAIT_FOR_INTERRUPT_MAGIC
    cmp w0,w1
    b.ne el1_irq_failure
    b nosave_handle_irq
el1_irq_failure:
    invalid_exception AARCH64_EVECTOR_EL1_IRQ
.align 7 /* 0x300 */
el1_fiq:
    invalid_exception AARCH64_EVECTOR_EL1_FIQ
.align 7 /* 0x380 */
el1_serror:
    invalid_exception AARCH64_EVECTOR_EL1_SERROR

/* Offset 0x400 */
/* Exceptions from user level, EL0, executing AArch64.  For any of these four
 * exceptions, the stack pointer is SP_EL1, which is left at the top of
 * 'kernel_stack'. */
 .align 7 /* 0x400 */
/*
 * Synchronous exceptions from a lower execution level using AArch64: SVC
 * (syscall), data abort, prefetch abort and undefined instruction.
 *
 * Assumption:
 * when coming from a syscall (SVC) the arguments are in registers x0-x11
 */
el0_aarch64_sync:
    /* Reenable breakpoints and aborts.  Interrupts remain disabled. */
    msr daifset, #3 /* IRQ and FIQ masked, Debug and Abort enabled. */

    /* Spill a few working registers.
     * Registers x0-x11 contain the syscall arguments. We use x12-, as these won't
     * need to be restored if we're doing a syscall; they're caller-saved.  We
     * preserve x0-x6 in registers unless we branch to the abort path, so that they're
     * immediately available to the syscall handler, sys_syscall.
     */
    stp x14, x15, [sp, #-(2 * 8)]!
    stp x12, x13, [sp, #-(2 * 8)]!

    /* The EL1 thread ID register holds the address of the currently-running
     * dispatcher's shared control block. */
    mrs x13, tpidr_el1

    /* x13 = dispatcher_shared_aarch64 */

    /* Exception PC */
    mrs x12, elr_el1

    /* x12 = EPC, x13 = dispatcher_shared_aarch64 */

    /* Check whether the current dispatcher is disabled.  See el0_aarch64_irq
     * for a description of this test. */
    ldp x14, x15, [x13, #OFFSETOF_DISP_CRIT_PC_LOW]
    cmp x14, x12
    ccmp x15, x12, #0, ls
    ldr w14, [x13, #OFFSETOF_DISP_DISABLED]
    ccmp w14, wzr, #0, ls
    /* NE <-> (low <= PC && PC < high) || disabled != 0 */

    /* Figure out what sort of exception we've got.  All paths need this. */
    mrs x14, esr_el1  /* Exception Syndrome Register */
    lsr x14, x14, #26 /* Exception Class field is bits [31:26] */

    /* x12 = EPC, x14 = EC, x13 = dispatcher_shared_aarch64 */

    /* Faults while disabled should be rare, if the critical section is short,
     * and will always be within the dispatcher code.  Therefore we branch out
     * to the slowpath here. */
    b.ne el0_sync_disabled

    /* 13 instructions to here. */

    /* All exceptions use the 'enabled' area if the dispatcher is enabled. */
    add x13, x13, #OFFSETOF_DISP_ENABLED_AREA

    /* x12 = EPC, x13 = base of save area, x14 = EC */

    /* now we branch off as we are running out of space */
    b save_syscall_context

    /* 15 instructions to here. */

.align 7 /* 0x480 */
/* An interrupt at user level */
el0_aarch64_irq:
    /* Reenable breakpoints and aborts.  Interrupts remain disabled. */
    msr daifset, #3 /* IRQ and FIQ masked, Debug and Abort enabled. */

    /* Free scratch registers. */
    stp x2, x3, [sp, #-(2 * 8)]!
    stp x0, x1, [sp, #-(2 * 8)]!

    /* Find the dispatcher. */
    mrs x3, tpidr_el1

    /* Get the exception address (EPC) */
    mrs x1, elr_el1

    /* x0 = crit_pc_low, x1 = EPC,
       x2 = crit_pc_high, x3 = dispatcher_shared_aarch64 */

    /* Dispatcher disabled? */
    ldp x0, x2, [x3, #OFFSETOF_DISP_CRIT_PC_LOW]

    /* Condition LS holds iff low <= PC. */
    cmp x0, x1
    /* Short-circuit computation of P /\ Q: If the success condition for P
     * (low <= PC) holds, here LS (Less or Same), then test Q (PC < high),
     * giving either HI (true) or LS (false).  Otherwise, set the condition
     * flags to a failing state for Q (LS), as the conjunction cannot hold if
     * the P does not. */
    ccmp x2, x1, #0, ls
    /* If HI (C=1, Z=0) holds, then the test executed and passed, which means
     * that P held previously (low <= PC), and Q holds now (PC < high).
     * Otherwise LS holds, and thus either HI held before this test, and thus
     * PC < low, or the test executed and failed, in which case high <= PC.
     * Thus condition HI holds iff low <= PC /\ PC < high. */
    ldr w0, [x3, #OFFSETOF_DISP_DISABLED]
    /* We do the same to calculate (P /\ Q) \/ R: If P /\ Q doesn't hold, we
     * need to test R (is the disabled flag 0), giving either EQ or NE.  If
     * P /\ Q holds, we skip the test, as it doesn't affect the result, and
     * instead set the condition code directly NE, i.e. disabled=1. */
    ccmp w0, wzr, #0, ls
    /* NE <-> (low <= PC && PC < high) || disabled != 0 */

    /* x1 = EPC, x3 = dispatcher_shared_aarch64 */

    /* Choose the right save area, using the condition flags. */
    add x0, x3, #OFFSETOF_DISP_ENABLED_AREA
    add x2, x3, #OFFSETOF_DISP_DISABLED_AREA
    csel x0, x2, x0, ne

    /* x0 = save area, x1 = EPC */

    /* 13 instructions. */

    /* Save the register context, starting from x4.  x0-x3, the scratch
     * registers, can be copied to the trap frame from within the C handler,
     * as can the user stack pointer and SPSR, which are sitting in their own
     * system registers. */
    stp  x4,  x5, [x0, #( 4 * 8)]
    stp  x6,  x7, [x0, #( 6 * 8)]
    stp  x8,  x9, [x0, #( 8 * 8)]
    stp x10, x11, [x0, #(10 * 8)]
    stp x12, x13, [x0, #(12 * 8)]
    stp x14, x15, [x0, #(14 * 8)]
    stp x16, x17, [x0, #(16 * 8)]
    stp x18, x19, [x0, #(18 * 8)]
    stp x20, x21, [x0, #(20 * 8)]
    stp x22, x23, [x0, #(22 * 8)]
    stp x24, x25, [x0, #(24 * 8)]
    stp x26, x27, [x0, #(26 * 8)]
    stp x28, x29, [x0, #(28 * 8)]
    str x30,      [x0, #(30 * 8)]

    /* 27 instructions. */

    /* Load the saved scratch registers, and pass them as arguments to the
     * handler.  We can't save them ourselves as we've run out of
     * instructions.  We need to do at least this, to clear our stack frame.
     * */
    ldp x2, x3, [sp], #16 /* x0, x1 */
    ldp x4, x5, [sp], #16 /* x2, x3 */

    /* x0 = save area, x1 = EPC,
     * x2 = user x0, x3 = user x1,
     * x4 = user x2, x5 = user x3 */
    b save_handle_irq

    /* 30 instructions. */

.align 7 /* 0x500 */
/* We don't implement fast IRQs */
el0_aarch64_fiq:
    invalid_exception AARCH64_EVECTOR_EL0_FIQ

.align 7 /* 0x580 */
/* A delayed abort.  We don't handle this. */
el0_aarch64_serror:
    invalid_exception AARCH64_EVECTOR_EL0_SERROR

/* Offset 0x600 */
/* Exceptions from user level, EL0, executing AArch32.  This is currently
 * unimplemented. */
.align 7 /* 0x600 */
el0_aarch32_sync:
    invalid_exception AARCH32_EVECTOR_EL0_SYNC
.align 7 /* 0x680 */
el0_aarch32_irq:
    invalid_exception AARCH32_EVECTOR_EL0_IRQ
.align 7 /* 0x700 */
el0_aarch32_fiq:
    invalid_exception AARCH32_EVECTOR_EL0_FIQ
.align 7 /* 0x780 */
el0_aarch32_serror:
    invalid_exception AARCH32_EVECTOR_EL0_SERROR

.align 11
/********************************/
/*** End of exception vectors ***/
/********************************/

/* The tail of the user syscall handler doesn't fit in the table. */
save_syscall_context:
    /* x12 = EPC, x13 = base of save area, x14 = EC */

    /*
     * We need to save callee save registers r19-30 no matter what, so get on with it.
     */

    /* Callee-saved registers */
    stp x19, x20, [x13, #(19 * 8)]
    stp x21, x22, [x13, #(21 * 8)]
    stp x23, x24, [x13, #(23 * 8)]
    stp x25, x26, [x13, #(25 * 8)]
    stp x27, x28, [x13, #(27 * 8)]
    stp x29, x30, [x13, #(29 * 8)] /* FP & LR */

    stp q0, q1, [x13, #(34 * 8)]
    stp q2, q3, [x13, #(38 * 8)]
    stp q4, q5, [x13, #(42 * 8)]
    stp q6, q7, [x13, #(46 * 8)]
    stp q8, q9, [x13, #(50 * 8)]
    stp q10, q11, [x13, #(54 * 8)]
    stp q12, q13, [x13, #(58 * 8)]
    stp q14, q15, [x13, #(62 * 8)]
    stp q16, q17, [x13, #(66 * 8)]
    stp q18, q19, [x13, #(70 * 8)]
    stp q20, q21, [x13, #(74 * 8)]
    stp q22, q23, [x13, #(78 * 8)]
    stp q24, q25, [x13, #(82 * 8)]
    stp q26, q27, [x13, #(86 * 8)]
    stp q28, q29, [x13, #(90 * 8)]
    stp q30, q31, [x13, #(94 * 8)]

    /* High registers are now available. */

    /* User SP and PC */
    mrs x20, sp_el0
    stp x20, x12, [x13, #(31 * 8)]

    /* SPSR */
    mrs x19, spsr_el1
    str x19, [x13, #(33 * 8)]

    /* Is this a syscall? */
    cmp x14, #0x15 /* SVC or HVC from AArch64 EL0 */
    b.ne el0_abort_enabled

    /*
     * we need to save r7-r11 as those are the syscall arguments which are passed
     * on the stack on a function call, so we need to move them to the trap frame
     */
    stp x7, x8,     [x13, #(7 * 8)]
    stp x9, x10,    [x13, #(9 * 8)]
    str x11,        [x13, #(11 * 8)]

    /* If we're here, this is a syscall and we don't need to restore the
     * scratch registers. Just throw the stack frame away. */
    add sp, sp, #(4 * 8)

    /* Pass the address of the trap frame as argument 8. */
    mov x7, x13

    /* Jump to the common (C) syscall handler. */
    b sys_syscall


/* The tail of the user abort handler doesn't fit in the table. */
el0_abort_enabled:
    /* x12 = EPC, x13 = base of save area, x14 = EC */

    /*
     * we need to save the caller-saved registers. As it is an
     * abort, those weren't saved by prior to the call.
     */


    /* We saved x12-x15 in our stack frame. We load them from
     * memory so we can store them we use x20-23 as they where
     * saved in the common path */
    ldp x20,  x21,  [sp], #16 /* x12, x13 */
    ldp x22,  x23,  [sp], #16 /* x14, x15 */

    /* now we can store the caller-saved registers in the trap frame */
    stp x0,   x1,  [x13]
    stp x2,   x3,  [x13, #(  2 * 8)]
    stp x4,   x5,  [x13, #(  4 * 8)]
    stp x6,   x7,  [x13, #(  6 * 8)]
    stp x8,   x9,  [x13, #(  8 * 8)]
    stp x10, x11,  [x13, #( 10 * 8)]
    stp x20, x21,  [x13, #(12 * 8)]
    stp x22, x23,  [x13, #(14 * 8)]
    stp x16, x17,  [x13, #(16 * 8)]
    str x18,       [x13, #(18 * 8)]

    /* x19-x30, SP, SPSR and ELR were saved in the common path. */

    /* x12 = EPC, x13 = base of save area, x14 = EC */

    /* All registers are now available. */

    /* Pass the EPC and the save area address to the handler. */
    mrs x0, far_el1
    mov x1, x14
    mov x2, x13

    /* Now we can jump to the handler. */
    b handle_user_fault

/* A synchronous exception while the dispatcher is disabled.  This is the
 * slowpath.  In fact, libbarrelfish currently (2015) just refuses to continue
 * if this ever happens. */
el0_sync_disabled:
    /* x12 = EPC, x14 = EC, x13 = dispatcher_shared_aarch64 */

    /* Filter out aborts. */
    cmp x14, #0x15 /* SVC or HVC from AArch64 EL0 */
    b.ne el0_abort_disabled

    /* Use the 'disabled' area. */
    add x13, x13, #OFFSETOF_DISP_DISABLED_AREA

    /* Jump back into the syscall path. */
    b save_syscall_context

/* This is the *really* unexpected path: a page fault in the dispatcher
 * critical section.  It's (relatively) slow, and libbarrelfish doesn't
 * actually handle it at present (2015). */
el0_abort_disabled:

    /* x12 = EPC, x13 = base of save area, x14 = EC */

    /* x11 = EC,
       x13 = dispatcher_shared_aarch64 */

    /* Use the 'trap' area. */
    add x13, x13, #OFFSETOF_DISP_TRAP_AREA

    /* Save the reduced context. */
    /* Callee-saved registers */
    stp x19, x20, [x13, #(19 * 8)]
    stp x21, x22, [x13, #(21 * 8)]
    stp x23, x24, [x13, #(23 * 8)]
    stp x25, x26, [x13, #(25 * 8)]
    stp x27, x28, [x13, #(27 * 8)]
    stp x29, x30, [x13, #(29 * 8)] /* FP & LR */

    /* SPSR */
    mrs x19, spsr_el1
    str x19, [x13, #(31 * 8)]

    /* User PC and SP */
    mrs x20, sp_el0
    stp x20, x12, [x13, #(31 * 8)]

    /* x12 = EPC, x13 = base of save area, x14 = EC */

    /* Now reuse the 'enabled' abort handler. */
    b el0_abort_enabled

/* Restore the dispatcher's execution context.  x0 holds the base of the
 * appropriate save area. */
do_resume:
    /* Skip to the end... */
    add x0, x0, #((34 + 30 * 2) * 8)

    /* Reset the kernel stack pointer. */
    ldr x1, =kernel_stack   // load the address of the kernel sack
    ldr x2,  [x1]           // read the entry of the kernel stack
    mov sp, x2

    /* Flush the TLB - XXX kill this with fire. */
    dsb sy
    tlbi vmalle1
    dsb sy
    isb

    /* Restore the NEON registers. */
    ldp q30, q31, [x0], #-(2 * 16)
    ldp q28, q29, [x0], #-(2 * 16)
    ldp q26, q27, [x0], #-(2 * 16)
    ldp q24, q25, [x0], #-(2 * 16)
    ldp q22, q23, [x0], #-(2 * 16)
    ldp q20, q21, [x0], #-(2 * 16)
    ldp q18, q19, [x0], #-(2 * 16)
    ldp q16, q17, [x0], #-(2 * 16)
    ldp q14, q15, [x0], #-(2 * 16)
    ldp q12, q13, [x0], #-(2 * 16)
    ldp q10, q11, [x0], #-(2 * 16)
    ldp q8,   q9, [x0], #-(2 * 16)
    ldp q6,   q7, [x0], #-(2 * 16)
    ldp q4,   q5, [x0], #-(2 * 16)
    ldp q2,   q3, [x0], #-(2 * 16)
    ldp q0,   q1, [x0], #-(2 *  8)

    /* Restore SPSR, PC (ELR) and SP, which need temporary registers, before
     * we restore those. */
    ldp x2, x3, [x0], #-8 /* pc, spsr */
    ldr x1, [x0], #-16 /* stack */
    msr spsr_el1, x3
    msr elr_el1,  x2
    msr sp_el0,   x1

    /* Restore the general-purpose registers. */
    ldp x29, x30, [x0], #-(2 * 8)
    ldp x27, x28, [x0], #-(2 * 8)
    ldp x25, x26, [x0], #-(2 * 8)
    ldp x23, x24, [x0], #-(2 * 8)
    ldp x21, x22, [x0], #-(2 * 8)
    ldp x19, x20, [x0], #-(2 * 8)
    ldp x17, x18, [x0], #-(2 * 8)
    ldp x15, x16, [x0], #-(2 * 8)
    ldp x13, x14, [x0], #-(2 * 8)
    ldp x11, x12, [x0], #-(2 * 8)
    ldp x9,  x10, [x0], #-(2 * 8)
    ldp x7,  x8,  [x0], #-(2 * 8)
    ldp x5,  x6,  [x0], #-(2 * 8)
    ldp x3,  x4,  [x0], #-(2 * 8)
    ldp x1,  x2,  [x0], #-(1 * 8)
    ldr x0,       [x0]

    /* Return from exception.  This clears the load exclusive monitor. */
    eret
