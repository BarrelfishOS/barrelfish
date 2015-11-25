/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <asmoffsets.h>

.global do_resume
.global vectors

.macro invalid_exception
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

    /* Base of the register save area. */
    mov x3, sp

    /* Arguments: x0 = EPC, x1 = SPSR, x2 = ESR, x3 = save area. */
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
    invalid_exception
.align 7 /* 0x080 */
el1_sp_el0_irq:
    invalid_exception
.align 7 /* 0x100 */
el1_sp_el0_fiq:
    invalid_exception
.align 7 /* 0x180 */
el1_sp_el0_serror:
    invalid_exception

/* Offset 0x200 */
/* Exceptions from the kernel itself, at EL1. */
.align 7 /* 0x200 */
el1_sync:
    invalid_exception
.align 7 /* 0x280 */
el1_irq:
    invalid_exception
.align 7 /* 0x300 */
el1_fiq:
    invalid_exception
.align 7 /* 0x380 */
el1_serror:
    invalid_exception

/* Offset 0x400 */
/* Exceptions from user level, EL0, executing AArch64.  For any of these four
 * exceptions, the stack pointer is SP_EL1, which is left at the top of
 * 'kernel_stack'. */
 .align 7 /* 0x400 */
/* Synchronous exceptions from a lower execution level using AArch64: SVC
 * (syscall), data abort, prefetch abort and undefined instruction. */
el0_aarch64_sync:
    /* Reenable breakpoints and aborts.  Interrupts remain disabled. */
    msr daifset, #3 /* IRQ and FIQ masked, Debug and Abort enabled. */

    /* Spill a few working registers.  We use x9-, as these won't need to be
     * restored if we're doing a syscall; they're caller-saved.  We preserve
     * x0-x7 in registers unless we branch to the abort path, so that they're
     * immediately available to the syscall handler, sys_syscall. */
    stp x11, x12, [sp, #-(2 * 8)]!
    stp x9,  x10, [sp, #-(2 * 8)]!

    /* The EL1 thread ID register holds the address of the currently-running
     * dispatcher's shared control block. */
    mrs x10, tpidr_el1

    /* x10 = dispatcher_shared_aarch64 */

    /* Exception PC */
    mrs x9, elr_el1

    /* x9 = EPC, x10 = dispatcher_shared_aarch64 */

    /* Check whether the current dispatcher is disabled. */
    /* disp->crit_pc_low, disp->crit_pc_high */
    ldp x11, x12, [x10, #OFFSETOF_DISP_CRIT_PC_LOW]
    cmp x11, x9
    /* LS -> low <= PC, HI -> PC < low */
    sub x12, x12, #8
    ccmp x12, x9, #0, ls
    /* LS -> PC < low || PC >= high, HI -> low <= PC && PC < high */
    ldr w11, [x10, #OFFSETOF_DISP_DISABLED]
    mov x12, #0
    ccmp x11, x12, #0, ls
    /* LS -> (PC < low || PC >= high) && disabled = 0,
       HI -> (low <= PC && PC < high) || disabled != 0 */

    /* Figure out what sort of exception we've got.  All paths need this. */
    mrs x11, esr_el1  /* Exception Syndrome Register */
    lsr x11, x11, #26 /* Exception Class field is bits [31:26] */

    /* x9 = EPC, x11 = EC, x10 = dispatcher_shared_aarch64 */

    /* Faults while disabled should be rare, if the critical section is short,
     * and will always be within the dispatcher code.  Therefore we branch out
     * to the slowpath here. */
    b.hi el0_sync_disabled

    /* 15 instructions to here. */

    /* All exceptions use the 'enabled' area if the dispatcher is enabled. */
    add x10, x10, #OFFSETOF_DISP_ENABLED_AREA

    /* x9 = EPC, x10 = base of save area, x11 = EC */

save_syscall_context:
    /* We need to save r7 & r19+ no matter what, so get on with it. */
    /* We need to pass the address of the trap frame to the handler without
     * spilling to the stace, which means a7 needs to go into the trap frame.
     * */
    str x7,       [x10, #(7 * 8)]

    /* Callee-saved registers */
    stp x19, x20, [x10, #(19 * 8)]
    stp x21, x22, [x10, #(21 * 8)]
    stp x23, x24, [x10, #(23 * 8)]
    stp x25, x26, [x10, #(25 * 8)]
    stp x27, x28, [x10, #(27 * 8)]
    stp x29, x30, [x10, #(29 * 8)] /* FP & LR */

    /* High registers are now available. */

    /* User SP and PC */
    mrs x20, sp_el0
    stp x20, x9, [x10, #(31 * 8)]

    /* SPSR */
    mrs x19, spsr_el1
    str x19, [x10, #(33 * 8)]

    /* Is this a syscall? */
    cmp x11, #0x15 /* SVC or HVC from AArch64 EL0 */
    b.ne el0_abort_enabled

    /* 29 instructions to here. */

    /* If we're here, this is a syscall and we don't need to restore the
     * scratch registers. Just throw the stack frame away. */
    add sp, sp, #(4 * 8)

    /* Pass the address of the trap frame as argument 8. */
    mov x7, x10

    /* Jump to the common (C) syscall handler. */
    b sys_syscall

    /* 32 instructions to here */

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
    cmp x0, x1
    /* LS -> low <= PC, HI -> PC < low */
    sub x2, x2, #8
    ccmp x2, x1, #0, ls
    /* LS -> PC < low || PC >= high, HI -> low <= PC && PC < high */
    ldr w0, [x3, #OFFSETOF_DISP_DISABLED]
    mov x1, #0
    ccmp x0, x1, #0, ls
    /* LS -> (PC < low || PC >= high) && disabled = 0,
       HI -> (low <= PC && PC < high) || disabled != 0 */

    /* x1 = EPC, x3 = dispatcher_shared_aarch64 */

    /* Choose the right save area. */
    add x0, x3, #OFFSETOF_DISP_ENABLED_AREA
    add x2, x3, #OFFSETOF_DISP_DISABLED_AREA
    csel x0, x0, x2, hi

    /* x0 = save area, x1 = EPC */

    /* 15 instructions. */

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

    /* 29 instructions. */

    /* Load the saved scratch registers, and pass them as arguments to the
     * handler.  We can't save them ourselves as we've run out of
     * instructions.  We need to do at least this, to clear our stack frame.
     * */
    ldp x2, x3, [sp], #16 /* x0, x1 */
    ldp x4, x5, [sp], #16 /* x2, x3 */

    /* x0 = save area, x1 = EPC,
     * x2 = user x0, x3 = user x1,
     * x4 = user x2, x5 = user x3 */
    b handle_irq

    /* 32 instructions. */

.align 7 /* 0x500 */
/* We don't implement fast IRQs */
el0_aarch64_fiq:
    invalid_exception

.align 7 /* 0x580 */
/* A delayed abort.  We don't handle this. */
el0_aarch64_serror:
    invalid_exception

/* Offset 0x600 */
/* Exceptions from user level, EL0, executing AArch32.  This is currently
 * unimplemented. */
.align 7 /* 0x600 */
el0_aarch32_sync:
    invalid_exception
.align 7 /* 0x680 */
el0_aarch32_irq:
    invalid_exception
.align 7 /* 0x700 */
el0_aarch32_fiq:
    invalid_exception
.align 7 /* 0x780 */
el0_aarch32_serror:
    invalid_exception

.align 11
/********************************/
/*** End of exception vectors ***/
/********************************/

/* The tail of the user abort handler doesn't fit in the table. */
el0_abort_enabled:
    /* x9 = EPC, x10 = base of save area, x11 = EC */

    /* Spill the argument registers and x8. x7 has already been saved. */
    stp x0,  x1,  [x10]
    stp x2,  x3,  [x10, #( 2 * 8)]
    stp x4,  x5,  [x10, #( 4 * 8)]
    str x6,       [x10, #( 6 * 8)]
    str x8,       [x10, #( 8 * 8)]

    /* We saved x9-x12 in our stack frame. Copy them. */
    ldp x0,  x1,  [sp], #16 /* x9,  x10 */
    ldp x2,  x3,  [sp], #16 /* x11, x12 */
    stp x0,  x1,  [x10, #( 9 * 8)]
    stp x2,  x3,  [x10, #(11 * 8)]

    /* Now spill the rest of the caller-saved registers. */
    stp x13, x14, [x10, #(13 * 8)]
    stp x15, x16, [x10, #(15 * 8)]
    stp x17, x18, [x10, #(17 * 8)]

    /* x19-x30, SP, SPSR and ELR were saved in the common path. */

    /* x9 = EPC, x10 = base of save area, x11 = EC */

    /* All registers are now available. */

    /* Pass the EPC and the save area address to the handler. */
    mov x0, x9
    mov x1, x10

    /* XXX - this should be handled differently on AArch64 - the C code can
     * switch on the Exception Context itself, or just pass it through to the
     * handler. */
    /* Now we can jump to the appropriate handler. */
    cmp x11, #0x24 /* Data Abort */
    b.eq handle_user_page_fault
    cmp x11, #0x20 /* Prefetch Abort */
    b.eq handle_user_page_fault
    /* Unknown Trap/Undefined Instruction */
    b handle_user_undef

/* A synchronous exception while the dispatcher is disabled.  This is the
 * slowpath.  In fact, libbarrelfish currently (2015) just refuses to continue
 * if this ever happens. */
el0_sync_disabled:
    /* x9 = EPC, x11 = EC, x10 = dispatcher_shared_aarch64 */

    /* Filter out aborts. */
    cmp x11, #0x15 /* SVC or HVC from AArch64 EL0 */
    b.ne el0_abort_disabled

    /* Use the 'disabled' area. */
    add x10, x10, #OFFSETOF_DISP_DISABLED_AREA

    /* Jump back into the syscall path. */
    b save_syscall_context

/* This is the *really* unexpected path: a page fault in the dispatcher
 * critical section.  It's (relatively) slow, and libbarrelfish doesn't
 * actually handle it at present (2015). */
el0_abort_disabled:
    /* x11 = EC,
       x13 = dispatcher_shared_aarch64 */

    /* Use the 'trap' area. */
    add x10, x10, #OFFSETOF_DISP_TRAP_AREA

    /* Save the reduced context. */
    /* Callee-saved registers */
    stp x19, x20, [x10, #(19 * 8)]
    stp x21, x22, [x10, #(21 * 8)]
    stp x23, x24, [x10, #(23 * 8)]
    stp x25, x26, [x10, #(25 * 8)]
    stp x27, x28, [x10, #(27 * 8)]
    stp x29, x30, [x10, #(29 * 8)] /* FP & LR */

    /* SPSR */
    mrs x19, spsr_el1
    str x19, [x10, #(31 * 8)]

    /* User PC and SP */
    mrs x20, sp_el0
    stp x20, x9, [x10, #(32 * 8)]

    /* Now reuse the 'enabled' abort handler. */
    b el0_abort_enabled

/* Restore the dispatcher's execution context.  x0 holds the base of the
 * appropriate save area. */
do_resume:
    /* Skip to the end... */
    add x0, x0, #(33 * 8)

    /* Flush the TLB - XXX kill this with fire. */
    dsb sy
    tlbi vmalle1
    dsb sy
    isb

    /* Restore SPSR, PC (ELR) and SP, which need temporary registers, before
     * we restore those. */
    ldr x3,     [x0], #-16 /* spsr */
    ldp x1, x2, [x0], #-16 /* stack, pc */
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
