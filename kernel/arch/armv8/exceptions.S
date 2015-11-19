/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

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
    b invalid_exception
.align 7 /* 0x080 */
el1_sp_el0_irq:
    b invalid_exception
.align 7 /* 0x100 */
el1_sp_el0_fiq:
    b invalid_exception
.align 7 /* 0x180 */
el1_sp_el0_serror:
    b invalid_exception

/* Offset 0x200 */
/* Exceptions from the kernel itself, at EL1. */
.align 7 /* 0x200 */
el1_sync:
    b invalid_exception
.align 7 /* 0x280 */
el1_irq:
    b invalid_exception
.align 7 /* 0x300 */
el1_fiq:
    b invalid_exception
.align 7 /* 0x380 */
el1_serror:
    b invalid_exception

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
     * restored if we're doing a syscall; they're caller-saved. */
    stp x11, x12, [sp, #-16]!
    stp x9,  x10, [sp, #-16]!

    /* The EL1 thread ID register holds the address of the currently-running
     * dispatcher's shared control block. */
    msr x10, tpidr_el1

    /* x10 = dispatcher_shared_aarch64 */

    /* Exception PC */
    msr x9, elr_el1

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
    /* We need to save r19+ no matter what, so get on with it. */
    /* Callee-saved registers */
    stp x19, x20, [x10]
    stp x21, x22, [x10, #16]
    stp x23, x24, [x10, #32]
    stp x25, x26, [x10, #48]
    stp x27, x28, [x10, #64]
    stp x29, x30, [x10, #80] /* FP & LR */

    /* High registers are now available. */

    /* SPSR */
    mrs x19, spsr_el1
    str x19, [x10, #96]

    /* User PC and SP */
    mrs x20, sp_el0
    stp x20, x9, [x10, #104]

    /* Is this a syscall? */
    cmp x11, #0x15 /* SVC or HVC from AArch64 EL0 */
    b.ne el0_abort_enabled

    /* 28 instructions to here. */

    /* If we're here, this is a syscall and we don't need to restore the
     * scratch registers. Just throw the stack frame away. */
    add sp, sp, #(16 + 16)

    /* Jump to the common (C) syscall handler. */
    b sys_syscall

    /* 30 instructions to here */

.align 7 /* 0x480 */
/* An interrupt at user level */
el0_aarch64_irq:
    /* Reenable breakpoints and aborts.  Interrupts remain disabled. */
    msr daifset, #3 /* IRQ and FIQ masked, Debug and Abort enabled. */

    /* Free scratch registers. */
    stp x2, x3, [sp, #-16]!
    stp x0, x1, [sp, #-16]!

    /* Find the dispatcher. */
    msr x3, tpidr_el1

    /* Get the exception address (EPC) */
    msr x1, elr_el1

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
    stp  x4,  x5, [x0, #32]
    stp  x6,  x7, [x0, #48]
    stp  x8,  x9, [x0, #64]
    stp x10, x11, [x0, #80]
    stp x12, x13, [x0, #96]
    stp x14, x15, [x0, #102]
    stp x16, x17, [x0, #128]
    stp x18, x19, [x0, #144]
    stp x20, x21, [x0, #160]
    stp x22, x23, [x0, #176]
    stp x24, x25, [x0, #192]
    stp x26, x27, [x0, #208]
    stp x28, x29, [x0, #224]
    str x30,      [x0, #240]

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
    b invalid_exception

.align 7 /* 0x580 */
/* A delayed abort.  We don't handle this. */
el0_aarch64_serror:
    b invalid_exception

/* Offset 0x600 */
/* Exceptions from user level, EL0, executing AArch32.  This is currently
 * unimplemented. */
.align 7 /* 0x600 */
el0_aarch32_sync:
    b invalid_exception
.align 7 /* 0x680 */
el0_aarch32_irq:
    b invalid_exception
.align 7 /* 0x700 */
el0_aarch32_fiq:
    b invalid_exception
.align 7 /* 0x780 */
el0_aarch32_serror:
    b invalid_exception

/********************************/
/*** End of exception vectors ***/
/********************************/

/* The tail of the user abort handler doesn't fit in the table. */
el0_abort_enabled:
    /* x10 = base of save area, x11 = EC */

    /* Spill the argument registers and x8. */
    stp x0,  x1,  [x10, #120]
    stp x2,  x3,  [x10, #136]
    stp x4,  x5,  [x10, #152]
    stp x6,  x7,  [x10, #168]
    str x8,       [x10, #184]

    /* We saved x9-x12 in our stack frame. Copy them. */
    ldp x0,  x1,  [sp], #16 /* x9,  x10 */
    ldp x2,  x3,  [sp], #16 /* x11, x12 */
    stp x0,  x1,  [x10, #192]
    stp x2,  x3,  [x10, #208]

    /* Now spill the rest of the caller-saved registers. */
    stp x13, x14, [x10, #224]
    stp x15, x16, [x10, #240]
    stp x17, x18, [x10, #256]

    /* x19-x30, SP, SPSR and ELR were saved in the common path. */

    /* x11 = EC */

    /* All registers are now available. */

    /* Now we can jump to the appropriate handler. */
    cmp x11, #0x24 /* Data Abort */
    b handle_user_page_fault
    cmp x11, #0x20 /* Prefetch Abort */
    b handle_user_page_fault
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
    add x10, x10, #OFFSET_OF_DISP_TRAP_AREA

    /* Save the reduced context. */
    /* Callee-saved registers */
    stp x19, x20, [x10]
    stp x21, x22, [x10, #16]
    stp x23, x24, [x10, #32]
    stp x25, x26, [x10, #48]
    stp x27, x28, [x10, #64]
    stp x29, x30, [x10, #80] /* FP & LR */

    /* SPSR */
    mrs x19, spsr_el1
    str x19, [x10, #96]

    /* User PC and SP */
    mrs x20, sp_el0
    stp x20, x9, [x10, #104]

    /* Now reuse the 'enabled' abort handler. */
    b el0_abort_enabled

fatal_kernel_fault:
    /* XXX - write me */
    b fatal_kernel_fault

/* Restore the dispatcher's execution context.  x0 holds the base of the
 * appropriate save area. */
do_resume:
    /* Skip to the end... */
    add x0, x0, #((NUM_REGS-1) * 8)

    /* Flush the TLB - XXX kill this with fire. */
    dsb sy
    tlbi alle1
    dsb sy
    isb

    /* Restore SPSR, PC (ELR) and SP, which need temporary registers, before
     * we restore those. */
    ldr x3,     [x0], #-8  /* spsr */
    ldr x1, x2, [x0], #-16 /* pc, stack */
    msr spsr_el1, x3
    msr elr_el1,  x2
    mov sp,       x1

    /* Restore the general-purpose registers. */
    ldp x29, x30, [x0], #-16
    ldp x27, x28, [x0], #-16
    ldp x25, x26, [x0], #-16
    ldp x23, x24, [x0], #-16
    ldp x21, x22, [x0], #-16
    ldp x19, x20, [x0], #-16
    ldp x17, x18, [x0], #-16
    ldp x15, x16, [x0], #-16
    ldp x13, x14, [x0], #-16
    ldp x11, x12, [x0], #-16
    ldp x9,  x10, [x0], #-16
    ldp x7,  x8,  [x0], #-16
    ldp x5,  x6,  [x0], #-16
    ldp x3,  x4,  [x0], #-16
    ldp x1,  x2,  [x0], #-16
    ldr x0,       [x0]

    /* Return from exception.  This clears the load exclusive monitor. */
    eret
