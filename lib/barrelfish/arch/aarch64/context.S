.global disp_save_context

/* Save the current thread's context.  This is only called for a voluntary
 * switch, and is therefore reached via a function call.  This means that
 * x0-x18 are expected to be clobbered, and will have been saved by the
 * caller, if required.  We are passed the base of the save area in x0. */
disp_save_context:
    /* Skip to x19 */
    add x0, x0, #(19 * 8)

    /* Save the callee-saved registers. */
    stp x19, x20, [x0], #(2 * 8)
    stp x21, x22, [x0], #(2 * 8)
    stp x23, x24, [x0], #(2 * 8)
    stp x25, x26, [x0], #(2 * 8)
    stp x27, x28, [x0], #(2 * 8)
    stp x29, x30, [x0], #(3 * 8) /* Skip SPSR - we can clobber it. */

    /* Save the stack pointer, and the address of our return stub. */
    mov x0, sp
    adr x1, disp_save_context_resume
    stp x0, x1, [x0], #(2 * 8)

/* This code is executed in two different contexts: First, it's just the
 * return to caller for disp_save_context.  Second, it's the return trampoline
 * to resume a thread - this is the return address that we stash in the
 * thread's trap frame, and it branches to whatever return address was saved
 * alongside it. */
disp_save_context_resume:
    br x30 /* LR */
