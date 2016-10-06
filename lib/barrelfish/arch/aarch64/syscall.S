/*
 * Copyright (c) 2007-2009, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
        .text
        .globl syscall

syscall:
        // Save pointer to return structure (r0), callee-save
        // registers (x19-r28) that are cloberred.
        // x13 is used as temporary register

        mov     x13, sp

        sub     sp, sp, #128
        stp     x19, x20, [sp, #16 * 0]
        stp     x21, x22, [sp, #16 * 1]
        stp     x23, x24, [sp, #16 * 2]
        stp     x25, x26, [sp, #16 * 3]
        stp     x27, x28, [sp, #16 * 4]
        stp     x29, x30, [sp, #16 * 5]
        stp     x8,  x9,  [sp, #16 * 6]
        stp     x10, x11, [sp, #16 * 7]

        ldp     x8,  x9,  [x13], #16
        ldp     x10, x11, [x13], #16

        ldr     x30, =swi_done
        svc     #0

swi_done:
        // This is where we end up executing after the system call?
        // This accesses the stack, which was restored in do_resume
        ldp     x19, x20, [sp], #16
        ldp     x21, x22, [sp], #16
        ldp     x23, x24, [sp], #16
        ldp     x25, x26, [sp], #16
        ldp     x27, x28, [sp], #16
        ldp     x29, x30, [sp], #16
        ldp     x8,  x9,  [sp], #16
        ldp     x10, x11, [sp], #16

        RET // return

        nop
        nop
        nop
        nop
