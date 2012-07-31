/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

        .arm
        .text
        .globl syscall
	
syscall:
        // Save pointer to return structure (r0), callee-save
	// registers (r4-r10,r12) that are cloberred.
        mov     r12, sp
        push    {r0, r4-r10, r11, r12, lr, pc}
        ldmia   r12, {r4-r10, r12}
        ldr     r0, [sp, #80]
        ldr     lr, =swi_done
        swi     #0
swi_done:
	nop
	nop
	nop
	nop
	// This is where we end up executing after the system call?
	// This accesses the stack, which was restored in do_resume
        pop     {r3,r4-r10} // pop 7 registers
        // r0,r1 contain return values, r3 points to return structure        
        str     r0, [r3, #0]
        str     r1, [r3, #4]
        // XXX: My assembler warns about the following line -AB 20110506
        // "Warning: writeback of base register when in register list is UNPREDICTABLE"
        // Popping the SP does seem a little dubious?
	// SK: I am very positive that the following instruction restores the wrong PC
        pop     {r11, sp, pc}
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
