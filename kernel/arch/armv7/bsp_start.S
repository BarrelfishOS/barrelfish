/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__
#endif // __ASSEMBLER__

#include <offsets.h>

    .globl bsp_stack, bsp_stack_top, bsp_start

    .arm
    .section .text.bsp

/*** Boot-core-only code. ***/

/* This is the entry point for BSP kernel, which loads GOT base and kernel
 * stack from their linked locations in the BSP image. */
bsp_start:
    // On entry:
    //
    // MMU, caches enabled.
    // No TLB lockdown.
    // CPU is in a system mode, interrupts and aborts disabled.
    //

    // Find the GOT, and place its address in the PIC register.  On the BSP,
    // we're using the data segment provided by the bootloader, against which
    // our text segment was relocated.  Thus it's safe to use code-relative
    // addressing to find it here.  After this point, we'll never again use
    // code-relative addressing outside the text segment, as other cores will
    // have relocated data segments, but may share this text segment.

    adr r4, bsp_stack_got_prel
    ldr r5, [r4]
    ldr r6, bsp_stack_got_offset
    // We have:
    //  - r4 = &stack_got_prel
    //  - r5 = GOT(bsp_stack_top) - &stack_got_prel
    //  - r6 = GOT(bsp_stack_top) - GOT_ORG

    add r4, r5, r4 // r4 = GOT(bsp_stack_top)
    sub r4, r4, r6 // r4 = GOT_ORG

    // Set the PIC register.
    mov PIC_REGISTER, r4

    // Save the GOT base to the PL1 thread ID register, whence it will be
    // reloaded on every kernel entry.
    mcr p15, 0, PIC_REGISTER, c13, c0, 4

    // Initialise kernel_stack_ptr to the BSP stack, via the GOT.
    ldr r5, [PIC_REGISTER, r6]
    ldr r8, kernel_stack_ptr_got
    ldr r7, [PIC_REGISTER, r8]
    str r5, [r7]

    // Load our stack pointer.
    mov sp, r5

    b arch_init
    b halt

/* These two relocations allow us to locate the GOT on the BSP core. */

    .align 2
    .type bsp_stack_got_offset, STT_OBJECT
bsp_stack_got_offset:
    // This is the address of the GOT entry for bsp_stack_top, relative to the
    // base of the GOT.
    .word bsp_stack_top(GOT)

    .type bsp_stack_got_prel, STT_OBJECT
bsp_stack_got_prel:
    // This is the address of that same GOT entry, relative to (here + 8
    // bytes) (PC bias).
    .word bsp_stack_top(GOT_PREL)

    .type kernel_stack_ptr_got, STT_OBJECT
kernel_stack_ptr_got:
    .word kernel_stack_ptr(GOT)

/* Any remaining constants go here. */
.ltorg

/**
 * \brief Kernel stack.
 *
 * This is the preallocated stack for the BSP kernel.
 */
    .section .data.bsp
    .align 3
bsp_stack:
    .space KERNEL_STACK_SIZE, 0
bsp_stack_top:
