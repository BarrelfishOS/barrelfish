/**
 * \file
 * \brief Bootstrap the bootloader.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * This is the entry point called from the gem5 bootloader. It loads and relocates the
 * kernel image, and sets some global parameters passed along to the kernel
 * On entry:
 *
 * r0 contains zero
 * r1 contains board id
 * r2 atags ptr
 * r3 kernel start address
 * r4 GIC address
 * r5 flag register address
 */

	.text
	.arm
	.globl	molly_start, molly_to_kernel_transition
	//.extern cd, kernel_entry

        // ........................................
        //
        // Entry point.  Initialize a stack and branch to the
        // C entry point in molly_init.c
molly_start:
	ldr		sp, =molly_kernel_stack
	add		sp, sp, #4096
	b		molly_init

        // Never reached
halt:
	b	halt

        // ........................................
        //
        // Transfer control to the proper kernel once it has
        // been loaded and relocated.  The caller supplies the
        // r0 value through which the kernel expects
        // the core_data info.
molly_to_kernel_transition:
        mov     lr, r0
        mov     r0, r1
        bx		lr

        // ........................................
        //
        // Stack for use by the C molly kernel.

        .data
molly_kernel_stack:
        .skip   4096

