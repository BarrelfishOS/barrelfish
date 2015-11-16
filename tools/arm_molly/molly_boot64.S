/**
 * \file
 * \brief Bootstrap the bootloader.
 */

/*
 * Copyright (c) 2007-2010,2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * This is the entry point called from the gem5 bootloader. It loads and
 * relocates the kernel image, and sets some global parameters passed along to
 * the kernel On entry:
 *
 * x0 contains zero
 * x1 contains board id
 * x2 atags ptr
 * x3 kernel start address
 * x4 GIC address
 * x5 flag register address
 */
	.text
	.globl	aarch64_start, aarch64_to_kernel_transition

        // ........................................
        //
        // Entry point.  Initialize a stack and branch to the
        // C entry point in init.c
aarch64_start:
	ldr		x6, =aarch64_kernel_stack
	add	    x6, x6, #2048//done in two steps to make sure it is thumb-compatible
	add     x6, x6, #2048
	mov     sp, x6
	b		aarch64_init

        // Never reached
halt:
	b	halt

        // ........................................
        //
        // Transfer control to the proper kernel once it has
        // been loaded and relocated.  The caller supplies the
        // r0 value through which the kernel expects
        // the core_data info.
aarch64_to_kernel_transition:
        mov     x30, x0
        mov     x0, x1
        br		x30

        // ........................................
        //
        // Stack for use by the C molly kernel.

        .data
aarch64_kernel_stack:		
        .skip   4096

