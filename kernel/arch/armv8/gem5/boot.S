/**
 * \file
 * \brief Bootstrap the kernel.
 */

/*
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__   1
#endif

#include <barrelfish_kpi/flags_arch.h> // ARM_MODE_MASK
#include <offsets.h> // BOOT_STACK_PHYS
        
        .text

        .globl start, halt, got_base
        .extern kernel_stack, glbl_core_data

        // Used to track phys memory allocator limit globally.
        alloc_top .req x11

start:
        // On entry:
        //
        // MMU disabled
        // Caches disabled
        // CPU is in EL1.
        //
        mov     x2, x0
        mov     x0, #3 << 20
        msr     cpacr_el1, x0 // FP and ASIMD instructions does not 
                              // cause any instruction to be trapped.

        //init stack
        ldr     x0, =kernel_stack
        add     x0, x0, #KERNEL_STACK_SIZE
        mov     sp, x0

        ldr PIC_REGISTER, =got_base

        //prepare argument
        mov     x0, x2
        b       arch_init
        b       halt


/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
        b       .

.ltorg

got_base:
        .word                                   // Initialized by linker

        .end
