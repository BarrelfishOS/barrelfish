/**
 * \file
 * \brief Bootstrap the kernel for ARMv7 processors.  This code is
 *      entered from the bootloader (typically arm_molly, RedBoot,
 *      etc.).
 */
/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
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
        // Caches in unknown state, but no lockdown
        // No TLB lockdown.
        // CPU is in a priviledged mode.
        //
        // TODO: ensure mode

        //init stack
        ldr     x1, =kernel_stack
        mov     sp, x1
        add     sp, sp, #KERNEL_STACK_SIZE

        ldr     PIC_REGISTER, got_base

        //prepare argument
        mov     x0, x2
        b       arch_init
        b       halt


/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
        b       .

/**********************************************************************/
.ltorg

got_base:
        .word                                   // Initialized by linker

        .end
