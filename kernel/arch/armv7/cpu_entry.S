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
#include <offsets.h> // KERNEL_STACK_SIZE

        .section .text.init
        .arm

        .globl start, halt, got_base
        .extern kernel_stack_top, glbl_core_data

        // Used to track phys memory allocator limit globally.
        alloc_top .req r11

start:
        // On entry:
        //
        // MMU, caches enabled.
        // No TLB lockdown.
        // CPU is in a system mode, interrupts and aborts disabled.
        //

        // Init stack
        ldr     sp, =kernel_stack_top

        // Initialise the got base register
        ldr     PIC_REGISTER, =got_base

        // Save the GOT base to the PL1 thread ID register, whence it will be
        // reloaded on every kernel entry.
        mcr     p15, 0, PIC_REGISTER, c13, c0, 4

        b       arch_init
        b       halt


/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
        b       .
