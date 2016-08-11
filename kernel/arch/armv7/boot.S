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

        .bss
        .align 8
boot_stack:
        .space KERNEL_STACK_SIZE, 0
boot_stack_top:

        .section .text
        .arm

        .globl start, halt
        .extern got_base

        // Used to track phys memory allocator limit globally.
        alloc_top .req r11

start:
        // On entry:
        //
        // MMU disabled
        // Caches in unknown state, but no lockdown
        // No TLB lockdown.
        // CPU is in a privileged mode.
        //

        // Switch to system mode, with aborts and interrupts off.
        cpsid aif, #ARM_MODE_SYS

        // Check to see if we're core 0.
        mrc p15, 0, r3, c0, c0, 5
        tst r3, #0xf

        beq keep_booting

        // If not, wait for the BSP to wake us up.
        wfe

keep_booting:
        // Init stack
        ldr     sp, =boot_stack_top

        ldr     PIC_REGISTER, =got_base

        // Prepare argument
        mov     r0, r2
        b       boot
        b       halt


/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
        b       .
