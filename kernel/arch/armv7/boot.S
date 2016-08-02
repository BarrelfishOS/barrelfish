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

    .globl start, halt, target_mpid

    .bss
    .align 8
// This stack is used by the boot driver on every core - the CPU driver has
// its own stack on each core.  This means that two boot drivers shouldn't run
// simultaneously, unless and until we change this.
boot_stack:
    .space KERNEL_STACK_SIZE, 0
boot_stack_top:

    .section .text
    .arm

    .extern got_base

// Used to track phys memory allocator limit globally.
    alloc_top .req r11

// On ARM platforms that don't implement their own boot protocol (e.g.
// simulators), all cores will start executing here, and it's up to us to park
// the non-BSP cores.
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

    // Init stack
    ldr sp, =boot_stack_top

    ldr PIC_REGISTER, =got_base

    // Check to see if we're core 0.
    mrc p15, 0, r3, c0, c0, 5
    and r3, r3, #0xffffff
    cmp r3, #0

    // If not, wait for the BSP to wake us up.
    bne wait_for_bsp

    // Prepare argument
    b boot_bsp_core
    b halt

wait_for_bsp:
    wfe

    // Check whether we're the core that's being woken.  If not, go back to
    // sleep.
    ldr r4, target_mpid
    cmp r3, r4
    bne wait_for_bsp

    b boot_app_core
    b halt

// The ID of the application processor to boot - the low 24 bits of MPIDR.
    .type target_mpid, STT_OBJECT
target_mpid:
    .word 0

/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
    b .
