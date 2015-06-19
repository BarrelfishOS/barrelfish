/**
 * \file
 * \brief Bootstrap the kernel for UEFI/ARMv8 platforms.  This code is
 *      entered from the bootloader (typically arm_molly, RedBoot,
 *      etc.).
 */
/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__   1
#endif

#include <offsets.h> // BOOT_STACK_PHYS

        .text

        .globl start, halt
        .extern glbl_core_data, exception_vectors

        // Used to track phys memory allocator limit globally.
        alloc_top .req x11

start:
        // On entry:
        //
        // Single core running (not guaranteed to be core 0)
        // CPU is in highest non-secure privilege mode: EL2 or EL1
        // MMU enabled, 4k translation granule, 1:1 mapping of all RAM, using
        // TTBR0.
        // Little-endian mode
        // Core caches (L1&L2) and TLB enabled
        // Non-architectural caches disabled
        // Interrupts enabled
        // Generic timer initialized and enabled
        // >= 128KiB stack
        // ACPI tables available
        // Register x0 contains handle to ACPI root table
        // Register x1 contains a pointer to the UEFI memory map
        //

        // set exception vectors for EL1 and EL2
        adrp    x15, exception_vectors
        msr     vbar_el1, x15
        msr     vbar_el2, x15

        // maybe prepare arguments
        b       arch_init
        b       halt


/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
.type halt @function
halt:   wfi
        b       halt

/**********************************************************************/
#if 0
.ltorg

got_base:
        .word                                   // Initialized by linker

        .end
#endif
