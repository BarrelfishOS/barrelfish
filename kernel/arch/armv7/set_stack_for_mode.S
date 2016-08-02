/*
 * Copyright (c) 2007-2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__
#endif // __ASSEMBLER__
#include <barrelfish_kpi/flags_arch.h> // CPSR_IF_MASK etc.

    .arm
    .globl set_stack_for_mode
    .text

//
// void set_stack_for_mode(uint8_t cpu_mode, void *stack_top)
//
set_stack_for_mode:
    mrs     r3, cpsr                // Save current mode
    and     r0, r0, # ARM_MODE_MASK
    bic     r2, r3, # ARM_MODE_MASK
    orr     r2, r2, r0
    msr     cpsr_c, r2              // Switch to cpu_mode
    mov     sp, r1
    msr     cpsr_c, r3              // Switch back
    bx      lr
