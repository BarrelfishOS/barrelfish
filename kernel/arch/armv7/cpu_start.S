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
#include <asmoffsets.h>

    .globl cpu_start

    .arm
    .section .text

/*** Application-core startup code. ***/

/* This is the entry point for application-core kernels, which read their GOT
 * base pointer from the boot record, and find their stack via the GOT. */
cpu_start:
    // On entry:
    //
    // MMU, caches enabled.
    // No TLB lockdown.
    // CPU is in a system mode, interrupts and aborts disabled.
    // r0 points to the core_data struct
    // r1 points to the boot record
    //

    // Our GOT base is passed in the first word of the core_data struct
    ldr PIC_REGISTER, [r0, #OFFSETOF_COREDATA_GOT_BASE]

    // Save the GOT base to the PL1 thread ID register, whence it will be
    // reloaded on every kernel entry.
    mcr p15, 0, PIC_REGISTER, c13, c0, 4

    // We locate our stack by looking in the GOT
    ldr r4, cpu_stack_got_offset
    ldr sp, [PIC_REGISTER, r4]
    add sp, sp, #KERNEL_STACK_SIZE

    b arch_init

    .align 2
    .type cpu_stack_got_offset, STT_OBJECT
cpu_stack_got_offset:
    .word kernel_stack(GOT)

/* Any remaining constants go here. */
.ltorg
