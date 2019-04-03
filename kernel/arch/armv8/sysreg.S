/**
 * \file
 * \brief Cache control routines for ARMv8.
 */

/*
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__   1
#endif

        .text
        .globl sysreg_invalidate_d_cache,\
               sysreg_invalidate_i_and_d_caches,\
               sysreg_invalidate_i_and_d_caches_fast, \
                  sysreg_invalidate_tlb_fn, \
                  sysreg_enable_mmu

/* Based on algorithm from ARM Architecture Reference Manual */
sysreg_invalidate_d_cache:

    sub     sp, sp, #96

    stp     x0, x1, [sp, #16 * 0]
    stp     x2, x3, [sp, #16 * 1]
    stp     x4, x5, [sp, #16 * 2]
    stp     x6, x7, [sp, #16 * 3]
    stp     x8, x9, [sp, #16 * 4]
    stp     x10, x11, [sp, #16 * 5]

    dmb    sy                // ensure ordering with previous memory accesses

    mrs x0, clidr_el1
    and w3, w0, #0x07000000     // get 2 x level of coherency
    lsr w3, w3, #23
    cbz w3, finished
    mov w10, #0                 // w10 = 2 x cache level
    mov w8, #1                     // w8 = constant 0b1

loop1: 
    add w2, w10, w10, lsr #1     // calculate 3 x cache level
    lsr w1, w0, w2                 // extract 3-bit cache type for this level
    and w1, w1, #0x7
    cmp w1, #2
    b.lt skip                     // no data or unified cache at this level
    msr csselr_el1, x10         // select this cache level
    isb                         // sync change of csselr
    mrs x1, ccsidr_el1             // read ccsidr
    and w2, w1, #7                 // w2 = log2(linelen)-4
    add w2, w2, #4                 // w2 = log2(linelen)
    ubfx w4, w1, #3, #10         // w4 = max way number, right aligned
    clz w5, w4                     // w5 = 32-log2(ways), bit position of way in dc operand
    lsl w9, w4, w5                 // w9 = max way number, aligned to position in dc operand
    lsl w16, w8, w5             // w16 = amount to decrement way number per iteration
loop2: 
    ubfx w7, w1, #13, #15         // w7 = max set number, right aligned
    lsl w7, w7, w2                 // w7 = max set number, aligned to position in dc operand
    lsl w17, w8, w2             // w17 = amount to decrement set number per iteration
loop3: 
    orr w11, w10, w9             // w11 = combine way number and cache number ...
    orr w11, w11, w7             // ... and set number for dc operand
    dc csw, x11                 // do data cache clean by set and way
    subs w7, w7, w17             // decrement set number
    b.ge loop3
    subs x9, x9, x16             // decrement way number
    b.ge loop2
skip: 
    add w10, w10, #2         // increment 2 x cache level
    cmp w3, w10
    b.gt loop1
finished:
    mov    x10, #0
    msr    csselr_el1, x10
    dsb    sy
    isb

    ldp x0, x1,  [sp], #16
    ldp x2, x3,  [sp], #16
    ldp x4, x5,  [sp], #16
    ldp x6, x7,  [sp], #16
    ldp x8, x9,  [sp], #16
    ldp x10, x11,  [sp], #16
    ret

sysreg_invalidate_i_and_d_caches:
    mov    x12, x30 //lr
    bl    sysreg_invalidate_d_cache
    mov    x0, #0
    ic    ialluis                // I+BTB cache invalidate
    ret    x12

sysreg_invalidate_d_cache_fast:
    mov x12, x30 //lr
    bl  sysreg_invalidate_d_cache
    ret x12

sysreg_invalidate_i_and_d_caches_fast:
    mov x12, x30 //lr
    bl  sysreg_invalidate_d_cache
    mov x0, #0
    ic  ialluis             // I+BTB cache invalidate
    ret x12

sysreg_invalidate_tlb:

    sub     sp, sp, #96

    stp     x0, x1, [sp, #16 * 0]
    stp     x2, x3, [sp, #16 * 1]
    stp     x4, x5, [sp, #16 * 2]
    stp     x6, x7, [sp, #16 * 3]
    stp     x8, x9, [sp, #16 * 4]
    stp     x10, x11, [sp, #16 * 5]

    tlbi    vmalle1
    dsb    sy
    isb

    ldp x0, x1,  [sp], #16
    ldp x2, x3,  [sp], #16
    ldp x4, x5,  [sp], #16
    ldp x6, x7,  [sp], #16
    ldp x8, x9,  [sp], #16
    ldp x10, x11,  [sp], #16
    ret

sysreg_invalidate_tlb_fn:
    mov x12, x30 //lr
    bl  sysreg_invalidate_tlb
    ret x12

sysreg_enable_mmu:
    mov x12, x30 //lr

    ic      iallu                           // I+BTB cache invalidate
    tlbi    vmalle1                         // invalidate I + D TLBs
    dsb     sy

    ldr     x0, =0xff440c0400
    msr        mair_el1, x0
    isb

    /*  TCR - Translation Control Register
     *    4K granularity, 32-bit addresses, two subranges:
     *
     * TTBR1_EL1 -> 0xffff_ffff_8000_0000 to 0xffff_ffff_ffff_ffff
     * TTBR0_EL1 -> 0x0000_0000_0000_0000 to 0x0000_0000_7fff_ffff
     */

    ldr     x0, =0x10b5203520
    msr     tcr_el1, x0
    isb

    ldr     x0, =0x30d0199d
    msr     sctlr_el1, x0
    isb

    ret x12

