/**
 * \file
 * \brief Bootstrap the kernel.
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
        .arm

        .globl start, halt, got_base, cp15_invalidate_d_cache,\
                cp15_invalidate_i_and_d_caches,\
                cp15_invalidate_i_and_d_caches_fast, cp15_invalidate_tlb_fn
        .extern kernel_stack, glbl_core_data

        // Used to track phys memory allocator limit globally.
        alloc_top .req r11

start:
        // On entry:
        //
        // MMU disabled
        // Caches in unknown state, but no lockdown
        // No TLB lockdown.
        // CPU is in a priviledged mode.
        //


        mrs     r3, cpsr                        // Ensure in SYS mode
        bic     r3, r3, #ARM_MODE_MASK
        orr     r3, r3, #ARM_MODE_SYS
//        orr     r3, r3, #ARM_MODE_PRIV      // Ensure in Priveledged mode
        msr     cpsr_c, r3

		//init stack
        ldr     sp, =kernel_stack
        add		sp, sp, #KERNEL_STACK_SIZE

		ldr		PIC_REGISTER, =got_base

        //prepare argument
        mov		r0, r2
        b 		arch_init
        b		halt


/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
        b       .

/* Based on algorithm from ARM Architecture Reference Manual */
cp15_invalidate_d_cache:
	stmdb	sp!, {r4, r5, r6, r7, r8, r9, r10, r11}

	MRC p15, 1, R0, c0, c0, 1 // Read CLIDR
	ANDS R3, R0, #0x7000000
	MOV R3, R3, LSR #23       // Cache level value (naturally aligned)
	BEQ Finished
	MOV R10, #0

Loop1:	ADD R2, R10, R10, LSR #1     // Work out 3xcachelevel
        MOV R1, R0, LSR R2           // bottom 3 bits are the Cache type for this level
        AND R1, R1, #7               // get those 3 bits alone
        CMP R1, #2
        BLT Skip                     // no cache or only instruction cache at this level
        MCR p15, 2, R10, c0, c0, 0   // write the Cache Size selection register
        ISB                          // ISB to sync the change to the CacheSizeID reg
        MRC p15, 1, R1, c0, c0, 0    // reads current Cache Size ID register
        AND R2, R1, #7              // extract the line length field
        ADD R2, R2, #4               // add 4 for the line length offset (log2 16 bytes)
        LDR R4, =0x3FF
        ANDS R4, R4, R1, LSR #3      // R4 is the max number on the way size (right aligned)
        CLZ R5, R4                   // R5 is the bit position of the way size increment
        LDR R7, =0x00007FFF
        ANDS R7, R7, R1, LSR #13     // R7 is the max number of the index size (right aligned)
Loop2:	MOV R9, R4                   // R9 working copy of the max way size (right aligned)
Loop3:	ORR R11, R10, R9, LSL R5    // factor in the way number and cache number into R11
         ORR R11, R11, R7, LSL R2    // factor in the index number
         MCR p15, 0, R11, c7, c10, 2 // clean by set/way
         SUBS R9, R9, #1             // decrement the way number
         BGE Loop3
         SUBS R7, R7, #1             // decrement the index
         BGE Loop2
Skip:	ADD R10, R10, #2             // increment the cache number
         CMP R3, R10
         BGT Loop1
Finished:
	dsb
	ldmia	sp!, {r4, r5, r6, r7, r8, r9, r10, r11}
	bx	lr

cp15_invalidate_i_and_d_caches:
	stmdb	sp!, {lr}
	bl cp15_invalidate_d_cache
	mcr	p15, 0, r0, c7, c5, 0     // Invalidate all I caches to PoU (ICIALLU)
	dsb
	isb
	ldmia	sp!, {lr}
	bx	lr




/**********************************************************************/
/* Based on algorithm from ARM Architecture Reference Manual errata comments

Reference: ARM Architecture Refrence Manual ARMv7-A
Section B2.2.4 Cache maintenance functionality
        (ARMv7 cache maintenance operation)
        page 1251:
    This version is supposed to be faster than cp15_invalidate_d_cache version
    (as per ARM architecture ref manual)
 */

cp15_invalidate_d_cache_fast:
    stmdb   sp!, {r4, r5, r6, r7, r8, r9, r10, r11}

   MRC p15, 1, R0, c0, c0, 1   // Read CLIDR into R0
   ANDS R3, R0, #0x7000000
   MOV R3, R3, LSR #23         // Cache level value (naturally aligned)
   BEQ Finished11
   MOV R10, #0

Loop11:  ADD R2, R10, R10, LSR #1   // Work out 3 x cachelevel
   MOV R1, R0, LSR R2          // bottom 3 bits are the Cache type for this level
   AND R1, R1, #7              // get those 3 bits alone
   CMP R1, #2
   BLT Skip11                  // no cache or only instruction cache at this level
   MCR p15, 2, R10, c0, c0, 0  // write CSSELR from R10
   ISB                         // ISB to sync the change to the CCSIDR
   MRC p15, 1, R1, c0, c0, 0   // read current CCSIDR to R1
   AND R2, R1, #7              // extract the line length field
   ADD R2, R2, #4              // add 4 for the line length offset (log2 16 bytes)
   LDR R4, =0x3FF
   ANDS R4, R4, R1, LSR #3     // R4 is the max number on the way size (right aligned)
   CLZ R5, R4                  // R5 is the bit position of the way size increment
   MOV R9, R4                  // R9 working copy of the max way size (right aligned)

Loop12:  LDR R7, =0x00007FFF
   ANDS R7, R7, R1, LSR #13    // R7 is the max number of the index size (right aligned)

Loop13:  ORR R11, R10, R9, LSL R5 // factor in the way number and cache number into R11
   ORR R11, R11, R7, LSL R2    // factor in the index number
   MCR p15, 0, R11, c7, c10, 2 // DCCSW, clean by set/way
   SUBS R7, R7, #1             // decrement the index
   BGE Loop13
   SUBS R9, R9, #1             // decrement the way number
   BGE Loop12

Skip11:
   ADD R10, R10, #2            // increment the cache number
   CMP R3, R10
   BGT Loop11
   DSB
Finished11:
    dsb
    ldmia   sp!, {r4, r5, r6, r7, r8, r9, r10, r11}
    bx	lr

cp15_invalidate_i_and_d_caches_fast:
    stmdb   sp!, {lr}
    bl cp15_invalidate_d_cache_fast
    mcr	p15, 0, r0, c7, c5, 0	/* Invalidate all I caches to PoU (ICIALLU) */
    dsb
    isb
    ldmia   sp!, {lr}
    bx	lr

/**********************************************************************/
/* TLBFlush Based on code from ARM Architecture Reference Manual

Reference: ARM Architecture Refrence Manual ARMv7-A
Section B3.12.34 CP15 c8, TLB maintenance operations
        page 1415:
 */
cp15_invalidate_tlb:
	stmdb	sp!, {r4, r5, r6, r7, r8, r9, r10, r11} // not needed!
        // invalidate all unlocked entries in instruction TLB
        MCR p15, 0, r0, c8, c5, 0 // ITLBIALL, invalidate instruction TLB
        MCR p15, 0, r0, c8, c6, 0 // DTLBIALL, invalidate data TLB
        MCR p15, 0, r0, c8, c7, 0 // TLBIALL, invalidate unified TLB
        nop         // Note: to avoid issues with pipelined instructions
        nop
        nop
        nop
        nop
        nop
        nop
        nop

	dsb
	ldmia	sp!, {r4, r5, r6, r7, r8, r9, r10, r11} // not needed!
	bx	lr

cp15_invalidate_tlb_fn:
	stmdb	sp!, {lr}
	bl cp15_invalidate_tlb
	dsb
	isb
	ldmia	sp!, {lr}
	bx	lr



/**********************************************************************/

.ltorg

got_base:
        .word                                   // Initialized by linker

        .end
