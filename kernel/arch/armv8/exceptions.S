/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__
#endif // __ASSEMBLER__

#include <asmoffsets.h> // OFFSETOF etc.
#include <barrelfish_kpi/registers_arch.h> // CPSR_REG etc.
#include <barrelfish_kpi/flags_arch.h> // CPSR_IF_MASK etc.
#include <exceptions.h>

// macro for creating table entries that are nicely aligned
.macro vector_entry label
// every entry is at 128 byte boundary
.align 7
    b   \label
.endm

// macro for getting current exception level (result in arg)
.macro get_cur_el reg
    mrs     \reg, currentel
    ubfm    \reg, \reg, #2, #3 // extract currentel[3:2]
.endm

/* XXX - add a cross-reference. */
// macro for saving exception frame: needs to match with struct layout in C
// code
.macro push_state_to_exn_frame
    // make room for struct on stack
    sub     sp, sp, #48 // XXX: figure out how much room we need beyond x0-x29/use BF Macros
    stp     x28, x29, [sp, #-16]!
    stp     x26, x27, [sp, #-16]!
    stp     x24, x25, [sp, #-16]!
    stp     x22, x23, [sp, #-16]!
    stp     x20, x21, [sp, #-16]!
    stp     x18, x19, [sp, #-16]!
    stp     x10, x17, [sp, #-16]!
    stp     x10, x15, [sp, #-16]!
    stp     x10, x13, [sp, #-16]!
    stp     x10, x11, [sp, #-16]!
    stp      x8,  x9, [sp, #-16]!
    stp      x6,  x7, [sp, #-16]!
    stp      x4,  x5, [sp, #-16]!
    stp      x2,  x3, [sp, #-16]!
    stp      x0,  x1, [sp, #-16]!
    // save old SP: 48 + 32 * 8
    add     x1, sp, #288
    get_cur_el x0
    cmp     x0, #1
    b.ne    push_get_el2_state_\@
    // save ELR_EL1
    mrs     x2, elr_el1
    // save SPSR_EL1
    mrs     x3, spsr_el1
    b       push_el_state_cont_\@
push_get_el2_state_\@:
    // save ELR_EL1
    mrs     x2, elr_el2
    // save SPSR_EL1
    mrs     x3, spsr_el2
push_el_state_cont_\@:
    // store pair of lr and sp just after x29
    stp     x30, x1, [sp, #240]
    // store pair of elr_el1, and spsr_el1 just after sp
    stp     x2, x3, [sp, #256]
.endm

// macro for restoring from exception frame: needs to match with struct layout
// in C code
.macro pop_state_from_exn_frame
    // load elr, spsr
    ldp     x21, x22, [sp, #256]
    ldp     x0, x1, [sp], #16
    ldp     x2, x3, [sp], #16
    ldp     x4, x5, [sp], #16
    ldp     x6, x7, [sp], #16
    ldp     x8, x9, [sp], #16
    get_cur_el x10
    cmp     x10, #1
    b.ne    pop_restore_el2_state_\@
    // set elr and spsr
    msr     elr_el1, x21
    msr     spsr_el1, x22
    b       pop_state_cont_\@
pop_restore_el2_state_\@:
    cmp     x10, #2
    b.ne    halt
    // set elr and spsr
    msr     elr_el2, x21
    msr     spsr_el2, x22
pop_state_cont_\@:
    ldp      x10, x11, [sp], #16
    ldp      x12, x13, [sp], #16
    ldp      x14, x15, [sp], #16
    ldp      x16, x17, [sp], #16
    ldp      x18, x19, [sp], #16
    ldp      x20, x21, [sp], #16
    ldp      x22, x23, [sp], #16
    ldp      x24, x25, [sp], #16
    ldp      x26, x27, [sp], #16
    ldp      x28, x29, [sp], #16
    // restore link register
    ldr     x30, [sp], #48
.endm

.global exception_vectors
// XXX - why?
.type exception_vectors @function
// page-align exception vector table, cf. ARM reference manual, p.D7-2138
.align 12
exception_vectors:
    // current EL with SP_EL0: NYI/unused
    vector_entry invalid_handler    // sync
    vector_entry invalid_handler    // IRQ/vIRQ
    vector_entry invalid_handler    // FIQ/vFIQ
    vector_entry invalid_handler    // SError/vSError

    // current EL with SP_ELcur
    vector_entry sync_handler       // sync
    vector_entry irq_handler        // IRQ/vIRQ
    vector_entry fiq_handler        // FIQ/vFIQ
    vector_entry serror_handler     // SError/vSError

    // lower EL running AArch64: NYI
    vector_entry invalid_handler    // sync
    vector_entry invalid_handler    // IRQ/vIRQ
    vector_entry invalid_handler    // FIQ/vFIQ
    vector_entry invalid_handler    // SError/vSError

    // lower EL running AArch32: unsupported
    vector_entry invalid_handler    // sync
    vector_entry invalid_handler    // IRQ/vIRQ
    vector_entry invalid_handler    // FIQ/vFIQ
    vector_entry invalid_handler    // SError/vSError


// we are using unsigned bitfield moves (ubfm, C6-789) to extract fields from
// registers.
//
// Exception Class (EC) in exception syndrome register (ESR): bits 26-31, cf.
// ARMv8 reference manual, p.D7-1944
#define ESR_EC_H 31
#define ESR_EC_L 26
// Instruction Specific Syndrome (ISS) in ESR: bits 24-0, cf. D7-1949
#define ESR_ISS_H 24
#define ESR_ISS_L 0
// ESR ISS fields for Data/Instr aborts: bits 5-0 encode data fault status
// code (DFSC), format: FFFFLL, where F bits indicate fault type, and L bits
// indicate fault level
#define ESR_DFSC_H 5
#define ESR_DFSC_L 0
// Data Abort value of EC in ESR: 0x24 when caused in lower EL, 0x25 when
// caused in same EL
#define ESR_EC_DATA_ABORT_LEL 0x24
#define ESR_EC_DATA_ABORT_SEL 0x25
// Instruction Abort value of EC in ESR: 0x20 when caused in lower EL, 0x21
// when caused in same EL
#define ESR_EC_INST_ABORT_LEL 0x20
#define ESR_EC_INST_ABORT_SEL 0x21

        //
        // void invalid_handler(void)
        //
        // handler for NYI/unused exceptions
        //
.global invalid_handler
.type invalid_handler @function
invalid_handler:
    mrs     x20, elr_el1    // store exception link register to x20
    mrs     x21, spsr_el1   // store saved pstate to x21
    mrs     x22, esr_el1    // store exception syndrome register to x22

    ubfm    x23, x22, #ESR_EC_L, #ESR_EC_H
    ubfm    x24, x22, #ESR_ISS_L, #ESR_ISS_H

    b       halt


        //
        // void sync_handler(void)
        //
        // handle synchronous exceptions (e.g. Data/Instr aborts)
        //
.global sync_handler
.type sync_handler @function
sync_handler:
    push_state_to_exn_frame

    // get current EL
    get_cur_el x0
    cmp     x0, #1
    // != -> check for EL2
    b.ne    sync_handler_check_el2
    // == -> extract from  esr_el
    // extract exception details from ESR_EL1
    mrs     x1, esr_el1
    b       sync_handler_common
sync_handler_check_el2:
    cmp     x0, #2
    b.ne    halt
    // == -> use el2 registers
    // extract exception details from ESR_EL1
    mrs     x1, esr_el2

sync_handler_common:
    // Exception Class to x2
    ubfm    x2, x1, #ESR_EC_L, #ESR_EC_H
    // Fault to x3
    ubfm    x3, x1, #ESR_DFSC_L, #ESR_DFSC_H
    // goto C if data abort (can only be from same EL right now)
    cmp     x2, #ESR_EC_DATA_ABORT_SEL
    b.eq    handle_pf_abort
    // goto C if instruction abort (can only be from same EL right now)
    cmp     x2, #ESR_EC_INST_ABORT_SEL
    b.eq    handle_pf_abort
    // otherwise, goto unknown_handler(esr)
    b.ne    handle_sync_abort
    b       sync_handler_epi
handle_pf_abort:
    // goto invalid_handler if fault == 0
    cbz     x3, invalid_handler
    // goto invalid_handler if fault > 3
    cmp     x3, #3
    b.hi    invalid_handler

    // save exn frame (top of stack) to x0
    mov     x0, sp
    bl      page_fault

sync_handler_epi:
    pop_state_from_exn_frame
    eret

        //
        // void irq_handler(void)
        //
.global irq_handler
.type irq_handler @function
irq_handler:
    push_state_to_exn_frame
    b   handle_irq
    pop_state_from_exn_frame
    eret

// FIQ/SError NYI
.global fiq_handler
.type fiq_handler @function
fiq_handler:
.global serror_handler
.type serror_handler @function
serror_handler:
    b   invalid_handler
