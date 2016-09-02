/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __EXCEPTIONS_H__
#define __EXCEPTIONS_H__

#define ARM_EVECTOR_RESET 0x00
#define ARM_EVECTOR_UNDEF 0x04
#define ARM_EVECTOR_SWI   0x08
#define ARM_EVECTOR_PABT  0x0c
#define ARM_EVECTOR_DABT  0x10
#define ARM_EVECTOR_IRQ   0x18
#define ARM_EVECTOR_FIQ   0x1c

#define CACHE_LINE_BYTES 32

#if !defined(__ASSEMBLER__)

#include <barrelfish_kpi/dispatcher_shared_arch.h>

/* This is the exception jump table, defined in armv7/exceptions.S. */
extern uint32_t exception_vectors[8];

/**
 * Handle page fault in user-mode process.
 *
 * This function should be called in SVC mode with interrupts disabled.
 */
void handle_user_page_fault(lvaddr_t                fault_address,
                            arch_registers_state_t* saved_context,
			    struct dispatcher_shared_arm *disp)
    __attribute__((noreturn));

/**
 * Handle undefined instruction fault in user-mode process.
 *
 * This function should be called in SVC mode with interrupts disabled.
 */
void handle_user_undef(lvaddr_t                fault_address,
                       arch_registers_state_t* saved_context,
		       struct dispatcher_shared_arm *disp)
    __attribute__((noreturn));

/**
 * Handle faults in occuring in a priviledged mode.
 *
 * This function should be called in SVC mode with interrupts disabled.
 */
void fatal_kernel_fault(uint32_t   evector,
                        lvaddr_t   fault_address,
                        arch_registers_state_t* saved_context)
    __attribute__((noreturn));

/**
 * Handle IRQs in occuring in SYS mode.
 *
 * This panics - we should never take an interrupt in SYS mode.
 */
void handle_irq_kernel(arch_registers_state_t* save_area, uintptr_t fault_pc)
    __attribute__((noreturn));

/**
 * Handle IRQs in occuring in USR mode.
 *
 * This function should be called in SVC mode with interrupts disabled.
 */
struct dispatcher_shared_generic;
void handle_irq(arch_registers_state_t* save_area, 
		uintptr_t fault_pc, 
		struct dispatcher_shared_arm *disp)
    __attribute__((noreturn));

/**
 * Handle FIQs in occuring in SYS mode.
 *
 * This panics - we should never take an interrupt in SYS mode.
 */
void handle_fiq_kernel(arch_registers_state_t* save_area, uintptr_t fault_pc)
    __attribute__((noreturn));

/**
 * Handle FIQs in occuring in USR mode.
 *
 * This function should be called in SVC mode with interrupts disabled.
 */
struct dispatcher_shared_generic;
void handle_fiq(arch_registers_state_t* save_area, 
		uintptr_t fault_pc, 
		struct dispatcher_shared_generic *disp)
    __attribute__((noreturn));

#endif // !defined(__ASSEMBLER__)

#endif // __EXCEPTIONS_H__

