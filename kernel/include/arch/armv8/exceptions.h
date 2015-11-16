/*
 * Copyright (c) 2007,2008,2009,2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARMV8_EXCEPTIONS_H
#define KERNEL_ARMV8_EXCEPTIONS_H

#define AARCH64_EVECTOR_RESET 0x00
#define AARCH64_EVECTOR_UNDEF 0x04
#define AARCH64_EVECTOR_SWI   0x08
#define AARCH64_EVECTOR_PABT  0x0c
#define AARCH64_EVECTOR_DABT  0x10
#define AARCH64_EVECTOR_IRQ   0x18
#define AARCH64_EVECTOR_FIQ   0x1c

#define CACHE_LINE_BYTES 32
#define ETABLE_ADDR     		0xffffffffffff0000
#define ETABLE_SECTION_OFFSET	0xf000
#define JUMP_TABLE_OFFSET		0x100
#define ETABLE_PHYS_BASE		0x800f0000

#if !defined(__ASSEMBLER__)

/**
 * Install and enable high-memory exception vectors.
 *
 * This routine switches the processor to use the high memory
 * exception table (starts at offset 0xffff0000). It then
 * installs the kernel exception handlers.
 */
void exceptions_init(void);

/**
 * Handle page fault in user-mode process.
 *
 * This function should be called in SVC mode with interrupts disabled.
 */
void handle_user_page_fault(lvaddr_t                fault_address,
                            arch_registers_state_t* saved_context)
    __attribute__((noreturn));

/**
 * Handle undefined instruction fault in user-mode process.
 *
 * This function should be called in SVC mode with interrupts disabled.
 */
void handle_user_undef(lvaddr_t                fault_address,
                       arch_registers_state_t* saved_context)
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
 * Handle IRQs in occuring in USR or SYS mode.
 *
 * This function should be called in SVC mode with interrupts disabled.
 */
void handle_irq(arch_registers_state_t* saved_context, uintptr_t fault_pc)
    __attribute__((noreturn));

#endif // !defined(__ASSEMBLER__)

#endif // __KERNEL_ARMV8_EXCEPTIONS_H__
