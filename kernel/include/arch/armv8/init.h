/**
 * \file
 * \brief ARM architecture initialization
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INIT_H
#define INIT_H

#ifndef __ASSEMBLER__

/*
 * \brief Main entry point to C from boot[.S|.c]
 */
void arch_init(uint32_t magic, void *pointer, uintptr_t stack) __attribute__((noreturn));

/*
 * Checking code for, e.g., platform-specific callouts
 */
extern bool cpu_is_bsp(void);

/*
 * Second-stage startup
 */
void  arm_kernel_startup(void *pointer) __attribute__((noreturn));

#endif // __ASSEMBLER__

#endif // INIT_H
