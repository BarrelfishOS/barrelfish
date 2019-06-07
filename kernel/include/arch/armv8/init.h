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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INIT_H
#define INIT_H

#ifndef __ASSEMBLER__

struct armv8_core_data;

/*
 * \brief Main entry point to C from boot[.S|.c]
 */
void arch_init(struct armv8_core_data *pointer) __attribute__((noreturn));

/*
 * Checking code for, e.g., platform-specific callouts
 */
extern bool cpu_is_bsp(void);

/*
 * Second-stage startup
 */
void  arm_kernel_startup(void) __attribute__((noreturn));

#endif // __ASSEMBLER__

#endif // INIT_H
