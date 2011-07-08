/**
 * \file
 * \brief A struct for all shared data between the kernels
 */

/*
 * Copyright (c) 2008, 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_BEEHIVE_KPUTCHAR_H
#define KERNEL_ARCH_BEEHIVE_KPUTCHAR_H

extern void kprintf_begin(void);
extern void kprintf_end(void);
extern int kputchar(int c);

#endif // KERNEL_ARCH_BEEHIVE_KPUTCHAR_H
