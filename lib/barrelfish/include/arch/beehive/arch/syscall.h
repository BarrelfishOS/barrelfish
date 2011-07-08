/**
 * \file
 * \brief User-side system call implementation
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef LIBBARRELFISH_ARCH_SYSCALL_H
#define LIBBARRELFISH_ARCH_SYSCALL_H

// Implemented in assembler
extern struct sysret syscall1(uintptr_t a);
extern struct sysret syscall2(uintptr_t a, uintptr_t b);
extern struct sysret syscall3(uintptr_t a, uintptr_t b, uintptr_t c);
extern struct sysret syscall4(uintptr_t a, uintptr_t b, uintptr_t c, uintptr_t d);
extern struct sysret syscall5(uintptr_t a, uintptr_t b, uintptr_t c, uintptr_t d, uintptr_t e);

#endif
