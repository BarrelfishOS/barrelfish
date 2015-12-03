/*
 * Copyright (c) 2007-2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaestr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef __SPINLOCK_H
#define __SPINLOCK_H

/* Need to include this for errval_t */
#include <errors/errno.h>

#define PRINTF_LOCK 0

extern errval_t spinlock_init(void);
extern errval_t spinlock_early_init(void);

/* XXX - this is unimplemented on ARMv8 */
static inline void spinlock_acquire(int locknumber) {}
static inline void spinlock_release(int locknumber) {}

#endif //__SPINLOCK_H
