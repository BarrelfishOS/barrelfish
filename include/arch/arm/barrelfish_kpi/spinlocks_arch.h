/**
 * \file
 * \brief Very simple ARMv7a spinlocks
 *
 * Not exactly scalable, but serviceable for infrequent use (e.g. in
 * the kernel for arbitrating the console UART).
 * 
 * We let the compiler do the heavy lifting here wrt. the memory model.
 */

/*
 * Copyright (c) 2010-2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaestr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H
#define ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H

/* Need to include this for errval_t */
#include <errors/errno.h>
#include <stdbool.h>

typedef bool spinlock_t;

/*
 * Initialize a spinlock
 */
static inline void spinlock_init(spinlock_t *l)
{
    __atomic_clear(l, __ATOMIC_RELEASE);
}

/*
 * acquire and release specific locks
 * chosen the names as a contrast to "acquire_spinlock", because the arguments differ
 * (here we want the index of the lock in the module, instead of a generic address)
 */
#define acquire_spinlock(_l) spinlock_acquire(_l)
static inline void spinlock_acquire(spinlock_t *l)
{
    while (__atomic_test_and_set(l, __ATOMIC_ACQUIRE));
}

#define release_spinlock(_l) spinlock_release(_l)
static inline void spinlock_release(spinlock_t *l)
{
    __atomic_clear(l, __ATOMIC_RELEASE);
}

#endif // ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H
