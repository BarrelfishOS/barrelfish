/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_KPI_SPINLOCKS_H
#define ARCH_BEEHIVE_BARRELFISH_KPI_SPINLOCKS_H

// required for kernel printf -- really!
// and it has to be here which is strange
// spinlock_t must be an integer because lots of places initialise it with = 0
typedef int spinlock_t;

static inline void acquire_spinlock(spinlock_t *lock)
{
    assert(*lock == 0);
    *lock = 1;
}

static inline void release_spinlock(spinlock_t *lock)
{
    assert(*lock == 1);
    *lock = 0;
}

#endif // ARCH_BEEHIVE_BARRELFISH_KPI_SPINLOCKS_H
