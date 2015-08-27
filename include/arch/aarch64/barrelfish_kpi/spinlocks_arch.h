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

#ifndef ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H
#define ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H

#include <barrelfish_kpi/asm_inlines_arch.h>

typedef volatile uint32_t spinlock_t;

static inline void acquire_spinlock(spinlock_t *spinlock)
{
    // TODO
}

static inline void release_spinlock(spinlock_t *spinlock)
{
    // TODO
}

#endif // ARCH_ARM_BARRELFISH_KPI_SPINLOCKS_H
