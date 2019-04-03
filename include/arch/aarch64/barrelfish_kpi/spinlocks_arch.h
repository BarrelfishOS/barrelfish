/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_KPI_SPINLOCKS_H
#define ARCH_AARCH64_BARRELFISH_KPI_SPINLOCKS_H

#include <barrelfish_kpi/asm_inlines_arch.h>

typedef volatile uint32_t spinlock_t;

// Refer to ARM Manual - Load-Acquire Exclusive, Store-Release Exclusive and barriers
static inline void acquire_spinlock(spinlock_t *spinlock)
{
    unsigned long tmp;

	__asm volatile(
			"	sevl\n"
			"   prfm pstl1keep, %1\n"
			"1:	wfe\n"
			"	ldaxr	%w0, %1\n"
			"	cbnz	%w0, 1b\n"
			"	stxr	%w0, %w2, %1\n"
			"	cbnz	%w0, 1b\n"
			: "=&r" (tmp), "+Q" (*spinlock)
			: "r" (1)
			: "memory");

    dmb();
}

static inline void release_spinlock(spinlock_t *spinlock)
{
    dmb();

	__asm volatile(
			"	stlr	%w1, %0\n"
			: "=Q" (*spinlock) : "r" (0) : "memory");
}

#endif // ARCH_AARCH64_BARRELFISH_KPI_SPINLOCKS_H
