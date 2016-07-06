/**
 * \file
 * \brief A struct for all shared data between the kernels
 */

/*
 * Copyright (c) 2008,2010,2016 ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef KERNEL_ARCH_ARM_GLOBAL_H
#define KERNEL_ARCH_ARM_GLOBAL_H

#include <barrelfish_kpi/spinlocks_arch.h>

/**
 * \brief State shared between CPU drivers
 * By design, this should be empty. A spinlock for kernel debug printing is
 * our only concession to shared state.
 */
struct global {
    /// Shared locks between the kernels
    struct {
        spinlock_t print;       ///< Lock for printing
    } locks;
};

extern struct global *global;

#endif
