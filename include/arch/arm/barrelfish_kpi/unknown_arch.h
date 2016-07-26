/**
 * \file
 * \brief Not sure where to put these definitions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_KPI_UNKNOWN_H
#define ARCH_ARM_BARRELFISH_KPI_UNKNOWN_H

#ifndef IN_KERNEL

#include <barrelfish/sys_debug.h>

// XXX: this code shouldn't be in the KPI, and it should be living behind a clean portability layer!
// required for lib/lwip/src/barrelfish/idc_barrelfish.c

#include <assert.h>

static inline void mfence(void)
{
    assert(!"mfence() NYI for ARM");
}

static inline void cache_flush_range(void *base, size_t len)
{
    assert(!"cache_flush_range() NYI for ARM");
}


static inline uint64_t rdtsc(void)
{
    /* XXX - only the lower 32 bits - the interface needs to be improved. */
    uint32_t timestamp;
    errval_t err= sys_debug_hardware_timer_read(&timestamp);
    (void)err;
    assert(err_is_ok(err));
    return timestamp;
}


#endif

#endif // ARCH_ARM_BARRELFISH_KPI_UNKNOWN_H
