/**
 * \file
 * \brief Not sure where to put these definitions
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

#ifndef ARCH_AARCH64_BARRELFISH_KPI_UNKNOWN_H
#define ARCH_AARCH64_BARRELFISH_KPI_UNKNOWN_H

#ifndef IN_KERNEL

// XXX: this code shouldn't be in the KPI, and it should be living behind a clean portability layer!
// required for lib/lwip/src/barrelfish/idc_barrelfish.c

#include <assert.h>
#include <stdint.h>

static inline void mfence(void)
{
    assert(!"mfence() NYI for ARM");
}

static inline void cache_flush_range(void *base, size_t len)
{
    assert(!"cache_flush_range() NYI for ARM");
}

#endif

#endif // ARCH_ARM_BARRELFISH_KPI_UNKNOWN_H
