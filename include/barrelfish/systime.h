/**
 * \file
 * \brief System time
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef BARRELFISH_SYSTIME_H
#define BARRELFISH_SYSTIME_H

#include <barrelfish_kpi/types.h> /* systime_t */
#include <barrelfish_kpi/generic_arch.h>

__BEGIN_DECLS

/// Frequency of the system time ticks (systime)
extern systime_t systime_frequency;

/**
 * Get the current system time from a hardware clock
 */
static inline systime_t systime_now(void)
{
    return rdtsc();
}

/**
 * Convert nanoseconds to a system time ticks
 */
systime_t ns_to_systime(uint64_t nanoseconds);

/**
 * Convert microseconds to a system time ticks
 */
systime_t us_to_systime(uint64_t microseconds);

/**
 * Convert a system time ticks to nanoseconds
 */
uint64_t systime_to_ns(systime_t time);

/**
 * Convert a system time ticks to microseconds
 */
uint64_t systime_to_us(systime_t time);

__END_DECLS

#endif // BARRELFISH_SYSTIME_H
