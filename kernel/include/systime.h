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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SYSTIME_H
#define __SYSTIME_H

#include <barrelfish_kpi/types.h> /* systime_t */

/// Frequency of the system time ticks (systime)
extern systime_t systime_frequency;

/**
 * Get the current system time from a hardware clock
 */
systime_t systime_now(void);

/**
 * Convert nanoseconds to a system time ticks
 */
systime_t ns_to_systime(uint64_t nanoseconds);

/**
 * Convert a system time ticks to nanoseconds
 */
uint64_t systime_to_ns(systime_t time);

/**
 * Set a point at which a timer interrupt should occur
 * if it's in a past, trigger immediately
 */
void systime_set_timeout(systime_t absolute_timeout);

/**
 * Set a timeout at which a timer interrupt should occur, counting from now
 * if it's 0, trigger immediately
 */
void systime_set_timer(systime_t relative_timeout);

#endif // __SYSTIME_H
