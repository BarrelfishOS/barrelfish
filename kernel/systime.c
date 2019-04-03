/**
 * \file
 * \brief System time, constants and convertors
 *
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <systime.h>

/// Number of system ticks per one millisecond
systime_t systime_frequency = 1;


/// Convert nanoseconds to system ticks
systime_t ns_to_systime(uint64_t nanoseconds)
{
    uint64_t q, r;

    q = nanoseconds / 1000000000;
    r = nanoseconds % 1000000000;

    // Adding half a tick to round properly
    return q * systime_frequency + (r * systime_frequency + 500000000) / 1000000000;
}

/// Convert system ticks to nanoseconds
uint64_t systime_to_ns(systime_t time)
{
    systime_t q, r;

    q = time / systime_frequency;
    r = time % systime_frequency;

    // Adding half a nanosecond to round properly
    return q * 1000000000 + (r * 1000000000 + systime_frequency / 2) / systime_frequency;
}
