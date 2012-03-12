/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_DISTCAPS_H
#define BARRELFISH_DISTCAPS_H

#include <stdbool.h>
#include <stdint.h>

#define DISTCAP_STATE_FOREIGN (1 << 0)
#define DISTCAP_STATE_BUSY (1 << 1)

typedef uint8_t distcap_state_t;

static inline bool
distcap_is_busy(distcap_state_t state)
{
    return state & DISTCAP_STATE_BUSY;
}

static inline bool
distcap_is_foreign(distcap_state_t state)
{
    return state & DISTCAP_STATE_FOREIGN;
}

#endif
