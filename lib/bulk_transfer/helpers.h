/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_TRANSFER_HELPERS_H
#define BULK_TRANSFER_HELPERS_H

#include <bulk_transfer/bulk_transfer.h>

static inline enum bulk_channel_role bulk_role_other(
        enum bulk_channel_role role)
{
    if (role == BULK_ROLE_MASTER) {
        return BULK_ROLE_SLAVE;
    } else if (role == BULK_ROLE_SLAVE){
        return BULK_ROLE_MASTER;
    } else {
        /* XXX: What do we do with that? */
        return BULK_ROLE_GENERIC;
    }
}

static inline enum bulk_channel_direction bulk_direction_other(
        enum bulk_channel_direction direction)
{
    if (direction == BULK_DIRECTION_TX) {
        return BULK_DIRECTION_RX;
    } else {
        return BULK_DIRECTION_TX;
    }
}

// Can we trust auto-assigned values to enums? better make this explicit here
static inline int bulk_trust_value(enum bulk_trust_level t)
{
    switch(t) {
        case BULK_TRUST_NONE: return 0;
        case BULK_TRUST_HALF: return 1;
        case BULK_TRUST_FULL: return 2;
        default: assert(!"Badness");
                 // BULK_TRUST_UNINITIALIZED and other surprises
    }
    return -1;
}

/**
 * \brief Compare trust levels.
 *
 * @return: -1 lhs lower trust than rhs
 *           0 trust levels match
 *          +1 lhs higher trust than rhs
 */
static inline int bulk_trust_compare(enum bulk_trust_level lhs,
                                     enum bulk_trust_level rhs)
{
    int l_n = bulk_trust_value(lhs);
    int r_n = bulk_trust_value(rhs);

    if (l_n < r_n)  return -1;
    if (l_n == r_n) return  0;
                    return  1;
}

#endif // BULK_TRANSFER_HELPERS_H
