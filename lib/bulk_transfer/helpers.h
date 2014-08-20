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


#endif // ndef BULK_TRANSFER_HELPERS_H

