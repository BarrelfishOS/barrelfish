/**
 * \file
 * \brief Routing library internal utility functions
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ROUTING_INTERNAL_H
#define ROUTING_INTERNAL_H

/**
 * \brief Checks if #coreid is in the #mask
 */
static inline bool mask_cmp(coreid_t coreid, coremask_t mask)
{
    return mask & ((coremask_t)1 << coreid);
}

static inline struct route* get_route_struct(routeid_t id)
{
    return (struct route*)id;
}

static inline routeid_t get_route_id(struct route* route)
{
    return (routeid_t)route;
}

#endif // ROUTING_INTERNAL_H
