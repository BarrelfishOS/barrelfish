/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

struct cast{
    uint8_t casttype;   /// Indicate if uni or multicast
    uint8_t phasetype;  /// Indicate single or two phase
    uint8_t iter;       /// Which iteration for two phase
    int count;          /// Number of monitors contacted
    uint64_t cores_bitmask;
    uint64_t parent_id;
    struct monitor_binding *dom_closure;
    struct intermon_binding * mon_closure;
};

static inline struct cast*
cast_alloc(uint8_t casttype, uint8_t phasetype, uint64_t parent_id,
           uint64_t cores_bitmask,
           struct monitor_binding *dom_closure,
           struct intermon_binding *mon_closure)
{
    struct cast *point = malloc(sizeof(struct cast));
    assert(point != NULL);

    point->casttype  = casttype;
    point->phasetype = phasetype;
    point->iter  = 0;
    point->count = 0;
    point->cores_bitmask = cores_bitmask;
    point->parent_id = parent_id;
    point->dom_closure = dom_closure;
    point->mon_closure = mon_closure;

    return point;
}
