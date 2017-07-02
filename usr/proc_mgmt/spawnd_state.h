/*
 * \brief Spawnd state internals for the process manager.
 *
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SPAWND_STATE_H
#define SPAWND_STATE_H

#include <stdbool.h>

#include <if/spawn_defs.h>
#include <barrelfish/barrelfish.h>

struct spawnd_state {
    coreid_t core_id;
    struct spawn_binding *b;
};

errval_t spawnd_state_alloc(coreid_t core_id, struct spawn_binding *b);
void spawnd_state_free(coreid_t core_id);
bool spawnd_state_exists(coreid_t core_id);
struct spawnd_state *spawnd_state_get(coreid_t core_id);

#endif  // SPAWND_STATE
