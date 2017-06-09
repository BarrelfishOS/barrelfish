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

#include <barrelfish/barrelfish.h>

#include "spawnd_state.h"

static struct spawnd_state *spawnds[MAX_SPAWNDS];

errval_t spawnd_state_alloc(coreid_t core_id, struct spawn_binding *b)
{
    spawnds[core_id] = (struct spawnd_state*) malloc(
            sizeof(struct spawnd_state));
    if (spawnds[core_id] == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    spawnds[core_id]->b = b;

    return SYS_ERR_OK;
}

void spawnd_state_free(coreid_t core_id)
{
    if (spawnds[core_id] != NULL) {
        free(spawnds[core_id]);
    }
}

inline bool spawnd_state_exists(coreid_t core_id)
{
    return spawnds[core_id] != NULL;
}

inline struct spawn_binding *spawnd_state_get_binding(coreid_t core_id)
{
    return spawnds[core_id]->b;
}
