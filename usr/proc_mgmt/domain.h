/*
 * \brief Domain internals for the process manager.
 *
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PROC_MGMT_DOMAIN_H
#define PROC_MGMT_DOMAIN_H

#include <barrelfish/barrelfish.h>
#include <if/proc_mgmt_defs.h>

#include "spawnd_state.h"

enum domain_status {
    DOMAIN_STATUS_NIL,
    DOMAIN_STATUS_RUNNING,
    DOMAIN_STATUS_STOP_PEND,
    DOMAIN_STATUS_STOPPED
    // TODO(razvan): Add the others, as per the state machine.
};

struct domain_waiter {
    struct proc_mgmt_binding *b;
    struct domain_waiter *next;
};

struct domain_spawnd_state {
    struct spawnd_state *spawnd_state;
    struct domain_spawnd_state *next;
};

struct domain_entry {
    struct capref domain_cap;              // Unique domain ID cap.
    enum domain_status status;             // Current domain state.
    struct domain_spawnd_state *spawnds;   // Spawnds running this domain.
    struct domain_waiter *waiters;         // Clients waiting after this domain.
};

errval_t domain_new(struct capref domain_cap, struct domain_entry **ret_entry);
errval_t domain_get_by_cap(struct capref domain_cap,
                           struct domain_entry **ret_entry);
void domain_run_on_spawnd(struct domain_entry *entry,
                          struct spawnd_state *spawnd);

errval_t domain_spawn(struct capref domain_cap, coreid_t core_id);
errval_t domain_can_span(struct capref domain_cap, coreid_t core_id);
errval_t domain_span(struct capref domain_cap, coreid_t core_id);
void domain_send_stop(struct domain_entry *entry);
// TODO(razvan): domain_exists, domain_remove etc.

#endif  // PROC_MGMT_DOMAIN_H