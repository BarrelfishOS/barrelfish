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

#define EXIT_STATUS_KILLED 9

enum domain_status {
    DOMAIN_STATUS_NIL,
    DOMAIN_STATUS_RUNNING,
    DOMAIN_STATUS_STOP_PEND,
    DOMAIN_STATUS_STOPPED,
    DOMAIN_STATUS_CLEANED
};

struct domain_waiter {
    struct proc_mgmt_binding *b;
    struct domain_waiter *next;
};

struct domain_cap_node {
    struct capref domain_cap;
    uint64_t hash;

    struct domain_cap_node *next;
};

struct domain_entry {
    struct domain_cap_node *cap_node;
    enum domain_status status;  // Current domain state.

    struct spawnd_state *spawnds[MAX_COREID];  // Spawnds running this domain.
    coreid_t num_spawnds_running;
    coreid_t num_spawnds_resources;


    struct domain_waiter *waiters;  // Clients waiting after this domain.

    /* Mainly used for ps like command (XXX currently duplicated also in spawnd) */
    domainid_t domainid;
    char *argbuf;
    size_t argbytes;

    uint8_t exit_status;
};

bool domain_should_refill_caps(void);
errval_t domain_prealloc_caps(void);
struct domain_cap_node *next_cap_node(void);
errval_t domain_get_by_cap(struct capref domain_cap,
                           struct domain_entry **ret_entry);
void domain_run_on_core(struct domain_entry *entry, coreid_t core_id);

errval_t domain_new(struct domain_cap_node *cap_node, const char* argbuf, 
                    size_t argbytes, struct domain_entry **ret_entry);
errval_t domain_spawn(struct domain_cap_node *cap_node, coreid_t core_id,
                      const char* argbuf, size_t argbytes);
errval_t domain_can_span(struct capref domain_cap, coreid_t core_id);
errval_t domain_span(struct capref domain_cap, coreid_t core_id);
void domain_get_all_ids(domainid_t** domains, size_t* len);
static inline void domain_stop_pending(struct domain_entry *entry)
{
    assert(entry != NULL);
    entry->status = DOMAIN_STATUS_STOP_PEND;
}

errval_t domain_get_by_id(domainid_t,
                          struct domain_entry **ret_entry);


#endif  // PROC_MGMT_DOMAIN_H
