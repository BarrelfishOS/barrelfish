/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PS_H
#define PS_H

#include <stdbool.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/init.h>

#define MAX_DOMAINS     256

enum ps_status {
    PS_STATUS_RUNNING,
    PS_STATUS_ZOMBIE
};

struct ps_waiter {
    struct ps_waiter *next;
    struct spawn_binding *binding;
};

struct ps_entry {
    char *argv[MAX_CMDLINE_ARGS];
    char *argbuf;
    size_t argbytes;

    domainid_t domain_id;

    struct capref domain_cap;
    uint64_t domain_cap_hash;
    
    struct capref rootcn_cap;
    struct capref dispframe;
    struct capref dcb;
    struct cnoderef rootcn;
    uint8_t exitcode;
    enum ps_status status;
    struct ps_waiter *waiters;
};

errval_t ps_allocate(struct ps_entry *entry, domainid_t *domainid);
void ps_remove(domainid_t domain_id);
bool ps_exists(domainid_t domain_id);
struct ps_entry *ps_get(domainid_t domain_id);

errval_t ps_hash_domain(struct ps_entry *entry, struct capref domain_cap);
errval_t ps_get_domain(struct capref domain_cap, struct ps_entry **ret_entry,
                       uint64_t *ret_hash_key);
errval_t ps_release_domain(struct capref domain_cap,
                           struct ps_entry **ret_entry);

#endif
