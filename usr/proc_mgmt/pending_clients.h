/*
 * \brief Client handling internals for the process manager.
 *
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PENDING_CLIENTS_H
#define PENDING_CLIENTS_H

#include <barrelfish/barrelfish.h>
#include <if/proc_mgmt_defs.h>

#define HASH_INDEX_BUCKETS 6151

enum ClientType {
	ClientType_Spawn,
	ClientType_SpawnWithCaps,
	ClientType_Span,
	ClientType_Kill,
	ClientType_Wait
	// TODO(razvan): Others?
};

struct pending_client {
    struct proc_mgmt_binding *b;
    coreid_t core_id;
    enum ClientType type;
};

errval_t pending_clients_add(struct capref domain_cap,
                             struct proc_mgmt_binding *b, enum ClientType type,
                             coreid_t core_id);
errval_t pending_clients_release(struct capref domain_cap,
                                 struct pending_client *ret_cl);

#endif  // PENDING_CLIENTS_H