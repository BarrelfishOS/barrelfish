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
#include <if/spawn_defs.h>

#include "domain.h"

#define HASH_INDEX_BUCKETS 6151

enum ClientType {
	ClientType_Spawn,
	ClientType_SpawnWithCaps,
	ClientType_Span,
	ClientType_Kill,
	ClientType_Exit,
	ClientType_Cleanup
};

struct pending_spawn {
    struct domain_cap_node *cap_node;

	struct spawn_binding *b;
	coreid_t core_id;
	
	const char *path;
	
	const char *argvbuf;
	size_t argvbytes;
	const char *envbuf;
	size_t envbytes;
	
	struct capref inheritcn_cap;
	struct capref argcn_cap;
	
	uint8_t flags;
};

struct pending_span {
	struct capref domain_cap;
    struct domain_entry *entry;

	struct spawn_binding *b;

	coreid_t core_id;
	struct capref vroot;
	struct capref dispframe;
};

struct pending_kill_cleanup {
	struct capref domain_cap;
    struct domain_entry *entry;
	struct spawn_binding *b;
};

struct pending_client {
	struct proc_mgmt_binding *b;
	enum ClientType type;
	void *st;
};

#endif  // PENDING_CLIENTS_H