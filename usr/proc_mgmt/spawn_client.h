/**
 * \file
 * \brief Spawn client for the process management service.
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PROC_MGMT_SPAWN_CLIENT_H
#define PROC_MGMT_SPAWN_CLIENT_H

#include <barrelfish/barrelfish.h>

errval_t spawn_with_caps(coreid_t core_id, const char *path,
	                     const char *argvbuf, size_t argvbytes,
	                     const char *envbuf, size_t envbytes,
	                     struct capref inheritcn_cap, struct capref argcn_cap,
	                     uint8_t flags, struct capref *ret_domain_cap);
errval_t spawn(coreid_t core_id, const char *path, const char *argvbuf,
	           size_t argvbytes, const char *envbuf, size_t envbytes,
	           uint8_t flags, struct capref *ret_domain_cap);
errval_t span(struct capref domain_cap, coreid_t core_id, struct capref vroot,
	          struct capref dispframe);

#endif  // PROC_MGMT_SPAWN_CLIENT_H