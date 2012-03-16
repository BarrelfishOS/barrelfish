/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

// XXX: prototypes and other decls that need to be moved to suitable locations
#ifndef CAPOPS_MAGIC_H
#define CAPOPS_MAGIC_H

#include <barrelfish_kpi/distcaps.h>
#include <barrelfish_kpi/capabilities.h>

/*
 * Magic NYI functions
 */

// create a copy of cap other copies exist, otherwise fail
errval_t monitor_copy_if_exists(struct capability*, struct capref*);

// delete all local copies of capref
errval_t monitor_delete_copies(struct capref);

// mark cap as in-delete
errval_t monitor_set_cap_deleted(struct capref);

// delete cap (must be owned and last copy) and perform cleanup
errval_t monitor_delete_last(struct capref);

// revoke a cap. when revoke reaches a non-trivially-deletable cap, it copies
// it into the specified null slot, for the monitor to perform a suitable
// delete
errval_t monitor_revoke(struct capref, struct capref);

// return SYS_ERR_OK if descendants exists, CAP_ERR_NOTFOUND otherwise
errval_t monitor_has_local_descendants(struct capability);

// create local caps as retype from one src cap
errval_t monitor_create_caps(enum objtype, size_t, struct capref, struct capref);

#endif
