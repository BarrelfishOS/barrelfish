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

// get owner core of given cap. used by routing layer only
errval_t cap_get_owner(struct capref, coreid_t*);

// get owner core of given cap. used by routing layer only
errval_t cap_set_owner(struct capref, coreid_t);

// create a cap from the given cap data owned by a given core
// may fail if given owner does not match owner of existing copies
errval_t cap_create_on(struct capability*, coreid_t, struct capref*);

// create a copy of cap other copies exist, otherwise fail
errval_t copy_if_exists(struct capability*, struct capref*);

// delete all local copies of capref
errval_t cap_delete_copies(struct capref);

// set cap state to busy
errval_t cap_set_busy(struct capref);

// set cap state to ready
errval_t cap_set_ready(struct capref);

errval_t cap_set_deleted(struct capref);

// return true if cap type permits moving ownership
bool cap_is_moveable(struct capability*);

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

// set cap to locked if unlocked, else return an error
errval_t monitor_lock_cap(struct capref cap);

#endif
