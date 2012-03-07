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

/*
 * Error codes
 */

#define CAP_ERR_NOTFOUND  696
#define CAP_ERR_FOREIGN   697
#define CAP_ERR_BUSY      698
#define CAP_ERR_LASTLOCAL 699

/*
 * Magic NYI functions
 */

typedef uint8_t capstate_t;

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

// get the state of the given cap
errval_t cap_get_state(struct capref, capstate_t*);

// cap state queries
bool cap_state_is_valid(capstate_t);
bool cap_state_is_owner(capstate_t);
bool cap_state_is_busy(capstate_t);

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
errval_t monitor_create_caps(enum objtype, size_t, struct capref, cslot_t, struct capref);

/*
 * NYI intermon.if functions
 */

errval_t intermon_recv_copy_result__tx(struct intermon_binding*, struct event_closure, errval_t, capaddr_t, genvaddr_t);
errval_t intermon_recv_copy__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, genvaddr_t);
errval_t intermon_request_copy__tx(struct intermon_binding*, struct event_closure, coreid_t, intermon_caprep_t, genvaddr_t);

errval_t intermon_move_request__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, genvaddr_t);
errval_t intermon_move_result__tx(struct intermon_binding*, struct event_closure, errval_t, genvaddr_t);

errval_t intermon_update_owner__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, genvaddr_t);
errval_t intermon_owner_updated__tx(struct intermon_binding*, struct event_closure, genvaddr_t);

errval_t intermon_find_core__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, genvaddr_t);
errval_t intermon_find_core_result__tx(struct intermon_binding*, struct event_closure, errval_t, genvaddr_t);

errval_t intermon_delete_remote__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, genvaddr_t st);
errval_t intermon_delete_remote_result__tx(struct intermon_binding*, struct event_closure, errval_t, genvaddr_t st);

errval_t intermon_request_revoke__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, genvaddr_t);
errval_t intermon_revoke_result__tx(struct intermon_binding*, struct event_closure, errval_t, genvaddr_t);

errval_t intermon_find_descendants__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, genvaddr_t);
errval_t intermon_find_descendants_result__tx(struct intermon_binding*, struct event_closure, errval_t, genvaddr_t);

errval_t intermon_request_retype__tx(struct intermon_binding*, struct event_closure, intermon_caprep_t, int, size_t, genvaddr_t);
errval_t intermon_retype_response__tx(struct intermon_binding*, struct event_closure, errval_t, genvaddr_t);

#endif
