/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_INVOCATIONS_H
#define MONITOR_INVOCATIONS_H

#include <stdbool.h>
#include <barrelfish/caddr.h>
#include <barrelfish/types.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/distcaps.h>
#include <domcap.h>

struct capability;

bool monitor_can_send_cap(struct capability *cap);
errval_t monitor_cap_identify(struct capref cap, struct capability *out);
errval_t monitor_domains_cap_identify(struct capref croot, capaddr_t cap,
                                      int vbits, struct capability *out);
errval_t monitor_domcap_remote_relations(struct capref croot, capaddr_t cptr,
                                         int bits, uint8_t relations, uint8_t
                                         mask, uint8_t *ret_relations);
errval_t monitor_remote_relations(struct capref cap, uint8_t relations, uint8_t
                                  mask, uint8_t *ret_relations);
errval_t monitor_cap_has_relations(struct capref cap, uint8_t mask,
                                   uint8_t *res);
errval_t monitor_cap_create(struct capref dest, struct capability *cap,
                            coreid_t owner);
errval_t monitor_identify_cnode_get_cap(struct capability *cnode_raw, 
                                        capaddr_t slot, struct capability *ret);
errval_t monitor_nullify_cap(struct capref cap);
errval_t monitor_retype_remote_cap(struct capref croot, 
                                   capaddr_t src, enum objtype newtype, 
                                   int objbits, capaddr_t to, capaddr_t slot, 
                                   int bits);
errval_t monitor_create_caps(struct capref croot, enum objtype newtype,
                             int objbits, capaddr_t src, int src_bits,
                             capaddr_t dest_cn, int dest_bits,
                             cslot_t dest_slot);
errval_t monitor_copy_if_exists(struct capability* cap, struct capref dest);
errval_t monitor_delete_remote_cap(struct capref croot, capaddr_t src, int bits);
errval_t monitor_revoke_remote_cap(struct capref croot, capaddr_t src, int bits);
errval_t monitor_get_cap_owner(struct capref croot, capaddr_t cptr, int bits, coreid_t *ret_owner);
errval_t monitor_set_cap_owner(struct capref croot, capaddr_t cptr, int bits, coreid_t owner);
errval_t monitor_lock_cap(struct capref croot, capaddr_t cptr, int bits);
errval_t monitor_unlock_cap(struct capref croot, capaddr_t cptr, int bits);
errval_t monitor_has_descendants(struct capability *cap, bool *res);

static inline errval_t
monitor_get_domcap_owner(struct domcapref cap, coreid_t *ret_owner)
{

    return monitor_get_cap_owner(cap.croot, cap.cptr << (CPTR_BITS - cap.bits),
                                 cap.bits, ret_owner);
}

static inline errval_t
monitor_set_domcap_owner(struct domcapref cap, coreid_t owner)
{
    return monitor_set_cap_owner(cap.croot, cap.cptr << (CPTR_BITS - cap.bits),
                                 cap.bits, owner);
}

/*
 * Delete- and revoke-related operations
 */

errval_t monitor_delete_last(struct capref croot, capaddr_t cptr, int bits,
                             struct capref ret_cap);
errval_t monitor_delete_foreigns(struct capref cap);
errval_t monitor_revoke_mark_target(struct capref croot,
                                    capaddr_t cptr,
                                    int bits);
errval_t monitor_revoke_mark_relations(struct capability *cap);
errval_t monitor_delete_step(struct capref ret_cap);
errval_t monitor_clear_step(struct capref ret_cap);

#endif
