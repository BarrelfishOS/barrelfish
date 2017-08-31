/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_INVOCATIONS_H
#define MONITOR_INVOCATIONS_H

#include <stdbool.h>
#include <barrelfish/caddr.h>
#include <barrelfish/types.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/distcaps.h>
#include <domcap.h>

#include <monitor_invocations_arch.h>

//{{{1 Cap identification and relation checking
static inline errval_t
invoke_monitor_remote_relations(capaddr_t root_cap, int root_level,
                                capaddr_t cap, int level,
                                uint8_t relations, uint8_t mask,
                                uint8_t *ret_remote_relations)
{
    struct sysret r = cap_invoke6(cap_kernel, KernelCmd_Remote_relations,
                                  root_cap, root_level, cap, level,
                                  ((uint16_t)relations) | (((uint16_t)mask)<<8));
    if (err_is_ok(r.error) && ret_remote_relations) {
        *ret_remote_relations = r.value;
    }
    return r.error;
}

static inline errval_t
invoke_monitor_cap_has_relations(capaddr_t caddr, uint8_t level, uint8_t mask,
                                 uint8_t *res)
{
    assert(res);
    struct sysret ret = cap_invoke4(cap_kernel, KernelCmd_Cap_has_relations,
                                    caddr, level, mask);
    if (err_is_ok(ret.error)) {
        *res = ret.value;
    }
    return ret.error;
}

static inline errval_t
invoke_monitor_identify_cap(capaddr_t cap, int level, struct capability *out)
{
    return cap_invoke4(cap_kernel, KernelCmd_Identify_cap, cap, level,
                       (uintptr_t)out).error;
}

static inline errval_t
invoke_monitor_identify_domains_cap(capaddr_t root_cap, int root_level,
                                    capaddr_t cap, int level,
                                    struct capability *out)
{
    return cap_invoke6(cap_kernel, KernelCmd_Identify_domains_cap,
                       root_cap, root_level, cap, level, (uintptr_t)out).error;
}

//{{{1 Cap zeroing
static inline errval_t
invoke_monitor_nullify_cap(capaddr_t cap, int level)
{
    return cap_invoke3(cap_kernel, KernelCmd_Nullify_cap, cap, level).error;
}

//{{{1 Cap ownership manipulation
static inline errval_t
invoke_monitor_get_cap_owner(capaddr_t root, int rlevel, capaddr_t cap, int clevel, coreid_t *ret_owner)
{
    assert(ret_owner);
    struct sysret sysret = cap_invoke5(cap_kernel, KernelCmd_Get_cap_owner, root, rlevel, cap, clevel);
    if (err_is_ok(sysret.error)) {
        *ret_owner = sysret.value;
    }
    return sysret.error;
}

static inline errval_t
invoke_monitor_set_cap_owner(capaddr_t root, int rlevel, capaddr_t cap, int clevel, coreid_t owner)
{
    return cap_invoke6(cap_kernel, KernelCmd_Set_cap_owner, root, rlevel, cap, clevel, owner).error;
}

//{{{1 Cap locking
static inline errval_t
invoke_monitor_lock_cap(capaddr_t root, int rlevel, capaddr_t cap, int clevel)
{
    return cap_invoke5(cap_kernel, KernelCmd_Lock_cap, root, rlevel, cap, clevel).error;
}

static inline errval_t
invoke_monitor_unlock_cap(capaddr_t root, int rlevel, capaddr_t cap, int clevel)
{
    return cap_invoke5(cap_kernel, KernelCmd_Unlock_cap, root, rlevel, cap, clevel).error;
}

//{{{1 Delete and revoke state machine stepping
static inline errval_t
invoke_monitor_delete_last(capaddr_t root, int rlevel, capaddr_t cap, int clevel,
                           capaddr_t retcn, int retcnlevel, cslot_t retslot)
{
    return cap_invoke8(cap_kernel, KernelCmd_Delete_last, root, rlevel, cap,
                       clevel, retcn, retcnlevel, retslot).error;
}

static inline errval_t
invoke_monitor_delete_foreigns(capaddr_t cap, int level)
{
    return cap_invoke3(cap_kernel, KernelCmd_Delete_foreigns, cap, level).error;
}

static inline errval_t
invoke_monitor_revoke_mark_target(capaddr_t root, int rlevel,
                                  capaddr_t cap, int clevel)
{
    return cap_invoke5(cap_kernel, KernelCmd_Revoke_mark_target,
                       root, rlevel, cap, clevel).error;
}

static inline errval_t
invoke_monitor_delete_step(capaddr_t retcn, int retcnlevel, cslot_t retslot)
{
    return cap_invoke4(cap_kernel, KernelCmd_Delete_step,
                       retcn, retcnlevel, retslot).error;
}

static inline errval_t
invoke_monitor_clear_step(capaddr_t retcn, int retcnlevel, cslot_t retslot)
{
    return cap_invoke4(cap_kernel, KernelCmd_Clear_step,
                       retcn, retcnlevel, retslot).error;
}

//{{{1 Register EP
static inline errval_t
invoke_monitor_register(struct capref ep)
{
    return cap_invoke2(cap_kernel, KernelCmd_Register, get_cap_addr(ep)).error;
}

//{{{1 IPI operations
static inline errval_t
invoke_monitor_ipi_register(struct capref ep, int chanid)
{
    return cap_invoke3(cap_kernel, KernelCmd_IPI_Register, get_cap_addr(ep),
                       chanid).error;
}

static inline errval_t
invoke_monitor_ipi_delete(int chanid)
{
    return cap_invoke2(cap_kernel, KernelCmd_IPI_Delete, chanid).error;
}

//{{{1 KCB operations
static inline errval_t
invoke_monitor_add_kcb(uintptr_t kcb_base)
{
    assert(kcb_base);

    return cap_invoke2(cap_kernel, KernelCmd_Add_kcb, kcb_base).error;
}

static inline errval_t
invoke_monitor_remove_kcb(uintptr_t kcb_base)
{
    assert(kcb_base);

    return cap_invoke2(cap_kernel, KernelCmd_Remove_kcb, kcb_base).error;
}

static inline errval_t
invoke_monitor_suspend_kcb_scheduler(bool suspend)
{
    return cap_invoke2(cap_kernel, KernelCmd_Suspend_kcb_sched, suspend).error;
}

//{{{1 Get information about HW
static inline errval_t
invoke_monitor_get_arch_id(uintptr_t *arch_id)
{
    assert(arch_id != NULL);

    struct sysret sysret = cap_invoke1(cap_kernel, KernelCmd_Get_arch_id);
    if (sysret.error == SYS_ERR_OK) {
        *arch_id = sysret.value;
    }
    return sysret.error;
}

static inline errval_t invoke_get_platform_info(uintptr_t pi)
{
    return cap_invoke2(cap_kernel, KernelCmd_Get_platform, pi).error;
}


struct capability;

bool monitor_can_send_cap(struct capability *cap);
errval_t monitor_cap_identify(struct capref cap, struct capability *out);
errval_t monitor_domains_cap_identify(struct capref croot, capaddr_t cap,
                                      int vlevel, struct capability *out);
errval_t monitor_domcap_remote_relations(struct capref croot, capaddr_t cptr,
                                         int level, uint8_t relations, uint8_t
                                         mask, uint8_t *ret_relations);
errval_t monitor_remote_relations(struct capref cap, uint8_t relations, uint8_t
                                  mask, uint8_t *ret_relations);
errval_t monitor_cap_has_relations(struct capref cap, uint8_t mask,
                                   uint8_t *res);
errval_t monitor_cap_create(struct capref dest, struct capability *cap,
                            coreid_t owner);
errval_t monitor_nullify_cap(struct capref cap);
errval_t monitor_retype_remote_cap(struct capref croot, capaddr_t src, gensize_t offset,
                                   enum objtype newtype, gensize_t objsize,
                                   gensize_t count, capaddr_t to,
                                   capaddr_t slot, int level);
errval_t monitor_create_caps(struct capref src_root, struct capref dest_root,
                             enum objtype newtype, gensize_t objsize,
                             size_t count, capaddr_t src,
                             int src_level, size_t offset, capaddr_t dest_cn,
                             int dest_level, cslot_t dest_slot);
errval_t monitor_copy_if_exists(struct capability* cap, struct capref dest);
errval_t monitor_delete_remote_cap(struct capref croot, capaddr_t src, int level);
errval_t monitor_revoke_remote_cap(struct capref croot, capaddr_t src, int level);
errval_t monitor_get_cap_owner(struct capref croot, capaddr_t cptr, int level, coreid_t *ret_owner);
errval_t monitor_set_cap_owner(struct capref croot, capaddr_t cptr, int level, coreid_t owner);
errval_t monitor_lock_cap(struct capref croot, capaddr_t cptr, int level);
errval_t monitor_unlock_cap(struct capref croot, capaddr_t cptr, int level);
errval_t monitor_has_descendants(struct capability *cap, bool *res);
errval_t monitor_is_retypeable(struct capability *cap, gensize_t offset,
                               gensize_t objsize, size_t count);

static inline errval_t
monitor_get_domcap_owner(struct domcapref cap, coreid_t *ret_owner)
{

    return monitor_get_cap_owner(cap.croot, cap.cptr, cap.level, ret_owner);
}

static inline errval_t
monitor_set_domcap_owner(struct domcapref cap, coreid_t owner)
{
    return monitor_set_cap_owner(cap.croot, cap.cptr, cap.level, owner);
}

/*
 * Delete- and revoke-related operations
 */

errval_t monitor_delete_last(struct capref croot, capaddr_t cptr, int level,
                             struct capref ret_cap);
errval_t monitor_delete_foreigns(struct capref cap);
errval_t monitor_revoke_mark_target(struct capref croot,
                                    capaddr_t cptr,
                                    int level);
errval_t monitor_revoke_mark_relations(struct capability *cap);
errval_t monitor_delete_step(struct capref ret_cap);
errval_t monitor_clear_step(struct capref ret_cap);

#endif
