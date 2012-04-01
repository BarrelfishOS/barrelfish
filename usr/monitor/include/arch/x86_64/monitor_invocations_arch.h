/**
 * \file
 * \brief Capability invocations specific to the monitors
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_INVOCATIONS_ARCH_H
#define MONITOR_INVOCATIONS_ARCH_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish/caddr.h>
#include <barrelfish/invocations_arch.h>

/**
 * \brief Spawn a new core.
 *
 * \param core_id    APIC ID of the core to try booting
 * \param cpu_type   Type of core to boot
 * \param entry      Kernel entry point in physical memory
 */
static inline errval_t
invoke_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                          forvaddr_t entry)
{
    return cap_invoke4(cap_kernel, KernelCmd_Spawn_core, core_id, cpu_type,
                       entry).error;
}

static inline errval_t
invoke_monitor_identify_cap(capaddr_t cap, int bits, struct capability *out)
{
    return cap_invoke4(cap_kernel, KernelCmd_Identify_cap, cap, bits,
                       (uintptr_t)out).error;
}

static inline errval_t
invoke_monitor_identify_domains_cap(capaddr_t root_cap, int root_bits,
                                    capaddr_t cap, int bits, 
                                    struct capability *out)
{
    return cap_invoke6(cap_kernel, KernelCmd_Identify_domains_cap,  
                       root_cap, root_bits, cap, bits, (uintptr_t)out).error;
}


static inline errval_t
invoke_monitor_nullify_cap(capaddr_t cap, int bits)
{
    return cap_invoke3(cap_kernel, KernelCmd_Nullify_cap, cap, bits).error;
}

static inline errval_t
invoke_monitor_cap_remote(capaddr_t cap, int bits, bool is_remote,
                          bool * has_descendents)
{
    struct sysret r = cap_invoke4(cap_kernel, KernelCmd_Remote_cap, cap, bits,
                                  is_remote);
    if (err_is_ok(r.error)) {
        *has_descendents = r.value;
    }
    return r.error;
}

static inline errval_t
invoke_monitor_create_cap(uint64_t *raw, capaddr_t caddr, int bits, capaddr_t slot, coreid_t owner)
{
    assert(sizeof(struct capability) % sizeof(uint64_t) == 0);
    assert(sizeof(struct capability) / sizeof(uint64_t) == 4);
    return cap_invoke9(cap_kernel, KernelCmd_Create_cap,
                       raw[0], raw[1], raw[2], raw[3],
                       caddr, bits, slot, owner).error;
}

static inline errval_t
invoke_monitor_register(struct capref ep)
{
    return cap_invoke2(cap_kernel, KernelCmd_Register, get_cap_addr(ep)).error;
}

static inline errval_t
invoke_monitor_identify_cnode_get_cap(uint64_t *cnode_raw, capaddr_t slot,
                                      struct capability *out)
{
    return cap_invoke4(cap_kernel, KernelCmd_Iden_cnode_get_cap,
                       (uintptr_t)cnode_raw, slot, (uintptr_t)out).error;
}


static inline errval_t
invoke_monitor_remote_cap_retype(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, enum objtype newtype, 
                                 int objbits, capaddr_t to, capaddr_t slot, 
                                 int bits) {
    return cap_invoke9(cap_kernel, KernelCmd_Retype, rootcap_addr, 
                       rootcap_vbits, src, newtype, objbits, to, slot,
                       bits).error;
}

static inline errval_t
invoke_monitor_copy_existing(uint64_t *raw, capaddr_t cn_addr, int cn_bits, cslot_t slot)
{
    assert(sizeof(struct capability) == 4*sizeof(uint64_t));
    return cap_invoke8(cap_kernel, KernelCmd_Copy_existing,
                       raw[0], raw[1], raw[2], raw[3],
                       cn_addr, cn_bits, slot).error;
}

/**
 * \brief Set up tracing in the kernel
 *
 */
static inline errval_t
invoke_trace_setup(struct capref cap)
{
    return cap_invoke2(cap_kernel, KernelCmd_Setup_trace,
                       get_cap_addr(cap)).error;
}

static inline errval_t
invoke_domain_id(struct capref cap, domainid_t domain_id)
{
    return cap_invoke3(cap_kernel, KernelCmd_Domain_Id, get_cap_addr(cap),
                       domain_id).error;
}

static inline errval_t invoke_monitor_sync_timer(uint64_t synctime)
{
    return cap_invoke2(cap_kernel, KernelCmd_Sync_timer, synctime).error;
}

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

static inline errval_t
invoke_monitor_get_cap_owner(capaddr_t root, int rbits, capaddr_t cap, int cbits, coreid_t *ret_owner)
{
    assert(ret_owner);
    struct sysret sysret = cap_invoke5(cap_kernel, KernelCmd_Get_cap_owner, root, rbits, cap, cbits);
    if (err_is_ok(sysret.error)) {
        *ret_owner = sysret.value;
    }
    return sysret.error;
}

static inline errval_t
invoke_monitor_set_cap_owner(capaddr_t root, int rbits, capaddr_t cap, int cbits, coreid_t owner)
{
    return cap_invoke6(cap_kernel, KernelCmd_Set_cap_owner, root, rbits, cap, cbits, owner).error;
}

static inline errval_t
invoke_monitor_lock_cap(capaddr_t root, int rbits, capaddr_t cap, int cbits)
{
    return cap_invoke5(cap_kernel, KernelCmd_Lock_cap, root, rbits, cap, cbits).error;
}

static inline errval_t
invoke_monitor_unlock_cap(capaddr_t root, int rbits, capaddr_t cap, int cbits)
{
    return cap_invoke5(cap_kernel, KernelCmd_Unlock_cap, root, rbits, cap, cbits).error;
}

static inline errval_t
invoke_monitor_delete_last(capaddr_t root, int rbits, capaddr_t cap, int cbits,
                           capaddr_t retcn, int retcnbits, cslot_t retslot)
{
    return cap_invoke8(cap_kernel, KernelCmd_Delete_last, root, rbits, cap,
                       cbits, retcn, retcnbits, retslot).error;
}

static inline errval_t
invoke_monitor_continue_revoke(capaddr_t root, int rbits, capaddr_t cap, int cbits,
                           capaddr_t retcn, int retcnbits, cslot_t retslot)
{
    return cap_invoke8(cap_kernel, KernelCmd_Revoke_step, root, rbits, cap,
                       cbits, retcn, retcnbits, retslot).error;
}

static inline errval_t
invoke_monitor_has_descendants(uint64_t *raw, bool *res)
{
    assert(sizeof(struct capability) % sizeof(uint64_t) == 0);
    assert(sizeof(struct capability) / sizeof(uint64_t) == 4);
    struct sysret sysret;
    sysret = cap_invoke5(cap_kernel, KernelCmd_Has_Descendants,
                         raw[0], raw[1], raw[2], raw[3]);
    if (err_is_ok(sysret.error)) {
        *res = sysret.value;
    }
    return sysret.error;
}

#endif
