/**
 * \file
 * \brief Capability invocations specific to the monitors
 */

/*
 * Copyright (c) 2007-2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_INVOCATIONS_ARCH_H
#define MONITOR_INVOCATIONS_ARCH_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish/caddr.h>
#include <barrelfish/invocations_arch.h>

static inline errval_t
invoke_monitor_create_cap(uint64_t *raw, capaddr_t caddr, int bits, capaddr_t slot, coreid_t owner)
{
    assert(sizeof(struct capability) <= 3*sizeof(uint64_t));
    return cap_invoke8(cap_kernel, KernelCmd_Create_cap,
                       raw[0], raw[1], raw[2],
                       caddr, bits, slot, owner).error;
}

static inline errval_t
invoke_monitor_remote_cap_retype(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, gensize_t offset, enum objtype newtype,
                                 gensize_t objsize, size_t count, capaddr_t to,
                                 capaddr_t slot, int bits) {
    assert(rootcap_addr <= 0xFFFFFFFF);
    assert(rootcap_vbits <= 32);
    return cap_invoke10(cap_kernel, KernelCmd_Retype,
                        ((uint64_t)rootcap_addr | ((uint64_t)rootcap_vbits << 32)),
                        src, offset, newtype, objsize, count, to, slot,
                        bits).error;
}

static inline errval_t
invoke_monitor_copy_existing(uint64_t *raw, capaddr_t cn_addr, int cn_bits, cslot_t slot)
{
    assert(sizeof(struct capability) <= 3*sizeof(uint64_t));
    return cap_invoke7(cap_kernel, KernelCmd_Copy_existing,
                       raw[0], raw[1], raw[2],
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
invoke_monitor_revoke_mark_relations(uint64_t *raw_base)
{
    assert(sizeof(struct capability) <= 3*sizeof(uint64_t));
    return cap_invoke4(cap_kernel, KernelCmd_Revoke_mark_relations,
                       raw_base[0], raw_base[1], raw_base[2]).error;
}

static inline errval_t
invoke_monitor_has_descendants(uint64_t *raw, bool *res)
{
    assert(sizeof(struct capability) <= 3*sizeof(uint64_t));
    struct sysret sysret;
    sysret = cap_invoke4(cap_kernel, KernelCmd_Has_descendants,
                         raw[0], raw[1], raw[2]);
    if (err_is_ok(sysret.error)) {
        *res = sysret.value;
    }
    return sysret.error;
}

#endif
