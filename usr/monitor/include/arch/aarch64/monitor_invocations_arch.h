/**
 * \file
 * \brief Capability invocations specific to the monitors
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_INVOCATIONS_ARCH_H
#define MONITOR_INVOCATIONS_ARCH_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish/caddr.h>
#include <barrelfish/invocations_arch.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/syscalls.h>
#include "monitor_debug.h"

static inline errval_t
invoke_monitor_create_cap(uint64_t *raw, capaddr_t caddr, int level, capaddr_t slot, coreid_t owner)
{
    DEBUG_INVOCATION("%s: called from %p\n", __FUNCTION__, __builtin_return_address(0));
    return cap_invoke6(cap_kernel, KernelCmd_Create_cap, caddr, level, slot,
                       owner, (uintptr_t)raw).error;
}

STATIC_ASSERT(ObjType_Num < 0xFFFF, "retype invocation argument packing does not truncate enum objtype");
static inline errval_t
invoke_monitor_remote_cap_retype(capaddr_t src_root, capaddr_t src, gensize_t offset,
                                 enum objtype newtype, gensize_t objsize, size_t count,
                                 capaddr_t to_cspace, capaddr_t to, capaddr_t slot,
                                 int level)
{
    DEBUG_INVOCATION("%s: called from %p\n", __FUNCTION__, __builtin_return_address(0));
    assert(newtype < ObjType_Num);
    assert(level <= 0xFF);
    assert(slot <= 0xFFFF);
    return cap_invoke10(cap_kernel, KernelCmd_Retype,
                        src_root, src, offset, ((uint32_t)level << 16) | newtype,
                        objsize, count, to_cspace, to, slot).error;
}

static inline errval_t
invoke_monitor_revoke_mark_relations(uint64_t *raw_base)
{
    DEBUG_INVOCATION("%s: called from %p\n", __FUNCTION__, __builtin_return_address(0));
    // XXX: this is assumed in client code of this function!
    assert(sizeof(struct capability) / sizeof(uint64_t) <= 4);
    return cap_invoke2(cap_kernel, KernelCmd_Revoke_mark_relations,
                       (uintptr_t)raw_base).error;
}

static inline errval_t
invoke_monitor_has_descendants(uint64_t *raw, bool *res)
{
    DEBUG_INVOCATION("%s: called from %p\n", __FUNCTION__, __builtin_return_address(0));
    // XXX: this is assumed in client code of this function!
    assert(sizeof(struct capability) / sizeof(uint64_t) <= 4);

    struct sysret sysret;
    sysret = cap_invoke2(cap_kernel, KernelCmd_Has_descendants,
                         (uintptr_t)raw);
    if (err_is_ok(sysret.error)) {
        *res = sysret.value;
    }
    return sysret.error;
}

static inline errval_t
invoke_monitor_is_retypeable(uint64_t *raw, gensize_t offset,
                             gensize_t objsize, size_t count)
{
    assert(sizeof(struct capability) <= 4*sizeof(uint64_t));
    return cap_invoke5(cap_kernel, KernelCmd_Is_retypeable,
                       (uintptr_t)raw, offset, objsize, count).error;
}

/**
 * \brief Set up tracing in the kernel
 *
 */
static inline errval_t
invoke_trace_setup(struct capref cap)
{
    DEBUG_INVOCATION("%s: called from %p\n", __FUNCTION__, __builtin_return_address(0));
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t
invoke_domain_id(struct capref cap, uint64_t domain_id)
{
    DEBUG_INVOCATION("%s: called from %p\n", __FUNCTION__, __builtin_return_address(0));
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t invoke_monitor_sync_timer(uint64_t synctime)
{
    DEBUG_INVOCATION("%s: called from %p\n", __FUNCTION__, __builtin_return_address(0));
    // XXX: could do cap_invoke1() here, as we have 64 bit GP registers
    return cap_invoke2(cap_kernel, synctime >> 32, synctime & 0xffffffff).error;
}

static inline errval_t
invoke_monitor_copy_existing(uint64_t *raw, capaddr_t croot_addr, capaddr_t cn_addr,
                             int cn_level, cslot_t slot)
{
    DEBUG_INVOCATION("%s: called from %p\n", __FUNCTION__, __builtin_return_address(0));
    // XXX: this is assumed in client code of this function!
    assert(sizeof(struct capability) <= 4*sizeof(uint64_t));

    return cap_invoke6(cap_kernel, KernelCmd_Copy_existing,
                       croot_addr, cn_addr, cn_level, slot, (uintptr_t)raw).error;
}

#endif
