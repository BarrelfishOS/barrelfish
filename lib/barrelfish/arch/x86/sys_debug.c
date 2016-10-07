/**
 * \file
 * \brief Debug system calls for all x86 architectures, user-side
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/sys_debug.h>
#include <barrelfish/sys_debug.h>
#include <stdio.h>
#include <inttypes.h>

errval_t sys_debug_get_tsc_per_ms(uint64_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_GET_TSC_PER_MS);
    *ret = sr.value;
    return sr.error;
}

errval_t sys_debug_create_irq_src_cap(struct capref cap, uint64_t start, uint64_t end)
{
    // XXX: check this
    uint8_t dcn_level = get_cnode_level(cap);
    capaddr_t dcn_addr = get_cnode_addr(cap);

    struct sysret sr = syscall7(SYSCALL_DEBUG, DEBUG_CREATE_IRQ_SRC_CAP, dcn_level, dcn_addr,
        cap.slot, start, end);
    return sr.error;
}

errval_t sys_debug_get_apic_id(uint8_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_GET_APIC_ID);
    *ret = sr.value;
    return sr.error;
}

errval_t sys_debug_get_apic_timer(uint32_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_GET_APIC_TIMER);
    *ret = sr.value;
    return sr.error;
}

errval_t sys_debug_get_apic_ticks_per_sec(uint32_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_GET_APIC_TICKS_PER_SEC);
    *ret = sr.value;
    return sr.error;
}
