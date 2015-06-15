/**
 * \file
 * \brief Debug system calls, specific for x86_32, user-side
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
#define ENABLE_FEIGN_FRAME_CAP
#include <barrelfish/sys_debug.h>
#include <stdio.h>
#include <inttypes.h>

errval_t sys_debug_feign_frame_cap(struct capref slot, lpaddr_t base,
                                   uint8_t bits)
{
    uint8_t cap_bits = get_cnode_valid_bits(slot);
    capaddr_t addr = get_cnode_addr(slot);

    return syscall5(SYSCALL_DEBUG,
                    DEBUG_FEIGN_FRAME_CAP, addr, base, bits | (cap_bits << 8) | (slot.slot << 16)).error;
}
