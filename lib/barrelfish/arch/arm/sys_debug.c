/**
 * \file
 * \brief Debug system calls, specific for arm, user-side
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

errval_t sys_debug_hardware_timer_read(uintptr_t* v)
{
    struct sysret sr
        = syscall2(SYSCALL_DEBUG, DEBUG_HARDWARE_TIMER_READ);
    *v = sr.value;
    return sr.error;
}

errval_t sys_debug_hardware_timer_hertz_read(uintptr_t* v)
{
    struct sysret sr
        = syscall2(SYSCALL_DEBUG, DEBUG_HARDWARE_TIMER_HERTZ_READ);
    *v = sr.value;
    return sr.error;
}

errval_t sys_debug_hardware_global_timer_read(uint64_t *ret)
{
    struct sysret sr;

    uint32_t l, h;

    do {
        h = syscall2(SYSCALL_DEBUG, DEBUG_HARDWARE_GLOBAL_TIMER_HIGH).value;
        l = syscall2(SYSCALL_DEBUG, DEBUG_HARDWARE_GLOBAL_TIMER_LOW).value;
        // read high again, in case it changed
        sr = syscall2(SYSCALL_DEBUG, DEBUG_HARDWARE_GLOBAL_TIMER_HIGH);
    } while(h != sr.value && err_is_ok(sr.error));

    if(err_is_ok(sr.error) && ret) {
        *ret = (((uint64_t) h) << 32) | ((uint32_t) l);
    }

    return sr.error;
}
