/**
 * \file
 * \brief Debug system calls, user-side
 */

/*
 * Copyright (c) 2007-2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/sys_debug.h>
#include <barrelfish/sys_debug.h>
#include <stdio.h>
#include <inttypes.h>

errval_t sys_nop(void)
{
    return syscall1(SYSCALL_NOP).error;
}

errval_t sys_reboot(void)
{
    return syscall1(SYSCALL_REBOOT).error;
}

errval_t sys_debug_context_counter_reset(void)
{
    return syscall2(SYSCALL_DEBUG, DEBUG_CONTEXT_COUNTER_RESET).error;
}

errval_t sys_debug_context_counter_read(uint64_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_CONTEXT_COUNTER_READ);
    *ret = sr.value;
    return sr.error;
}

errval_t sys_debug_print_context_counter(void)
{
    uint64_t val;
    errval_t err = sys_debug_context_counter_read(&val);
    if (err_is_ok(err)) {
        printf("core %d: csc = %" PRIu64 "\n", disp_get_core_id(), val);
    }
    return err;
}

errval_t sys_debug_timeslice_counter_read(uint64_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_TIMESLICE_COUNTER_READ);
    *ret = sr.value;
    return sr.error;
}

errval_t sys_debug_print_timeslice(void)
{
    uint64_t val;
    errval_t err = sys_debug_timeslice_counter_read(&val);
    if (err_is_ok(err)) {
        printf("core %d: kernel_now = %" PRIu64 "\n", disp_get_core_id(), val);
    }
    return err;
}

errval_t sys_debug_flush_cache(void)
{
    return syscall2(SYSCALL_DEBUG, DEBUG_FLUSH_CACHE).error;
}

errval_t sys_debug_send_ipi(uint8_t destination, uint8_t shorthand, uint8_t vector)
{
    return syscall5(SYSCALL_DEBUG,
                   DEBUG_SEND_IPI, destination, shorthand, vector).error;
}

errval_t sys_debug_set_breakpoint(uintptr_t addr, uint8_t mode, uint8_t length)
{
    return syscall5(SYSCALL_DEBUG,
                    DEBUG_SET_BREAKPOINT, addr, mode, length).error;
}

errval_t sys_debug_cap_trace_ctrl(bool enable, genpaddr_t start, gensize_t size)
{
    return syscall5(SYSCALL_DEBUG,
                    DEBUG_TRACE_PMEM_CTRL, enable, start, size).error;
}

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
