/*
 * Copyright (c) 2007-2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/dispatch.h>

#include <barrelfish/syscalls.h>
#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/syscalls.h>

STATIC_ASSERT_SIZEOF(struct sysret, 2 * sizeof(uintptr_t));
STATIC_ASSERT_OFFSETOF(struct sysret, error, 0 * sizeof(uintptr_t));
STATIC_ASSERT_OFFSETOF(struct sysret, value, 1 * sizeof(uintptr_t));
STATIC_ASSERT(SYSCALL_REG == 0, "Bad register for system call argument.");

struct sysret
syscall(uint64_t num, uint64_t arg1, uint64_t arg2, uint64_t arg3,
        uint64_t arg4, uint64_t arg5, uint64_t arg6, uint64_t arg7,
        uint64_t arg8, uint64_t arg9, uint64_t arg10, uint64_t arg11)
{
    register uint64_t ret1 __asm("x0")  = num;
    register uint64_t ret2 __asm("x1")  = arg1;
    register uint64_t a2   __asm("x2")  = arg2;
    register uint64_t a3   __asm("x3")  = arg3;
    register uint64_t a4   __asm("x4")  = arg4;
    register uint64_t a5   __asm("x5")  = arg5;
    register uint64_t a6   __asm("x6")  = arg6;
    register uint64_t a7   __asm("x7")  = arg7;
    register uint64_t a8   __asm("x8")  = arg8;
    register uint64_t a9   __asm("x9")  = arg9;
    register uint64_t a10  __asm("x10") = arg10;
    register uint64_t a11  __asm("x11") = arg11;

    __asm volatile (
        "svc #0                         \n\t"
        :  "=r" (ret1), "=r" (ret2)
        : "r" (ret1), "r" (ret2), "r" (a2), "r" (a3), "r" (a4), "r" (a5), \
          "r" (a6), "r" (a7), "r" (a8), "r" (a9), "r" (a10), "r" (a11)
    );

    return (struct sysret){/*error*/ ret1, /*value*/ ret2};
}

//
// System call wrappers
//
