/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
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

//
// System call wrappers
//

void
sys_armv7_cache_clean_pou(void *start, void *end) {
    struct sysret r=
        syscall4(SYSCALL_ARMv7_CACHE_CLEAN,
                 (uint32_t)start, (uint32_t)end, 0);
    assert(err_is_ok(r.error));
}

void
sys_armv7_cache_clean_poc(void *start, void *end) {
    struct sysret r=
        syscall4(SYSCALL_ARMv7_CACHE_CLEAN,
                 (uint32_t)start, (uint32_t)end, 1);
    assert(err_is_ok(r.error));
}

void
sys_armv7_cache_invalidate(void *start, void *end) {
    struct sysret r=
        syscall3(SYSCALL_ARMv7_CACHE_INVAL,
                 (uint32_t)start, (uint32_t)end);
    assert(err_is_ok(r.error));
}
