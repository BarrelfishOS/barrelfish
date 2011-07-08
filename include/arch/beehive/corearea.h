/**
 * \file
 * \brief Per-core debug stub save area
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_COREAREA_H
#define ARCH_BEEHIVE_COREAREA_H

#include <barrelfish/static_assert.h>

struct corearea {
    uint32_t regs[32];
    uint32_t rqarea[64];
    uint32_t flags0;
    uint32_t pad0[7];
    /* This next cache line is invalidated and read by the slave and
     * only ever written and flushed by the master */
    uint32_t master_stops;
    uint32_t master_ticks;
    uint32_t pad1[6];
    /* This next cache line is invalidated and read by the master and
     * only ever written and flushed by the slave */
    uint32_t slave_stops;
    uint32_t slave_ticks;
    uint32_t pad2[6];
    /* This cache line is used for communicating between the debug
     * stub and the kernel */
    uint32_t kernel_begins;  // code address
    uint32_t kernel_ends;    // code address
    uint32_t kernel_ticker;  // code address
    uint32_t kernel_pending; // != 0 implies software interrupt pending
    uint32_t syspad[3];
    uint32_t syscall;        // code address
};

STATIC_ASSERT_SIZEOF(struct corearea, 128 * 4);
// Flags areas must be each in their own cache line
STATIC_ASSERT((sa_offsetof(struct corearea, flags0) & 31) == 0, "alignment");
STATIC_ASSERT((sa_offsetof(struct corearea, master_stops) & 31) == 0, "alignment");
STATIC_ASSERT((sa_offsetof(struct corearea, slave_stops) & 31) == 0, "alignment");
STATIC_ASSERT((sa_offsetof(struct corearea, kernel_begins) & 31) == 0, "alignment");

#define COREAREA_FOR_CORE(_core) \
	((struct corearea *)(0x4000 + ((_core) << 9)))

static inline struct corearea *my_corearea(void)
{
    struct corearea *result;
    __asm("j7 1; ld %[result], link"
	  : [result]"=r" (result) :: "link", "cc");
    return result;
}

#endif // ARCH_BEEHIVE_COREAREA_H
