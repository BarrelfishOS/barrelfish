/**
 * \file
 * \brief Prototypes for Cache Management
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_DCACHE_H
#define ARCH_BEEHIVE_DCACHE_H

/*
 * Atomically flush and then invalidate the entire cache
 */

extern void bee_dcache_empty_all(void);

/*
 * Flush (with writeback) the entire cache
 */

static inline void bee_dcache_flush_all(void)
{
    __asm volatile ("aqw_long_ld vb,0x8001fc03 ROL 2"
	  ::: "link", "cc");
}

#endif // ARCH_BEEHIVE_DCACHE_H
