/**
 * \file
 * \brief Some arch specific asm inlines
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_KPI_ASM_INLINES_H
#define ARCH_AARCH64_BARRELFISH_KPI_ASM_INLINES_H

#ifndef __ASSEMBLER__

static inline void dmb(void)
{
	__asm volatile ("dmb sy" : : : "memory");
}

static inline uint8_t is_cycle_counter_overflow(void)
{
	//NYI
	return 0;
}

static inline uint32_t get_cycle_count(void)
{
	//NYI
	return 0;
}


static inline void reset_cycle_counter(void)
{
	//NYI
}

#endif // __ASSEMBLER__

#endif // ARCH_AARCH64_BARRELFISH_KPI_ASM_INLINES_H
