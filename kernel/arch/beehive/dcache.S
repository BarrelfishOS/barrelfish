/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <regconvention.h>

#if 0 /* inline in dcache.h */
	.code
	.globl _bee_dcache_flush_all
_bee_dcache_flush_all:
	ld	t1,link
	aqw_long_ld vb,0x8001fc03 ROL 2
	j	t1
#endif

	.globl _bee_dcache_empty_all
_bee_dcache_empty_all:
	ld	t1,link
	/* must avoid context switch between flush and invalidate */
	ld	wq, zero
	aqw_long_ld vb,0x8001fc03 ROL 2
	aqw_long_ld vb,0x8003fc03 ROL 2
	aqw_sub	vb, sp, 4
	j	t1

#ifndef IN_KERNEL
	.globl _bee_dcache_flush_lines
_bee_dcache_flush_lines:
	lsl	a2, a2, 7
	add_lsl	a1, a2, a1, 5
	aqw_add a1, a1, 0x0E
	j	link

	.globl _bee_dcache_empty_lines
_bee_dcache_empty_lines:
	lsl	a2, a2, 7
	add_lsl	a1, a2, a1, 5
	/* must avoid context switch between flush and invalidate */
	ld	wq, zero
	aqw_add a1, a1, 0x0E
	lsl	t1, 1, 19 /* invalidate ends up in AQ[17] */
	aqw_add	t1, a1, t1
	aqw_sub	vb, sp, 4
	j	link
#endif
