/*
 * Copyright (c) 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__   1
#endif

#include <regconvention.h>

// ------------------------------------------------------------

	.code

	.globl	_find_least_clear32
	.globl	_find_least_set32
_find_least_clear32:
	xorn	a1, a1, zero
_find_least_set32:
	ld	r1, 0
	ld	void, a1
flsjoin:jz	minusone
	lsl	void, a1, 16
	jnz	.+3
	lsr	a1, a1, 16
	add	r1, r1, 16
	and	void, a1, 0xff
	jnz	.+3
	lsr	a1, a1, 8
	add	r1, r1, 8
	and	void, a1, 0x0f
	jnz	.+3
	lsr	a1, a1, 4
	add	r1, r1, 4
	and	void, a1, 0x03
	jnz	.+3
	lsr	a1, a1, 2
	add	r1, r1, 2
	and	void, a1, 0x01
	jnz	link
	add	r1, r1, 1
	j	link
minusone:
	xorn	r1, zero, zero
	j	link

	.globl	_find_least_clear64
	.globl	_find_least_set64
_find_least_clear64:
	xorn	a1, a1, zero
	xorn	a2, a2, zero
_find_least_set64:
	ld	void, a1
	jnz	_find_least_set32
	ld	r1, 32
	ld	a1, a2
	j	flsjoin
		

	.globl	_find_highest_clear32
	.globl	_find_highest_set32
_find_highest_clear32:
	xorn	a1, a1, zero
_find_highest_set32:
	ld	r1, 31
	ld	void, a1
fhsjoin:jz	minusone
	lsr	void, a1, 16
	jnz	.+3
	sub	r1, r1, 16
	lsl	a1, a1, 16
	lsr	void, a1, 24
	jnz	.+3
	sub	r1, r1, 8
	lsl	a1, a1, 8
	lsr	void, a1, 28
	jnz	.+3
	sub	r1, r1, 4
	lsl	a1, a1, 4
	lsr	void, a1, 30
	jnz	.+3
	sub	r1, r1, 2
	lsl	a1, a1, 2
	lsr	a1, a1, 31
	jnz	link
	sub	r1, r1, 1
	j	link


	.globl	_find_highest_clear64
	.globl	_find_highest_set64
_find_highest_clear64:
	xorn	a1, a1, zero
	xorn	a2, a2, zero
_find_highest_set64:
	ld	void, a2
	jz	_find_highest_set32
	ld	a1, a2
	ld	r1, 63
	j	fhsjoin


	.globl	_population_count
_population_count:
	ld	r1, 0
	ld	void, a1
	jz	link
	add	r1, r1, 1
	sub	t1, a1, 1
	and	a1, a1, t1
	j	.-4
