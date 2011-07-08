/**
 * \file
 * \brief Bootstrap the kernel.
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
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
#include <asmoffsets.h>

stack = 0x4000

// ------------------------------------------------------------

	.code

	.globl main /* bsim requires that this be called main */
	.globl __code_ibefore
__code_ibefore:
main:	j	.+8		/* must be +8 for correct magic number */
btorg:	.word	0
btsize:	.word	0
bdorg:	.word	0
bdsize:	.word	0
bborg:	.word	0
bbsize:	.word	0
bcksum:	.word	0
after:	orn	r0, r0, r0
	or	r1, r0, r0
	jnz	badv1
	
vok:
	sub	r2, pc, 11	/* r2 points at cs:main */
	add_lsl	r3, r2, zero, 2	/* r3 points at ds:main */
	aqr_add	vb, r3, 4	/* read btorg */
	ld	r4, rq
	sub	r5, r4, r2
	jnz	badhdr

	/* now we can assume we are running at the address we were
	 * linked for. XXX data and bss layout ??? */
	ld	t1, r3
	aqr_add	vb, r3, 20
	aqr_add	vb, r3, 24
	sub	r3, rq, r2	/* r3 has (word) bborg - btorg*/
	add	r3, rq, r3	/* r3 has (word) bborg + bbsize - btorg */
	add	r4, zero, zero	/* r4 is checksum */
	sub	r2, t1, 4	/* loop incs r2 first before load */
cklp:				/* r2 is data pointer, r3 is size in words */
	aqr_add	r2, r2, 4
	add	r4, rq, r4
	sub	r3, r3, 1
	jnz	cklp
	rol	r5, r4, 1
	xorn	vb, r4, r5
	jnz	badck

ckok:	
	long_ld	sp, stack
	long_call _bmain
	j __end_simulation


badv1:	andn	t2, t2, t2
	add	r1, t2, "v"
	add	t2, t2, "1"
	j	dieloop

badhdr:	simctrl	4
	andn	t2, t2, t2
	add	r1, t2, "h"
	add	r2, t2, "d"
	j	dieloop

badck:	simctrl	4
	andn	t2, t2, t2
	add	r1, t2, "c"
	add	r2, t2, "k"
	j	dieloop

	/* r1 with char 1, r2 with char 2 */
dieloop:andn	t2, t2, t2
T1:	aqr_add	t3, t2, 0x02
	add	t3, rq, t2
	and	t3, t3, 0x200
	sub	t3, t3, 0
	jz	T1
	aqw_add	t3, t2, 0x02
	add	wq, r1, 0x200 /* r1 + go*/
T2:	aqr_add	t3, t2, 0x02
	add	t3, rq, t2
	and	t3, t3, 0x200
	sub	t3, t3, 0
	jz	T2
	aqw_add	t3, t2, 0x02
	add	wq, r2, 0x200 /* r2 + go */
	simctrl	1
	j	T1

		
	.globl __end_simulation
__end_simulation:
	simctrl 1
	j __end_simulation
