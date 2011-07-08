/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <regconvention.h>
#include <simctrl.h>
#include <beej7.h>

// ------------------------------------------------------------
// Program start.  Initialize and jump to main.
// ------------------------------------------------------------
    .code

	.globl main /* bsim requires that this be called main */
main:	j	after
btorg:	.word	0
btsize:	.word	0
bdorg:	.word	0
bdsize:	.word	0
bborg:	.word	0
bbsize:	.word	0
bcksum:	.word	0

after:
	/* check header and checksum without touching arg regs */
	sub	t1, pc, 8	/* t1 points at cs:main */
	sub_lsl	t2, t1, zero, 2	/* t2 points at ds:main */
	aqr_add vb, t2, 4	/* read btorg */
	sub	vb, rq, t1	/* compare &main with *btorg */
	jnz	badhdr

	aqr_add	vb, t2, 20	/* read bborg */
	lsl	vb, rq, 2
	aqr_ld	vb, vb
	ld	vb, rq
	jnz	badsum

	aqr_add	vb, t2, 20	/* read bborg */
	aqr_add	vb, t2, 24	/* read bbsize */
	sub	t3, rq, t1	/* t3 has (word) bborg - btorg */
	add	t3, rq, t3	/* t3 has (word) bborg + bbsize - btorg */

	ld	t1, 0
	sub	t2, t2, 4	/* loop incs r2 first before load */
cklp:				/* t2 has data pointer, r3 is size in words */
	aqr_add	t2, t2, 4
	add	t1, rq, t1
	sub	t3, t3, 1
	jnz	cklp
	rol	t3, t1, 1
	xorn	vb, t1, t3
	jnz	badsum

	long_ld	sp,stack         // initialize sp
	
	/* _barrelfish_init_disabled expects first arg is disp pointer
	 * which is actually stored in the thread register at this point
	 */
	ld	a1, p1
	long_call	_barrelfish_init_disabled
	simctrl BEE_SIMCTRL_REGISTERS
    
// XXX	.globl     _exit
_exit:
	j7	BEE_J7_BREAKPOINT
	j	_exit

badsum:
	simctrl BEE_SIMCTRL_REGISTERS
	j7	BEE_J7_BREAKPOINT
	j	badsum
badhdr:
	simctrl BEE_SIMCTRL_REGISTERS
	j7	BEE_J7_BREAKPOINT
	j	badhdr

    .bss
    .blkw 512
stack:
