/*
 * \file
 * \brief Provide user context switch code
 */
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

/*
 * __disp_resume is called only from C disp_resume.  The first argument
 * is the context to resume, the second is the address to write to zero
 * atomically with the resume.  We can do this on Beehive because
 * interrupts are not delivered if the next instruction is a jump.
 */
	.globl __disp_resume
__disp_resume:
	ld	$28, 31
	ld	$29, a2
	aqr_ld	link, a1	// entry 0 is RQ count
dr1:	aqr_add	link, link, 4
	sub	$28, $28, 1
	jnz	dr1
	/* debug hack - kill this context by setting pc to zero */
	aqw_ld	void, link
	ld	wq, 0
	// Now load them
	ld	$28, rq		// entry 0 is RQ count
	ld	$1, rq
	ld	$2, rq
	ld	$3, rq
	ld	$4, rq
	ld	$5, rq
	ld	$6, rq
	ld	$7, rq
	ld	$8, rq
	ld	$9, rq
	ld	$10, rq
	ld	$11, rq
	ld	$12, rq
	ld	$13, rq
	ld	$14, rq
	ld	$15, rq
	ld	$16, rq
	ld	$17, rq
	ld	$18, rq
	ld	$19, rq
	ld	$20, rq
	ld	$21, rq
	ld	$22, rq
	ld	$23, rq
	ld	$24, rq
	ld	$25, rq
	ld	$26, rq
	ld	$27, rq

	/* rq now has r28, r29, link, pc
	 * r28 has rqcount, r29 has addres to zap */
dr2:	sub	$28, $28, 1
	jm	dr3
	aqr_add	link, link, 4
	j	dr2
dr3:
	aqw_add	void, zero, $29	// address to zap
	ld	$28, rq
	ld	$29, rq
	ld	link, rq
	ld	wq, zero	// write 0 to disabled flag
	j	rq

/*
 * __disp_switch is called only from C disp_switch.  It has extremely
 * unusual semantics compared to the assembler functions required for
 * most operating systems as it combines a variant of setjmp with a resume
 */
	.globl __disp_switch
__disp_switch:
	aqw_ld	a2, a2
	ld	wq, zero	// RQ count = 0
	aqw_add	a2, a2, 36	// r9 is first callee save
	ld	t1, 14		// 14 callee save regs plus fp
	aqw_add	a2, a2, 4
	sub	t1, t1, 1
	jnz	.-2
	ld	wq, s1
	ld	wq, s2
	ld	wq, s3
	ld	wq, s4
	ld	wq, s5
	ld	wq, s6
	ld	wq, s7
	ld	wq, s8
	ld	wq, s9
	ld	wq, s10
	ld	wq, s11
	ld	wq, s12
	ld	wq, s13
	ld	wq, s14
	ld	wq, fp
	aqw_add	a2, a2, 16	// skip save for t1, t2, t3
	ld	wq, p1
	aqw_add	a2, a2, 4
	ld	wq, sp
	aqw_add	a2, a2, 12	// skip save for vb and link
	ld	wq, link

	ld	a2, a3
	long_j	_disp_resume	// NB this is the C version

/*
 * __disp_save is called only from C disp_save.  Saves only voluntary
 * state, but in a format suitable for resuming with __disp_resume.
 * It works like setjmp/longjump in that it returns a 0 when first called
 * and sets the return register in the save area to non-zero.
 */

	.globl __disp_save
__disp_save:
	aqw_ld	a1, a1
	ld	wq, zero	// RQ count = 0
	aqw_add	a1, a1, 4
	orn	wq, zero, zero	// r1 in save area is ~0
	aqw_add	a1, a1, 32	// r9 is first callee save
	ld	t1, 14		// 14 callee save regs plus fp
	aqw_add	a1, a1, 4
	sub	t1, t1, 1
	jnz	.-2
	ld	wq, s1
	ld	wq, s2
	ld	wq, s3
	ld	wq, s4
	ld	wq, s5
	ld	wq, s6
	ld	wq, s7
	ld	wq, s8
	ld	wq, s9
	ld	wq, s10
	ld	wq, s11
	ld	wq, s12
	ld	wq, s13
	ld	wq, s14
	ld	wq, fp
	aqw_add	a1, a1, 16	// skip save for t1, t2, t3
	ld	wq, p1
	aqw_add	a1, a1, 4
	ld	wq, sp
	aqw_add	a1, a1, 12	// skip save for vb and link
	ld	wq, link

	ld	r1, zero
	j	link
