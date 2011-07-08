/**
 * \file
 * \brief Provide kernel context switch code
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
#include <beej7.h>
#include <asmoffsets.h>
#include "beekernel.h"

// ------------------------------------------------------------

	.code

	.globl __resume
	/* Argument a1 is union registers_beehive *regs */
__resume:
	/* We need to check if a software interrupt is pending. If so
	 * we will pretend that this is the context we just saved.
	 * If a hardware interrupt happens during this routine we
	 * need to ensure it happens after the return to user mode.
	 * This is tricky since we are resuming a full state of
	 * all registers here, we use a trick where the address to
	 * empty the wq token comes from the rq and not from register.
	 */
	add	wq, a1, 31*4	// wq non empty with address to zap

	/* look at pending flag, non zero take interrupt */
	j7	BEE_J7_COREAREA
	aqr_add	void, link, OFFSETOF_COREAREA_KERNEL_PENDING
	ld	void, rq
	jnz	resintr

	/* we are going to resume this context and poison it to
	 * prevent reuse (catch bugs) by setting pc to zero */
	ld	wq, zero	// wq additional non empty
	aqw_sub	void, sp, 4	// write zap address to stack
	
	aqr_ld	link, a1	// fetch count
	ld	t1, 30		// fetch 30 more
res1:	aqr_add	link, link, 4
	sub	t1, t1, 1
	jnz	res1
	aqr_sub	void, sp, 4	// fetch zap address from stack
	aqr_add	link, link, 4	// fetch pc
	
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
	/* rq now has r28, r29, link, zapaddr, pc.  r28=rqcount */
res2:	sub	$28, $28, 1
	jm	res3
	aqr_add	link, link, 4
	j	res2
res3:
	ld	$28, rq
	ld	$29, rq
	ld	link, rq
	aqw_ld	void, rq	// zap context and release empty wq
	j	rq

resintr:
	/* wq still has dummy in it.  a1 still has the address of
	 * the context, which is what we need for calling _handle_timer.
	 * Just need to empty out wq and pass how argument. */
	long_ld	sp, stack
	aqw_sub	void, sp, 4
	ld	fp, 0
	ld	a2, HANDLE_TIMER_HOW_RESUME
	long_call _handle_timer
	/* should not return */
	j7	BEE_J7_BREAKPOINT

// ------------------------------------------------------------

	.globl	__execute
	/* Argument a1 is entry address */
	/* Argument a2 is dispatcher pointer */
__execute:
	/* We need to check if a software interrupt is pending.  If so
	 * we will abort this execute.  We know that if we are executing
	 * it is because we are making a scheduler activation on an
	 * enabled domain and have set it to disabled in the dispatcher
	 * (so the scheduler activation handler is resumed until such
	 * time as it has decided to re-enable activations) so all
	 * we need to do is undo the scheduler activation mark in the
	 * dispatcher. */
	ld	wq, zero		// wq non empty

	j7	BEE_J7_COREAREA
	aqr_add	void, link, OFFSETOF_COREAREA_KERNEL_PENDING
	ld	void, rq
	jnz	exeintr

	/* we are going to make the jump */
	ld	p1, a2
	aqw_sub	void, sp, 4		// drop dummy to stack
	call	a1
	/* should not return */
	j7	BEE_J7_BREAKPOINT

exeintr:
	/* wq still has a zero in it.  Write that to disp->disabled
	 * and call into _handle_timer */
	aqw_add	void, a2, OFFSETOF_DISP_DISABLED
	ld	a1, zero
	ld	a2, HANDLE_TIMER_HOW_EXECUTE
	long_call _handle_timer
	/* should not return */
	j7	BEE_J7_BREAKPOINT

/* End */
