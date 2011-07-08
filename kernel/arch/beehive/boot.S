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
#include <barrelfish_kpi/syscalls.h>
#include <asmoffsets.h>
#include <beej7.h>
#include <simctrl.h>
#include "beekernel.h"
	
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

	.data
	.globl __data_ibefore
__data_ibefore:
	.code
	
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
	long_ld	a1, guardz
	lsr	a1, a1, 2
	ld	a2, 512 // 512 words of guard
	ld	a3, 0
	ld	a4, 0
	simctrl	BEE_SIMCTRL_ADDRCHECK
	/* Argument to bmain used cross-core is in s1 */
	ld	a1, s1
	long_call _bmain
	long_call _reboot
	j __end_simulation


badv1:	andn	t2, t2, t2
	add	r1, t2, "v"
	add	t2, t2, "1"
	j	dieloop

badhdr:	simctrl	BEE_SIMCTRL_REGISTERS
	andn	t2, t2, t2
	add	r1, t2, "h"
	add	r2, t2, "d"
	j	dieloop

badck:	simctrl	BEE_SIMCTRL_REGISTERS
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
	simctrl	BEE_SIMCTRL_TERMINATE
	j	T1

		
	.globl __end_simulation
__end_simulation:
	simctrl BEE_SIMCTRL_TERMINATE
	j __end_simulation

/* Performing system calls.  At the C level they all return a struct
 * sysret which, by beehive calling convention means that the first
 * *result* register is used to *pass* the address where the return
 * value should be written.  At the assembler level we use t1 to
 * contain the system call number.  There are several other
 * errors here including that we trust the user r1 for pointer of
 * location to write the struct sysret return value into */

#if SYSCALL_INVOKE != 0
#error "This code assumes invoke is zero"
#endif
#if SYS_ERR_OK != 0
#error "This code assumes SYS_ERR_OK is zero"
#endif

	.globl _systrap
_systrap:
	/* check for system call number in range */
	sub	vb, t1, SYSCALL_COUNT
	jc	systrapbad
systrap2:
	/* switch to kernel stack and store old link and sp */
	ld	wq, link
	ld	wq, sp
	long_ld	sp, stack
	aqw_sub	sp, sp, 4
	aqw_sub	sp, sp, 4

	lsl	t1, t1, 2
	jz	systrapinvoke

	/* table dispatch */
	long_ld link, _syscalls
	aqr_add	vb, link, t1
systrap3:
	call	rq
	/* atomically, are there any pending timer interrupts?  we
	 * want it to look like the intererupt happened right at the
	 * return from the system call */
	ld	wq, zero	// wq non empty prevents interrupts
	j7	BEE_J7_COREAREA
	aqr_add	t1, link, OFFSETOF_COREAREA_KERNEL_PENDING
	ld	t2, rq
	jnz	systrap4
	/* no interrupt pending, return */
	aqr_ld	void, sp
	aqr_add	link, sp, 4
	ld	sp, rq
	aqw_ld	void, link	// release wq non empty, write into k stack
	j	rq		// next is j so no interrupt in gap

systrapinvoke:
	/* This code saves the callee-save registers into the
	 * user dispatcher state just in case this invoke
	 * happens to be IDC. */
	// Set return code to zero
	ld	r1, zero
	call	syssavecontext
	aqr_long_ld link, _syscalls	// assumes invoke is 0
	j	systrap3

systrap4:
	/* kernel pending non zero, wq still has dummy in it */
	/* we no longer need to keep the wq non empty. An additional
	 * timer interrupt here will only increment pending which will
	 * either be noticed when we call handle_timer, or will be
	 * noticed when the dispatcher resumes something, or idles */
	aqw_sub	void, sp, 4	// safe dump of wq non empty token
	call	syssavecontext
	sub	a1, t3, 31*4	// arg is move t3 back to start
	ld	a2, HANDLE_TIMER_HOW_SYSCALL
	add	sp, sp, 8
	long_call _handle_timer
	/* Shouldnt come back, but if it does, ok to fall through */

syssavep1bad:
	simctrl	BEE_SIMCTRL_REGISTERS
	j7	BEE_J7_BREAKPOINT
	j	syssavep1bad

syssavecontext:
	/* This subroutine saves the callee-save registers and return
	 * value into the user dispatcher state.  It assumes that the
	 * users stack pointer and return address are on the current
	 * stack. It propagates disp->disabled to dcb->disabled.
	 * The address of the save_area pc slot is returned in t3. */
	ld	t2, link
	// Check that dcb_current->disp == p1
	aqr_long_ld link, _dcb_current
	ld	link, rq
	aqr_add	vb, link, OFFSETOF_DCB_DISP
	sub	vb, rq, p1
	jnz	syssavep1bad
	// t3 = dcb_current->disabled = disp->disabled
	aqr_add	vb, p1, OFFSETOF_DISP_DISABLED
	aqw_add	vb, link, OFFSETOF_DCB_DISABLED
	ld	t3, rq
	ld	wq, t3
	// Selected save area t3 = (t3 ? disabled_save_area : enabled_save_area)
	jz	.+3
	add	t3, p1, OFFSETOF_DISP_DISABLED_AREA
	j	.+2
	add	t3, p1, OFFSETOF_DISP_ENABLED_AREA

	// Save callee-save and return value etc. Note save r1 and r2
	aqw_ld	vb, t3
	aqw_add	t3, t3, 4
	aqw_add	t3, t3, 4
	ld	wq, zero
	ld	wq, r1
	ld	wq, r2

	add	t3, t3, 6*4	// r2 to r8
	ld	t1, 15		// r9--r23
	aqw_add	t3, t3, 4	// first to r9==s1
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

	aqr_ld	void, sp	// read stacked sp
	aqr_add	void, sp, 4	// read stacked link
	aqw_add	t3, t3, 4*4	// from r23 to r27==p1
	aqw_add	t3, t3, 4	// p28==sp
	aqw_add	t3, t3, 3*4	// from r28 to r31
	ld	wq, p1		// have to save this since it will be loaded
	ld	wq, rq
	ld	wq, rq
	j	t2	

systrapbad:
	/* here is t1 is an out of range system call number */
	simctrl BEE_SIMCTRL_REGISTERS
	ld	a1, t1
	ld	t1, SYSCALL_COUNT
	j	systrap2

/* Performing timer interrupts.  The assembler in slave.S has taken
 * stop from the master core and if the core was not running in the
 * kernel then it enters us here.  We have to do most of the saving
 * and copy over a few registers that were saved in the corearea.
 */

	.globl	_interrupt
_interrupt:
	// Check that dcb_current->disp == p1
	long_ld link, _dcb_current
	aqr_ld	void, link
	ld	link, rq
	aqr_add	void, link, OFFSETOF_DCB_DISP
	sub	void, rq, p1
	jnz	interruptbad
	// t3 = dcb_current->disabled = disp->disabled
	aqr_add	void, p1, OFFSETOF_DISP_DISABLED
	aqw_add	void, link, OFFSETOF_DCB_DISABLED
	ld	t3, rq
	ld	wq, t3
	// Selected save area t3 = (t3 ? disabled_save_area : enabled_save_area)
	jz	inten
intdi:	add	t3, p1, OFFSETOF_DISP_DISABLED_AREA
	j	intsave
inten:	add	t3, p1, OFFSETOF_DISP_ENABLED_AREA
intsave:
	// Load t1, t2, t3, link, pc into rq
	j7	BEE_J7_COREAREA
	aqr_add	void, link, 24 * 4
	aqr_add	void, link, 25 * 4
	aqr_add	void, link, 26 * 4
	aqr_add	void, link, 30 * 4
	aqr_add	void, link, 31 * 4
	ld	wq, $1
	ld	wq, $2
	ld	wq, $3
	ld	wq, $4
	ld	wq, $5
	ld	wq, $6
	ld	wq, $7
	ld	wq, $8
	ld	wq, $9
	ld	wq, $10
	ld	wq, $11
	ld	wq, $12
	ld	wq, $13
	ld	wq, $14
	ld	wq, $15
	ld	wq, $16
	ld	wq, $17
	ld	wq, $18
	ld	wq, $19
	ld	wq, $20
	ld	wq, $21
	ld	wq, $22
	ld	wq, $23
	ld	wq, rq // t1 = 24
	ld	wq, rq // t2 = 25
	ld	wq, rq // t3 = 26
	ld	wq, $27
	ld	wq, $28
	add	wq, zero, $29    //r29 (via a port) is rq. Use the b port
	ld	wq, rq // link = 30
	ld	wq, rq // link = 31

	ld	t1, t3
	ld	t2, 31
	// t1 points to base, t3 to current (first is r1), t2 count
ints1:	aqw_add	t3, t3, 4
	sub	t2, t2, 1
	jnz	ints1
	// Still need to move count and the rq words
	//simctrl	BEE_SIMCTRL_TRACEON
	aqr_add	void, link, 0
	aqw_add	void, t1, 0
	add	link, link, 32 * 4 - 4
	ld	t2, rq
	ld	wq, t2
ints2:	sub	t2, t2, 1
	jm	ints3
	aqr_add	link, link, 4
	aqw_add	t3, t3, 4
	ld	wq, rq
	j	ints2
ints3:	// rq copied
	/* Upcall to C need stack */
	//simctrl	BEE_SIMCTRL_TRACEOFF
	ld	a1, t1
	ld	a2, HANDLE_TIMER_HOW_SYNC
	long_ld	sp, stack
	ld	fp, 0
	long_call _handle_timer
	/* Shouldnt come back, but if it does, ok to fall through */

interruptbad:
	simctrl BEE_SIMCTRL_REGISTERS
	j7	BEE_J7_BREAKPOINT
	j	interruptbad

/* Extra misc things
 */

// Look at corearea->kernel_pending with wq non empty
	.globl _halt_unless_timer_pending
_halt_unless_timer_pending:
	ld	wq, link		// save it also defers interrupts
	j7	BEE_J7_COREAREA
	aqr_add	void, link, OFFSETOF_COREAREA_KERNEL_PENDING
	ld	void, rq
	jnz	nohalt
	simctrl	BEE_SIMCTRL_HALT
nohalt:	aqw_sub	void, sp, 4
	aqr_sub	void, sp, 4
	j	rq


	.globl _mmu_add_entry
_mmu_add_entry:
	simctrl BEE_SIMCTRL_ADDRCHECK
	j	link

	.globl	_lli_test
_lli_test:
	ld	t1, link
	aqr_ld	void, sp
	j7	BEE_J7_TEST_RQ_EMPTY
	ld	void, link
	jz	. - 2
	x_lli	0xE8000000
	ld	void, void
	ld	void, void
	j7	BEE_J7_TEST_RQ_EMPTY
	ld	r1, link
	jz	t1
	ld	void, rq
	j	t1

	.globl _skip_test
_skip_test:
	ld	a2, 0x003
	ld	a3, 0x00C
	ld	a4, 0x030
	ld	a5, 0x0C0
	ld	a6, 0x300
	ld	r1, 0
st1:	
	xor	a2, a2, 0x003
	xor	a3, a3, 0x00C
	xor	a4, a4, 0x030
	xor	a5, a5, 0x0C0
	xor	a6, a6, 0x300

	xor	a2, a2, 0x001
	xor	a3, a3, 0x004
	xor	a4, a4, 0x010
	xor	a5, a5, 0x040
	xor	a6, a6, 0x100

	xor	a2, a2, 0x002
	xor	a3, a3, 0x008
	xor	a4, a4, 0x020
	xor	a5, a5, 0x080
	xor	a6, a6, 0x200
	
	xor	r1, r1, 0x3FF
	xor	r1, r1, a2
	xor	r1, r1, a3
	xor	r1, r1, a4
	xor	r1, r1, a5
	xor	r1, r1, a6
	jnz	st2

	sub	a1, a1, 1
	jnz	st1
	j	link

	j7	BEE_J7_BREAKPOINT

st2:	simctrl BEE_SIMCTRL_REGISTERS
	j	link

	.bss
guardz:
	.blkw 512 // 2kB
	.blkw 768 // 3kB
	.globl	stack		// also used in context.S
stack:	// is_copy needs 1320 bytes all on its own!
	
