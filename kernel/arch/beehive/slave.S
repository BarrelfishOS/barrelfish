/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <simctrl.h>
#include <beej7.h>
#include <asmoffsets.h>
#include <regconvention.h>
#include <stopcode.h>

.code
// ------------------------------------------------------------
// The slave stop/start code.
//
// When the system is reset, the program starts at location 0.
// ------------------------------------------------------------

//.globl main
.globl _slaveStart
.globl _slaveAfter


invalByteAddress = 0x7ffff000 //used when building the table
JPLUS8 = 0xf800220c
JLINK = 0xf000030c

// inv=0 cnt=127 line=0 dev=3
FLUSHALL = (0 LSL 19) + (127 LSL 12) + (0 LSL 5) + (3 LSL 2) + 2

// inv=0 cnt=0 line=? dev=3
FLUSHONE = (0 LSL 19) + (0 LSL 12) + (3 LSL 2) + 2

// inv=1 cnt=127 line=0 dev=3
INVALALL = (1 LSL 19) + (127 LSL 12) + (0 LSL 5) + (3 LSL 2) + 2

// inv=1 cnt=0 line=? dev=3
INVALONE = (1 LSL 19) + (0 LSL 12) + (3 LSL 2) + 2

// inv=1<<19
INVALSHIFT = 19

// dev = 3
CACHEACCESS = (3 LSL 2) + 2

// line mask
LINEMASK = (127 LSL 5)

// type=1 len=1 dst=1 dev=4
STOPACKMSG = (1 LSL 15) + (1 LSL 9) + (1 LSL 5) + (4 LSL 2) + 2



main:
_slaveStart:
/* The instruction at address 0 must be a nop because it might or
 * might not be nullified depending on the pipeline state at the time
 * the debug unit triggers.
 */
	ld	zero, 0

/* We want to save the minimum registers to check if we are running
 * or took a wild branch to zero.  If the latter then we want link
 * to be preserved since it is useful to help us find where we came
 * from.
 */
	ld	wq, link
	j7	BEE_J7_TEST_RUNNING
	ld	void, link
	jnz	wildjump

/* If this is power-up condition savedPc will be 0.  We dont need to
 * save lots of stuff but might want to do other initialisation.
 */
	j7	BEE_J7_SAVEDPC
	ld	void, link
	jz	powerup

/* Next we want to check for stop interrupts.  Timer interrupts might
 * also be pending but that is true after a restart anyway so we
 * punt timer+stop to dealing with timer after a restart.  A breakpoint
 * might also happen simultaneously but the savedPC will be pointing
 * at the breakpoint so we will just break again after the restart.
 */
	ld	wq, t1
	ld	wq, t2
	ld	wq, t3
	add	wq, link, 1		// savedPc. Instruction already executed
	j7	BEE_J7_COREAREA
	aqw_add	void, link, 30 * 4	// original link (==savedLink)
	aqw_add	void, link, 24 * 4	// t1
	aqw_add	void, link, 25 * 4	// t2
	aqw_add	void, link, 26 * 4	// t3
	aqw_add	t2, link, 31 * 4	// savedPc, also set t2 for rq2wq

	ld	t3, zero
	j	rq2wq

	/* TODO: XXX: nasty code can arrange more than 64 words in RQ! */
rq2wql:	ld	wq, rq
	aqw_add	t2, t2, 4
	add	t3, t3, 1
rq2wq:	j7	BEE_J7_TEST_RQ_EMPTY
	ld	void, link
	jnz	rq2wql

	j7	BEE_J7_COREAREA
	ld	wq, t3
	aqw_ld	void, link		// overwrite count

	/* We would like to invalidate one line if a particular
	 * address is cached there, but we cant do that - the
	 * invalidate invalidates the specified line always so we must
	 * always flush first (why does invalidate-only even exist?) */
	
	add	t1, link, OFFSETOF_COREAREA_MASTER + CACHEACCESS
	aqw_and	t2, t1, LINEMASK + CACHEACCESS // flush that line
	lsl	t3, 1, INVALSHIFT
	aqw_add	void, t2, t3		// invalidate that line
	
	aqr_add	void, link, OFFSETOF_COREAREA_MASTER_STOPS
	aqr_add	void, link, OFFSETOF_COREAREA_SLAVE_STOPS
	ld	t1, rq
	sub	void, rq, t1
	jnz	stop

/* Not a stop, is it a timer interrupt? */

	aqr_add	void, link, OFFSETOF_COREAREA_MASTER_TICKS
	aqr_add	void, link, OFFSETOF_COREAREA_SLAVE_TICKS
	ld	t1, rq
	sub	void, rq, t1
	jz	break

/* Mark to ourselves that we have noticed this one */
	aqw_add	void, link, OFFSETOF_COREAREA_SLAVE_TICKS
	ld	wq, t1

/* It is a timer interrupt.  We cannot make ourselves running, and the
 * master cant send us a running until it knows we have stopped
 * (otherwise we cant tell the difference from a wild branch to
 * zero).  So we need to get off a timerAck to the master as soon
 * as possible so we dont stall too long waiting to be made
 * running. */

	ld	wq, STOPCODE_TIMER
	aqw_long_ld void, STOPACKMSG

/* If we are running in the kernel we want to mark for a deferred
 * interrupt otherwise we want to take the interrupt now. We compare
 * the savedPc against kernel boundaries.
 */
	j7	BEE_J7_SAVEDPC
	ld	t2, link
	j7	BEE_J7_COREAREA
	aqr_add	void, link, OFFSETOF_COREAREA_KERNEL_BEGINS
	aqr_add	void, link, OFFSETOF_COREAREA_KERNEL_ENDS
	//simctrl	BEE_SIMCTRL_BREAK_SIM
	// Beehive is 6502 carry (not 8080 carry) which I find
	// hard to get my head round.  A subtraction carries
	// if the addition of the negation carries, which is
	// if the subtraction has no carry.
	ld	t3, rq
	sub	void, rq, t2	// (end - t2 > 0 => carry) => (end > t2 => carry)
	jnc	take		// hence nc if t2 > end
	sub	void, t3, t2	// (start - t2 > 0 => carry) => (start > t2 => carry)
	jnc	defer		// hence nc if t2 < start

/* Take the interrupt now (as soon as we are running). */
take:	aqr_add	void, link, OFFSETOF_COREAREA_KERNEL_TICKER
	simctrl	BEE_SIMCTRL_HALT
	j7	BEE_J7_TEST_RUNNING
	ld	void, link
	jz	. - 2
	j	rq

defer:	aqr_add	void, link, OFFSETOF_COREAREA_KERNEL_PENDING
	aqw_add	void, link, OFFSETOF_COREAREA_KERNEL_PENDING
	add	wq, rq, 1

/* Now load up the saved registers and rq and back to where we came
 * from (assumes link is saved area) */
iret:	aqr_add	void, link, 0		// read count
	aqr_add	void, link, 24 * 4	// t1
	aqr_add	void, link, 25 * 4	// t2
	aqr_add	void, link, 26 * 4	// t3

	add	t1, rq, 2		// +2 for link and pc adjacent
	add	link, link, 30 * 4 - 4	// &link -4
iretrq:	aqr_add	link, link, 4
	sub	t1, t1, 1
	jnz	iretrq

	ld	t1, rq
	ld	t2, rq
	ld	t3, rq

	simctrl	BEE_SIMCTRL_HALT
	j7	BEE_J7_TEST_RUNNING
	ld	void, link
	jz	. - 2

	ld	link, rq
	j	rq

wildjump:
	j7	BEE_J7_COREAREA
	aqw_add	void, link, 30 * 4	// link is in wq
	aqr_add	void, link, 30 * 4	// read it back
	ld	link, rq
wjloop:	j7	BEE_J7_BREAKPOINT
	j	wjloop
	
/* At power-up we must create the icache flush code table because we cant rely
 * on the master having code to do it, or it finishing it before we
 * need to use it.  Also we clear our own core area */
powerup:
	j7	BEE_J7_COREAREA
	aqw_add	void, link, 30 * 4
	ld	t2, 128
	sub	link, link, 4
zarea:	aqw_add	link, link, 4
	ld	wq, 0
	sub	t2, t2, 1
	jnz	zarea

	/* build the table of instructions at invalIcache.  127 of "j . + 8"
	 * followed by a j link */
	
	long_ld	t1, invalByteAddress - 32
	ld	t2, 127
itable:	aqw_add	t1, t1, 32
	long_ld	wq, JPLUS8
	sub	t2, t2, 1
	jnz	itable
	aqw_add	t1, t1, 32
	long_ld	wq, JLINK

	/* Note that "stopped" flushes the cache before executing the
	 * icache flush */

	ld	t1, STOPCODE_POWERUP
	j	stopped	

/* We were sent a stop, so acknowledge that and come to a full stop */
		
stop:
	ld	wq, t1
	aqw_add	void, link, OFFSETOF_COREAREA_SLAVE_STOPS

	ld	t1, STOPCODE_STOP
	j	fullstop

/* A breakpoint was executed.  Tell master and come to a full stop */

break:
	ld	t1, STOPCODE_BREAK

/* This code saves all the registers out and flushes the whole
 * cache so the master can examine everything, and then informs the
 * master. link=corearea already t1=argument */
fullstop:
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
	// we already did t1=24 t2=25 and t3=26
	ld	wq, $27
	ld	wq, $28
	add	wq, zero, $29    //r29 (via a port) is rq. Use the b port

	ld	t2, 23
save23:	aqw_add	link, link, 4
	sub	t2, t2, 1
	jnz	save23
	aqw_add	link, link, 4 * 4	 // skip t1, t2, t3
	aqw_add	link, link, 4
	aqw_add	link, link, 4

stopped:
	aqw_long_ld void,FLUSHALL
	aqw_long_ld void,INVALALL

	/* Report that we are stopped */
	ld	wq, t1
	aqw_long_ld void, STOPACKMSG

	/* We have to invalidate the icache at some point.  We might
	 * as well do it while the master is thinking about us */

	long_call invalByteAddress ROR 2

	/* Wait to be restarted */
	simctrl	BEE_SIMCTRL_HALT
	j7	BEE_J7_TEST_RUNNING
	ld	void, link
	jz	. - 3

fullstart:
	j7	BEE_J7_COREAREA
	ld	$28, 31
	aqr_ld	link, link
fs1:	aqr_add	link, link, 4
	sub	$28, $28, 1
	jnz	fs1

	ld	$28, rq		//put RQ count into r28
	ld	$1, rq		//load registers 1-27, r29
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

	/* rq now has r28, r29, savedLink, savedPC
	 * $28 contains the number of RQ entries to restore.
	 * We use it because r29 isn't a good place to stand
	 * continue reading until $28 < 0.
	 */

loadRQ:
	sub	$28, $28, 1	//all RQ entries read?
	jm	rqDone
	aqr_add	link, link, 4
	j	loadRQ
rqDone:
	/* rq now contains r28, r29, savedLink, savedPC, and the RQ entries. */

	ld	$28, rq		//load r28
	ld	$29, rq		//load r29
	ld	link, rq	//load link
	simctrl	BEE_SIMCTRL_TRACEOFF
	j	rq		//transfer to user code.

_slaveAfter:
