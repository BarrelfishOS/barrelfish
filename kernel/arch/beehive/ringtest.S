/**
 * \file
 * \brief Bootstrap the kernel.
 */
/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <regconvention.h>

	
// Exports:
	.globl	_message_send
	.globl	_message_send_with_header
	.globl	_message_recv

// Constants:
	msgControl = 18
	lockControl = 22

////////////////////////////////////////////////////////////////////////////
//                                                                        //
// void message_send(int dest, int type, msg_t *buf, int len)             //
//                                                                        //
// Send a message to core number "dest", using "len" words at "buf".      //
//                                                                        //
// Arguments are in r3-r6                                                 //
//                                                                        //
// The implementation has no uses of LINK, including long_* ops, so we    //
// don't need to preserve LINK on the stack.                              //
//                                                                        //
////////////////////////////////////////////////////////////////////////////
	//.type	_message_send, @function
_message_send:
					// ... first place data on wq
	ld	r7,r6			// r7 (loop variable) = msg length
x0:
	sub	r5,r5,4			// we will preincrement
x1:
	sub	r7,r7,1
	jm	x2			// bail if done copying
	aqr_add	r5,r5,4			// preincremnet and fetch word
	ld	wq,rq			// copy word onto wq
	j	x1
x2:					// ... now assemble the address
	lsl	r4,r4,6			// r4 = type << 6
	add_lsl	r4,r4,r6,4		// r4 = (r4 + len) << 4
	add_lsl r4,r4,r3,5		// r4 = (r4 + dest) << 5
	aqw_add	r4,r4,msgControl	// initiate the write
	j	link
	.size	_message_send,.-_message_send

////////////////////////////////////////////////////////////////////////////
//                                                                        //
// void message_send_with_header(int dest, int type, msg_t *buf, int len, //
//                               unsigned hdr)                            //
//                                                                        //
// Varaint in which we prepend message with word "hdr".                   //
// Tweak a few things and then jump into _message_send.                   //
// Arguments are in r3-r7                                                 //
//                                                                        //
////////////////////////////////////////////////////////////////////////////
_message_send_with_header:
	ld	wq, r7			// place header word on wq
	ld	r7, r6			// loop variable additional words
	add	r6, r6, 1		// length includes hdr
	j	x0
	
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// int message_recv(int *srce, int *type, msg_t * buf)                    //
//                                                                        //
// Receive a message into "buf" and return its word length, or 0.         //
//                                                                        //
// Arguments are in r3-r5, result goes in r1                              //
//                                                                        //
////////////////////////////////////////////////////////////////////////////
	.type	_message_recv, @function
_message_recv:
	aqw_sub	sp,sp,4
	ld	wq,link
	aqr_ld	vb,msgControl		// read the device
	ld	r1,rq			// r1 = status
	jz	x4			// if status == 0 then return 0
	ld	r6,r1			// r6 = status
	and	r1,r6,63		// r1 = msg length
	aqw_ld	vb,r4			// &type
	lsr	r6,r6,6
	and	wq,r6,15
	aqw_ld	vb,r3			// &srce
	lsr	r6,r6,4
	and	wq,r6,15
	ld	r7,r1			// r7 (loop variable) = msg length
x3:
	sub	r7,r7,1
	jm	x4			// exit loop if done copying
	aqw_ld	vb,r5
	ld	wq,rq			// copy word onto wq
	add	r5,r5,4			// next word address
	j	x3
x4:
	aqr_add	sp,sp,0
	add	sp,sp,4
	j	rq
	.size	_message_recv,.-_message_recv


	.globl _lockunit_read
_lockunit_read:
	lsl	r3, r3, 5
	aqr_add	r3, r3, lockControl
	ld	r1, rq
	j	link

	.globl _lockunit_write
_lockunit_write:
	lsl	r3, r3, 5
	aqw_add	r3, r3, lockControl
	j	link


// ------------------------------------------------------------
#if 0
	.code
	/* Read the cycle counter register several times */
	.globl ringt0
ringt0:	
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	r0, r0
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	r0, r0
	ld	r0, r0
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2

	ld	r1, rq
	sub	r2, rq, r1
	sub	r3, rq, r1
	sub	r4, rq, r1
	sub	r5, rq, r1
	sub	r6, rq, r1
	sub	r7, rq, r1
		
	j	link

/* ================================================== */

	.globl _ringt0
_ringt0:
	ld	t2, ringt0
	j	doperf

	.globl _ringt1
_ringt1:
	ld	t2, ringt1
	j	doperf

	.globl _ringt2
_ringt2:
	ld	t2, ringt2
	j	doperf

	.globl _ringt3
_ringt3:
	ld	t2, ringt3
	j	doperf

	.globl _ringt4
_ringt4:
	ld	t2, ringt4
	j	doperf

	
	/* doperf: t2 is function to call, a1 is ptr, a2 is count */
doperf:
	aqw_sub	sp, sp, 4
	ld	wq, link
	aqw_sub	sp, sp, 4
	ld	wq, s1
	aqw_sub	sp, sp, 4
	ld	wq, s2
	aqw_sub	sp, sp, 4
	ld	wq, s3

	ld	s3, t2
	sub	s1, a1, 4
	ld	s2, a2
	jz	dp0B
dp0A:	
	aqw_long_ld vb, 0x8001fc03 ROL 2 /* flush whole cache */
	add_lsl	r1, s2, 0x40, 8
dp0A1:	sub	r1, r1, 1
	jnz	dp0A1
	call	s3
	aqw_add	s1, s1, 4
	ld	wq, r2
	aqw_add	s1, s1, 4
	sub	wq, r3, r2
	aqw_add	s1, s1, 4
	sub	wq, r4, r3
	aqw_add	s1, s1, 4
	sub	wq, r5, r4
	aqw_add	s1, s1, 4
	sub	wq, r6, r5
	aqw_add	s1, s1, 4
	sub	wq, r7, r6
	sub	s2, s2, 1
	jnz	dp0A
dp0B:
	aqr_add	sp, sp, 0
	aqr_add	sp, sp, 4
	aqr_add sp, sp, 4
	aqr_add sp, sp, 4
	add	sp, sp, 4
	ld	s3, rq
	ld	s2, rq
	ld	s1, rq
	simctrl	3
	j	rq
	
/* ================================================== */
	
	.globl ringt1
ringt1:
	ld	t3, link

	// t1 gets message send address, type=1 length=4 dest=2, io=4
	long_ld	t1, (0x80000000 + (1 LSL 13) + (4 LSL 7) + (2 LSL 3) + 4) ROL 2

	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	wq, t1
	ld	wq, t1
	ld	wq, t1
	ld	wq, t1
	aqw_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2

	ld	r1, rq
	sub	r2, rq, r1
	sub	r3, rq, r1

	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
rt1p1:	sub	r4, rq, r1
	aqr_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	t2, rq
	lsl	vb, t2, 18
	jz	rt1p1
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	sub	r5, rq, r1
	ld	r6, r5
	ld	r7, r5
	j	t3

/* ================================================== */

	.globl ringt2
ringt2:
	ld	t3, link

	// t1 gets message send address, type=1 length=3 dest=1, io=4
	long_ld	t1, (0x80000000 + (1 LSL 13) + (3 LSL 7) + (1 LSL 3) + 4) ROL 2

	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	wq, t1
	ld	wq, t1
	ld	wq, t1
	aqw_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	wq, t1
	ld	wq, t1
	ld	wq, t1
	aqw_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2

	ld	r1, rq
	sub	r2, rq, r1
	sub	r3, rq, r1

	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
rt2p1:	sub	r4, rq, r1
	aqr_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	t2, rq
	lsl	vb, t2, 18
	jz	rt2p1
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	sub	r5, rq, r1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
rt2p2:	sub	r6, rq, r1
	aqr_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	t2, rq
	lsl	vb, t2, 18
	jz	rt2p2
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	sub	r7, rq, r1

	j	t3

/* ================================================== */

	.globl ringt3
ringt3:
	ld	t3, link

	// t1 gets message send address, type=1 length=4 dest=1, io=4
	long_ld	t1, (0x80000000 + (1 LSL 13) + (4 LSL 7) + (1 LSL 3) + 4) ROL 2

	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	wq, t1
	ld	wq, t1
	ld	wq, t1
	ld	wq, t1
	aqw_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	wq, t1
	ld	wq, t1
	ld	wq, t1
	ld	wq, t1
	aqw_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2

	ld	r1, rq
	sub	r2, rq, r1
	sub	r3, rq, r1

	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
rt3p1:	sub	r4, rq, r1
	aqr_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	t2, rq
	lsl	vb, t2, 18
	jz	rt3p1
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	sub	r5, rq, r1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
rt3p2:	sub	r6, rq, r1
	aqr_ld	vb, t1
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	t2, rq
	lsl	vb, t2, 18
	jz	rt3p2
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	ld	vb, rq		/* discard one word */
	sub	r7, rq, r1

	j	t3

/* ================================================== */

	.globl ringt4
xringt4:
	ld	t3, link

	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	aqr_ld	vb, (0x80000010 + 5) ROL 2
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	//aqr_ld	vb, (0x80000010 + 5) ROL 2
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2

	ld	r1, rq
	ld	t1, rq
	sub	r2, rq, r1
	//ld	t2, rq
	sub	r3, rq, r1

	add	r4, t1, r3
	add	r5, t2, r4
	j	t3
	
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	aqw_ld	vb, (0x80000000 + 5) ROL 2
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	aqw_ld	vb, (0x80000000 + 5) ROL 2
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	
	sub	r4, rq, r1
	sub	r5, rq, r1
	sub	r6, rq, r1

	add	r7, r6, t1
	add	r7, r7, t2
	j	t3

yringt4:
	ld	t3, link
	aqw_ld	vb, (0x80000010 + 5) ROL 2
	j	foo1
foo3:	
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	ld	r1, rq
	aqr_ld	vb, (0x80000010 + 5) ROL 2
	ld	t1, rq
	j	foo1a
foo1:	j	foo2
foo1a:	
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	sub	r2, rq, r1
	j	foo2a
foo2:	j	foo3
foo2a:	
	add	r3, r2, t1
	j	t3

ringt4:
	ld	t3, link
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	aqw_ld	vb, (0x80000010 + 5) ROL 2
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	aqr_ld	vb, (0x80000010 + 5) ROL 2
	aqr_ld	vb, (0x80000000 + (1 LSL 3)) ROL 2
	j	bar1
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
bar1:	j	bar2
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
bar2:	j	bar3
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
bar3:	j	bar4
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
	ld	r0, r0
bar4:
	ld	r1, rq
	sub	r2, rq, r1
	ld	t1, rq
	sub	r3, rq, r1

	add	r4, t1, r3
	j	t3
#endif
