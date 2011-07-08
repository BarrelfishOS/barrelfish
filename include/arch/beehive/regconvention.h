/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

// ------------------------------------------------------------
// Standard assembler definitions

r0 = $0      // fixed zero
r1 = $1      // function return 1
r2 = $2      // not callee save, function return 2
r3 = $3      // not callee save, function argument 1
r4 = $4      // not callee save, function argument 2
r5 = $5      // not callee save, function argument 3
r6 = $6      // not callee save, function argument 4
r7 = $7      // not callee save, function argument 5
r8 = $8      // not callee save, function argument 6
r9 = $9      // callee save
r10 = $10    // callee save
r11 = $11    // callee save
r12 = $12    // callee save
r13 = $13    // callee save
r14 = $14    // callee save
r15 = $15    // callee save
r16 = $16    // callee save
r17 = $17    // callee save
r18 = $18    // callee save
r19 = $19    // callee save
r20 = $20    // callee save
r21 = $21    // callee save
r22 = $22    // callee save

fp = $23     // callee save, frame pointer
t1 = $24     // not callee save, temporary 1, not avail for reg alloc
t2 = $25     // not callee save, temporary 2, not avail for reg alloc
t3 = $26     // not callee save, temporary 3, not avail for reg alloc
p1 = $27     // not callee save, platform 1, not avail for reg alloc
sp = $28     // callee save, stack pointer
vb = $29     // not callee save, rw & rb only, not avail for reg alloc

zero = $0    // fixed zero
void = $0    // discard register
a1 = $3      // not callee save, function argument 1
a2 = $4      // not callee save, function argument 2
a3 = $5      // not callee save, function argument 3
a4 = $6      // not callee save, function argument 4
a5 = $7      // not callee save, function argument 5
a6 = $8      // not callee save, function argument 6
s1 = $9      // callee save
s2 = $10     // callee save
s3 = $11     // callee save
s4 = $12     // callee save
s5 = $13     // callee save
s6 = $14     // callee save
s7 = $15     // callee save
s8 = $16     // callee save
s9 = $17     // callee save
s10 = $18    // callee save
s11 = $19    // callee save
s12 = $20    // callee save
s13 = $21    // callee save
s14 = $22    // callee save

    .assume   zero,0

// ------------------------------------------------------------
