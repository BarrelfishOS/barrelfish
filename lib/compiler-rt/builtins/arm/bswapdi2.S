//===------- bswapdi2 - Implement bswapdi2 --------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "../assembly.h"

	.syntax unified
	.text

//
// extern uint64_t __bswapdi2(uint64_t);
//
// Reverse all the bytes in a 64-bit integer.
//
	.p2align 2
DEFINE_COMPILERRT_FUNCTION(__bswapdi2)
    rev r2, r0  // r2 = rev(r0)
    rev r0, r1  // r0 = rev(r1)
    mov r1, r2  // r1 = r2 = rev(r0)
    JMP(lr)
END_COMPILERRT_FUNCTION(__bswapdi2)
