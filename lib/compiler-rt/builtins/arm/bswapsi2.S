//===------- bswapsi2 - Implement bswapsi2 --------------------------------===//
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
// extern uint32_t __bswapsi2(uint32_t);
//
// Reverse all the bytes in a 32-bit integer.
//
	.p2align 2
DEFINE_COMPILERRT_FUNCTION(__bswapsi2)
    rev r0, r0
    JMP(lr)
END_COMPILERRT_FUNCTION(__bswapsi2)
