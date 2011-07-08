/**
 * \file
 * \brief simulator control pseudo-instructions
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_SIMCTRL_H
#define ARCH_BEEHIVE_SIMCTRL_H

#define BEE_SIMCTRL_NOP 0
#define BEE_SIMCTRL_TERMINATE 1
#define BEE_SIMCTRL_TRACEON 2
#define BEE_SIMCTRL_TRACEOFF 3
#define BEE_SIMCTRL_REGISTERS 4
#define BEE_SIMCTRL_ILLEGAL 5
#define BEE_SIMCTRL_NOSEQTRACE 6
#define BEE_SIMCTRL_ADDRCHECK 7
#define BEE_SIMCTRL_CACHE_DELTA 8
#define BEE_SIMCTRL_CACHE_STAT 9
#define BEE_SIMCTRL_BREAK_SIM 10
#define BEE_SIMCTRL_TRACE 11
#define BEE_SIMCTRL_HALT 12

#ifndef __ASSEMBLER__
#include <sys/cdefs.h>

#define BEE_SIMCTRL(_s) \
    __asm volatile("simctrl " __XSTRING(_s) ";")

#endif // __ASSEMBLER__

#endif // ARCH_BEEHIVE_SIMCTRL_H
