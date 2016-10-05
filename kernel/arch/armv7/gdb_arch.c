/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdbool.h>
#include <init.h>

extern void dbg_break(void);
void dbg_break(void)
{
#ifndef __thumb__
    __asm("bkpt #0xffff");
#else
    //heteropanda: smaller breakpoint immediate in pure thumb2
    __asm("bkpt #0xff");
#endif
}
