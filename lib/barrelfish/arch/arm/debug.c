/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/debug.h>
#include <barrelfish/dispatch.h>

void debug_dump(arch_registers_state_t *archregs)
{
#define dpr(reg) debug_printf("%-6s 0x%08"PRIx32 "\n", #reg, archregs->named. reg)
#define dpd(reg) debug_printf("%-6s 0x%016"PRIx64 "\n", #reg, archregs->named. reg)
    dpr(r0);    dpr(r1);        dpr(r2);        dpr(r3);
    dpr(r4);    dpr(r5);        dpr(r6);        dpr(r7);
    dpr(r9);    dpr(r10);       dpr(r11);       dpr(r12);
    dpr(stack); dpr(link);      dpr(pc);        dpr(cpsr);
    dpd(d0);    dpd(d1);        dpd(d2);        dpd(d3);
    dpd(d4);    dpd(d5);        dpd(d6);        dpd(d7);
    dpd(d8);    dpd(d9);        dpd(d10);       dpd(d11);
    dpd(d12);   dpd(d13);       dpd(d14);       dpd(d15);
    dpd(d16);   dpd(d17);       dpd(d18);       dpd(d19);
    dpd(d20);   dpd(d21);       dpd(d22);       dpd(d23);
    dpd(d24);   dpd(d25);       dpd(d26);       dpd(d27);
    dpd(d28);   dpd(d29);       dpd(d30);       dpd(d31);
    dpr(fpscr);
}

void debug_call_chain(arch_registers_state_t *archregs)
{
    // TODO: use regs argument
    void* fp = __builtin_frame_address(0);
    void* ra = __builtin_return_address(0);
    if (fp != NULL) {
        debug_printf("%8d frame %p return %p\n", 0, fp, ra);
    }
}

void debug_print_save_area(arch_registers_state_t *state)
{
    debug_dump(state);
}
