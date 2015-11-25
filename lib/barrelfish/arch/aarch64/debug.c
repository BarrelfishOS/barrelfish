/*
 * Copyright (c) 2007-2009, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/debug.h>
#include <barrelfish/dispatch.h>

void debug_dump(arch_registers_state_t *archregs)
{
#define dpr(reg) debug_printf("%-6s 0x%08"PRIx32 "\n", #reg, archregs->named. reg)
    dpr(x0);    dpr(x1);  dpr(x2);   dpr(x3);
    dpr(x4);    dpr(x5);  dpr(x6);   dpr(x7);
    dpr(x8);    dpr(x9);  dpr(x10);  dpr(x11);
    dpr(x12);   dpr(x13); dpr(x14);  dpr(x15);
    dpr(x16);   dpr(x17); dpr(x18);  dpr(x19);
    dpr(x20);   dpr(x21); dpr(x22);  dpr(x23);
    dpr(x24);   dpr(x25); dpr(x26);  dpr(x27);
    dpr(x28);   dpr(x29); dpr(x30);

    dpr(stack); dpr(pc);  dpr(spsr);
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
