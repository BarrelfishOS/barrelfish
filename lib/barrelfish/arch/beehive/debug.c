/**
 * \file
 * \brief Arch specific debugging functions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/debug.h>
#include <barrelfish/dispatch.h>
#include <if/monitor_defs.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#define NR_OF_DISPLAYED_RET_ADDRS   10

/**
 * \brief Dump out various memory regions and a partial backtrace.
 *
 * Mainly for debugging traps and faults in the dispatcher handlers.
 */
void debug_dump(arch_registers_state_t *archregs)
{
    /* NYI */
}

static void debug_call_chain_rbp(uintptr_t bp)
{
    uintptr_t ret_addr;
    uintptr_t user_rbp = bp;

    for (int it = 0; it < NR_OF_DISPLAYED_RET_ADDRS; it++) {
        if (user_rbp < BASE_PAGE_SIZE || (user_rbp % sizeof(uintptr_t)) != 0) {
            break;
        }
	ret_addr = ((uintptr_t *)user_rbp)[1];
	debug_printf("return address = 0x%" PRIxPTR " frame address = 0x%" PRIxPTR "\n",
		     ret_addr, user_rbp);
	user_rbp = ((uintptr_t *)user_rbp)[0];
    }
}

void debug_call_chain(arch_registers_state_t *archregs)
{
    // TODO: use regs argument
    register uintptr_t fp;
    fp = (uintptr_t)__builtin_frame_address(0);
    debug_printf("current fp = 0x%" PRIxPTR "\n", fp);
    debug_call_chain_rbp(fp);
}

/**
 * \brief Print out the registers in a dispatcher save area, for trap handlers.
 */
void debug_print_save_area(arch_registers_state_t *state)
{
    /* NYI */
}

void debug_return_addresses(void)
{
    debug_printf("return address = %p\n", __builtin_return_address(0));
    debug_printf("return address = %p\n", __builtin_return_address(1));
    debug_printf("return address = %p\n", __builtin_return_address(2));
    debug_printf("return address = %p\n", __builtin_return_address(3));
    debug_printf("return address = %p\n", __builtin_return_address(4));
}
