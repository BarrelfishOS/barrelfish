/** \file
 * \brief Miscellaneous kernel support code.
 *
 * This file contains miscellaneous architecture-independent kernel support
 * code that doesn't belong anywhere else.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish_kpi/cpu.h>
#include <exec.h>
#include <misc.h>
#include <dispatch.h>
#include <trace/trace.h>

/**
 * 'true' if kernel should handle and context switch on timer ticks.
 * Pass the ticks parameter on the kernel command line if you
 * want to change this.
 */
bool kernel_ticks_enabled = true;

void
wait_cycles(uint64_t duration)
{
    uint64_t last, elapsed;

    printk(LOG_NOTE, "Waiting %" PRIu64 " cycles...\n", duration);

    last = arch_get_cycle_count();
    elapsed = 0;
    while (elapsed < duration) {
        uint64_t now = arch_get_cycle_count();
        elapsed += (now - last);
        last = now;
    }
}

/**
 * Stack protection handler
 */
void __stack_chk_fail(void); // Existence implied by (certainly configured) GCC.

void __stack_chk_fail (void)
{
    panic("finally reached __stack_chk_fail()");
}
