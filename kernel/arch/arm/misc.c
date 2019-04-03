/** \file
 * \brief Miscellaneous kernel support code.
 *
 * This file contains miscellaneous architecture-independent kernel support
 * code that doesn't belong anywhere else.
 */

/*
 * Copyright (c) 2007, 2008, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdarg.h>
#include <stdio.h>
#include <exec.h>
#include <misc.h>

/**
 * 'true' if kernel should handle and context switch on timer ticks.
 * Pass the ticks parameter on the kernel command line if you
 * want to change this.
 */
bool kernel_ticks_enabled = true;

/**
 * \brief Spin forever
 *
 */
void halt(void)
{
    while(1) __asm volatile("wfe");
}
