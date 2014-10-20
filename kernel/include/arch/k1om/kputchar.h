/**
 * \file
 * \brief A struct for all shared data between the kernels
 */

/*
 * Copyright (c) 2008, 2010, ETH Zurich/
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <serial.h>
#include <arch/x86/global.h>

#define kprintf_begin()  acquire_spinlock(&global->locks.print)
#define kprintf_end()    release_spinlock(&global->locks.print)


/* send all output to both VGA console and serial port, for now */
static inline int
kputchar(int c)
{
    serial_console_putchar(c);
    return c;
}
