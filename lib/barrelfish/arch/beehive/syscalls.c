/**
 * \file
 * \brief User-side system call implementation
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/syscall_arch.h>

/* For documentation on system calls see include/barrelfish/syscalls.h
 */

#error "you should have compiled syscall.S instead"

errval_t sys_yield(capaddr_t target)
{
    // This is painful, perhaps we should just write it all in assembler
    struct sysret retval;
    __asm volatile("ld r1,%0\n\t"
		   "ld r3,%1\n\t"
		   "ld t1," __XSTRING(SYSCALL_YIELD) "\n\t"
		   "long_call 0x1009"
		   : /* no outputs */
		   : "r" (&retval), "r" (target)
		   : /* clobbers */ "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "t1", "t2", "t3", "vb", "link", "cc");
    return retval.error;
    // return syscall(SYSCALL_YIELD, target).error;
}

errval_t sys_print(const char *string, size_t length)
{
    // This is painful, perhaps we should just write it all in assembler
    struct sysret retval;
    __asm volatile("ld r1,%0\n\t"
		   "ld r3,%1\n\t"
		   "ld r4,%2\n\t"
		   "ld t1," __XSTRING(SYSCALL_PRINT) "\n\t"
		   "long_call 0x1009"
		   : /* no outputs */
		   : "r" (&retval), "r" (string), "r" (length)
		   : /* clobbers */ "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "t1", "t2", "t3", "vb", "link", "cc");
    return retval.error;
    // return syscall(SYSCALL_PRINT, (uintptr_t)string, length).error;
}
