/**
 * \file
 * \brief User-side system call implementation
 */

/*
 * Copyright (c) 2007-2016, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_SYSCALL_H
#define ARCH_AARCH64_BARRELFISH_SYSCALL_H

#include <barrelfish_kpi/syscalls.h>  // for struct sysret.

/**
 * \brief the actual syscall function
 *
 * the arguments are left in the registers x0-x11
 * the return value is stored in x0 and x1 when returning from the syscall
 */
struct sysret
syscall(uint64_t num, uint64_t arg1, uint64_t arg2, uint64_t arg3,
        uint64_t arg4, uint64_t arg5, uint64_t arg6, uint64_t arg7,
        uint64_t arg8, uint64_t arg9, uint64_t arg10, uint64_t arg11);


//
// System call argument 0 is encoded thus:
//
// arg[3:0]  = syscall ordinal (e.g. SYSCALL_YIELD)
// arg[7:4]  = number of system call arguments (for sanity checking)
// arg[31:8] = SYSCALL_INVOKE arguments | do_not_care
//

//C_ASSERT(SYSCALL_COUNT <= 0xf);
#define sysord(a,n) (a) | ((n) << 4)

// The following macros add the argument count to arg0

#define syscall12(a,b,c,d,e,f,g,h,i,j,k,l)                              \
    syscall(sysord(a,12),(b),(c),(d),(e),(f),(g),(h),(i),(j),(k),(l))

#define syscall11(a,b,c,d,e,f,g,h,i,j,k)                                \
    syscall(sysord(a,11),(b),(c),(d),(e),(f),(g),(h),(i),(j),(k),0)

#define syscall10(a,b,c,d,e,f,g,h,i,j)                                  \
    syscall(sysord(a,10),(b),(c),(d),(e),(f),(g),(h),(i),(j),0,0)

#define syscall9(a,b,c,d,e,f,g,h,i)                                     \
    syscall(sysord(a,9),(b),(c),(d),(e),(f),(g),(h),(i),0,0,0)

#define syscall8(a,b,c,d,e,f,g,h)                                       \
    syscall(sysord(a,8),(b),(c),(d),(e),(f),(g),(h),0,0,0,0)

#define syscall7(a,b,c,d,e,f,g)                                         \
    syscall(sysord(a,7),(b),(c),(d),(e),(f),(g),0,0,0,0,0)

#define syscall6(a,b,c,d,e,f)                                           \
    syscall(sysord(a,6),(b),(c),(d),(e),(f),0,0,0,0,0,0)

#define syscall5(a,b,c,d,e)                                             \
    syscall(sysord(a,5),(b),(c),(d),(e),0,0,0,0,0,0,0)

#define syscall4(a,b,c,d)                                               \
    syscall(sysord(a,4),(b),(c),(d),0,0,0,0,0,0,0,0)

#define syscall3(a,b,c)                                                 \
    syscall(sysord(a,3),(b),(c),0,0,0,0,0,0,0,0,0)

#define syscall2(a,b)                                                   \
    syscall(sysord(a,2),(b),0,0,0,0,0,0,0,0,0,0)

#define syscall1(a)                                                     \
    syscall(sysord(a,1),0,0,0,0,0,0,0,0,0,0,0)

#endif
