/**
 * \file
 * \brief Beehive intra-kernel prototypes etc.
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_BEEHIVE_BEEKERNEL_H
#define KERNEL_ARCH_BEEHIVE_BEEKERNEL_H

#define HANDLE_TIMER_HOW_SYNC 0
#define HANDLE_TIMER_HOW_SYSCALL 1
#define HANDLE_TIMER_HOW_RESUME 2
#define HANDLE_TIMER_HOW_EXECUTE 3

#ifndef __ASSEMBLER__

#include <barrelfish_kpi/types.h> // coreid_t

/**
 * \brief Return ID of current core 
 *
 */

static inline coreid_t arch_get_core_id(void)
{
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;
 
    return (val >> 10) & 0xf;
}

/**
 * \brief Returns ID of the Ethernet core, one more than
 * last regular core.
 *
 */

static inline coreid_t arch_get_max_core_id(void)
{
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;
    return (val >> 14) & 0xf;
}

/**
 * \brief Returns read of identity register
 *
 */

static inline uint32_t arch_read_identity_register(void)
{
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;
    return val;
}

/**
 * \brief returns non zero if running on simulator
 *
 */

#ifndef LIBBARRELFISH_TRACE_H

static inline int arch_is_simulator(void)
{
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;

    val >>= 18;
    val &= 0x7f;
    return val == 2 ? 1 : 0;
}

#endif // LIBBARRELFISH_TRACE_H

#endif // __ASSEMBLER__

#endif // KERNEL_ARCH_BEEHIVE_BEEKERNEL_H
