/**
 * \file
 * \brief User-side system call implementation
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_SYSCALL_H
#define ARCH_BEEHIVE_BARRELFISH_SYSCALL_H

#include <barrelfish/caddr.h> // for get_cap_valid_bits
#include <barrelfish_kpi/legacy_idc_buffer.h> // for struct idc_send_msg

// Implemented in assembler

#if 0 // these are already declared in include/barrelfish/syscalls.h
extern errval_t sys_yield(capaddr_t target);
extern errval_t sys_nop(void);
extern errval_t sys_print(const char *string, size_t length);
#endif

extern errval_t sys_invoke(uintptr_t *const pvalue,
			   uint8_t invokebits,
			   capaddr_t invokecptr,
			   struct idc_send_msg *const msg);


extern errval_t sys_rundown(void);

/*
 *  This must be here since it is relied on variously by
 *  incovations.h, monitor_invocations.h and lmp_chan_arch.h
 */

/**
 * \brief Invoke (send a message to) a capability, returning an error and a value
 *
 * \param to Capability to invoke
 * \param msg Message to send
 *
 * This is needed for some system calls that return a structure of two values.
 *
 * \returns System return structure (#sysret)
 */
static inline struct sysret
cap_invoke(struct capref to, struct idc_send_msg *msg)
{
    // TODO: XXX: This should have a typedef not a uint8_t
    uint8_t invokebits = get_cap_valid_bits(to);
    capaddr_t invokecptr = get_cap_addr(to) >> (CPTR_BITS - invokebits);

    struct sysret retval;
    retval.error = sys_invoke(&retval.value, invokebits, invokecptr, msg);
    return retval;
}


#endif // ARCH_BEEHIVE_BARRELFISH_SYSCALL_H
