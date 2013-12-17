/**
 * \file
 * \brief Kernel control block declarations.
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KCB_H
#define KCB_H

#include <irq.h>
#include <dispatch.h>
struct dcb;

/**
 * this is the memory layout of ObjType_KernelControlBlock
 * this struct should contain all the persistent state that belongs to a
 * kernel.
 */
struct kcb {
    // mdb root node
    lvaddr_t mdb_root;

#if defined(CONFIG_SCHEDULER_RR)
    // RR scheduler state
    struct dcb *ring_current;
#elif defined(CONFIG_SCHEDULER_RBED)
    // RBED scheduler state
    struct dcb *queue_head, *queue_tail;
    struct dcb *last_disp;
#else
# error Must define a kernel scheduling policy!
#endif

#if defined(__x86_64__)
    bool idt_initialized; ///< iff true, IDT is initialized and exceptions can be caught
    struct gate_descriptor idt[NIDT] __attribute__ ((aligned (16)));
    struct cte irq_dispatch[NDISPATCH];
#endif
    // TODO: maybe add a shared part which can replace struct core_data?
};

///< The kernel control block
extern struct kcb *kcb;

#endif
