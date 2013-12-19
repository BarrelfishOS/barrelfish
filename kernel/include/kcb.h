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

#include <capabilities.h>
#include <irq.h>
struct cte;
struct dcb;

/**
 * this is the memory layout of ObjType_KernelControlBlock
 * this struct should contain all the persistent state that belongs to a
 * kernel.
 */
struct kcb {
    bool is_valid; ///< kcb has been initialized by a kernel before

    /// mdb root node
    lvaddr_t mdb_root;
    // XXX: need memory for a rootcn here because we can't have it static in
    // the kernel data section anymore
    struct cte init_rootcn;

    /// RR scheduler state
    struct dcb *ring_current;
    /// RBED scheduler state
    struct dcb *queue_head, *queue_tail;
    struct dcb *lastdisp;
    unsigned int u_hrt, u_srt, w_be, n_be;
    /// current time since kernel start in timeslices. This is necessary to
    /// make the scheduler work correctly
    size_t kernel_now;
    /// wakeup queue head
    struct dcb *wakeup_queue_head;

#if defined(__x86_64__)
    struct cte irq_dispatch[NDISPATCH];
#endif
    // TODO: maybe add a shared part which can replace struct core_data?
};

///< The kernel control block
extern struct kcb *kcb;

static inline void print_kcb(void)
{
    printf("kcb contents:\n");
    printf("  mdb_root = 0x%"PRIxLVADDR"\n", kcb->mdb_root);
    printf("  queue_head = %p\n", kcb->queue_head);
    printf("  queue_tail = %p\n", kcb->queue_tail);
    printf("  lastdisp = %p\n", kcb->lastdisp);
    printf("  wakeup_queue_head = %p\n", kcb->wakeup_queue_head);
    printf("  u_hrt = %u, u_srt = %u, w_be = %u, n_be = %u\n",
            kcb->u_hrt, kcb->u_srt, kcb->w_be, kcb->n_be);
    printf("  kernel_now = %zu\n", kcb->kernel_now);
    // TODO interrupt state
}


#endif
