/**
 * \file
 * \brief Kernel control block declarations.
 */

/*
 * Copyright (c) 2013, 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KCB_H
#define KCB_H

#include <kernel.h>
#include <capabilities.h>
#include <irq.h>
#include <mdb/mdb_tree.h>

struct cte;
struct dcb;

enum sched_state {
    SCHED_RR,
    SCHED_RBED,
};

/**
 * this is the memory layout of ObjType_KernelControlBlock
 * this struct should contain all the persistent state that belongs to a
 * kernel.
 */
struct kcb {
    bool is_valid; ///< kcb has been initialized by a kernel before

    /// kcb scheduling ring.
    /// These fields point to the next and previous kcb that should be
    /// scheduled when we're running multiple kcbs on the same kernel.
    /// invariant: next == NULL --> prev == NULL
    /// invariant: next is the next kcb in the ring and prev is the previous
    ///            kcb in the ring
    struct kcb *next, *prev;

    /// mdb root node
    lvaddr_t mdb_root;
    // XXX: need memory for a rootcn here because we can't have it static in
    // the kernel data section anymore
    struct cte init_rootcn;

    /// which scheduler state is valid
    enum sched_state sched;
    /// RR scheduler state
    struct dcb *ring_current;
    /// RBED scheduler state
    struct dcb *queue_head, *queue_tail;
    unsigned int u_hrt, u_srt, w_be, n_be;
    /// current time since kernel start in timeslices. This is necessary to
    /// make the scheduler work correctly
    /// wakeup queue head
    struct dcb *wakeup_queue_head;
    /// last value of kernel_now before shutdown/migration
    //needs to be signed because it's possible to migrate a kcb onto a cpu
    //driver whose kernel_now > this kcb's kernel_off.
    int64_t kernel_off;

    struct cte irq_dispatch[NDISPATCH];
    // TODO: maybe add a shared part which can replace struct core_data?
};

///< The kernel control block
extern struct kcb *kcb_current;
///< flag that indicates whether kcb scheduling should happen
extern bool kcb_sched_suspended;

static inline void print_kcb(void)
{
    printk(LOG_DEBUG, "kcb contents:\n");
    printk(LOG_DEBUG, "  next = %p, prev = %p\n",
            kcb_current->next, kcb_current->prev);
    printk(LOG_DEBUG, "  mdb_root = 0x%"PRIxLVADDR"\n", kcb_current->mdb_root);
    printk(LOG_DEBUG, "  queue_head = %p\n", kcb_current->queue_head);
    printk(LOG_DEBUG, "  queue_tail = %p\n", kcb_current->queue_tail);
    printk(LOG_DEBUG, "  wakeup_queue_head = %p\n", kcb_current->wakeup_queue_head);
    printk(LOG_DEBUG, "  u_hrt = %u, u_srt = %u, w_be = %u, n_be = %u\n",
            kcb_current->u_hrt, kcb_current->u_srt, kcb_current->w_be,
            kcb_current->n_be);
    // TODO interrupt state
}

// XXX: this is from RBED, don't know how to properly have this here -SG
extern struct dcb *queue_tail;
static inline void switch_kcb(struct kcb *next)
{
    assert (next != NULL);
    kcb_current = next;
    mdb_init(kcb_current);
    // update queue tail to make associated assembly not choke
    queue_tail = kcb_current->queue_tail;
}

void kcb_add(struct kcb* new_kcb);
errval_t kcb_remove(struct kcb *to_remove);
void kcb_update_core_id(struct kcb *kcb);

#endif
