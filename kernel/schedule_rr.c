/**
 * \file
 * \brief Kernel round-robin scheduling policy
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <kcb.h>

#include <timer.h> // update_sched_timer

/**
 * \brief Scheduler policy.
 *
 * \return Next DCB to schedule or NULL if wait for interrupts.
 */
struct dcb *schedule(void)
{
    // empty ring
    if(kcb_current->ring_current == NULL) {
        return NULL;
    }

    assert(kcb_current->ring_current->next != NULL);
    assert(kcb_current->ring_current->prev != NULL);

    kcb_current->ring_current = kcb_current->ring_current->next;
    #ifdef CONFIG_ONESHOT_TIMER
    update_sched_timer(kernel_now + kernel_timeslice);
    #endif
    return ring_current;
}

void make_runnable(struct dcb *dcb)
{
    // Insert into schedule ring if not in there already
    if(dcb->prev == NULL || dcb->next == NULL) {
        assert(dcb->prev == NULL && dcb->next == NULL);

        // Ring empty
        if(kcb_current->ring_current == NULL) {
            kcb_current->ring_current = dcb;
            dcb->next = dcb;
        }

        // Insert after current ring position
        dcb->prev = kcb_current->ring_current;
        dcb->next = kcb_current->ring_current->next;
        kcb_current->ring_current->next->prev = dcb;
        kcb_current->ring_current->next = dcb;
    }
}

/**
 * \brief Remove 'dcb' from scheduler ring.
 *
 * Removes dispatcher 'dcb' from the scheduler ring. If it was not in
 * the ring, this function is a no-op. The postcondition for this
 * function is that dcb is not in the ring.
 *
 * \param dcb   Pointer to DCB to remove.
 */
void scheduler_remove(struct dcb *dcb)
{
    // No-op if not in scheduler ring
    if(dcb->prev == NULL || dcb->next == NULL) {
        assert(dcb->prev == NULL && dcb->next == NULL);
        return;
    }

    struct dcb *next = kcb_current->ring_current->next;

    // Remove dcb from scheduler ring
    dcb->prev->next = dcb->next;
    dcb->next->prev = dcb->prev;
    dcb->prev = dcb->next = NULL;

    // Removing ring_current
    if(dcb == kcb_current->ring_current) {
        if(dcb == next) {
            // Only guy in the ring
            kcb_current->ring_current = NULL;
        } else {
            // Advance ring_current
            kcb_current->ring_current = next;
        }
    }
}

/**
 * \brief Yield 'dcb' for the rest of the current timeslice.
 *
 * Re-sorts 'dcb' into the scheduler queue with its release time increased by
 * the timeslice period. It is an error to yield a dispatcher not in the
 * scheduler queue.
 *
 * \param dcb   Pointer to DCB to remove.
 */
void scheduler_yield(struct dcb *dcb)
{
    if(dcb->prev == NULL || dcb->next == NULL) {
        struct dispatcher_shared_generic *dsg =
            get_dispatcher_shared_generic(dcb->disp);
        panic("Yield of %.*s not in scheduler queue", DISP_NAME_LEN,
              dsg->name);
    }

    // No-op for the round-robin scheduler
}

void scheduler_reset_time(void)
{
    // No-Op in RR scheduler
}

void scheduler_convert(void)
{
    enum sched_state from = kcb_current->sched;
    switch (from) {
        case SCHED_RBED:
        {
            // initialize RR ring
            struct dcb *last = NULL;
            for (struct dcb *i = kcb_current->queue_head; i; i = i->next)
            {
                i->prev = last;
                last = i;
            }
            // at this point: we have a dll, but need to close the ring
            kcb_current->queue_head->prev = kcb_current->queue_tail;
            kcb_current->queue_tail->next = kcb_current->queue_head;
            break;
        }
        case SCHED_RR:
            // do nothing
            break;
        default:
            printf("don't know how to convert %d to RBED state\n", from);
            break;
    }
    kcb_current->ring_current = kcb_current->queue_head;
    for (struct dcb *i = kcb_current->ring_current; i != kcb_current->ring_current; i=i->next) {
        printf("dcb %p\n  prev=%p\n  next=%p\n", i, i->prev, i->next);
    }
}

void scheduler_restore_state(void)
{
    // No-Op in RR scheduler
}
