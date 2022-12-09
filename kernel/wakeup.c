/**
 * \file
 * \brief DCB wakeup queue management
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <kcb.h> // kcb_current->wakeup_queue_head
#include <timer.h> // update_wakeup_timer()
#include <wakeup.h>
#include <systime.h>

/* wrapper to change the head, and update the next wakeup tick */
void wakeup_set_queue_head(struct dcb *h)
{
    kcb_current->wakeup_queue_head = h;
    #ifdef CONFIG_ONESHOT_TIMER
    // we changed the first dcb in the wakeup queue, which means
    // that we need to update the next tick value
    systime_t next_wakeup = h ? h->wakeup_time : TIMER_INF;
    update_wakeup_timer(next_wakeup);
    #endif
}
static inline void set_queue_head(struct dcb *h)
{
    wakeup_set_queue_head(h);
}

void wakeup_remove(struct dcb *dcb)
{
    if (dcb->wakeup_time != 0) {
        if (dcb->wakeup_prev == NULL) {
            assert(kcb_current->wakeup_queue_head == dcb);
            set_queue_head(dcb->wakeup_next);
        } else {
            assert(dcb->wakeup_prev->wakeup_next == dcb);
            dcb->wakeup_prev->wakeup_next = dcb->wakeup_next;
        }
        if (dcb->wakeup_next != NULL) {
            assert(dcb->wakeup_next->wakeup_prev == dcb);
            dcb->wakeup_next->wakeup_prev = dcb->wakeup_prev;
        }
        dcb->wakeup_prev = dcb->wakeup_next = NULL;
        dcb->wakeup_time = 0;
    }

    // No-Op if not in queue...
}

/// Set the wakeup time for the given DCB
void wakeup_set(struct dcb *dcb, systime_t waketime)
{
    assert(dcb != NULL);

    // if we're already enqueued, remove first
    wakeup_remove(dcb);

    dcb->wakeup_time = waketime;

    for (struct dcb *d = kcb_current->wakeup_queue_head, *p = NULL; ; p = d, d = d->wakeup_next) {
        if (d == NULL || d->wakeup_time > waketime) {
            if (p == NULL) { // insert at head
                assert(d == kcb_current->wakeup_queue_head);
                dcb->wakeup_prev = NULL;
                dcb->wakeup_next = d;
                if (d != NULL) {
                    d->wakeup_prev = dcb;
                }
                set_queue_head(dcb);
            } else {
                dcb->wakeup_next = d;
                dcb->wakeup_prev = p;
                p->wakeup_next = dcb;
                if (d != NULL) {
                    d->wakeup_prev = dcb;
                }
            }
            break;
        }
    }
}

/// Check for wakeups, given the current time
void wakeup_check(systime_t now)
{
    struct dcb *d = kcb_current->wakeup_queue_head, *next = NULL;
    for (; d != NULL && d->wakeup_time <= now; d = next) {
        next = d->wakeup_next;
        d->wakeup_time = 0;
        d->wakeup_prev = d->wakeup_next = NULL;
        make_runnable(d);
        schedule_now(d);
    }
    if (d != NULL) {
        d->wakeup_prev = NULL;
    }
    set_queue_head(d);
}

bool wakeup_is_pending(void)
{
    return kcb_current->wakeup_queue_head != NULL;
}
