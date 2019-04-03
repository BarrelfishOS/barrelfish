/**
 * \file
 * \brief Kernel scheduling API
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_SCHEDULE_H
#define KERNEL_SCHEDULE_H

/* Return the DCB to dispatch. */
struct dcb *schedule(void);

/* Unblock a dispatcher - insert into scheduler queue. */
/* scheduler_add() */
void make_runnable(struct dcb *dcb);

/* Break RBED by setting release time of this DCB to *now*. */
/* schedule(r) */
void schedule_now(struct dcb *dcb);

/**
 * \brief Remove 'dcb' from scheduler ring.
 *
 * Removes dispatcher 'dcb' from the scheduler ring. If it was not in
 * the ring, this function is a no-op. The postcondition for this
 * function is that dcb is not in the ring.
 *
 * \param dcb   Pointer to DCB to remove.

 * Opposite of make_runnable.
 */
void scheduler_remove(struct dcb *dcb);

/* Yield. */
void scheduler_yield(struct dcb *dcb);

/* Coreboot stuff from here on. */

/* Kernel has rebooted, start scheduling from scratch. */
void scheduler_reset_time(void);

/* Convert scheduler parameters to the current scheduler for the whole KCB. */
void scheduler_convert(void);

/* Necessary? Calls reset_time. */
void scheduler_restore_state(void);

#endif
