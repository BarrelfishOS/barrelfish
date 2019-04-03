/**
 * \file
 * \brief Kernel scheduling policy: Rate-Based Earliest Deadline (RBED)
 *
 * The algorithm is described in the paper "Dynamic Integrated
 * Scheduling of Hard Real-Time, Soft Real-Time and Non-Real-Time
 * Processes" by Scott A. Brandt of UC Santa Cruz.
 *
 * Note that while in the paper real number arithmetic is used on some
 * variables, we employ fixed-point integer arithmetic within #SPECTRUM
 * in these cases.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/**
 * Implementation Notes
 *
 * real-time tasks:
 *  the behaviour of real-time tasks is characterized by four parameters: wcet
 *  (worst case execution time), period, (relative) deadline, and
 *  release_time.  Besides release_time, the values of these parameters are
 *  not changed by the scheduler. RT tasks are considered to be periodic. Note
 *  that the interpretation of the parameters is a little different than the
 *  original RBED paper.
 *
 *  ->release_time is the time that the task is ready to be scheduled. RT tasks
 *  with ->release_time in the future are not effectively considered to be on
 *  the runqueue and are ignored by the scheduler.  In order to meet its
 *  deadline the task needs to be scheduled no later than ->release_time +
 *  deadline - wcet. EDF guarantees this property, as long as the utilization
 *  rate is <= 1.
 *
 *  The execution of an rt task for a particular period ends either when: (a)
 *  the task runs outs of budget (->etime >= ->wcet), or (b) the task yields
 *  using scheduler_yield(). When this happens the task's ->etime is reset to
 *  0, while ->release_time is increased by ->period. Note that
 *  scheduler_remove() does not finalize the current period of the task. Also,
 *  note that depending on ->period, there might be a case that an rt task is
 *  not executed, even if the CPU is idle.
 *
 * best-effort tasks:
 *  for best-effort tasks the scheduler is responsible to assigns proper values
 *  the RT parameters. Also, To prioritize between BE tasks, the scheduler uses
 *  ->weight.
 */

#include <limits.h>
#ifndef SCHEDULER_SIMULATOR
#       include <kernel.h>
#       include <dispatch.h>
#       include <trace/trace.h>
#       include <trace_definitions/trace_defs.h>
#       include <timer.h> // update_sched_timer
#       include <kcb.h>
#include <systime.h>
#endif

#define SPECTRUM        1000000

/**
 * Minimum resource rate reserved for best-effort processes, in #SPECTRUM.
 * We set this to 10%.
 */
#define BETA            (SPECTRUM / 10)


// queue_tail has to be global, as its used from assembly
// this is always kept in sync with kcb_current->queue_tail
struct dcb *queue_tail = NULL;

/// Last (currently) scheduled task, for accounting purposes
static struct dcb *lastdisp = NULL;

/**
 * \brief Returns whether dcb is in scheduling queue.
 * \param dcb   Pointer to DCB to check.
 * \return True if in queue, false otherwise.
 */
static inline bool in_queue(struct dcb *dcb)
{
    return dcb->next != NULL || kcb_current->queue_tail == dcb;
}

static inline unsigned int u_target(struct dcb *dcb)
{
    return (dcb->wcet * SPECTRUM) / dcb->period;
}

static inline unsigned int u_actual_srt(struct dcb *dcb)
{
    if(u_target(dcb) != 0) {
        return MIN(u_target(dcb), (1 - BETA - kcb_current->u_hrt) / (kcb_current->u_srt / u_target(dcb)));
    } else {
        return 0;
    }
}

static inline systime_t deadline(struct dcb *dcb)
{
    return dcb->release_time + dcb->deadline;
}

static void queue_insert(struct dcb *dcb)
{
    // Empty queue case
    if(kcb_current->queue_head == NULL) {
        assert(kcb_current->queue_tail == NULL);
        kcb_current->queue_head = kcb_current->queue_tail = queue_tail = dcb;
        return;
    }

    /* Insert into priority queue (this is doing EDF). We insert at
     * the tail of a train of tasks with equal deadlines, as well as
     * equal release times for best-effort tasks, so that trains of
     * best-effort tasks with equal deadlines (and those released at
     * the same time) get scheduled in a round-robin fashion. The
     * release time equality check is important, as best-effort tasks
     * have lazily allocated deadlines. In some circumstances (like
     * when another task blocks), this might otherwise cause a wrong
     * yielding behavior when old deadlines are encountered.
     */
    struct dcb *prev = NULL;
    for(struct dcb *i = kcb_current->queue_head; i != NULL; prev = i, i = i->next) {
        // Skip over equal, smaller release times if best-effort
        if(dcb->type == TASK_TYPE_BEST_EFFORT &&
           dcb->release_time >= i->release_time) {
            continue;
        }

        // Skip over equal deadlines
        if(deadline(dcb) >= deadline(i)) {
            continue;
        }

        dcb->next = i;
        if(prev == NULL) {      // Insert before head
            kcb_current->queue_head = dcb;
        } else {                // Insert inside queue
            prev->next = dcb;
        }

        return;
    }

    // Insert after queue tail
    kcb_current->queue_tail->next = dcb;
    kcb_current->queue_tail = queue_tail = dcb;
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
static void queue_remove(struct dcb *dcb)
{
    // No-op if not in scheduler ring
    if(!in_queue(dcb)) {
        return;
    }

    if(dcb == kcb_current->queue_head) {
        kcb_current->queue_head = dcb->next;
        if(kcb_current->queue_head == NULL) {
            kcb_current->queue_tail = queue_tail =  NULL;
        }

        goto out;
    }

    for(struct dcb *i = kcb_current->queue_head; i != NULL; i = i->next) {
        if(i->next == dcb) {
            i->next = i->next->next;
            if(kcb_current->queue_tail == dcb) {
                kcb_current->queue_tail = queue_tail = i;
            }
            break;
        }
    }

 out:
    dcb->next = NULL;
}

#if 0
/**
 * \brief (Re-)Sort the scheduler priority queue.
 */
static void queue_sort(void)
{
 start_over:
    for(struct dcb *i = queue_head; i != NULL && i->next != NULL; i = i->next) {
        if(deadline(i) > deadline(i->next)) {
            // Gotta re-sort
            queue_remove(i);
            queue_insert(i);
            goto start_over;
        }
    }
}

static void queue_reset(void)
{
    for(struct dcb *i = queue_head; i != NULL; i = i->next) {
        if(i->type == TASK_TYPE_BEST_EFFORT) {
            i->release_time = kernel_now;
        }
    }
}
#endif

/**
 * \brief Allocates resources for tasks.
 *
 * \param dcb   Pointer to dcb to allocate resources for.
 *
 * \return u_actual for 'dcb' in percent.
 */
static unsigned int do_resource_allocation(struct dcb *dcb)
{
    unsigned int u_actual;

    switch(dcb->type) {
    case TASK_TYPE_HARD_REALTIME:
        u_actual = u_target(dcb);
        break;

    case TASK_TYPE_SOFT_REALTIME:
        u_actual = u_actual_srt(dcb);
        break;

    case TASK_TYPE_BEST_EFFORT:
        assert(kcb_current->w_be > 0 && kcb_current->n_be > 0);
        assert(dcb->weight < UINT_MAX / SPECTRUM);
        u_actual = (MAX(BETA, SPECTRUM - kcb_current->u_hrt - kcb_current->u_srt) * dcb->weight) / kcb_current->w_be;
        dcb->deadline = dcb->period = kcb_current->n_be * kernel_timeslice;
        break;

    default:
        panic("Unknown task type %d!", dcb->type);
        break;
    }

    return u_actual;
}

#if 0
// XXX: Don't understand this yet
static void adjust_weights(void)
{
    // No runnable best-effort tasks have a positive weight
    if(w_be == 0) {
        // Re-assign weights
        for(struct dcb *i = queue_head; i != NULL; i = i->next) {
            if(i->type != TASK_TYPE_BEST_EFFORT) {
                continue;
            }

            i->weight = 1;
            w_be++;
        }
    }
}
#endif

static void set_best_effort_wcet(struct dcb *dcb)
{
    unsigned int u_actual = do_resource_allocation(dcb);
    systime_t wcet_undiv = (kcb_current->n_be * kernel_timeslice * u_actual);

    // Assert we are never overloaded
    assert(kcb_current->u_hrt + kcb_current->u_srt + u_actual <= SPECTRUM);

    // Divide with proper rounding
    dcb->wcet = (wcet_undiv + SPECTRUM / 2) / SPECTRUM;
}

/**
 * \brief Scheduler policy.
 *
 * \return Next DCB to schedule or NULL if wait for interrupts.
 */
struct dcb *schedule(void)
{
    struct dcb *todisp;
    systime_t now = systime_now();

    // Assert we are never overloaded
    assert(kcb_current->u_hrt + kcb_current->u_srt + BETA <= SPECTRUM);

    // Update executed time of last dispatched task
    if(lastdisp != NULL) {
        assert(lastdisp->last_dispatch <= now);
        if(lastdisp->release_time <= now) {
            lastdisp->etime += now -
                MAX(lastdisp->last_dispatch, lastdisp->release_time);
        }
    }

 start_over:
    todisp = kcb_current->queue_head;

#ifndef SCHEDULER_SIMULATOR
#define PRINT_NAME(d) \
    do { \
        if (!(d) || !(d)->disp) { \
            debug(SUBSYS_DISPATCH, "todisp == NULL\n"); \
            break; \
        } \
        struct dispatcher_shared_generic *dst = \
            get_dispatcher_shared_generic(d->disp); \
        debug(SUBSYS_DISPATCH, "looking at '%s', release_time=%lu, kernel_now=%zu\n", \
                dst->name, d->release_time, now); \
    }while(0)
#else
#define PRINT_NAME(d) do{}while(0)
#endif

    // Skip over all tasks released in the future, they're technically not
    // in the schedule yet. We just have them to reduce book-keeping.
    while(todisp != NULL && todisp->release_time > now) {
        PRINT_NAME(todisp);
        todisp = todisp->next;
    }
#undef PRINT_NAME

    // nothing to dispatch
    if(todisp == NULL) {
#ifndef SCHEDULER_SIMULATOR
        debug(SUBSYS_DISPATCH, "schedule: no dcb runnable\n");
#endif
        lastdisp = NULL;
        return NULL;
    }

    // Lazy resource allocation for best-effort processes
    if(todisp->type == TASK_TYPE_BEST_EFFORT) {
        set_best_effort_wcet(todisp);

        /* We might've shortened the deadline into the past (eg. when
         * another BE task was removed while we already ran well into
         * our timeslice). In that case we need to re-release.
         */
        if(deadline(todisp) < now) {
            todisp->release_time = now;
        }
    }

    // Assert we never miss a hard deadline
    if(todisp->type == TASK_TYPE_HARD_REALTIME && now > deadline(todisp)) {
        panic("Missed hard deadline: now = %zu, deadline = %lu", now,
              deadline(todisp));
        assert(false && "HRT task missed a dead line!");
    }

    // Deadline's can't be in the past (or EDF wouldn't work properly)
    assert(deadline(todisp) >= now);

    // Dispatch first guy in schedule if not over budget
    if(todisp->etime < todisp->wcet) {
        todisp->last_dispatch = now;

        // If nothing changed, run whatever ran last (task might have
        // yielded to another), unless it is blocked
        if(lastdisp == todisp && dcb_current != NULL && in_queue(dcb_current)) {
            /* trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_CURRENT, */
            /*             (uint32_t)(lvaddr_t)dcb_current & 0xFFFFFFFF); */
            return dcb_current;
        }

        /* trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_SCHEDULE, */
        /*             (uint32_t)(lvaddr_t)todisp & 0xFFFFFFFF); */

        // Remember who we run next
        lastdisp = todisp;
        #ifdef CONFIG_ONESHOT_TIMER
        // we might be able to do better than that...
        // (e.g., check if there is only one task in the queue)
        update_sched_timer(now + (todisp->wcet - todisp->etime));
        #endif
        return todisp;
    }

    /* we selected a task that is over budget. do the necessary bookkeeping, put
     * it back on the queue and re-select a task */

    // Best-effort task consumed WCET
    // XXX: Don't understand this yet
#if 0
    if(todisp->type == TASK_TYPE_BEST_EFFORT) {
        w_be -= todisp->weight;
        todisp->weight = 0;
        adjust_weights();
    }
#endif

    // Update periodic task and re-sort into run-queue
    struct dcb *dcb = todisp;
    queue_remove(todisp);
    if(dcb->type != TASK_TYPE_BEST_EFFORT) {
        if(now > dcb->release_time) {
            dcb->release_time += dcb->period;
        }
    } else {
        dcb->release_time = now;
    }
    dcb->etime = 0;
    queue_insert(dcb);
    lastdisp = NULL;

    goto start_over;
}

void schedule_now(struct dcb *dcb)
{
    systime_t now = systime_now();
    if (dcb->release_time >= now) {
        dcb->release_time = now;
    }
    dcb->deadline = 1;
}

void make_runnable(struct dcb *dcb)
{
    systime_t now = systime_now();

    // No-Op if already in schedule
    if(in_queue(dcb)) {
        return;
    }

    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_MAKE_RUNNABLE,
                (uint32_t)(lvaddr_t)dcb & 0xFFFFFFFF);

    // Keep counters up to date
    switch(dcb->type) {
    case TASK_TYPE_BEST_EFFORT:
        if(dcb->weight == 0) {
            dcb->weight = 1;
        } else {
            // XXX: Don't understand this yet
#if 0
            // Give blocked processes a boost
            dcb->weight = dcb->weight / 2 + 6;
#endif
        }
        kcb_current->w_be += dcb->weight;
        kcb_current->n_be++;
        dcb->deadline = dcb->period = kcb_current->n_be * kernel_timeslice;
        dcb->release_time = now;
        /* queue_sort(); */
        break;

    case TASK_TYPE_SOFT_REALTIME:
        panic("Unimplemented!");
        break;

    case TASK_TYPE_HARD_REALTIME:
        kcb_current->u_hrt += u_target(dcb);
        break;

    default:
        panic("Unknown task type %d", dcb->type);
        break;
    }

    // Never overload the scheduler
    if(kcb_current->u_hrt + kcb_current->u_srt + BETA > SPECTRUM) {
        panic("RBED scheduler overload (loaded %d%%)!",
              (kcb_current->u_hrt + kcb_current->u_srt + BETA) / (SPECTRUM / 100));
    }

    if(dcb->release_time < now) {
        panic("Released in the past! now = %zu, release_time = %lu\n",
              now, dcb->release_time);
    }
    /* assert(dcb->release_time >= kernel_now); */
    dcb->etime = 0;
    queue_insert(dcb);
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
    // No-Op if not in schedule
    if(!in_queue(dcb)) {
        return;
    }

    queue_remove(dcb);

    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_REMOVE,
                (uint32_t)(lvaddr_t)dcb & 0xFFFFFFFF);

    // Update counters
    switch(dcb->type) {
    case TASK_TYPE_BEST_EFFORT:
        kcb_current->w_be -= dcb->weight;
        kcb_current->n_be--;
        /* queue_sort(); */
        /* adjust_weights(); */
        break;

    case TASK_TYPE_SOFT_REALTIME:
        panic("Unimplemented!");
        break;

    case TASK_TYPE_HARD_REALTIME:
        kcb_current->u_hrt -= u_target(dcb);
        break;
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
    systime_t now = systime_now();
    // For tasks not running yet, yield is a no-op
    if(!in_queue(dcb) || dcb->release_time > now) {
        return;
    }

    /* trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_YIELD, */
    /*             (uint32_t)(lvaddr_t)dcb & 0xFFFFFFFF); */

    queue_remove(dcb);
    switch(dcb->type) {
    case TASK_TYPE_HARD_REALTIME:
    case TASK_TYPE_SOFT_REALTIME:
        dcb->release_time += dcb->period;
        break;

    case TASK_TYPE_BEST_EFFORT:
        // Shuffle us around one time
        dcb->release_time = now;
        break;
    }
    dcb->etime = 0;
    lastdisp = NULL;    // Don't account for us anymore
    queue_insert(dcb);
}

#ifndef SCHEDULER_SIMULATOR
void scheduler_reset_time(void)
{
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_TIMER_SYNC, 0);

    // XXX: Currently, we just re-release everything now
    struct kcb *k = kcb_current;
    do {
        printk(LOG_NOTE, "clearing kcb %p\n", k);
        for(struct dcb *i = k->queue_head; i != NULL; i = i->next) {
            i->release_time = 0;
            i->etime = 0;
            i->last_dispatch = 0;
        }
        k = k->next;
    }while(k && k!=kcb_current);

    // Forget all accounting information
    lastdisp = NULL;
}

void scheduler_convert(void)
{
    enum sched_state from = kcb_current->sched;
    switch (from) {
        case SCHED_RBED:
            // do nothing
            break;
        case SCHED_RR:
        {
            // initialize RBED fields
            // make all tasks best effort
            struct dcb *tmp = NULL;
            printf("kcb_current: %p\n", kcb_current);
            printf("kcb_current->ring_current: %p\n", kcb_current->ring_current);
            printf("kcb_current->ring_current->prev: %p\n", kcb_current->ring_current->prev);
            struct dcb *i = kcb_current->ring_current;
            do {
                printf("converting %p\n", i);
                i->type = TASK_TYPE_BEST_EFFORT;
                tmp = i->next;
                i->next = i->prev = NULL;
                make_runnable(i);
                i = tmp;
            } while (i != kcb_current->ring_current);
            for (i = kcb_current->queue_head; i; i=i->next) {
                printf("%p (-> %p)\n", i, i->next);
            }
            break;
        }
        default:
            printf("don't know how to convert %d to RBED state\n", from);
            break;
    }
}

void scheduler_restore_state(void)
{
    // clear time slices
    scheduler_reset_time();
}
#endif
