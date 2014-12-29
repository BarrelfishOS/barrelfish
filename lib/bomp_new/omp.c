/**
 * \file
 * \brief OpenMP API implementation as defined in OpenMP Version 4.0
 *
 * Source: http://www.openmp.org/mp-documents/OpenMP4.0.0.pdf
 */

/*
 * Copyright (c)2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <bomp_internal.h>


/*
 * ===========================================================================
 * OpenMP 4.0 API
 * ===========================================================================
 */

/*
 * ---------------------------------------------------------------------------
 * 3.2 Execution Environment Routines
 * ---------------------------------------------------------------------------
 *
 * Execution environment routines affect and monitor threads, processors, and
 * the parallel environment. The library routines are external functions with
 * “C” linkage.
 */

/**
 * \brief Sets the number of threads to be used for parallel regions
 *
 * \param num_threads   the number of threads
 *
 * Affects the number of threads used for subsequent parallel regions not
 * specifying a num_threads clause, by setting the value of the first element of
 * the nthreads-var ICV of the current task to num_threads.
 */
void omp_set_num_threads(int num_threads)
{
    if (num_threads > 0) {
        if (num_threads > OMP_GET_ICV_GLOBAL(thread_limit)) {
            num_threads = OMP_GET_ICV_GLOBAL(thread_limit);
        }

        OMP_SET_ICV_TASK(nthreads, num_threads);
    }
}

/**
 * \brief returns the current number of threads used (innermost parallel region)
 *
 * \returns number of used threads
 *
 * Returns the number of threads in the current team. The binding region for an
 * omp_get_num_threads region is the innermost enclosing parallel region.
 * If called from the sequential part of a program, this routine returns 1.
 */
int omp_get_num_threads(void)
{
    /*
      struct gomp_team *team = gomp_thread ()->ts.team;
      return team ? team->nthreads : 1;

      XXX: we dont't have teams yet so we just return the number of threads
           participating in working in the task
      */

    if (bomp_icv_get()->task) {
        if (OMP_GET_ICV_TASK(active_levels) > 1) {
            return 1; /// if we are nested return 1
        }
        return OMP_GET_ICV_TASK(nthreads);
    }
    return 1;
}

/**
 * \brief the maximum number of threads that can be used for a new parallel task
 *
 * \returns number of usable threads
 *
 * Returns an upper bound on the number of threads that could be used to form a
 * new team if a parallel construct without a num_threads clause were encountered
 * after execution returns from this routine.
 *
 * The value returned by omp_get_max_threads is the value of the first element of
 * the nthreads-var ICV of the current task. This value is also an upper bound on
 * the number of threads that could be used to form a new team if a parallel
 * region without a num_threads clause were encountered after execution returns
 * from this routine.
 */
int omp_get_max_threads(void)
{
    if (bomp_icv_get()->task) {
        return OMP_GET_ICV_TASK(thread_limit);
    }
    return OMP_GET_ICV_GLOBAL(thread_limit);
}

/**
 * \brief Returns the thread number of the calling thread within the current team.
 *
 * \returns ThreadID
 */
int omp_get_thread_num(void)
{
    if (bomp_icv_get()->task) {
        return ((struct bomp_tls *)thread_get_tls())->thread_id;
    }
    return 0;
}

/**
 * \brief returns the number of available processors
 *
 * \returns available processor count
 *
 * Returns the number of processors that are available to the device at the time
 * the routine is called.
 */
int omp_get_num_procs(void)
{
    return numa_num_configured_cpus();
}

/**
 * \brief checks if we are currently in a parallel region
 *
 * \returns TRUE  active threads is greater than 1
 *          FALSE active threads is 1 (main thread)
 *
 * Returns true if the active-levels-var ICV is greater than zero; otherwise it
 * returns false. The effect of the omp_in_parallel routine is to return true if
 * the current task is enclosed by an active parallel region, and the parallel
 * region is enclosed by the outermost initial task region on the device;
 * otherwise it returns false.
 */
int omp_in_parallel(void)
{
    if (bomp_icv_get()->task) {
        return (OMP_GET_ICV_TASK(active_levels) > 0);
    } else {
        return 0;
    }
}

/**
 * \brief enables / disables the dynamic behavior
 *
 * \param dynamic_threads zero to disable dynamic behavior
 *                        non-zero to enable dynamic behavior
 *
 * Returns the value of the dyn-var ICV, which indicates if dynamic adjustment
 * of the number of threads is enabled or disabled.
 */
void omp_set_dynamic(int dynamic_threads)
{
#if OMP_SUPPORT_DYNAMIC
    OMP_SET_ICV_TASK(dynamic, (!!dynamic_threads));
#endif
}

/**
 * \brief checks if the dynamic behavior is enabled for the current task
 *
 * \returns TRUE if dynamic behavior enabled
 *          FALSE if disabled
 *
 * This routine returns the value of the dyn-var ICV, which is true if dynamic
 * adjustment of the number of threads is enabled for the current task.
 */
int omp_get_dynamic(void)
{
#if OMP_SUPPORT_DYNAMIC
    return OMP_GET_ICV_TASK(dynamic);
#else
    return 0;
#endif
}

/**
 * \brief Enables or disables nested parallelism, by setting the nest-var ICV.
 *
 * \param nested TRUE: enable nested behavior
 *               FALSE: disable nested behavior
 */
void omp_set_nested(int nested)
{
#if OMP_SUPPORT_NESTED
    OMP_SET_ICV_TASK(nested, !!nested);
#endif

}

/**
 * \brief checks if the nested behavior is enabled
 *
 * \returns TRUE if nested behavior is enabled
 *          FALSE if disabled
 *
 * Returns the value of the nest-var ICV, which indicates if nested parallelism
 * is enabled or disabled.
 */
int omp_get_nested(void)
{
#if OMP_SUPPORT_NESTED
    return OMP_GET_ICV_TASK(nested);
#else
    return 0;
#endif
}

/**
 * \brief sets the schedule to be used
 *
 * \param kind      which schedule to be used (one of OMP_SCHED_*)
 * \param modifier  modifier to tweak the scheduler (depends on kind)
 *
 * The omp_set_schedule routine affects the schedule that is applied when runtime
 * is used as schedule kind, by setting the value of the run-sched-var ICV.
 */
void omp_set_schedule(omp_sched_t kind,
                      int modifier)
{
    OMP_SET_ICV_TASK(run_sched, kind);
    OMP_SET_ICV_TASK(run_sched_modifier, modifier);
}

/**
 * \brief returns the current scheduler settings
 *
 * \param kind      returns the current scheduler setting (one of OMP_SCHED_*)
 * \param modifier  returns the modifier of the scheduler
 *
 * Returns the value of run-sched-var ICV, which is the schedule applied when
 * runtime schedule is used.
 */
void omp_get_schedule(omp_sched_t *kind,
                      int *modifier)
{
    if (kind) {
        *kind = OMP_GET_ICV_TASK(run_sched);
    }
    if (modifier) {
        *modifier = OMP_GET_ICV_TASK(run_sched_modifier);
    }
}

/**
 * \brief obtains he maximum number of OpenMP threads available
 *
 * \returns number of available threads
 *
 * Returns the value of the thread-limit-var ICV, which is the maximum number
 * of OpenMP threads available.
 *
 * The binding thread set for an omp_get_thread_limit region is all threads on the
 * device. The effect of executing this routine is not related to any specific
 * region corresponding to any construct or API routine.
 */
int omp_get_thread_limit(void)
{
    return OMP_GET_ICV_TASK(thread_limit);
}

/**
 * \brief limits the nested depth
 *
 * \param max_active_levels maximum nested level
 *
 * Limits the number of nested active parallel regions, by setting
 * max-active-levels-var ICV.
 */
void omp_set_max_active_levels(int max_active_levels)
{
    if (max_active_levels > 0) {
        OMP_SET_ICV_DEV(max_active_levels, max_active_levels);
    }
}

/**
 * \brief returns the maximim nested depth
 *
 * \returns maximum nested level
 *
 * Returns the value of max-active-levels-var ICV, which determines the maximum
 * number of nested active parallel regions.
 */
int omp_get_max_active_levels(void)
{
    return OMP_GET_ICV_DEV(max_active_levels);
}

/**
 * \brief returns the level the task is runnig at
 *
 * \param number enclosing nested parallel regions
 *
 * For the enclosing device region, returns the levels-vars ICV, which is the
 * number of nested parallel regions that enclose the task containing the call.
 */
int omp_get_level(void)
{
    return OMP_GET_ICV_TASK(levels);
}

/**
 * \brief returns the ancestor thread number of a thread at a given level
 *
 * \param level the level of the ancestor
 *
 * \returns thread number of ancestor thread
 *
 * The omp_get_ancestor_thread_num routine returns the thread number of the
 * ancestor at a given nest level of the current thread or the thread number of
 * the current thread. If the requested nest level is outside the range of 0 and
 * the nest level of the current thread, as returned by the omp_get_level routine,
 * the routine returns -1.
 */
int omp_get_ancestor_thread_num(int level)
{
    int my_level = omp_get_level();
    if (level > my_level || level < 0) {
        return -1;
    } else if (my_level == level) {
        return omp_get_thread_num();
    } else {
        /* TODO */
        assert(!"NYI");
        return 0;
    }
}

/**
 * \brief returns the team size of a thread at a given level
 *
 * \param level the level to consider
 *
 * \returns number of threads in the team *
 *
 * The omp_get_team_size routine returns the size of the thread team to which the
 * ancestor or the current thread belongs. If the requested nested level is outside
 * the range of 0 and the nested level of the current thread, as returned by the
 * omp_get_level routine, the routine returns -1. Inactive parallel regions are
 * regarded like active parallel regions executed with one thread.
 */
int omp_get_team_size(int level)
{
    int my_level = omp_get_level();
    if (level > my_level || level < 0) {
        return -1;
    } else {
        /* TODO */
        assert(!"NYI");
        return 0;
    }
}

/**
 * \brief returns the number of active, nested parallel regions
 *
 * \returns number of nested parallel regions *
 *
 * The effect of the omp_get_active_level routine is to return the number of nested,
 * active parallel regions enclosing the current task such that all of the parallel
 * regions are enclosed by the outermost initial task region on the current device.
 */
int omp_get_active_level(void)
{
    return OMP_GET_ICV_TASK(active_levels);
}

/**
 * \brief checks if thread is in the final task region
 *
 * \returns TRUE if thread is in the final task region
 *          FALSE otherwise
 *
 * Returns true if the routine is executed in a final task region; otherwise,
 * it returns false.
 */
int omp_in_final(void)
{
    assert(!"NYI");
    return 1;  // TODO
}

#if OMP_VERSION >= OMP_VERSION_40

/**
 * \brief returns the cancellation value
 *
 * \returns cancellation value
 *
 * Returns the value of the cancel-var ICV, which controls the behavior of
 * cancel construct and cancellation points.
 */
int omp_get_cancellation(void)
{
    return OMP_GET_ICV_DEV(cancel);
}

/**
 * \brief returns the thread affinitiy policy
 *
 * \returns OpenMP thread policy value
 *
 * Returns the thread affinity policy to be used for the subsequent nested
 * parallel regions that do not specify a proc_bind clause.
 */
omp_proc_bind_t omp_get_proc_bind(void)
{
    return OMP_GET_ICV_TASK(bind);
}

/**
 * \brief controls the default target device
 *
 * \param device_num device number of the target device
 *
 * The effect of this routine is to set the value of the default-device-var ICV
 * of the current task to the value specified in the argument. When called from
 * within a target region the effect of this routine is unspecified.
 */
void omp_set_default_device(int device_num)
{
    OMP_SET_ICV_TASK(default_device, device_num);
}

/**
 * \brief Returns the default target device.
 *
 * \returns device number of default target device
 *
 * The omp_get_default_device routine returns the value of the default-device-var
 * ICV of the current task. When called from within a target region the effect of
 * this routine is unspecified.
 */
int omp_get_default_device(void)
{
    // TODO: behavior if on target
    return OMP_GET_ICV_TASK(default_device);
}

/**
 * \brief Returns the number of target devices.
 *
 * \returns number of target devices
 *
 * The omp_get_num_devices routine returns the number of available target devices.
 * When called from within a target region the effect of this routine is
 * unspecified.
 */
int omp_get_num_devices(void)
{
    return 0;  // TODO
}

/**
 * \brief returns the number of teams in the current region
 *
 * \returns number of teams
 *
 * The effect of this routine is to return the number of teams in the current teams
 * region. The routine returns 1 if it is called from outside of a teams region.
 */
int omp_get_num_teams(void)
{
    assert(!"NYI: Teams");
    return 1;  // TODO: team counting
}

/**
 * \brief gets the team number of the calling thread
 *
 * \returns team number
 *
 * Returns the team number of calling thread. The team number is an integer
 * between 0 and one less than the value returned by omp_get_num_teams, inclusive.
 */
int omp_get_team_num(void)
{
    assert(!"NYI: Teams");
    return 0;
}

/**
 * \brief checks if the task is executing as the host device
 *
 * \returns TRUE if task is host device
 *          FALSE otherwise
 * Returns true if the current task is executing on the host device; otherwise,
 * it returns false.
 */
int omp_is_initial_device(void)
{
    assert(!"NYI: Initial device");
    return 1;
}
#endif

/*
 * ---------------------------------------------------------------------------
 * 3.3 Lock Routines
 * ---------------------------------------------------------------------------
 * General-purpose lock routines. Two types of locks are supported: simple locks
 * and nestable locks. A nestable lock can be set multiple times by the same task
 * before being unset; a simple lock cannot be set if it is already owned by the
 * task trying to set it.
 *
 * XXX: we may have to consider something different when we are dealing with
 *      non-shared address spaces such as XOMP
 */


/*
 * Simple OpenMP locks
 */

/**
 * \brief initializes and allocates a simple OpenMP lock
 *
 * \param arg returned pointer to the lock
 *
 * The effect of these routines is to initialize the lock to the unlocked state;
 * that is, no task owns the lock.
 */
void omp_init_lock(omp_lock_t *arg)
{
    struct __omp_lock *lock = (struct __omp_lock *)arg;

    assert(lock != NULL);

    thread_mutex_init(&lock->mutex);
    lock->initialized = 0x1;
}

/**
 * \brief destroys a simple OpenMP lock
 *
 * \param arg OpenMP lock to destroyed (set to zero)
 *
 * The effect of these routines is to change the state of the lock to uninitialized.
 */
void omp_destroy_lock(omp_lock_t *arg)
{
    struct __omp_lock *lock = (struct __omp_lock *) arg;

    /* acquire the lock to make sure there are no other threads holding the lock */
    thread_mutex_lock(&lock->mutex);
    /* we have the lock now */
    memset(lock, 0, sizeof (*lock));
}

/**
 * \brief acquires a simple OpenMP lock
 *
 * \param arg   The lock to acquire
 *
 * Each of these routines causes suspension of the task executing the routine
 * until the specified lock is available and then sets the lock.
 */
void omp_set_lock(omp_lock_t *arg)
{
    struct __omp_lock *lock = (struct __omp_lock *) arg;
    assert(lock->initialized);
    thread_mutex_lock(&lock->mutex);
}

/**
 * \brief Releases the simple OpenMP lock
 *
 * \param arg   The lock to be released
 *
 * For a simple lock, the omp_unset_lock routine causes the lock to become
 * unlocked.
 */
void omp_unset_lock(omp_lock_t *arg)
{
    struct __omp_lock *lock = (struct __omp_lock *) arg;
    assert(lock->initialized);
    thread_mutex_unlock(&lock->mutex);
}

/**
 * \brief tries to acquire a simple openMP lock
 *
 * \param arg   The OpenMP lock to acquire
 *
 * \returns TRUE if lock is acquired successfully
 *          FALSE if lock is already held by other thread
 *
 * These routines attempt to set a lock in the same manner as omp_set_lock and
 * omp_set_nest_lock, except that they do not suspend execution of the task
 * executing the routine.
 * For a simple lock, the omp_test_lock routine returns true if the lock is
 * successfully set; otherwise, it returns false.
 */
int omp_test_lock(omp_lock_t *arg)
{
    struct __omp_lock *lock = (struct __omp_lock *) arg;
    assert(lock->initialized);
    return thread_mutex_trylock(&lock->mutex);
}

/*
 * Nested OpenMP locks
 */

/**
 * \brief initializes and allocates a nested OpenMP lock
 *
 * \param arg returned pointer to the lock
 *
 * The effect of these routines is to initialize the lock to the unlocked state;
 * that is, no task owns the lock. In addition, the nesting count for a nestable
 * lock is set to zero.
 */
void omp_init_nest_lock(omp_nest_lock_t *arg)
{

    struct __omp_nested_lock *nlock = (struct __omp_nested_lock *)arg;
    assert(nlock != NULL);
    thread_mutex_init(&nlock->mutex);
    nlock->owner = NULL;
    nlock->count = 0;
    nlock->initialized = 1;
}

/**
 * \brief destroys a Nested OpenMP lock
 *
 * \param arg OpenMP lock to destroyed (set to zero)
 *
 * The effect of these routines is to change the state of the lock to uninitialized.
 */
void omp_destroy_nest_lock(omp_nest_lock_t *arg)
{
    struct __omp_nested_lock *nlock = (struct __omp_nested_lock *) arg;

    /*acquire the lock to make sure there are no other threads holding the lock */
    thread_mutex_lock(&nlock->mutex);
    /* we have the lock now */
    memset(nlock, 0, sizeof (*nlock));
}

/**
 * \brief acquires a simple OpenMP lock
 *
 * \param arg   The lock to acquire
 *
 * Each of these routines causes suspension of the task executing the routine
 * until the specified lock is available and then sets the lock.
 *
 * A nestable lock is available if it is unlocked or if it is already owned by
 * the task executing the routine. The task executing the routine is granted,
 * or retains, ownership of the lock, and the nesting count for the lock is
 * incremented.
 */
void omp_set_nest_lock(omp_nest_lock_t *arg)
{
    struct __omp_nested_lock *nlock = (struct __omp_nested_lock *) arg;
    assert(nlock->initialized);

    if (nlock->owner != thread_self()) {
        thread_mutex_lock (&nlock->mutex);
        nlock->owner = thread_self();
    }
    nlock->count++;
}

/**
 * \brief Releases the simple OpenMP lock
 *
 * \param arg   The lock to be released
 *
 * For a nestable lock, the omp_unset_nest_lock routine decrements the nesting
 * count, and causes the lock to become unlocked if the resulting nesting count
 * is zero.
 */
void omp_unset_nest_lock(omp_nest_lock_t *arg)
{
    struct __omp_nested_lock *nlock = (struct __omp_nested_lock *) arg;
    assert(nlock->initialized);

    nlock->count--;

    // if we were the last holder unlock the mutex
    if (nlock->count == 0) {
        thread_mutex_unlock(&nlock->mutex);
    }
}

/**
 * \brief tries to acquire a simple openMP lock
 *
 * \param arg   The OpenMP lock to acquire
 *
 * \returns TRUE if lock is acquired successfully
 *          FALSE if lock is already held by other thread
 *
 * These routines attempt to set a lock in the same manner as omp_set_lock and
 * omp_set_nest_lock, except that they do not suspend execution of the task
 * executing the routine.
 * For a nestable lock, the omp_test_nest_lock routine returns the new nesting
 * count if the lock is successfully set; otherwise, it returns zero.
 */
int omp_test_nest_lock(omp_nest_lock_t *arg)
{
    struct __omp_nested_lock *nlock = (struct __omp_nested_lock *) arg;
    assert(nlock->initialized);

    if (nlock->owner != thread_self()) {
        if (!thread_mutex_trylock(&nlock->mutex)) {
            return 0;
        }
        nlock->owner = thread_self();
    }

    nlock->count++;

    return nlock->count;
}

/*
 * ---------------------------------------------------------------------------
 * 3.4 Timing Routines
 * ---------------------------------------------------------------------------
 * Timing routines support a portable wall clock timer. These record elapsed
 * time per-thread and are not guaranteed to be globally consistent across all
 * the threads participating in an application.
 */

/**
 * \brief returns elapsed wall clock time in seconds.
 *
 * \returns call clock time
 *
 * The omp_get_wtime routine returns a value equal to the elapsed wall clock time
 * in seconds since some “time in the past”. The actual “time in the past” is
 * arbitrary, but it is guaranteed not to change during the execution of the
 * application program. The time returned is a “per-thread time”, so it is not
 * required to be globally consistent across all the threads participating in an
 * application.
 */
double omp_get_wtime(void)
{
    cycles_t t_start = OMP_GET_ICV_GLOBAL(time_start);
    cycles_t t_current = rdtsc();
    assert(!"conversion to ms");
    return (t_current - t_start);
}

/**
 * \brief returns the precision of the timer used by omp_get_wtime.
 *
 * \returns the timer precision
 *
 * The omp_get_wtick routine returns a value equal to the number of seconds
 * between successive clock ticks of the timer used by omp_get_wtime.
 */
double omp_get_wtick(void)
{
    return 1.0 / 1e6;
}

