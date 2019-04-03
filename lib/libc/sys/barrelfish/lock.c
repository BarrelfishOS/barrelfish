/**
 * \file
 * \brief Barrelfish libc lock backend. These functions are used in
 *        lib/newlib/newlib/libc/include/sys/lock.h
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <sys/lock.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/threads.h>

static void make_work_disabled(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    if (disp->disabled) {
        disp->disabled = 0;
    }
}

void bf_libc_lock_init(struct thread_mutex *lock)
{
    thread_mutex_init(lock);
}

void bf_libc_lock_close(struct thread_mutex *lock)
{ }

void bf_libc_lock_acquire(struct thread_mutex *lock)
{
    make_work_disabled();
    thread_mutex_lock(lock);
}

void bf_libc_lock_acquire_recursive(struct thread_mutex *lock)
{
    make_work_disabled();
    thread_mutex_lock_nested(lock);
}

void bf_libc_lock_try_acquire(struct thread_mutex *lock)
{
    make_work_disabled();
    thread_mutex_trylock(lock);
}

void bf_libc_lock_try_acquire_recursive(struct thread_mutex *lock)
{
    // can't do try_acquire_recursive
    assert(!"NYI");
}

void bf_libc_lock_release(struct thread_mutex *lock)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_shared_generic* disp =
        get_dispatcher_shared_generic(handle);
    if (disp->disabled) {
        thread_mutex_unlock_disabled(curdispatcher(), lock);
    } else {
        thread_mutex_unlock(lock);
    }
}
