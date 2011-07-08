/**
 * \file
 * \brief Threads.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_THREADS_H
#define LIBBARRELFISH_THREADS_H

#include <barrelfish_kpi/spinlocks_arch.h>

typedef int (*thread_func_t)(void *);

/// A thread of execution
struct thread;

struct thread_mutex {
    volatile int        locked;
    struct thread       *queue;
    spinlock_t          lock;
    struct thread       *holder;
};
#ifndef __cplusplus
#       define THREAD_MUTEX_INITIALIZER \
    { .locked = false, .queue = NULL, .lock = 0 }
#else
#       define THREAD_MUTEX_INITIALIZER                                \
    { false, (struct thread *)NULL, 0, (struct thread *)NULL }
#endif

struct thread_cond {
    struct thread       *queue;
    spinlock_t          lock;
};
#ifndef __cplusplus
#       define THREAD_COND_INITIALIZER \
    { .queue = NULL, .lock = 0 }
#else
#       define THREAD_COND_INITIALIZER \
    { (struct thread *)NULL, 0 }
#endif

/// A thread of execution
struct thread;

/// Default size of a thread's stack
#define THREADS_DEFAULT_STACK_BYTES     (64 * 1024)

struct thread *thread_create(thread_func_t start_func, void *data);
struct thread *thread_create_varstack(thread_func_t start_func, void *arg,
                                      size_t stacksize);
void thread_yield(void);
void thread_yield_dispatcher(struct capref endpoint);
void thread_exit(void);
struct thread *thread_self(void);
errval_t thread_join(struct thread *thread, int *retval);
errval_t thread_detach(struct thread *thread);

void thread_pause(struct thread *thread);
void thread_pause_and_capture_state(struct thread *thread, 
                                    arch_registers_state_t **ret_regs,
                                    arch_registers_fpu_state_t **ret_fpuregs);
void thread_resume(struct thread *thread);

void thread_mutex_init(struct thread_mutex *mutex);
void thread_mutex_lock(struct thread_mutex *mutex);
bool thread_mutex_trylock(struct thread_mutex *mutex);
void thread_mutex_lock_nested(struct thread_mutex *mutex);
void thread_mutex_unlock(struct thread_mutex *mutex);
struct thread *thread_mutex_unlock_disabled(dispatcher_handle_t handle,
                                            struct thread_mutex *mutex);

void thread_cond_init(struct thread_cond *cond);
void thread_cond_signal(struct thread_cond *cond);
void thread_cond_broadcast(struct thread_cond *cond);
void thread_cond_wait(struct thread_cond *cond, struct thread_mutex *mutex);

struct thread_sem {
    volatile unsigned int       value;
    struct thread               *queue;
    spinlock_t                  lock;
};
#define THREAD_SEM_INITIALIZER          { .value = 0, .queue = NULL, .lock = 0 }

void thread_sem_init(struct thread_sem *sem, unsigned int value);
void thread_sem_wait(struct thread_sem *sem);
bool thread_sem_trywait(struct thread_sem *sem);
void thread_sem_post(struct thread_sem *sem);

void thread_set_tls(void *);
void *thread_get_tls(void);

#endif
