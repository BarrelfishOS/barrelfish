/**
 * \file
 * \brief Threads.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_THREADS_H
#define LIBBARRELFISH_THREADS_H

#include <assert.h>
#include <sys/cdefs.h>

#include <barrelfish/caddr.h> // for struct capref.
#include <barrelfish/thread_sync.h>
#include <barrelfish_kpi/registers_arch.h>
#include <barrelfish_kpi/dispatcher_handle.h>
#include <errors/errno.h>
#include <barrelfish/waitset.h>

__BEGIN_DECLS

typedef int (*thread_func_t)(void *);

/// Default size of a thread's stack
#define THREADS_DEFAULT_STACK_BYTES     (64 * 1024)

struct thread *thread_create(thread_func_t start_func, void *data);
struct thread *thread_create_varstack(thread_func_t start_func, void *arg,
                                      size_t stacksize);
void thread_yield(void);
void thread_yield_dispatcher(struct capref endpoint);
void thread_exit(int status);
struct thread *thread_self(void);
struct thread *thread_self_disabled(void);
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

void thread_sem_init(struct thread_sem *sem, unsigned int value);
void thread_sem_wait(struct thread_sem *sem);
bool thread_sem_trywait(struct thread_sem *sem);
void thread_sem_post(struct thread_sem *sem);

void thread_set_tls(void *);
void *thread_get_tls(void);

void thread_set_tls_key(int, void *);
void *thread_get_tls_key(int);

uintptr_t thread_id(void);
uintptr_t thread_get_id(struct thread *t);
void thread_set_id(uintptr_t id);

uint32_t thread_set_token(struct waitset_chanstate *channel);
void thread_clear_token(struct waitset_chanstate *channel);
uint32_t thread_current_token(void);

void thread_set_outgoing_token(uint32_t token);
void thread_get_outgoing_token(uint32_t *token);

/// Set/get a local trigger for currently processed event channel
void thread_set_local_trigger(struct waitset_chanstate *trigger);
struct waitset_chanstate * thread_get_local_trigger(void);

struct flounder_rpc_context;

void thread_set_rpc_in_progress(bool v);
bool thread_get_rpc_in_progress(void);
void thread_set_async_error(errval_t e);
errval_t thread_get_async_error(void);

void thread_store_recv_slot(struct capref recv_slot);
struct capref thread_get_next_recv_slot(void);

extern __thread thread_once_t thread_once_local_epoch;
extern void thread_once_internal(thread_once_t *control, void (*func)(void));

/**
 * \brief Run a routine exactly once; use this for thread-safe initialization.
 *
 * \param control Control word - should be initialized with THREAD_ONCE_INIT.
 * \param func Callback to be invoked.
 */
static inline void thread_once(thread_once_t *control, void (*func)(void)) {
    assert(control != NULL);
    assert(func != NULL);
    thread_once_t x = *control; // unprotected access
    if (x > thread_once_local_epoch) {
        thread_once_internal(control, func);
    }
}

/**
 * \brief Set a thread's exit status.
 *
 * \param status The status.
 */
void thread_set_status(int status);

__END_DECLS

#endif  // LIBBARRELFISH_THREADS_H
