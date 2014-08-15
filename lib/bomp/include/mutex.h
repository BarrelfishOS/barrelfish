/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BOMP_MUTEX_H
#define BOMP_MUTEX_H

#ifdef BARRELFISH
#include <barrelfish/threads.h>
#include <barrelfish/thread_sync.h>
typedef struct thread_mutex bomp_mutex_t;

static inline void bomp_mutex_init (bomp_mutex_t *mutex)
{
    thread_mutex_init(mutex);
}

static inline void bomp_mutex_lock (bomp_mutex_t *mutex)
{
    thread_mutex_lock (mutex);
}

static inline void bomp_mutex_unlock (bomp_mutex_t *mutex)
{
    thread_mutex_unlock (mutex);
}

static inline void bomp_mutex_destroy (bomp_mutex_t *mutex)
{
    /* nop */
}
#else
/* LINUX */

typedef pthread_mutex_t bomp_mutex_t;

static inline void bomp_mutex_init (bomp_mutex_t *mutex)
{
    pthread_mutex_init (mutex, NULL);
}

static inline void bomp_mutex_lock (bomp_mutex_t *mutex)
{
    pthread_mutex_lock (mutex);
}

static inline void bomp_mutex_unlock (bomp_mutex_t *mutex)
{
    pthread_mutex_unlock (mutex);
}

static inline void bomp_mutex_destroy (bomp_mutex_t *mutex)
{
    pthread_mutex_destroy (mutex);
}

#endif

#endif /* BOMP_MUTEX_H */
