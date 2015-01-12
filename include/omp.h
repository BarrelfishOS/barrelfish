/**
 * \file
 * \brief Include to use the bomp library
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OMP_H
#define OMP_H

/**
 * defines which OpenMP version is supported by the backend
 *
 * OpenMP 4.0: 0x0400
 * OpenMP 3.1: 0x0310
 */
#define OMP_VERSION_31 0x0310
#define OMP_VERSION_40 0x0400
#define OMP_VERSION OMP_VERSION_31

// some configuration switches
#define OMP_SUPPORT_NESTED 0
#define OMP_SUPPORT_DYNAMIC 0

#include <stddef.h> // for size_t
#include <barrelfish_kpi/spinlocks_arch.h>
#include <barrelfish/thread_sync.h>

/* a simple OpenMP lock */
typedef struct __omp_lock
{
    struct thread_mutex mutex;
    uint8_t initialized;
} omp_lock_t;

/* a nestable OpenMP lock */
typedef struct __omp_nested_lock
{
    struct thread_mutex mutex;
    void *owner;
    uint32_t count;
    uint8_t initialized;
} omp_nest_lock_t;


/**
 * BOMP backend types
 */
typedef enum bomp_backend {
    BOMP_BACKEND_INVALID = 0,
    BOMP_BACKEND_BOMP    = 1,
    BOMP_BACKEND_XOMP    = 2,
    BOMP_BACKEND_LINUX   = 3
} bomp_backend_t;

/**
 * OpenMP schedule types
 */
typedef enum omp_sched {
    OMP_SCHED_STATIC  = 1,
    OMP_SCHED_DYNAMIC = 2,
    OMP_SCHED_GUIDED  = 3,
    OMP_SCHED_AUTO    = 4
} omp_sched_t;

#if OMP_VERSION >= OMP_VERSION_40
/**
 * OpenMP processor task affinitiy
 */
typedef enum omp_proc_bind {
    OMP_PROC_BIND_FALSE  = 0,
    OMP_PROC_BIND_TRUE   = 1,
    OMP_PROC_BIND_MASTER = 2,
    OMP_PROC_BIND_CLOSE  = 3,
    OMP_PROC_BIND_SPREAD = 4
} omp_proc_bind_t;
#endif


#ifdef __cplusplus
extern "C" {
#endif

/*
 * Backend specific initialization functions
 */
int bomp_bomp_init(unsigned int nthreads);
int bomp_bomp_init_varstack(unsigned int nthreads, size_t stack_size);
int bomp_linux_init(unsigned int nthreads);
int bomp_xomp_init(void *args);

/*
 * Backend specific main thread runners
 * needed on Barrelfish as we don't dynamically grow thread stacks
 */
typedef int (*main_func_t)(void *);
int bomp_run_main(main_func_t mainfunc, void *mainarg, size_t stacksize);

/**
 * \brief switches the backend to be used
 *
 * \param backend   Backend to activate
 *
 * XXX: this has only to be used if XOMP and BOMP are used in the same
 *      library
 */
int bomp_switch_backend(bomp_backend_t backend);

/**
 * \brief gets the currently enabled backend
 *
 * \returns BOMP_BACKEND_*
 */
bomp_backend_t bomp_get_backend(void);


///< Default Stacksize for BOMP threads
#define BOMP_DEFAULT_STACKSIZE (64 * 1024)

///< Flag indicating that all threads should be used
#define BOMP_THREADS_ALL ((unsigned int) -1)

/*
 * ------------------------------------------------------------------------------
 * BOMP initialization Functions
 * ------------------------------------------------------------------------------
 */

/**
 * \brief initializes the BOMP library using the indicated cores of the BM
 *
 * \param coresbm		bitmap representing the cores to run on
 * \param stack_size	size of the thread's stack in bytes
 *
 * \returns	0 on SUCCESS
 *          non-zero on FAILURE
 */
int bomp_init_cores(void *coresbm, size_t stack_size);

/**
 * \brief initializes the BOMP library with the given stack sizes
 *
 * \param stack_size	size of the thread's stack in bytes
 *
 * \returns	0 on SUCCESS
 *          non-zero on FAILURE
 *
 * This function will use the first nthreads cores to run on
 */
int bomp_init_varstack(unsigned int nthreads, size_t stack_size);

/**
 * \brief initializes the BOMP library using the given thread number
 *
 * \param nthreads	the number of threads to prepare
 *
 * \returns	0 on SUCCESS
 *          non-zero on FAILURE
 *
 * This function will use the first nthreads cores and the default stack size
 * for the BOMP threads to run on.
 */
static inline int bomp_init(unsigned int nthreads)
{
	return bomp_init_varstack(nthreads, BOMP_DEFAULT_STACKSIZE);
}




#if 0
/*
 * Backend specific main thread runners
 * needed on Barrelfish as we don't dynamically grow thread stacks
 */
typedef int (*main_func_t)(void *);
int bomp_run_main(main_func_t mainfunc, void *mainarg, size_t stacksize);
#endif

/*
 * OpenMP Library API as defined by openmp.org
 */

extern void omp_set_num_threads(int num_threads);
extern int omp_get_num_threads(void);
extern int omp_get_max_threads(void);
extern int omp_get_thread_num(void);
extern int omp_get_num_procs(void);
extern int omp_in_parallel(void);
extern void omp_set_dynamic(int dynamic_threads);
extern int omp_get_dynamic(void);
extern void omp_set_nested(int nested);
extern int omp_get_nested(void);
extern void omp_set_schedule(omp_sched_t kind, int modifier);
extern void omp_get_schedule(omp_sched_t *kind, int *modifier);
extern int omp_get_thread_limit(void);
extern void omp_set_max_active_levels(int max_active_levels);
extern int omp_get_max_active_levels(void);
extern int omp_get_level(void);
extern int omp_get_ancestor_thread_num(int level);
extern int omp_get_team_size(int level);
extern int omp_get_active_level(void);
extern int omp_in_final(void);

extern void omp_init_lock(omp_lock_t *lock);
extern void omp_destroy_lock(omp_lock_t *lock);
extern void omp_set_lock(omp_lock_t *lock);
extern void omp_unset_lock(omp_lock_t *lock);
extern int omp_test_lock(omp_lock_t *lock);

extern void omp_init_nest_lock(omp_nest_lock_t *lock);
extern void omp_destroy_nest_lock(omp_nest_lock_t *lock);
extern void omp_set_nest_lock(omp_nest_lock_t *lock);
extern void omp_unset_nest_lock(omp_nest_lock_t *lock);
extern int omp_test_nest_lock(omp_nest_lock_t *lock);

extern double omp_get_wtime(void);
extern double omp_get_wtick(void);

/* these functions are valid in OpenMP 4.0 or higher */
#if OMP_VERSION >= OMP_VERSION_40
extern int omp_get_cancellation(void);
extern omp_proc_bind_t omp_get_proc_bind(void);
extern void omp_set_default_device(int device_num);
extern int omp_get_default_device(void);
extern int omp_get_num_devices(void);
extern int omp_get_num_teams(void);
extern int omp_get_team_num(void);
extern int omp_is_initial_device(void);
#endif

#ifdef __cplusplus
}
#endif
#endif /* OMP_H */
