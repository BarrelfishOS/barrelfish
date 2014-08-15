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

#include <stddef.h> // for size_t

typedef enum bomp_backend {
    BOMP_BACKEND_BOMP  = 1,
    BOMP_BACKEND_XOMP  = 2,
    BOMP_BACKEND_LINUX = 3
} bomp_backend_t;

typedef enum omp_sched_t {
    omp_sched_static  = 1,
    omp_sched_dynamic = 2,
    omp_sched_guided  = 3,
    omp_sched_auto    = 4
} omp_sched_t;

typedef enum omp_proc_bind_t {
    omp_proc_bind_false  = 0,
    omp_proc_bind_true   = 1,
    omp_proc_bind_master = 2,
    omp_proc_bind_close  = 3,
    omp_proc_bind_spread = 4
} omp_proc_bind_t;

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Backend specific initialization functions
 */
int bomp_bomp_init(unsigned int nthreads);
int bomp_linux_init(unsigned int nthreads);
int bomp_xomp_init(void *args);

/**
 * \brief switches the backend to be used
 *
 * \param backend   Backend to activate
 *
 * XXX: this has only to be used if XOMP and BOMP are used in the same
 *      library
 */
int bomp_switch_backend(bomp_backend_t backend);


bomp_backend_t bomp_get_backend(void);

/*
 *
 */
void bomp_get_init_time(int cores);

/*
 * OpenMP Library API as defined by openmp.org
 */

void omp_set_num_threads(int);
int omp_get_num_threads(void);
int omp_get_max_threads(void);
int omp_get_thread_num(void);
int omp_get_num_procs(void);

int omp_in_parallel(void);

void omp_set_dynamic(int);
int omp_get_dynamic(void);

void omp_set_nested(int);
int omp_get_nested(void);

#if 0
void omp_init_lock(omp_lock_t *);
void omp_destroy_lock(omp_lock_t *);
void omp_set_lock(omp_lock_t *);
void omp_unset_lock(omp_lock_t *);
int omp_test_lock(omp_lock_t *);

void omp_init_nest_lock(omp_nest_lock_t *);
void omp_destroy_nest_lock(omp_nest_lock_t *);
void omp_set_nest_lock(omp_nest_lock_t *);
void omp_unset_nest_lock(omp_nest_lock_t *);
int omp_test_nest_lock(omp_nest_lock_t *);
#endif

double omp_get_wtime(void);
double omp_get_wtick(void);

void omp_set_schedule(omp_sched_t, int);
void omp_get_schedule(omp_sched_t *, int *);
int omp_get_thread_limit(void);
void omp_set_max_active_levels(int);
int omp_get_max_active_levels(void);
int omp_get_level(void);
int omp_get_ancestor_thread_num(int);
int omp_get_team_size(int);
int omp_get_active_level(void);

int omp_in_final(void);

int omp_get_cancellation(void);
omp_proc_bind_t omp_get_proc_bind(void);

void omp_set_default_device(int);
int omp_get_default_device(void);
int omp_get_num_devices(void);
int omp_get_num_teams(void);
int omp_get_team_num(void);

int omp_is_initial_device(void);

#ifdef __cplusplus
}
#endif
#endif /* OMP_H */
