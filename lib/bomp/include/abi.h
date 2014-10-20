/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 64, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __GOMP_ABI_H
#define	__GOMP_ABI_H

/* This header specifies the function signatures of the GOMP library as described
 * in the GCC runtime library
 *
 * These functions have to be implemented in order to give full OpenMP support
 */

/* atomic.c */
void GOMP_atomic_start(void);
void GOMP_atomic_end(void);

/* barrier.c */
void GOMP_barrier(void);
bool GOMP_barrier_cancel (void);

/* critical.c */
void GOMP_critical_start(void);
void GOMP_critical_end(void);
void GOMP_critical_name_start(void **pptr);
void GOMP_critical_name_end(void **pptr);

/* loop.c */
bool GOMP_loop_static_start (long, long, long, long, long *, long *);
bool GOMP_loop_dynamic_start (long, long, long, long, long *, long *);
bool GOMP_loop_guided_start (long, long, long, long, long *, long *);
bool GOMP_loop_runtime_start (long, long, long, long *, long *);

bool GOMP_loop_ordered_static_start (long, long, long, long,
                        long *, long *);
bool GOMP_loop_ordered_dynamic_start (long, long, long, long,
                         long *, long *);
bool GOMP_loop_ordered_guided_start (long, long, long, long,
                        long *, long *);
bool GOMP_loop_ordered_runtime_start (long, long, long, long *, long *);

bool GOMP_loop_static_next (long *, long *);
bool GOMP_loop_dynamic_next (long *, long *);
bool GOMP_loop_guided_next (long *, long *);
bool GOMP_loop_runtime_next (long *, long *);

bool GOMP_loop_ordered_static_next (long *, long *);
bool GOMP_loop_ordered_dynamic_next (long *, long *);
bool GOMP_loop_ordered_guided_next (long *, long *);
bool GOMP_loop_ordered_runtime_next (long *, long *);

void GOMP_parallel_loop_static_start (void (*)(void *), void *,
                         unsigned, long, long, long, long);
void GOMP_parallel_loop_dynamic_start (void (*)(void *), void *,
                         unsigned, long, long, long, long);
void GOMP_parallel_loop_guided_start (void (*)(void *), void *,
                         unsigned, long, long, long, long);
void GOMP_parallel_loop_runtime_start (void (*)(void *), void *,
                          unsigned, long, long, long);
void GOMP_parallel_loop_static (void (*)(void *), void *,
                       unsigned, long, long, long, long,
                       unsigned);
void GOMP_parallel_loop_dynamic (void (*)(void *), void *,
                    unsigned, long, long, long, long,
                    unsigned);
void GOMP_parallel_loop_guided (void (*)(void *), void *,
                       unsigned, long, long, long, long,
                       unsigned);
void GOMP_parallel_loop_runtime (void (*)(void *), void *,
                    unsigned, long, long, long,
                    unsigned);

void GOMP_loop_end (void);
void GOMP_loop_end_nowait (void);
bool GOMP_loop_end_cancel (void);

/* ordered.c */
void GOMP_ordered_start(void);
void GOMP_ordered_end(void);

/* parallel.c */
void GOMP_parallel(void (*fn)(void *), void *data, unsigned num_threads, unsigned int flags);
void GOMP_parallel_start(void (*) (void *), void *, unsigned);
void GOMP_parallel_end(void);
bool GOMP_cancel(int which, bool do_cancel);
bool GOMP_cancellation_point(int which);

/* sections.c */
unsigned GOMP_sections_start(unsigned count);
unsigned GOMP_sections_next(void);
void GOMP_parallel_sections_start(void (*fn)(void *), void *data,
                                  unsigned num_threads, unsigned count);
void GOMP_parallel_sections(void (*fn)(void *), void *data, unsigned num_threads,
                            unsigned count, unsigned flags);
void GOMP_sections_end(void);
bool GOMP_sections_end_cancel(void);
void GOMP_sections_end_nowait(void);

/* single.c */
bool GOMP_single_start(void);
void *GOMP_single_copy_start (void);
void GOMP_single_copy_end (void *data);

/* target.c */
void GOMP_target (int, void (*) (void *), const void *,
                  size_t, void **, size_t *, unsigned char *);
void GOMP_target_data (int, const void *,
                       size_t, void **, size_t *, unsigned char *);
void GOMP_target_end_data (void);
void GOMP_target_update (int, const void *,
                         size_t, void **, size_t *, unsigned char *);
void GOMP_teams (unsigned int, unsigned int);

#endif	/* __GOMP_ABI_H */
