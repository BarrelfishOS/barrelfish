/*
 *  Written by Joel Sherrill <joel@OARcorp.com>.
 *
 *  COPYRIGHT (c) 1989-2010.
 *  On-Line Applications Research Corporation (OAR).
 *
 *  Permission to use, copy, modify, and distribute this software for any
 *  purpose without fee is hereby granted, provided that this entire notice
 *  is included in all copies of any software which is or includes a copy
 *  or modification of this software.
 *
 *  THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 *  WARRANTY.  IN PARTICULAR,  THE AUTHOR MAKES NO REPRESENTATION
 *  OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS
 *  SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 *
 *  $Id$
 */

#ifndef _SCHED_H_
#define _SCHED_H_

#include <string.h> /* memcpy */
#include <assert.h>
#include <sys/types.h>
#include <sys/sched.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_POSIX_PRIORITY_SCHEDULING)
/*
 *  XBD 13 - Set Scheduling Parameters, P1003.1b-2008, p. 1803
 */
int sched_setparam(
  pid_t                     __pid,
  const struct sched_param *__param
);

/*
 *  XBD 13 - Set Scheduling Parameters, P1003.1b-2008, p. 1800
 */
int sched_getparam(
  pid_t                     __pid,
  struct sched_param       *__param
);

/*
 *  XBD 13 - Set Scheduling Policy and Scheduling Parameters,
 *         P1003.1b-2008, p. 1805
 */
int sched_setscheduler(
  pid_t                     __pid,
  int                       __policy,
  const struct sched_param *__param
);

/*
 *  XBD 13 - Get Scheduling Policy, P1003.1b-2008, p. 1801
 */
int sched_getscheduler(
  pid_t                     __pid
);

/*
 *  XBD 13 - Get Scheduling Parameter Limits, P1003.1b-2008, p. 1799
 */
int sched_get_priority_max(
  int __policy
);

int sched_get_priority_min(
  int  __policy
);

/*
 *  XBD 13 - Get Scheduling Parameter Limits, P1003.1b-2008, p. 1802
 */
int sched_rr_get_interval(
  pid_t             __pid,
  struct timespec  *__interval
);
#endif /* _POSIX_PRIORITY_SCHEDULING */

#if defined(_POSIX_THREADS) || defined(_POSIX_PRIORITY_SCHEDULING)

/*
 *  XBD 13 - Yield Processor, P1003.1b-2008, p. 1807
 */
int sched_yield( void );

#endif /* _POSIX_THREADS or _POSIX_PRIORITY_SCHEDULING */

#ifdef __cplusplus
}
#endif


/* Access macros for `cpu_set'.  */
#define CPU_BITFIELD_SIZE 10

/**
 * The constant CPU_SETSIZE (currently 1024) specifies a value
 * one greater than the maximum CPU number that can be stored in cpu_set_t.
 */
#define CPU_SETSIZE (CPU_BITFIELD_SIZE*64)

struct bitmap {
    int num_cpus;
    uint64_t field[CPU_BITFIELD_SIZE];
};
typedef struct bitmap cpu_set_t;

static inline cpu_set_t* CPU_ALLOC(int num_cpus) {
    assert(num_cpus < CPU_SETSIZE);

    cpu_set_t* set = (cpu_set_t*) calloc(1, sizeof(cpu_set_t));
    set->num_cpus = num_cpus;
    return set;
}

static inline int CPU_COUNT(const cpu_set_t *set) {
    return set->num_cpus;
}
#include <stdio.h>

static inline void CPU_ZERO(cpu_set_t *set) {
    // XXX: this seems to be the only sane thing in case someone does not call CPU_ALLOC..?
    set->num_cpus = CPU_SETSIZE;

    memset(set->field, 0, sizeof(set->field));
}

static inline int CPU_ISSET(int cpu, const cpu_set_t *set) {
    assert (cpu < set->num_cpus);
    size_t slot = cpu / 64;
    size_t idx = cpu % 64;
    return (set->field[slot] & (1 << idx)) > 0;
}

static inline void CPU_SET(int cpu, cpu_set_t *set) {
    assert (cpu < set->num_cpus);
    size_t slot = cpu / 64;
    size_t idx = cpu % 64;
    set->field[slot] |= (1 << idx);
}

static inline void CPU_CLR(int cpu, cpu_set_t *set) {
    assert (cpu < set->num_cpus);
    size_t slot = cpu / 64;
    size_t idx = cpu % 64;
    set->field[slot] = set->field[slot] & ~(1 << idx);
}


/* Set the CPU affinity for a task */
extern int sched_setaffinity (__pid_t __pid, size_t __cpusetsize,
                  __const cpu_set_t *__cpuset);

/* Get the CPU affinity for a task */
extern int sched_getaffinity (__pid_t __pid, size_t __cpusetsize,
                  cpu_set_t *__cpuset);

#endif /* _SCHED_H_ */
