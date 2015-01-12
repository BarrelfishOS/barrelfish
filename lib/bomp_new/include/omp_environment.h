/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _LIBBOMP_ENVIRONEMNT_H
#define _LIBBOMP_ENVIRONEMNT_H

/*
 * ---------------------------------------------------------------------------
 * 4.0 Environment Variables
 * ---------------------------------------------------------------------------
 *
 * Environment variable names are upper case, and the values assigned to them
 * are case insensitive and may have leading and trailing white space.
 *
 * These are the values which are set as the default when initializing the
 * library
 */

/**
 * Sets the cancel-var ICV. policy may be true (non-zero) or false (zero). If true,
 * the effects of the cancel construct and of cancellation points are enabled and
 * cancellation is activated
 */
#define OMP_CANCELLATION 0

/**
 * Sets the default-device-var ICV that controls the default device number to
 * use in device constructs.
 */
#define OMP_DEFAULT_DEVICE 0

/**
 * If var is TRUE, instructs the runtime to display the OpenMP version number
 * and the value of the ICVs associated with the environment variables as name=value
 * pairs. If var is VERBOSE, the runtime may also display vendor-specific variables.
 * If var is FALSE, no information is displayed.
 */
#define OMP_DISPLAY_ENV 0

/**
 * Sets the dyn-var ICV. If true, the implementation may dynamically adjust the
 * number of threads to use for executing parallel regions.
 */
#define OMP_DYNAMIC 0

/**
 * Sets the max-active-levels-var ICV that controls the maximum number of nested
 * active parallel regions.
 */
#define OMP_MAX_ACTIVE_LEVELS 0

/**
 * Sets the nest-var ICV to enable or to disable nested parallelism. Valid values
 * for nested are true or false.
 */
#define OMP_NESTED 0

/**
 * Sets the nthreads-var ICV for the number of threads to use for parallel regions.
 */
#define OMP_NUM_THREADS 40

/**
 * Sets the place-partition-var ICV that defines the OpenMP places available to
 * the execution environment. places is an abstract name (threads, cores, sockets,
 * or implementation-defined), or a list of non-negative numbers.
 */
#define OMP_PLACES 0

/**
 * Sets the value of the global bind-var ICV, which sets the thread affinity
 * policy to be used for parallel regions at the corresponding nested level.
 * policy can be the values true, false, or a comma-separated list of master,
 * close, or spread in quotes.
 */
#define OMP_PROC_BIND 0

/**
 * Sets the run-sched-var ICV for the runtime schedule type and chunk size.
 * Valid OpenMP schedule types are static, dynamic, guided, or auto.
 */
#define OMP_SCHEDULE OMP_SCHED_STATIC

/**
 * Sets the stacksize-var ICV that specifies the size of the stack for threads
 * created by the OpenMP implementation. size is a positive integer that specifies
 * stack size. If unit is not specified, size is measured in kilobytes (K).
 */
#define OMP_STACKSIZE (64*1024)

/**
 * Sets the thread-limit-var ICV that controls the number of threads participating
 * in the OpenMP program.
 */
#define OMP_THREAD_LIMIT 40

/**
 * Sets the wait-policy-var ICV that provides a hint to an OpenMP implementation
 * about the desired behavior of waiting threads. Valid values for policy are
 * ACTIVE (waiting threads consume processor cycles while waiting) and PASSIVE.
 */
#define OMP_WAIT_POLICY 0


#endif	/* _LIBBOMP_ENVIRONEMNT_H */
