/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XOMP_MASTER_H_
#define LIB_XOMP_MASTER_H_

typedef enum xomp_master_copy {
    XOMP_MASTER_COPY_INVALID,
    XOMP_MASTER_COPY_UPDATE,
    XOMP_MASTER_COPY_WRITE_BACK
} xomp_master_copy_t;

#define XOMP_MASTER_COPY_NODE_ALL 0xFFFF

#define XOMP_MASTER_BENCH_SPAWN   (1 << 0)
#define XOMP_MASTER_BENCH_MEM_ADD (1 << 1)
#define XOMP_MASTER_BENCH_DO_WORK (1 << 2)

/**
 * \brief initializes the Xeon Phi openMP library
 *
 * \param args struct containing the master initialization values
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xomp_master_init(struct xomp_args *args);

/**
 * \brief Spawns the worker threads on the Xeon Phi
 *
 * \param nworkers    Number of total workers this includes the Master
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xomp_master_spawn_workers(uint32_t nworkers);

/**
 * \brief Adds a memory region to be used for work
 *
 * \param frame Frame to be shared
 * \param info  information about the frame i.e. virtual address to map
 * \oaram type  Type of the frame
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xomp_master_add_memory(struct capref frame,
                                uint64_t info,
                                xomp_frame_type_t type);

/**
 * \brief tells the gateway domains to update their local replicas
 *
 * \param frame      capability of the shared frame
 * \param offset     offset into the capability to copy
 * \param length     number of bytes to copy
 * \param node       which node to send the copy request to
 * \param direction  UPDATE or WRITE BACK
 *
 * \return SYS_ERR_OK on sucess,
 *         errval on failure
 */
errval_t xomp_master_copy_memory(struct capref frame,
                                 size_t offset,
                                 size_t length,
                                 uint16_t node,
                                 xomp_master_copy_t direction);

/**
 * \brief executes some work on each worker domains
 *
 * \param task information about the task
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xomp_master_do_work(struct xomp_task *task);

/**
 * \brief builds the argument path based on the own binary name
 *
 * \param local  pointer where to store the local path
 * \param remote pointer where to store the remote path
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xomp_master_build_path(char **local, char **remote);


#if XOMP_BENCH_ENABLED
/**
 * \brief enables basic benchmarking facilities
 *
 * \param runs   the number of runs of the experiment
 * \param flags  flags which benchmarks to enable
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xomp_master_bench_enable(size_t runs,
                                  size_t nthreads,
                                  uint8_t flags);

/**
 * \brief prints the results of the enabled benchmarks
 */
void xomp_master_bench_print_results(void);

#else
#include <barrelfish/debug.h>

static inline errval_t xomp_master_bench_enable(size_t runs,
                                                size_t nthreads,
                                                uint8_t flags)
{
    USER_PANIC("XOMP BENCHMARK NOT ENABLED");
    return -1;
}

static inline void xomp_master_bench_print_results(void)
{
    USER_PANIC("XOMP BENCHMARK NOT ENABLED");
}
#endif

#endif // LIB_XOMP_MASTER_H_
