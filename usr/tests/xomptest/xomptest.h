/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XOMPTTEST_H_
#define XOMPTTEST_H_

// cache-line size / sizeof(int) == 64 Bytes / 4 Bytes == 16
#define IT 16
#define MAX 100000000
#define WORK_SIZE   (2 * MAX * sizeof(uint32_t))

#define BENCH_N_RUNS 10
#define NTHREADS 30
#define STACKSIZE 0

#define BOMP_NTHREADS NTHREADS

void do_process(uint32_t *src,
                uint32_t *dst);

void do_process_single(uint32_t *src,
                       uint32_t *dst);

#endif /* XOMPTTEST_H_ */
