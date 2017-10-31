/**
 * \file
 * \brief Global variable definitions for distops benchmarks
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>

//{{{1 Benchmark controls

// set default values here, override if necessary
uint32_t NUM_COPIES_START = 256;
uint32_t NUM_COPIES_END = 65536;
uint32_t ITERS = 1000;

void bench_enable_tracing(void);
void bench_enable_tracing(void)
{
    NUM_COPIES_START = NUM_COPIES_END = 4096;
}
