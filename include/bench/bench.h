/**
 * \file
 * \brief Bench library.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BENCH_H
#define BENCH_H

#include <barrelfish/types.h> // for cycles_t
#include <bench/bench_arch.h>
#include <sys/cdefs.h>

#define BENCH_IGNORE_WATERMARK 0XDEADBEEF

__BEGIN_DECLS
void bench_init(void);
cycles_t bench_avg(cycles_t *array, size_t len);
cycles_t bench_variance(cycles_t *array, size_t len);
cycles_t bench_tscoverhead(void);
__END_DECLS

#endif // BENCH_H
