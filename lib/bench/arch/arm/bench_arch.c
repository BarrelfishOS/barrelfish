/**
 * \file
 * \brief Bench library initialization.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <bench/bench_arch.h>
#include <barrelfish/sys_debug.h>


uint64_t tsc_hz;
static uint64_t tscperms;
static uint64_t tscperus;

void bench_arch_init(void)
{
    errval_t err = sys_debug_hardware_timer_hertz_read((uintptr_t *)&tsc_hz);
    assert(err_is_ok(err));
    tscperms = tsc_hz / 1000;
    tscperus = tscperms / 1000;
}

uint64_t bench_tsc_to_ms(cycles_t tsc)
{
    return tsc / tscperms;
}
uint64_t bench_tsc_to_us(cycles_t tsc)
{
    return tsc / tscperus;
}
uint64_t bench_tsc_per_us(void)
{
    return tscperus;
}
uint64_t bench_tsc_per_ms(void)
{
    return tscperms;
}

