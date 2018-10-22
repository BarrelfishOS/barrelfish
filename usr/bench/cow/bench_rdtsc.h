/*
 * libbenchtsc is a simple benchmarking library that uses the rdtsc
 * x86 instruction.
 * Copyright (c) 2014, Simon Gerber <gesimu@gmail.com>
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef BENCH_RDTSC_H
#define BENCH_RDTSC_H
#include <math.h>
#include <stdbool.h>
#include <inttypes.h>
#include <stdlib.h>

extern bool rdtscp_flag;
extern double ticks_per_nano;

static inline uint64_t rdtscp(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtscp" : "=a" (eax), "=d" (edx) :: "ecx");
    return ((uint64_t)edx << 32) | eax;
}

static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}


static inline uint64_t bench_tsc()
{
    if (rdtscp_flag) {
        return rdtscp();
    } else {
        return rdtsc();
    }
}

static inline void cpuid(uint32_t function, uint32_t *eax, uint32_t *ebx,
                         uint32_t *ecx, uint32_t *edx)
{
    // make it possible to omit certain return registers
    uint32_t a, b, c, d;
    if (eax == NULL) {
        eax = &a;
    }
    if (ebx == NULL) {
        ebx = &b;
    }
    if (ecx == NULL) {
        ecx = &c;
    }
    if (edx == NULL) {
        edx = &d;
    }
    __asm volatile("cpuid"
                   : "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)
                   : "a" (function)
                   );
}

static inline double bench_tsc_to_ms(uint64_t tsc)
{
    return tsc / (ticks_per_nano * 1e6);
}

void bench_init(void);

struct bench_calc_st;
double bench_avg(double *measurements, int runs, struct bench_calc_st *st);
double bench_sdev(double *measurements, int runs, struct bench_calc_st *st);
size_t bench_calc_st_sz(void);

#endif // BENCH_RDTSC_H
