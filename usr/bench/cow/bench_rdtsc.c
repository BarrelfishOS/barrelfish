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

#include "bench_rdtsc.h"
#include <time.h>

double ticks_per_nano = 0.0;
bool rdtscp_flag = false;

const int NANO_SECONDS_IN_SEC = 1000000000;
/* returns a static buffer of struct timespec with the time difference of ts1 and ts2
   ts1 is assumed to be greater than ts2 */
static struct timespec *TimeSpecDiff(struct timespec *ts1, struct timespec *ts2)
{
  static struct timespec ts;
  ts.tv_sec = ts1->tv_sec - ts2->tv_sec;
  ts.tv_nsec = ts1->tv_nsec - ts2->tv_nsec;
  if (ts.tv_nsec < 0) {
    ts.tv_sec--;
    ts.tv_nsec += NANO_SECONDS_IN_SEC;
  }
  return &ts;
}

static void calibrate_ticks(void)
{
    struct timespec begints, endts;
    uint64_t begin = 0, end = 0;
    clock_gettime(CLOCK_MONOTONIC, &begints);
    begin = bench_tsc();
    uint64_t i;
    for (i = 0; i < 1000000; i++); /* must be CPU intensive */
    end = bench_tsc();
    clock_gettime(CLOCK_MONOTONIC, &endts);
    struct timespec *tmpts = TimeSpecDiff(&endts, &begints);
    uint64_t nsecElapsed = tmpts->tv_sec * 1000000000LL + tmpts->tv_nsec;
    ticks_per_nano = (double)(end - begin)/(double)nsecElapsed;
}

void bench_init(void)
{
    uint32_t eax, ebx, ecx, edx;
    // Check for rdtscp instruction
    cpuid(0x80000001, &eax, &ebx, &ecx, &edx);
    if ((edx >> 27) & 1) {
        rdtscp_flag = true;
    } else {
        rdtscp_flag = false;
    }

    calibrate_ticks();
}

struct bench_calc_st {
    double *series;
    int runs;
    double avg;
    double sdev;
};

static void calculate_(double *measurements, int runs, struct bench_calc_st *st)
{
    double mean = 0.0, M2 = 0.0;
    for (int i = 0; i < runs; i++) {
        double delta = measurements[i] - mean;
        mean = mean + delta / (i+1);
        M2 = M2 + delta*(measurements[i]-mean);
    }

    st->series = measurements;
    st->runs = runs;
    st->avg = mean;
    st->sdev = sqrt(M2/(runs-1));
}

double bench_avg(double *measurements, int runs, struct bench_calc_st *st)
{
    struct bench_calc_st tmp = { 0 };
    if (!st) {
        st = &tmp;
    }
    if (st->series != measurements || st->runs != runs) {
        calculate_(measurements, runs, st);
    }
    return st->avg;
}

double bench_sdev(double *measurements, int runs, struct bench_calc_st *st)
{
    struct bench_calc_st tmp = { 0 };
    if (!st) {
        st = &tmp;
    }
    if (st->series != measurements || st->runs != runs) {
        calculate_(measurements, runs, st);
    }
    return st->sdev;
}

size_t bench_calc_st_sz(void)
{
    return sizeof(struct bench_calc_st);
}
