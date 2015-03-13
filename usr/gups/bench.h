/*
 * bench.h
 *
 *  Created on: Mar 11, 2015
 *      Author: acreto
 */

#ifndef USR_GUPS_BENCH_H_
#define USR_GUPS_BENCH_H_


#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
typedef uint64_t cycles_t;
#include <sys/cdefs.h>

#ifndef DUNE
#include <inttypes.h>
#else
#include <dune.h>
#endif

#define BENCH_IGNORE_WATERMARK 0XDEADBEEF

#define BENCH_DUMP_OCTAVE 1

__BEGIN_DECLS
cycles_t bench_avg(cycles_t *array, size_t len);
void bench_stddev(cycles_t *array, size_t len, uint8_t correction,
                  cycles_t *ret_avg, cycles_t *ret_stddev);
cycles_t bench_time_diff(cycles_t tsc_start, cycles_t tsc_end);
__END_DECLS


/*
 * Control functions for benchmarks
 */

enum bench_ctl_mode {
    // Fixed number of runs (exactly min_runs)
    BENCH_MODE_FIXEDRUNS,
};

struct bench_ctl {
    enum bench_ctl_mode mode;
    size_t              result_dimensions;

    size_t              min_runs;
    size_t              dry_runs;

    size_t              result_count;
    cycles_t           *data;
};

typedef struct bench_ctl bench_ctl_t;


/**
 * Initialize a benchmark control instance.
 *
 * @param mode       Mode of the benchmark (enum bench_ctl_mode)
 * @param dimensions Number of values each run produces
 * @param min_runs   Minimal number of runs to be executed
 *
 * @return Control handle, to be passed on subsequent calls to bench_ctl_
 *         functions.
 */
bench_ctl_t *bench_ctl_init(enum bench_ctl_mode mode,
                            size_t              dimensions,
                            size_t              min_runs);

/**
 * Frees all resources associated with this benchmark control instance. Should
 * be called after the benchmark is done and the results are dumped.
 *
 * @param ctl Control handle
 */
void bench_ctl_destroy(bench_ctl_t *ctl);

/**
 * Add a fixed number of dry runs whose results should not be recorded. Should
 * be called before any calls to bench_ctl_add_run().
 *
 * @param ctl      Control handle
 * @param dry_runs Number of dry runs
 */
void bench_ctl_dry_runs(bench_ctl_t *ctl,
                        size_t       dry_runs);

/**
 * Add results from one run of the benchmark.
 *
 * @param ctl    Control handle
 * @param result Pointer to the 'dimensions' values of this run
 *
 * @return true if this was the last run necessary, false if more runs are
 *         needed.
 */
bool bench_ctl_add_run(bench_ctl_t *ctl,
                       cycles_t* result);

/**
 * Dump results of the benchmark to the standard output. One line per run, the
 * lines will be prefixed with prefix, the values separeted using commas.
 *
 * @param ctl    Control handle
 * @param prefix String to be printed before each line
 */
void bench_ctl_dump_csv(bench_ctl_t *ctl,
                        const char  *prefix, uint64_t tscperus);


void bench_ctl_dump_analysis(bench_ctl_t *ctl,
                                    size_t dimension,
                                    const char *prefix,
                                    cycles_t tscperus);




#endif /* USR_GUPS_BENCH_H_ */
