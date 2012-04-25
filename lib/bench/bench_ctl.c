/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <bench/bench.h>

bench_ctl_t *bench_ctl_init(enum bench_ctl_mode mode,
                            size_t              dimensions,
                            size_t              min_runs)
{
    bench_ctl_t *ctl;

    ctl = calloc(1, sizeof(*ctl));
    ctl->mode = mode;
    ctl->result_dimensions = dimensions;
    ctl->min_runs = min_runs;

    if (mode == BENCH_MODE_FIXEDRUNS) {
        ctl->data = calloc(min_runs * dimensions, sizeof(*ctl->data));
    } else {
        assert(!"NYI");
    }

    return ctl;
}

void bench_ctl_destroy(bench_ctl_t *ctl)
{
    free(ctl->data);
    free(ctl);
}

void bench_ctl_dry_runs(bench_ctl_t *ctl,
                        size_t       dry_runs)
{
    ctl->dry_runs = dry_runs;
}

bool bench_ctl_add_run(bench_ctl_t *ctl,
                       cycles_t* result)
{
    cycles_t *dst;

    if (ctl->result_count == ctl->min_runs) {
        return true;
    }

    dst = ctl->data + ctl->result_count * ctl->result_dimensions;
    memcpy(dst, result, sizeof(dst) * ctl->result_dimensions);

    ctl->result_count++;

    return ctl->result_count == ctl->min_runs;
}

void bench_ctl_dump_csv(bench_ctl_t *ctl,
                        const char  *prefix)
{
    size_t i, j;
    cycles_t *v;
    size_t dim = ctl->result_dimensions;

    for (i = 0; i < ctl->result_count; i++) {
        printf("%s", prefix);

        v = ctl->data + i * dim;
        for (j = 0; j < dim; j++) {
            printf("%"PRIu64, v[j]);
            if (j != dim - 1) {
                printf(",");
            }
        }
        printf("\n");
    }
    fflush(stdout);
}


/**
 * Return bin index for this value. We keep two more bins than bin count, one
 * for the values below min (bin_count), and one for those above (bin_count + 1)
 */
static inline size_t val2bin(size_t bin_count, cycles_t min, cycles_t max,
                             cycles_t value)
{
    cycles_t bin_width = (max - min) / bin_count;

    if (value < min) {
        return bin_count;
    } else if (value >= max) {
        return bin_count + 1;
    }

    return (value - min) / bin_width;
}

/** Return the lower value for a bin */
static inline cycles_t bin2val(size_t bin_count, cycles_t min, cycles_t max,
                               size_t idx)
{
    cycles_t bin_width = (max - min) / bin_count;
    return min + idx * bin_width;
}

/**
 * Returns a newly allocated array of bins, filled with the desired values.
 * The array has bin_count + 2 elements. result[bin_count] contains the number
 * of values below the minium, result[bin_count + 1] those above the maximum.
 * The caller is responsible for freeing the array.
 */
static cycles_t *do_bincounting(bench_ctl_t *ctl,
                                size_t dimension,
                                size_t bin_count,
                                cycles_t min,
                                cycles_t max)
{
    size_t *bins;
    size_t i;
    cycles_t *v;

    bins = calloc(bin_count + 2, sizeof(size_t));

    for (i = 0; i < ctl->result_count; i++) {
        v = ctl->data + (ctl->result_dimensions * i + dimension);
        bins[val2bin(bin_count, min, max, *v)]++;
    }

    return bins;
}


void bench_ctl_dump_csv_bincounting(bench_ctl_t *ctl,
                                    size_t dimension,
                                    size_t bin_count,
                                    cycles_t min,
                                    cycles_t max,
                                    const char *prefix)
{
    size_t *bins;
    size_t i;
    cycles_t val;

    bins = do_bincounting(ctl, dimension, bin_count, min, max);

    printf("%sbellow,%"PRIu64"\n", prefix, bins[bin_count]);
    printf("%sabove,%"PRIu64"\n", prefix, bins[bin_count+1]);
    for (i = 0; i < bin_count; i++) {
        if (bins[i] > 0) {
            val = bin2val(bin_count, min, max, i);
            printf("%s%"PRIu64",%"PRIu64"\n", prefix, val, bins[i]);
        }
    }

    free(bins);
}

