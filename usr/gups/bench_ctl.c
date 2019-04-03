/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <bench.h>
#include <limits.h>

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
                        const char  *prefix,
                        uint64_t tscperus)
{
    size_t i, j;
    cycles_t *v;
    size_t dim = ctl->result_dimensions;

#if BENCH_DUMP_OCTAVE
    printf("%s = [\n", prefix);
    for (i = 0; i < ctl->result_count; i++) {
        printf("    ");
        v = ctl->data + i * dim;
        for (j = 0; j < dim; j++) {
            printf("%"PRIu64", %f", v[j], v[j]/(float)tscperus);
            if (j != dim - 1) {
                printf(",");
            }
        }
        printf(";\n");
    }
    printf("];\n");
#else

    for (i = 0; i < ctl->result_count; i++) {
        printf("%% %s", prefix);

        v = ctl->data + i * dim;
        for (j = 0; j < dim; j++) {
            printf("%"PRIu64", %f", v[j], v[j]/(float)tscperus);
            if (j != dim - 1) {
                printf(",");
            }
        }
        printf("\n");
    }
#endif
    fflush(stdout);
}



static cycles_t *get_array(bench_ctl_t *ctl,
                          size_t dimension)
{
    cycles_t *array = calloc(ctl->result_count, sizeof(cycles_t));
    assert(array != NULL);

    for (size_t i = 0; i < ctl->result_count; i++) {
        array[i] = *(ctl->data + (ctl->result_dimensions * i
                    + dimension));
    }
    return array;
}

static cycles_t *do_sorting(cycles_t *array,
                            size_t len)
{
    size_t i, j;
    cycles_t *sorted_array = array;
    cycles_t temp_holder;


    // sort the array
    for (i = 0; i < len; ++i) {
        for (j = i; j < len; ++j) {
            if (sorted_array[i] > sorted_array[j]) {
                temp_holder = sorted_array[i];
                sorted_array[i] = sorted_array[j];
                sorted_array[j] = temp_holder;
            }
        } // end for: j
    } // end for: i
    return sorted_array;
} // end function: do_sorting

void bench_ctl_dump_analysis(bench_ctl_t *ctl,
                             size_t dimension,
                             const char *prefix,
                             cycles_t tscperus)
{
    size_t len = ctl->result_count;
    cycles_t *array = get_array(ctl, dimension);

#if BENCH_DUMP_OCTAVE
    cycles_t avg, std_dev;
    bench_stddev(array, len, 0, &avg, &std_dev);
#endif

    cycles_t *final_array =  do_sorting(array, len);

    size_t max99 = (size_t)((0.99 * len) + 0.5);
#if BENCH_DUMP_OCTAVE

    // printf("\% [name]  [runs]  [avg]  [stdev]  [min]  [med]  [P99]  [max]\n");

    //printf("%% %s\n%"PRIu64", %"PRIu64", %"PRIu64", %"PRIu64", %"PRIu64", %"PRIu64
    //       ", %"PRIu64"; \n", prefix,(uint64_t)len, avg, std_dev, final_array[len/2],
    //       final_array[0], final_array[max99-1], final_array[len-1]);

    printf("%% %s, %"PRIu64", %f, %f, %f, %f, %f, %f;\n",prefix, (uint64_t)len,
           (avg /(float)tscperus), (std_dev / ((float)tscperus*(float)tscperus)),
           (final_array[len/2]/(float)tscperus), (final_array[0]/(float)tscperus),
           (final_array[max99-1]/(float)tscperus),(final_array[len-1]/(float)tscperus));
#else
    printf("run [%"PRIu64"], med_pos[%"PRIu64"], min_pos[%"PRIu64"], "
           "P99[%"PRIu64"], max[%"PRIu64"]\n",
           (uint64_t)len,
           (uint64_t)(len/2),
           (uint64_t)0,
           (uint64_t)(max99-1),
           (uint64_t)(len-1));

    printf("run [%"PRIu64"], med[%"PRIu64"], min[%"PRIu64"], "
           "P99[%"PRIu64"], max[%"PRIu64"]\n",
           (uint64_t)len,
           (uint64_t)final_array[len/2],
           (uint64_t)final_array[0],
           (uint64_t)final_array[max99-1],
           (uint64_t)final_array[len-1]);

    printf("run [%"PRIu64"], med[%f], min[%f], "
           "P99[%f], max[%f]\n",
           (uint64_t)len,
           (final_array[len/2]/(float)tscperus),
           (final_array[0]/(float)tscperus),
           (final_array[max99-1]/(float)tscperus),
           (final_array[len-1]/(float)tscperus));

    printf("%s, %"PRIu64" %f %f %f %f\n",
           prefix,
           (uint64_t)len,
           (final_array[len/2]/(float)tscperus),
           (final_array[0]/(float)tscperus),
           (final_array[max99-1]/(float)tscperus),
           (final_array[len-1]/(float)tscperus));
#endif
} // end function: bench_ctl_dump_analysis




/**
 * \brief computes the differences of two time stamps with respecting overflow
 *
 * This function also accounts for the overhead when taking timestamps
 *
 * \param tsc_start timestamp of start
 * \param tsc_end   timestamp of end
 *
 * \return elaped cycles
 */
cycles_t bench_time_diff(cycles_t tsc_start, cycles_t tsc_end)
{
    cycles_t result;
    if (tsc_end < tsc_start) {
        result = (LONG_MAX - tsc_start) + tsc_end;
    } else {
        result = (tsc_end - tsc_start);
    }
    return result;
}

/**
 * \brief Compute averages
 *
 * If certain datapoints should be ignored, they should be marked with
 * #BENCH_IGNORE_WATERMARK
 */
cycles_t bench_avg(cycles_t *array, size_t len)
{
    cycles_t sum = 0;
    size_t count = 0;

    // Discarding some initial observations
    for (size_t i = len >> 3; i < len; i++) {
        if (array[i] != BENCH_IGNORE_WATERMARK) {
            sum += array[i];
            count++;
        }
    }

    return sum / count;
}


/**
 * \brief computes the standard deviation s^2 of the sample data
 *
 * \param array         array of data to analyze
 * \param len           size of the array
 * \param correction    apply Bessel's correction (using N-1 instead of N)
 * \param ret_avg       returns the average of the sample
 * \param ret_stddev    returns the standard deviation squared of the sample
 */
void bench_stddev(cycles_t *array, size_t len, uint8_t correction,
                  cycles_t *ret_avg, cycles_t *ret_stddev)
{
    cycles_t avg = bench_avg(array, len);

    cycles_t sum = 0;
    size_t count = 0;
    /// discard some initiali observations
    for (size_t i = len >> 3; i < len; i++) {
        if (array[i] != BENCH_IGNORE_WATERMARK) {
            cycles_t subsum = array[i] - avg;
            sum += (subsum * subsum);
            count++;
        }
    }

    cycles_t std_dev = 0;
    if (correction && count > 1) {
        std_dev = sum / (count - 1);
    } else {
        std_dev = sum / count;
    }

    if (ret_avg) {
        *ret_avg = avg;
    }

    if (ret_stddev) {
        *ret_stddev = std_dev;
    }
}


