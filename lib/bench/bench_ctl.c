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

