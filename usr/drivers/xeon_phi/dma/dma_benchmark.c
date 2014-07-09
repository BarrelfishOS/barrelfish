/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <limits.h>
#include "xeon_phi_internal.h"
#include "dma.h"
#include "dma_channel.h"
#include "dma_descriptor_ring.h"
#include "dma_benchmark.h"
#include "debug.h"

#include <bench/bench.h>

struct dma_info
{
    struct xdma_channel channels[XEON_PHI_DMA_CHAN_NUM];
    xeon_phi_dma_t dma_dev;
    uint8_t chan_alloc_next;
};

errval_t xeon_phi_dma_bench_run_default(struct dma_info *di)
{
    struct xdma_channel *chan = di->channels;

    for (uint32_t i = 0x0; i < 0xFFFF; ++i) {
        event_dispatch_non_block(get_default_waitset());
        thread_yield();
    }

    debug_printf("DMA-BENCH: Card Local\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    xeon_phi_dma_bench_run(chan, 4, XEON_PHI_DMA_BENCH_CARD_BASE,
    XEON_PHI_DMA_BENCH_CARD_BASE + XEON_PHI_DMA_BENCH_SIZE);

    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");

    debug_printf("DMA-BENCH: Card-Host\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    xeon_phi_dma_bench_run(chan, 4,
                           XEON_PHI_DMA_BENCH_CARD_BASE,
                           XEON_PHI_DMA_BENCH_HOST_BASE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: Host-Card\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    xeon_phi_dma_bench_run(chan, 4,
                           XEON_PHI_DMA_BENCH_HOST_BASE,
                           XEON_PHI_DMA_BENCH_CARD_BASE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: Card-Card\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    xeon_phi_dma_bench_run(chan, 4,
                           XEON_PHI_DMA_BENCH_CARD_BASE,
                           XEON_PHI_DMA_BENCH_CARD_BASE2);

    debug_printf("DMA-BENCH: Card-Card\n");
    xeon_phi_dma_bench_run(chan, 4,
                           XEON_PHI_DMA_BENCH_CARD_BASE2,
                           XEON_PHI_DMA_BENCH_CARD_BASE);

    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("DONE.\n");
    while (1)
        ;
    return SYS_ERR_OK;
}

static volatile uint8_t dma_done;

static errval_t dma_done_cb(void *a,
                            errval_t err,
                            xeon_phi_dma_id_t id)
{
    dma_done = 0x1;
    return SYS_ERR_OK;
}

static inline cycles_t calculate_time(cycles_t tsc_start,
                                      cycles_t tsc_end)
{
    cycles_t result;
    if (tsc_end < tsc_start) {
        result = (LONG_MAX - tsc_start) + tsc_end - bench_tscoverhead();
    } else {
        result = (tsc_end - tsc_start - bench_tscoverhead());
    }
    return result;
}

errval_t xeon_phi_dma_bench_run(struct xdma_channel *chanarr,
                                uint8_t nchan,
                                lpaddr_t src,
                                lpaddr_t dst)
{
    errval_t err;
    cycles_t tsc_start, tsc_end;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    cycles_t result;

    bench_init();

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    for (uint8_t i = XEON_PHI_DMA_BENCH_MIN_BITS; i <= XEON_PHI_DMA_BENCH_MAX_BITS;
                    ++i) {
        size_t size = (1UL << i);

        ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1,
        XEON_PHI_DMA_BENCH_REPS);

        uint8_t idx = 0;
        //debug_printf("Benchmark: Run %u, size = %lu bytes, [%016lx] -> [%016lx]\n", idx, size, src, dst);
        do {
            struct xdma_channel *chan = &chanarr[(idx++) % nchan];
            if (!(idx % 25)) {
                // debug_printf("Benchmark: Run %u, size = %lu bytes, [%016lx] -> [%016lx]\n", idx, size, src, dst);
            }
            struct dma_req_setup setup = {
                .type = XDMA_REQ_TYPE_MEMCPY,
                .cb = dma_done_cb
            };
            setup.info.mem.dst = dst;
            setup.info.mem.src = src;
            setup.info.mem.bytes = size;
            xeon_phi_dma_id_t did;

            dma_done = 0x0;
            tsc_start = bench_tsc();

            err = xdma_channel_req_memcpy(chan, &setup, &did);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not set the memcy request");
            }

            assert(err_is_ok(err));

            while (!dma_done) {
                err = xdma_channel_poll(chan);
            }

            tsc_end = bench_tsc();
            result = calculate_time(tsc_start, tsc_end);
            idx++;
        } while (!bench_ctl_add_run(ctl, &result));
        char buf[50];

        snprintf(buf, sizeof(buf), "%u", i);
        bench_ctl_dump_analysis(ctl, 0, buf, tscperus);

        bench_ctl_destroy(ctl);

    }

    return SYS_ERR_OK;
}

