/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <limits.h>
#include <string.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>
#include <xeon_phi/xeon_phi.h>

#include <dma_internal.h>
#include <dma/dma_bench.h>
#include <dma_device_internal.h>
#include <dma_channel_internal.h>
#include <dma_request_internal.h>

#include <debug.h>

#define DMA_BENCH_PRINT(x...) debug_printf(x)
//#define DMA_BENCH_PRINT(x...)

static volatile int dma_done_flag = 0;

uint64_t request_counter = 0;

static void dma_done_cb(errval_t err,
                        dma_req_id_t id,
                        void *arg)
{
    assert(err_is_ok(err));
    dma_done_flag = 1;
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



/*
 * ============================================================================
 *
 * ============================================================================
 */

errval_t dma_bench_run_default_xphi(struct dma_device *dev)
{
    debug_printf("dma_bench_run_default_xphi\n");
#ifdef __k1om__

    if (disp_xeon_phi_id() == 0) {
        return SYS_ERR_OK;
    }
#else
    return SYS_ERR_OK;
    if (disp_get_core_id() >= 20) {
        return SYS_ERR_OK;
    }
#endif

    debug_printf("DMA BENCHMARK started\n");
    debug_printf("================================\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: xphi[0] -> xphi[0]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");

    dma_bench_run(dev, DMA_BENCH_XPHI_BASE_OFFSET,
                  DMA_BENCH_XPHI_BASE_OFFSET + DMA_BENCH_BUFFER_SIZE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");

    debug_printf("DMA-BENCH: xphi[0] -> host[0]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    dma_bench_run(dev, DMA_BENCH_XPHI_BASE_OFFSET,
                  XEON_PHI_SYSMEM_BASE + DMA_BENCH_HOST_BASE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");

    debug_printf("DMA-BENCH: host[0] -> xphi[0]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    dma_bench_run(dev, XEON_PHI_SYSMEM_BASE + DMA_BENCH_HOST_BASE,
                  DMA_BENCH_XPHI_BASE_OFFSET);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");

    debug_printf("DMA-BENCH: xphi[0] -> xphi[1]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    dma_bench_run(dev, DMA_BENCH_XPHI_BASE_OFFSET,
                  XEON_PHI_SYSMEM_BASE + 31 * XEON_PHI_SYSMEM_PAGE_SIZE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");

    debug_printf("DMA-BENCH: xphi[1] -> xphi[0]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    dma_bench_run(dev,XEON_PHI_SYSMEM_BASE + 31 * XEON_PHI_SYSMEM_PAGE_SIZE,
                  DMA_BENCH_XPHI_BASE_OFFSET);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");

    return SYS_ERR_OK;
}

errval_t dma_bench_run_default(struct dma_device *dev)
{
    debug_printf("DMA BENCHMARK started\n");
    debug_printf("================================\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: host[0] -> host[0]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    //dma_bench_run(dev, DMA_BENCH_HOST_BASE,
    //              DMA_BENCH_HOST_BASE + DMA_BENCH_BUFFER_SIZE);
    return SYS_ERR_OK;
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: host[0] -> host[1]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    dma_bench_run(dev, DMA_BENCH_HOST_BASE, DMA_BENCH_HOST_BASE2);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: host[1] -> host[0]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    dma_bench_run(dev, DMA_BENCH_HOST_BASE2, DMA_BENCH_HOST_BASE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: host[1] -> host[1]\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    dma_bench_run(dev, DMA_BENCH_HOST_BASE2,DMA_BENCH_HOST_BASE2+ DMA_BENCH_BUFFER_SIZE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: host->card\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    dma_bench_run(dev, DMA_BENCH_HOST_BASE, DMA_BENCH_HOST_XEON_PHI_BASE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    debug_printf("DMA-BENCH: card->card\n");
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
   // dma_bench_run(dev, DMA_BENCH_HOST_XEON_PHI_BASE,DMA_BENCH_HOST_XEON_PHI_BASE + DMA_BENCH_BUFFER_SIZE);
    debug_printf("\n");
    debug_printf("--------------------------------\n");
    debug_printf("\n");
    debug_printf("DONE.\n");
    debug_printf("=========================================================");
    return SYS_ERR_OK;
}

errval_t dma_bench_run(struct dma_device *dev, lpaddr_t src, lpaddr_t dst)
{
    errval_t err;
     cycles_t tsc_start, tsc_end;
     uint64_t tscperus;
     bench_ctl_t *ctl;

     cycles_t result;

     bench_init();

     struct dma_channel *chan = dma_device_get_channel((struct dma_device *)dev);

     err = sys_debug_get_tsc_per_ms(&tscperus);
     assert(err_is_ok(err));
     tscperus /= 1000;

     for (uint8_t i = DMA_BENCH_MIN_BITS; i <= DMA_BENCH_MAX_BITS; ++i) {
         size_t size = (1UL << i);

         ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, DMA_BENCH_NUM_REPS);

         uint8_t idx = 0;
         do {
             tsc_start = bench_tsc();
             dma_done_flag = 0x0;

             struct dma_req_setup setup = {
                 .done_cb = dma_done_cb,
                 .cb_arg = &request_counter,
                 .args = {
                     .memcpy = {
                         .src = src,
                         .dst = dst,
                         .bytes = size
                     }
                 }
             };

             dma_req_id_t id;

             err = dma_request_memcpy_chan(chan, &setup, &id);
             if (err_is_fail(err)) {
                 USER_PANIC_ERR(err, "could not set the memcy request");
             }

             tsc_end = bench_tsc();
             result = calculate_time(tsc_start, tsc_end);

             while (!dma_done_flag) {
                 dma_channel_poll(chan);
             }

             tsc_end = bench_tsc();
             result = calculate_time(tsc_start, tsc_end);
             idx++;
             request_counter++;
         } while (!bench_ctl_add_run(ctl, &result));
         char buf[50];

         snprintf(buf, sizeof(buf), "%u", i);
         bench_ctl_dump_analysis(ctl, 0, buf, tscperus);

         bench_ctl_destroy(ctl);

     }

     return SYS_ERR_OK;
}

errval_t dma_bench_run_memcpy(void *dst, void *src)
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
    debug_printf("starting benchmark memcpy\n");
    debug_printf("======================================\n");

    for (uint8_t i = DMA_BENCH_MIN_BITS; i <= DMA_BENCH_MAX_BITS; ++i) {
        size_t size = (1UL << i);

        ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, DMA_BENCH_NUM_REPS);

        uint8_t idx = 0;
        do {
            tsc_start = bench_tsc();
            memcpy(dst, src, size);
            tsc_end = bench_tsc();

            result = calculate_time(tsc_start, tsc_end);
            idx++;
        } while (!bench_ctl_add_run(ctl, &result));
        char buf[50];

        snprintf(buf, sizeof(buf), "%u", i);
        bench_ctl_dump_analysis(ctl, 0, buf, tscperus);

        bench_ctl_destroy(ctl);
    }
    debug_printf("======================================\n");

    return SYS_ERR_OK;
}
