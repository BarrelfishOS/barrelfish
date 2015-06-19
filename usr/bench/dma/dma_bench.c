/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 *
 *
 */
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <bench/bench.h>

#include <dma/dma.h>
#include <dma/dma_request.h>
#include <dma/dma_bench.h>
#include <dma/client/dma_client_device.h>
#include <dma/dma_manager_client.h>

#define BENCH_XEON_PHI_DMA 0
#define BENCH_XEON_PHI_DMA_BASE (4UL * 1024 * 1024 * 1024)
#define BENCH_XEON_PHI_DMA_BASE2 (5UL * 1024 * 1024 * 1024)

#define DMA_BUFFER_SIZE  28
#define DMA_BUFFER_COUNT 2

#define EXPECT_SUCCESS(err, msg) if (err_is_fail(err)) {USER_PANIC_ERR(err, msg);}

static struct capref frame;
static size_t frame_size;
static lpaddr_t frame_addr;
static void *frame_virt;

#ifndef __k1om__
static struct capref frame2;
static size_t frame_size2;
static lpaddr_t frame_addr2;
static void *frame_virt2;
#endif

static uint8_t *buffers[2*DMA_BUFFER_COUNT];
static lpaddr_t phys[2*DMA_BUFFER_COUNT];

static void prepare(void)
{
    errval_t err;

    debug_printf("Preparing resources...\n");

    ram_set_affinity(0, (128UL * 1024 * 1024 * 1024));

    err = frame_alloc(&frame, DMA_BUFFER_COUNT * (1UL << DMA_BUFFER_SIZE),
                      &frame_size);
    EXPECT_SUCCESS(err, "allocating frame");

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    EXPECT_SUCCESS(err, "Frame identify");

    frame_addr = id.base;

    err = vspace_map_one_frame(&frame_virt, frame_size, frame, NULL, NULL);
    EXPECT_SUCCESS(err, "Mapping of frame");

    uint8_t *b = frame_virt;
    lpaddr_t p = frame_addr;
    for (uint32_t i = 0; i < DMA_BUFFER_COUNT; ++i) {
        buffers[i] = b;
        phys[i] = p;
        b += ((1UL << DMA_BUFFER_SIZE));
        p += ((1UL << DMA_BUFFER_SIZE));
    }

#ifndef __k1om__

    ram_set_affinity((128UL * 1024 * 1024 * 1024), (256UL * 1024 * 1024 * 1024));

    EXPECT_SUCCESS(err, "allocating frame");
    err = frame_alloc(&frame2, DMA_BUFFER_COUNT * (1UL << DMA_BUFFER_SIZE),
                      &frame_size2);
    struct frame_identity id2;
    err = invoke_frame_identify(frame2, &id2);
    EXPECT_SUCCESS(err, "Frame identify");

    frame_addr2 = id2.base;
    err = vspace_map_one_frame(&frame_virt2, frame_size2, frame2, NULL, NULL);
    EXPECT_SUCCESS(err, "Mapping of frame");

    b = frame_virt2;
    p = frame_addr2;
    for (uint32_t i = 0; i < DMA_BUFFER_COUNT; ++i) {
        buffers[i+DMA_BUFFER_COUNT] = b;
        phys[i+DMA_BUFFER_COUNT] = p;
        b += ((1UL << DMA_BUFFER_SIZE));
        p += ((1UL << DMA_BUFFER_SIZE));
    }
#endif

    debug_printf("preparation done.\n");
}

int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("DMA Test domain started\n");

    prepare();

    bench_init();

#if 0
    char svc_name[30];
    uint8_t numa_node = (disp_get_core_id() >= 20);
    snprintf(svc_name, 30, "ioat_dma_svc.%u", numa_node);
    iref_t iref;
    err = nameservice_blocking_lookup(svc_name, &iref);
#endif



#ifdef __k1om__
    err = dma_manager_wait_for_driver(DMA_DEV_TYPE_XEON_PHI, disp_xeon_phi_id());
    EXPECT_SUCCESS(err, "waiting for driver");
    struct dma_client_info info = {
        .type = DMA_CLIENT_INFO_TYPE_NAME,
        .device_type = DMA_DEV_TYPE_XEON_PHI,
        .args = {
            .name = XEON_PHI_DMA_SERVICE_NAME
        }
    };
#else
#if BENCH_XEON_PHI_DMA
    err = dma_manager_wait_for_driver(DMA_DEV_TYPE_XEON_PHI, 1);
    EXPECT_SUCCESS(err, "waiting for driver");
    struct dma_client_info info = {
        .type = DMA_CLIENT_INFO_TYPE_NAME,
        .device_type = DMA_DEV_TYPE_XEON_PHI,
        .args = {
            .name = "xeon_phi_dma_svc.0"
        }
    };
#else
    err = dma_manager_wait_for_driver(DMA_DEV_TYPE_IOAT, 0);
    EXPECT_SUCCESS(err, "waiting for driver");
    struct dma_client_info info = {
        .type = DMA_CLIENT_INFO_TYPE_NAME,
        .device_type = DMA_DEV_TYPE_IOAT,
        .args = {
            .name = "ioat_dma_svc.0"
        }
    };
#endif
#endif

    struct dma_client_device *dev;
    err = dma_client_device_init(&info, &dev);

    err = dma_register_memory((struct dma_device *)dev, frame);
    EXPECT_SUCCESS(err, "registering memory\n");

#ifndef __k1om
    debug_printf("NUMA 0 -> NUMA 1\n");
 //   err = dma_bench_run_memcpy(buffers[0], buffers[DMA_BUFFER_COUNT]);
    EXPECT_SUCCESS(err, "dma_bench_run_memcpy\n");

    debug_printf("NUMA 1 -> NUMA 0\n");
  //  err = dma_bench_run_memcpy(buffers[DMA_BUFFER_COUNT], buffers[0]);
    EXPECT_SUCCESS(err, "dma_bench_run_memcpy\n");
#endif
    debug_printf("Numa 0 -> Numa 0\n");
    //err = dma_bench_run_memcpy(buffers[0], buffers[1]);
    EXPECT_SUCCESS(err, "dma_bench_run_memcpy\n");

#if BENCH_XEON_PHI_DMA
    err = dma_bench_run((struct dma_device *)dev, BENCH_XEON_PHI_DMA_BASE,
                        BENCH_XEON_PHI_DMA_BASE2);
    EXPECT_SUCCESS(err, "dma_bench_run\n");
    debug_printf("DMA Benchmark done.\n");
    return 0;
#else
    err = dma_bench_run((struct dma_device *)dev, phys[0], phys[1]);
    EXPECT_SUCCESS(err, "dma_bench_run\n");
#ifndef __k1om__
    err = dma_register_memory((struct dma_device *)dev, frame2);
    EXPECT_SUCCESS(err, "registering memory\n");

    debug_printf("Numa 0 -> Numa 1\n");
    debug_printf("%lx -> %lx, \n", phys[0], phys[DMA_BUFFER_COUNT]);
    err = dma_bench_run((struct dma_device *)dev, phys[0], phys[DMA_BUFFER_COUNT]);
    EXPECT_SUCCESS(err, "dma_bench_run\n");

    debug_printf("Numa 1 -> Numa 0\n");
    err = dma_bench_run((struct dma_device *)dev, phys[DMA_BUFFER_COUNT], phys[0]);
    EXPECT_SUCCESS(err, "dma_bench_run\n");
#endif
#endif
    debug_printf("DMA Benchmark done.\n");

    return 0;
}

