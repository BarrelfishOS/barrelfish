/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_BENCH_H
#define LIB_DMA_BENCH_H

/// DMA Benchmark control
#define DMA_BENCH_RUN_BENCHMARK 0

/* minimum and maximum buffer sizes */
#define DMA_BENCH_MIN_BITS 6
#define DMA_BENCH_MAX_BITS 27

#define DMA_BENCH_BUFFER_SIZE (1UL << DMA_BENCH_MAX_BITS)

#define DMA_BENCH_NUM_RUNS \
    (DMA_BENCH_MAX_BITS - DMA_BENCH_MIN_BITS + 1)

#define DMA_BENCH_NUM_REPS 500

#define DMA_BENCH_HOST_NUMA_OFFSET (128UL * 1024 * 1024 * 1024)

#define DMA_BENCH_HOST_BASE (64UL * 1024 * 1024 * 1024)
#define DMA_BENCH_HOST_BASE2 (DMA_BENCH_HOST_BASE + DMA_BENCH_HOST_NUMA_OFFSET)

/// hardcoded base of the first Xeon Phi in babybel
#define DMA_BENCH_HOST_XEON_PHI_BASE 0x0000380000000000ULL

/// hardcoded base of the second Xeon Phi in babybel
#define DMA_BENCH_HOST_XEON_PHI_BASE2 0x0000380600000000ULL

#define DMA_BENCH_XPHI_BASE_OFFSET (4UL * 1024 * 1024 * 1024)



errval_t dma_bench_run_default(struct dma_device *dev);

errval_t dma_bench_run_default_xphi(struct dma_device *dev);

errval_t dma_bench_run(struct dma_device *dev, lpaddr_t src, lpaddr_t dst);

errval_t dma_bench_run_memcpy(void *dst, void *src);

#endif /* DMA_BENCH_INTERNAL_H */
