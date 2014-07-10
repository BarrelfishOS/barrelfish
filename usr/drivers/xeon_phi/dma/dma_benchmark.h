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
#ifndef XEON_PHI_DMA_BENCHMARK_H
#define XEON_PHI_DMA_BENCHMARK_H

#define XEON_PHI_DMA_BENCH_MIN_BITS 6
#define XEON_PHI_DMA_BENCH_MAX_BITS 27
#define XEON_PHI_DMA_BENCH_SIZE (1UL << XEON_PHI_DMA_BENCH_MAX_BITS)
#define XEON_PHI_DMA_BENCH_RUNS \
    (XEON_PHI_DMA_BENCH_MAX_BITS - XEON_PHI_DMA_BENCH_MIN_BITS + 1)
#define XEON_PHI_DMA_BENCH_REPS 50


#define XEON_PHI_DMA_BENCH_HOST_BASE ((512UL+64UL)*1024*1024*1024)
#define XEON_PHI_DMA_BENCH_CARD_BASE (5UL*1024*1024*1024)

#define XEON_PHI_DMA_BENCH_CARD_BASE2 ((512UL+496UL + 5UL)*1024*1024*1024)
/* XXX: there is an offset of 8G to be added when runnig the benchmark on the card0 */
//#define XEON_PHI_DMA_BENCH_CARD_BASE2 ((512UL+496UL + 8UL + 5UL)*1024*1024*1024)


struct dma_info;
struct xdma_channel;

errval_t xeon_phi_dma_bench_run_default(struct dma_info *di);

errval_t xeon_phi_dma_bench_run(struct xdma_channel *chan,
                                uint8_t nchan,
                                lpaddr_t src,
                                lpaddr_t dst);

#endif /* XEON_PHI_DMA_BENCHMARK_H */
