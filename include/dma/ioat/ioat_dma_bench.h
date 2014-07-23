/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_BENCH_H
#define LIB_IOAT_DMA_BENCH_H

#include <dma/dma_bench.h>

struct ioat_dma_device;

errval_t ioat_dma_bench_run_default(struct ioat_dma_device *dev);


errval_t ioat_dma_bench_run(struct ioat_dma_device *dev, lpaddr_t src, lpaddr_t dst);

#endif /* IOAT_DMA_BENCH_INTERNAL_H */
