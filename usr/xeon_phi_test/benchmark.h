/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_MSG_BENCHMARK
#define XEON_PHI_MSG_BENCHMARK

#define XPHI_BENCH_MSG_NUM 1024
#define XPHI_BENCH_MSG_SIZE 64
#define XPHI_BENCH_MSG_BUF_SIZE  (XPHI_BENCH_MSG_NUM*XPHI_BENCH_MSG_SIZE)

#define XPHI_BENCH_BUF_NUM  XPHI_BENCH_MSG_NUM
#define XPHI_BENCH_BUF_SIZE 4096
#define XPHI_BENCH_BUF_FRAME_SIZE (XPHI_BENCH_BUF_NUM * XPHI_BENCH_BUF_SIZE)

#define XPHI_BENCH_ASYNC 1
#define XPHI_BENCH_PROCESS_HOST 1
#define XPHI_BENCH_PROCESS_CARD 2
#define XPHI_BENCH_PROCESS XPHI_BENCH_PROCESS_HOST
#define XPHI_BENCH_PROCESS_RUNS 10

#define XPHI_BENCH_NUM_REPS 1 // how many repetitions
#define XPHI_BENCH_NUM_RUNS 1 // how many runs per repetition

#define XPHI_BENCH_BUF_LOC_HOST 1
#define XPHI_BENCH_BUF_LOC_CARD 2
#define XPHI_BENCH_BUF_LOCATION XPHI_BENCH_BUF_LOC_HOST


#define XPHI_BENCH_CHAN_MIX_RECV_SIDE 1
#define XPHI_BENCH_CHAN_MIX_SEND_SIDE 2
#define XPHI_BENCH_CHAN_HOST 3
#define XPHI_BENCH_CHAN_CARD 4
#define XPHI_BENCH_CHAN_LOCATION XPHI_BENCH_CHAN_HOST

#define XPHI_BENCH_CORE_HOST 3

#define XPHI_BENCH_CORE_CARD 5

#define XPHI_BENCH_DBU_EN 1
#if XPHI_BENCH_DBU_EN
#define XPHI_BENCH_DBG(x...) debug_printf("[bench]" x)
#else
#define XPHI_BENCH_DBG(x...)
#endif

#if XPHI_BENCH_CHAN_LOCATION == XPHI_BENCH_CHAN_HOST
#define XPHI_BENCH_CHAN_SIZE_HOST (2* XPHI_BENCH_MSG_BUF_SIZE)
#define XPHI_BENCH_CHAN_SIZE_CARD 0
#else
#if XPHI_BENCH_CHAN_LOCATION == XPHI_BENCH_CHAN_CARD
#define XPHI_BENCH_CHAN_SIZE_HOST 0
#define XPHI_BENCH_CHAN_SIZE_CARD (2*XPHI_BENCH_MSG_BUF_SIZE)
#else
#define XPHI_BENCH_CHAN_SIZE_HOST (XPHI_BENCH_MSG_BUF_SIZE)
#define XPHI_BENCH_CHAN_SIZE_CARD (XPHI_BENCH_MSG_BUF_SIZE)
#endif
#endif

#if XPHI_BENCH_BUF_LOCATION == XPHI_BENCH_BUF_LOC_HOST
#define XPHI_BENCH_FRAME_SIZE_HOST (XPHI_BENCH_CHAN_SIZE_HOST + XPHI_BENCH_BUF_FRAME_SIZE)
#define XPHI_BENCH_FRAME_SIZE_CARD (XPHI_BENCH_CHAN_SIZE_CARD)
#else
#define XPHI_BENCH_FRAME_SIZE_HOST (XPHI_BENCH_CHAN_SIZE_HOST)
#define XPHI_BENCH_FRAME_SIZE_CARD (XPHI_BENCH_CHAN_SIZE_CARD + XPHI_BENCH_BUF_FRAME_SIZE)
#endif


struct bench_buf {
    uint8_t data[XPHI_BENCH_BUF_SIZE];
};

struct bench_bufs {
    struct bench_buf *buf;
    uint32_t num;
    uint32_t buf_size;
};


errval_t xphi_bench_init(void);

void xphi_bench_start_processor(void);

errval_t xphi_bench_start_initator_sync(void);

errval_t xphi_bench_start_initator_async(void);

static inline void xphi_bench_fill_buffer(struct bench_buf *buf, uint32_t runs)
{
    for (uint32_t j=0; j<runs; ++j) {
        for (uint32_t i = 0; i < sizeof(buf->data); ++i) {
            buf->data[i]=(uint8_t)(i+j);
        }
    }
}

static inline void xphi_bench_fill_buffer_random(struct bench_buf *buf, uint32_t runs)
{
    for (uint32_t j=0; j<runs; ++j) {
        for (uint32_t i = 0; i < sizeof(buf->data); ++i) {
            uint32_t idx = rand() * sizeof(buf->data);
            buf->data[idx]=(uint8_t)(i+j);
        }
    }
}

#endif /* SERVICE_H_ */
