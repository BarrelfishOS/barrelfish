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

/*
 * Benchmark settings runs / repetitions
 */
#define XPHI_BENCH_NUM_REPS       50  // how many repetitions
#define XPHI_BENCH_NUM_RUNS       1000 // how many runs per repetition
#define XPHI_BENCH_PROCESS_RUNS   10  // how many times the buffer is read/written
#define XPHI_BENCH_CHECK_STOP     1

/*
 * Benchmark flags
 */
/// enables the throughput benchmark instead of RTT
//#define XPHI_BENCH_THROUGHPUT     1

/// enables the processing on the card instead of the host
#define XPHI_BENCH_PROCESS_CARD 1

/// enables the waiting for a reply instead of keeping sending
//#define XPHI_BENCH_SEND_SYNC    1

/// sets the buffer location to the card side
#define XPHI_BENCH_BUFFER_CARD 1

/// sets the messaging channel buffers to card side only
//#define XPHI_BENCH_CHAN_CARD        1

/// sets the messaging channel buffers to host side only
//#define XPHI_BENCH_CHAN_HOST        1

/// reverses the in/out buffers if they are located on card and host
//#define XPHI_BENCH_CHAN_REVERSED    1


/*
 * channel size and buffer sizes
 */
/// number of messages in the UMP channel
#define XPHI_BENCH_MSG_NUM 1024UL
/// size of a message has to be cacheline size
#define XPHI_BENCH_MSG_SIZE 64UL
/// the resulting size of our message buffer of the UMP channel (per direction)
#define XPHI_BENCH_MSG_FRAME_SIZE  (XPHI_BENCH_MSG_NUM * XPHI_BENCH_MSG_SIZE)

/// how many buffers we have (we keep it consistent with the number of messages
#define XPHI_BENCH_BUF_NUM  XPHI_BENCH_MSG_NUM
/// the size of a single buffer
#define XPHI_BENCH_BUF_SIZE (1UL << 18)
/// the resulting size of the buffer frame we have to allocate
#define XPHI_BENCH_BUF_FRAME_SIZE (XPHI_BENCH_BUF_NUM * XPHI_BENCH_BUF_SIZE)

#define XPHI_BENCH_SIZE_MIN_BITS 6
#define XPHI_BENCH_SIZE_MAX_BITS 27

/*
 * core placements of the two domains
 *
 * XXX: keep it consistent with your menu.lst (host side)
 */
#define XPHI_BENCH_CORE_HOST 6
#define XPHI_BENCH_CORE_CARD 5


/*
 * calculation of the frame sizes we need to allocate. the possible frame layouts
 * are:
 *
 * - Host:  [UMP | Buffers] Guest: [UMP]
 * - Host:  [UMP] Guest: [UMP | Buffers]
 * - Host:  [UMP | UMP | Buffers] Guest: []
 * - Host:  [UMP | UMP] Guest: [Buffers]
 * - Host:  [Buffers] Guest: [UMP | UMP]
 * - Host:  [] Guest: [UMP | UMP | Buffers]
 *
 * The buffers can either be on host side or card side.
 * The message buffers can be on host side, card side or mixed
 */

#ifdef XPHI_BENCH_CHAN_HOST
#define XPHI_BENCH_CHAN_SIZE_HOST (2* XPHI_BENCH_MSG_FRAME_SIZE)
#define XPHI_BENCH_CHAN_SIZE_CARD (2* XPHI_BENCH_MSG_FRAME_SIZE)
#endif

#ifdef XPHI_BENCH_CHAN_CARD
#define XPHI_BENCH_CHAN_SIZE_HOST (2* XPHI_BENCH_MSG_FRAME_SIZE)
#define XPHI_BENCH_CHAN_SIZE_CARD (2* XPHI_BENCH_MSG_FRAME_SIZE)
#endif

#ifndef XPHI_BENCH_CHAN_SIZE_HOST
#define XPHI_BENCH_CHAN_DEFAULT
#define XPHI_BENCH_CHAN_SIZE_HOST (2* XPHI_BENCH_MSG_FRAME_SIZE)
#define XPHI_BENCH_CHAN_SIZE_CARD (2* XPHI_BENCH_MSG_FRAME_SIZE)
#endif


/*
 * ram affinity to make sure we are in the right numa node
 */
#define XPHI_BENCH_RAM_MINBASE  (16UL*1024*1024*1024)
#define XPHI_BENCH_RAM_MAXLIMIT (64UL*1024*1024*1024)

/*
 * add up the
 */
#define XPHI_BENCH_FRAME_SIZE_HOST (XPHI_BENCH_CHAN_SIZE_HOST + XPHI_BENCH_BUF_FRAME_SIZE)
#define XPHI_BENCH_FRAME_SIZE_CARD (XPHI_BENCH_CHAN_SIZE_CARD + XPHI_BENCH_BUF_FRAME_SIZE)

/*
 * Debugging messages
 */
#define XPHI_BENCH_DBU_EN 0
#if XPHI_BENCH_DBU_EN
#define XPHI_BENCH_DBG(x...) debug_printf("[bench] " x)
#else
#define XPHI_BENCH_DBG(x...)
#endif

#define XPHI_BENCH_STOP_FLAG 0xFF00FF00FF00FF00UL

struct bench_buf {
    uint8_t data[XPHI_BENCH_BUF_SIZE];
};

struct bench_bufs {
    struct bench_buf *buf;
    uint32_t num;
    uint32_t buf_size;
};


errval_t xphi_bench_init(void);

void xphi_bench_start_echo(struct bench_bufs *bufs,
                           struct ump_chan *uc);

void xphi_bench_start_processor(struct bench_bufs *bufs,
                                struct ump_chan *uc);

errval_t xphi_bench_start_initator_sync(struct bench_bufs *bufs,
                                        struct ump_chan *uc);

errval_t xphi_bench_start_initator_async(struct bench_bufs *bufs,
                                         struct ump_chan *uc);

errval_t xphi_bench_start_initator_rtt(struct bench_bufs *bufs,
                                        struct ump_chan *uc);

errval_t xphi_bench_memwrite(void *target);

errval_t xphi_bench_memcpy(struct dma_device *dev,
                           void *dst,
                           void *src,
                           size_t size,
                           lpaddr_t pdst,
                           lpaddr_t psrc);

static inline void xphi_bench_fill_buffer(struct bench_buf *buf, uint32_t runs)
{
    XPHI_BENCH_DBG("reading/writing %u x %lu bytes\n", runs, sizeof(buf->data));
    for (uint32_t j=0; j<runs; ++j) {
        for (uint32_t i = 0; i < sizeof(buf->data); ++i) {
            buf->data[i]=(uint8_t)(i+j);
        }
    }
}

static inline void xphi_bench_fill_buffer_random(struct bench_buf *buf, uint32_t runs)
{
    XPHI_BENCH_DBG("reading/writing %u x %lu bytes\n", runs, sizeof(buf->data));
    for (uint32_t j=0; j<runs; ++j) {
        for (uint32_t i = 0; i < sizeof(buf->data); ++i) {
            uint32_t idx = rand() * sizeof(buf->data);
            buf->data[idx]=(uint8_t)(buf->data[idx]+i+j);
        }
    }
}

static inline void xphi_bench_read_buffer(struct bench_buf *buf,
                                          uint32_t runs,
                                          uint32_t *count)
{
    XPHI_BENCH_DBG("reading %u x %lu bytes\n", runs, sizeof(buf->data));
    volatile uint32_t counter = 0;
    for (uint32_t j=0; j<runs; ++j) {
        for (uint32_t i = 0; i < sizeof(buf->data); ++i) {
            counter += buf->data[i];
        }
    }
    *count = counter;
}

#endif /* SERVICE_H_ */
