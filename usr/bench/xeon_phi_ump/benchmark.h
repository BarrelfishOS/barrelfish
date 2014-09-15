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

#include <barrelfish/ump_chan.h>

/*
 * Benchmark settings runs / repetitions
 */
#define XPHI_BENCH_NUM_REPS       500  // how many repetitions
#define XPHI_BENCH_RTT_NUM_ROUNDS 10000
#define XPHI_BENCH_NUM_RUNS       1000 // how many runs per repetition
#define XPHI_BENCH_CHECK_STOP     1

/*
 * Benchmark flags
 */
/// enables the throughput benchmark instead of RTT
//#define XPHI_BENCH_THROUGHPUT     1

/// enables the processing on the card instead of the host
#define XPHI_BENCH_INITIATOR_HOST 0

/// enables the waiting for a reply instead of keeping sending
//#define XPHI_BENCH_SEND_SYNC    1

/// sets the messaging channel buffers to card side only
#define XPHI_BENCH_CHAN_SEPARATED        1


/*
 * channel size and buffer sizes
 */

/// number of messages in the UMP channel
#define XPHI_BENCH_MSG_NUM 1024UL

/// size of a message has to be cacheline size
#define XPHI_BENCH_MSG_SIZE 64UL

/// the resulting size of our message buffer of the UMP channel (per direction)
#define XPHI_BENCH_MSG_CHAN_SIZE  (XPHI_BENCH_MSG_NUM * XPHI_BENCH_MSG_SIZE)

/// the size of the message frame to allocate
#define XPHI_BENCH_MSG_FRAME_SIZE (2 * XPHI_BENCH_MSG_CHAN_SIZE)



/*
 * core placements of the two domains
 *
 * XXX: keep it consistent with your menu.lst (host side)
 */
#define XPHI_BENCH_CORE_HOST 6
#define XPHI_BENCH_CORE_CARD 5
#define XPHI_BENCH_XPHI_ID 1

/*
 * ram affinity to make sure we are in the right numa node
 */
#define XPHI_BENCH_RAM_MINBASE  (16UL*1024*1024*1024)
#define XPHI_BENCH_RAM_MAXLIMIT (64UL*1024*1024*1024)


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



errval_t xphi_bench_init(void);

void xphi_bench_start_echo(struct ump_chan *chan);

void xphi_bench_start_processor(struct ump_chan *chan);

errval_t xphi_bench_start_initator_sync(struct ump_chan *chan);

errval_t xphi_bench_start_initator_async(struct ump_chan *chan);

errval_t xphi_bench_start_initator_rtt(struct ump_chan *chan);





#endif /* SERVICE_H_ */
