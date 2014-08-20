/**
 * \file
 * \brief Network server thread of the bulk server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */



#ifndef BS_USER_BENCHMARK_H
#define BS_USER_BENCHMARK_H

#include <bulk_transfer/bulk_transfer.h>

#include "bs_connector.h"

#define BS_BENCH_NUM_RUNS 10

#define BS_BENCH_NUM_BUFS 0x10

#define BS_BENCH_BUF_SIZE 0x1000

#define BS_BENCH_MAX_BLOCKS 10

#define BS_BENCH_DEBUG(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
//#define BS_BENCH_DEBUG(fmt, msg...)

struct bulk_channel_callbacks *bench_get_rx_cb(void);

struct bulk_channel_callbacks *bench_get_tx_cb(void);

void bench_signal(errval_t err,
                  uint32_t seqn,
                  uint32_t req);

errval_t bench_init(struct bs_connection *conn);

errval_t bench_run(void);

#endif /* BS_USER_BENCHMARK_H */
