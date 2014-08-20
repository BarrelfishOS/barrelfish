/**
 * \file
 * \brief block_server client process.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BLOCK_SERVER_H
#define BLOCK_SERVER_H

#include <bulk_transfer/bulk_transfer.h>




/*
 * NETWORK BACKEND OPTIONS
 */
//#define NET_PROXY
//#define NET_TRANSP
#define NET_NOCOPY



/// The default number and size of blocks
#define DEFAULT_BLOCK_COUNT 1024
#define DEFAULT_BLOCK_SIZE 4096

/// The number of receive buffers in the network backend
#define BLOCK_NET_BUFFER_COUNT 511

/// enables the network connector, disable for shared memory only benchmark
#define BLOCK_ENABLE_NETWORKING 1

/*
 * BENCHMARK CONTROL
 */

/// enables the benchmarking facility
#define BLOCK_BENCH_ENABLE 1

/// number of buffers that will be allocated in the pools
#define BLOCK_BENCH_NUMBUFS BLOCK_NET_BUFFER_COUNT

/// the size of the buffers to allocate (keep in sync with block size)
#define BLOCK_BENCH_BUFSIZE DEFAULT_BLOCK_SIZE

/// the number of runs executed in the benchmark
#define BLOCK_BENCH_NUMRUNS 1000

/// the number of requests executed in a signle rn
#define BLOCK_BENCH_NUMREQUESTS 250

/// the number of blocks read in a single read request
#define BLOCK_BENCH_READ_BATCHSIZE 250
#define BLOCK_BENCH_NUMBATCH_REQUESTS (BLOCK_BENCH_NUMRUNS / BLOCK_BENCH_READ_BATCHSIZE)



/*
 * NETWORK BACKEND SETTINGS
 */

/// service layer port
#define BLOCK_NET_PORT 9000

/// bulk transfer ports
#define BLOCK_NET_PORT_RX 10000
#define BLOCK_NET_PORT_TX 12000

/// queue settings
#define BLOCK_NET_MAX_QUEUES 10
#define BLOCK_NET_RX_QUEUE 2
#define BLOCK_NET_TX_QUEUE (BLOCK_NET_RX_QUEUE + BLOCK_NET_MAX_QUEUES)

#define BLOCK_SERVER_NAME "blockservice"

/* setting the actual values for the block store */
#define BLOCK_COUNT DEFAULT_BLOCK_COUNT
#define BLOCK_SIZE DEFAULT_BLOCK_SIZE

/* setting the backend flags */
#ifdef NET_PROXY
#define BULK_NET_BACKEND_PROXY 1
#define BULK_NET_BACKEND_TRANSPARENT 0
#define BULK_NET_BACKEND_NOCOPY 0
#define BULK_NET_PROXY_NUMBUFS BLOCK_NET_BUFFER_COUNT
#define BULK_NET_PROXY_BUFSIZE BLOCK_SIZE
#else
#ifdef NET_NOCOPY
#define BULK_NET_BACKEND_PROXY 0
#define BULK_NET_BACKEND_TRANSPARENT 1
#define BULK_NET_BACKEND_NOCOPY 1
#else
#define BULK_NET_BACKEND_PROXY 0
#define BULK_NET_BACKEND_TRANSPARENT 1
#define BULK_NET_BACKEND_NOCOPY 0
#endif
#endif


#include "network_common.h"

enum block_err_code
{
    BLOCK_ERR_OK,
    BLOCK_ERR_NOT_CONNECTED,
    BLOCK_ERR_BAD_REQUEST,
    BLOCK_ERR_BAD_BLOCK_ID,
    BLOCK_ERR_NO_BUFS,
};

struct bs_meta_data
{
    struct bulk_continuation cont;
    size_t block_id;
    uint32_t req_id;
};

struct bs_callback_arg
{
    void *binding;
    struct bulk_buffer *buf;
    size_t block_id;
    uint32_t req_id;
};

struct buffer_list
{
    struct bulk_buffer *buf;
    struct bulk_channel *channel;
    struct buffer_list *next;
};

/* debug */
//#define BS_NET_DEBUG_BULK(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
#define BS_NET_DEBUG_BULK(fmt, msg...) do{}while(0);

#define BS_NET_DEBUG_NET(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
//#define BS_NET_DEBUG_NET(fmt, msg...)

/* print the call traces */
//#define BS_NET_DEBUG_TRACE debug_printf("%s\n", __func__);
#define BS_NET_DEBUG_TRACE do{}while(0);

#define BS_BS_DEBUG(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
//#define BS_BS_DEBUG(fmt, msg...) do{}while(0);

#define BS_LOCAL_DEBUG(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
//#define BS_LOCAL_DEBUG(fmt, msg...) do{}while(0);

/* print the call traces */
//#define BS_LOCAL_DEBUG_TRACE debug_printf("%s\n", __func__);
#define BS_LOCAL_DEBUG_TRACE

extern struct buffer_list *bs_bulk_buffers;

struct bulk_buffer *block_server_get_buffer(struct buffer_list **l,
                                            struct bulk_channel *chan);

void block_server_insert_buffer(struct buffer_list **l,
                                struct bulk_buffer *buf,
                                struct bulk_channel *channel);

errval_t testrun_handle_status(enum block_net_msg_type req,
                               uint32_t reqid,
                               enum block_net_err stats);

void testrun_bulk_move_received(struct bulk_channel *channel,
                                struct bulk_buffer *buffer,
                                void *meta);

void testrun_bulk_buffer_received(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta);

void run_test(struct bulk_channel *txc,
              struct bulk_channel *rxc,
              void *block_service);
#endif /* BLOCK_SERVER_H */
