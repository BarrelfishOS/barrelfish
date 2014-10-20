#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

#include "common.h"

#define USE_DEFWAITSET 1

#define DEBUG(x...) do {} while (0)
//#define DEBUG(x...) debug_printf(x)

#define BUFSZ 0x1000
#define NUMBUFS 32
#define NUM_RUNS 10000

static volatile uint8_t wait_flag = 0;

static void panic_handler(void *arg, errval_t err, struct bulk_channel *chan)
{
    expect_success(err);
}

static void wait_handler(void *arg, errval_t err, struct bulk_channel *chan)
{
    expect_success(err);
    wait_flag = 1;
}




static struct bulk_continuation panic_cont = {
    .handler = panic_handler,
    .arg     = NULL,
};

static struct bulk_continuation wait_cont = {
    .handler = wait_handler,
    .arg     = NULL,
};

static struct bulk_channel rxc, txc;
static struct bulk_allocator txalloc;
static volatile uint32_t rx_count = 0;
static volatile uint32_t buf_count = 0;

static volatile uint8_t wait_no_copy = 0;


static errval_t cb_pool_assigned(struct bulk_channel *channel,
                                 struct bulk_pool *pool)
{
    DEBUG("pool_assigned\n");
    wait_no_copy = 0;
    return SYS_ERR_OK;
}

static void cb_move_received(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{
    DEBUG("move_received: %"PRId32"\n", rx_count);
    assert(channel == &rxc);
    if (is_no_copy) {
        expect_success(bulk_channel_pass(&rxc, buffer, NULL, panic_cont));
    } else if (memcmp(&txalloc.pool->id, &buffer->pool->id, sizeof(txalloc.pool->id))
            == 0)
    {
        expect_success(bulk_alloc_return_buffer(&txalloc, buffer));
        buf_count++;
    } else {
        expect_success(bulk_channel_pass(&rxc, buffer, NULL, panic_cont));
    }
    rx_count++;
}

static void cb_buffer_received(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta)
{
    DEBUG("buffer_received: %"PRId32"\n", buf_count);
    assert(channel == &txc);
    expect_success(bulk_alloc_return_buffer(&txalloc, buffer));
    buf_count++;
}


static void cb_move_done(void *arg, errval_t err, struct bulk_channel *chan)
{
    expect_success(err);
}

static void init(void)
{
    static struct bulk_allocator rxalloc;
    struct bulk_buffer *buf;
    size_t i;
    debug_printf("init: enter\n");
    cycles_t tsc_start, result;
    uint64_t tscperus;
    bench_ctl_t *ctl;
    errval_t err;

    expect_success(sys_debug_get_tsc_per_ms(&tscperus));
    tscperus /= 1000;

    if (rxc.role == BULK_ROLE_MASTER) {
        DEBUG("Start allocating RX buffers...\n");
        // If we're in receive master mode, we need to allocate and pass buffers
        //set the trust level we want in our pool from the start
        struct bulk_pool_constraints pool_constraints = {
            .range_min = 0,
            .range_max = 0,
            .alignment = 0,
            .trust = rxc.trust,
        };
        expect_success(bulk_alloc_init(&rxalloc, NUMBUFS, BUFSZ, &pool_constraints));

        wait_flag = 0;
        expect_success(bulk_channel_assign_pool(&rxc, rxalloc.pool, wait_cont));
        while (!wait_flag) event_dispatch(rxc.waitset);

        for (i = 0; i < NUMBUFS; i++) {
            buf = bulk_alloc_new_buffer(&rxalloc);
            assert(buf != NULL);
            expect_success(bulk_channel_pass(&rxc, buf, NULL, panic_cont));
        }
        debug_printf("Allocated and passed RX buffers\n");
    }

    debug_printf("Prepare TX buffers\n");
    //set the trust level we want in our pool from the start
    struct bulk_pool_constraints pool_constraints = {
        .range_min = 0,
        .range_max = 0,
        .alignment = 0,
        .trust = txc.trust,
    };
    expect_success(bulk_alloc_init(&txalloc, NUMBUFS, BUFSZ, &pool_constraints));

    wait_flag = 0;
    expect_success(bulk_channel_assign_pool(&txc, txalloc.pool, wait_cont));
    while (!wait_flag) event_dispatch(txc.waitset);

    while(wait_no_copy) event_dispatch(rxc.waitset);

    // Send out data
    debug_printf("Starting benchmark...\n");
    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, NUM_RUNS);
    do {
        DEBUG("  Starting run...\n");
        rx_count = 0;
        tsc_start = rdtsc();
        buf = bulk_alloc_new_buffer(&txalloc);
        expect_success(bulk_channel_move(&txc, buf, NULL,
                    MK_BULK_CONT(cb_move_done, NULL)));

        // Wait for all responses
        while (rx_count < 1) {
            event_dispatch(rxc.waitset);
        }
        result = rdtsc() - tsc_start;
        DEBUG("  Ended run\n");
        do {
            err = event_dispatch_non_block(txc.waitset);
        } while (err_is_ok(err));

    } while (!bench_ctl_add_run(ctl, &result));

    // Output results
    bench_ctl_dump_analysis(ctl, 0, "", tscperus);
    printf("Benchmark Finished!\n");
}


static struct bulk_channel_callbacks cb = {
    .bind_received = cb_bind_received,
    .pool_assigned = cb_pool_assigned,
    .move_received = cb_move_received,
    .buffer_received = cb_buffer_received,
};

int main(int argc, char *argv[])
{
    bool tx_done = false, rx_done = false;
    struct waitset *ws;
#if !USE_DEFWAITSET
    struct waitset l_ws;
    waitset_init(&l_ws);
    ws = &l_ws;
#else
    ws = get_default_waitset();
#endif

    assert(argc == 3);
    debug_printf("Initializing TX channel...\n");
    initialize_channel(argv[2], &txc, &cb, ws, BULK_DIRECTION_TX,  BUFSZ, 0,
            &tx_done);
    debug_printf("Initializing RX channel...\n");
    initialize_channel(argv[1], &rxc, &cb, ws, BULK_DIRECTION_RX,  BUFSZ, 0,
            &rx_done);
    debug_printf("Waiting for channels to be ready\n");
    while (!rx_done || !tx_done) {
        event_dispatch(ws);
    }
    init();
    while (1) {
        event_dispatch(ws);
    }

    return 0;
}

