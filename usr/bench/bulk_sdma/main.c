/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>

#include <thc/thc.h>

#include <if/omap_sdma_defs.h>
#include <if/omap_sdma_thc.h>

#include <stdio.h>
#include <string.h>

#include "bulk_sim.h"

#define FRAME_SIZE (16UL*1024*1024)
#define FRAME_COUNT (8UL)

static struct capref src_frame[FRAME_COUNT];
static struct capref dst_frame[FRAME_COUNT];

#define BENCH_MIN_SIZE (64)
#define BENCH_MAX_SIZE FRAME_SIZE

#define BENCH_ROUNDS   10

#define MEASURE(outval, CALL) {                     \
    uint64_t start, end;                            \
    sys_debug_hardware_global_timer_read(&start);   \
    CALL;                                           \
    sys_debug_hardware_global_timer_read(&end);     \
    assert(start > measure_overhead);               \
    start -= measure_overhead;                      \
    assert(end > measure_overhead);                 \
    end -= measure_overhead;                        \
    outval = end - start;                           \
} while(0);

static uint64_t measure_overhead;

errval_t vspace_map_whole_frame(void **retbuf, struct capref cap)
{
    errval_t err;
    struct frame_identity id;

    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) return err;

    return vspace_map_one_frame(retbuf, 1UL << id.bits, cap, NULL, NULL);
}


static void measure_init(void)
{
    size_t runs = 10000;
    uint64_t start, end;
    sys_debug_hardware_global_timer_read(&start);
    for (int i=0; i<runs; i++) {
        sys_debug_hardware_global_timer_read(&end);
    }

    measure_overhead = (end - start) / runs;
}

static void bench_frames_init(void)
{
    errval_t err;
    size_t retbytes;

    for (size_t i=0; i<FRAME_COUNT; i++) {
        err = frame_alloc(&src_frame[i], FRAME_SIZE, &retbytes);
        assert(err_is_ok(err));
        assert(retbytes >= FRAME_SIZE);
    }

    for (size_t i=0; i<FRAME_COUNT; i++) {
        err = frame_alloc(&dst_frame[i], FRAME_SIZE, &retbytes);
        assert(err_is_ok(err));
        assert(retbytes >= FRAME_SIZE);
    }
}

enum TransferMode {
    TransferMode_MAPPED,
    TransferMode_PREMAPPED,
    TransferMode_SDMA,
    TransferMode_SDMA_ASYNC,
};

static const char *TRANSFER_MODE[] = {
    "Mapped", "Premapped", "SDMA", "SDMA Async"
};

static void bench_run_sdma_async(bool iterate, size_t len)
{
    DO_FINISH({
        for (uint32_t id=0; id<FRAME_COUNT; id++) {
            ASYNC({
                uint32_t slot = iterate ? id : 0;
                bulk_sdma_transfer(dst_frame[slot], src_frame[slot], len);
            });
        }
    });
}



static void bench_run(enum TransferMode mode, bool iterate)
{
    for (size_t len=BENCH_MIN_SIZE; len<=BENCH_MAX_SIZE; len*=2) {

        uint64_t cycles[BENCH_ROUNDS];

        switch(mode) {
        case TransferMode_MAPPED:

            for(int i=0; i<BENCH_ROUNDS; i++) {
                MEASURE(cycles[i], {
                    for (uint32_t id=0; id<FRAME_COUNT; id++) {
                        uint32_t slot = iterate ? id : 0;
                        bulk_mapped_transfer(dst_frame[slot], src_frame[slot], len);
                    }
                });
            }

            break;
        case TransferMode_PREMAPPED:

            for(int i=0; i<BENCH_ROUNDS; i++) {
                MEASURE(cycles[i], {
                    for (uint32_t id=0; id<FRAME_COUNT; id++) {
                        uint32_t slot = iterate ? id : 0;
                        bulk_premapped_transfer(slot, slot, len);
                    }
                });
            }

            break;
        case TransferMode_SDMA:

            for(int i=0; i<BENCH_ROUNDS; i++) {
                MEASURE(cycles[i], {
                    for (uint32_t id=0; id<FRAME_COUNT; id++) {
                        uint32_t slot = iterate ? id : 0;
                        bulk_sdma_transfer(dst_frame[slot], src_frame[slot], len);
                    }
                });
            }

            break;
        case TransferMode_SDMA_ASYNC:

                for(int i=0; i<BENCH_ROUNDS; i++) {
                    MEASURE(cycles[i], {
                        bench_run_sdma_async(iterate, len);
                    });
                }

                break;
        }



        for(int i=0; i<BENCH_ROUNDS; i++) {
            printf("%15s | %12zu | %4lu | %12"PRIu64"\n",
                TRANSFER_MODE[mode], len, iterate, cycles[i]);
        }
    }
}

static void bench_mapped(void)
{
    uint64_t cycles;
    MEASURE(cycles, {
        bulk_mapped_setup();
    });

    printf("bulk_mapped_setup: %"PRIu64"\n", cycles);

    bench_run(TransferMode_MAPPED, false);
    bench_run(TransferMode_MAPPED, true);
}

static void bench_premapped(void)
{
    uint64_t cycles;
    MEASURE(cycles, {
        bulk_premapped_setup(dst_frame, FRAME_COUNT, src_frame, FRAME_COUNT);
    });

    printf("bulk_premapped_setup: %"PRIu64"\n", cycles);


    bench_run(TransferMode_PREMAPPED, false);
    bench_run(TransferMode_PREMAPPED, true);
}

static void bench_sdma(void)
{
    uint64_t cycles;
    MEASURE(cycles, {
        bulk_sdma_setup();
    });

    printf("bulk_sdma_setup: %"PRIu64"\n", cycles);

    bench_run(TransferMode_SDMA, false);
    bench_run(TransferMode_SDMA, true);
    bench_run(TransferMode_SDMA_ASYNC, false);
    bench_run(TransferMode_SDMA_ASYNC, true);
}


int main(int argc, char *argv[])
{
    debug_printf("Initializing.\n");
    measure_init();
    bench_frames_init();

    printf("           Type |      Payload | NumT |        Ticks\n");

    bench_mapped();
    bench_premapped();
    bench_sdma();

    printf("----------------------------------------------------\n");

    return 0;
}
