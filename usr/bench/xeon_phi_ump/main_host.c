/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>


#include "common.h"
#include "benchmark.h"



int main(int argc,
         char **argv)
{
    errval_t err;

    debug_printf("XEON PHI BENCH STARTED (HOST).\n");

    xeon_phi_client_set_callbacks(&callbacks);

    coreid_t core = XPHI_BENCH_CORE_CARD;
    char *name = "k1om/sbin/benchmarks/xphi_ump_bench";

    debug_printf("spawning %s on [%u, %u]\n", name, 0, core);

    xphi_dom_id_t domid;
    err = xeon_phi_client_spawn(XPHI_BENCH_XPHI_ID, core, name, argv, NULL_CAP,
                                0, &domid);
    EXPECT_SUCCESS(err, "xeon_phi_client_spawn");

    // wait until client ready
    for (uint32_t i = 0; i < 1000000; ++i) {
        thread_yield();
    }

    alloc_local();

    debug_printf("opening channel to client %lx...\n", domid);
    err = xeon_phi_client_chan_open(XPHI_BENCH_XPHI_ID, domid, 0, local_frame, 2);
    EXPECT_SUCCESS(err, "xeon_phi_client_chan_open");

    wait_for_connection();

#if XPHI_BENCH_INITIATOR_HOST
#ifndef XPHI_BENCH_THROUGHPUT
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_initator_rtt(&xphi_uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_initator_rtt(&xphi_uc_rev);
#else
#ifdef XPHI_BENCH_SEND_SYNC
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_initator_sync(&xphi_uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_initator_sync(&xphi_uc_rev);
#else
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_initator_async(&xphi_uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_initator_async(&xphi_uc_rev);
#endif
#endif
#else
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_echo(&xphi_uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_echo(&xphi_uc_rev);
#endif

}

