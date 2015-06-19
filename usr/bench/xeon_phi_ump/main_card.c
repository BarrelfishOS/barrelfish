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
#include <barrelfish/dispatch.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>


#include "benchmark.h"
#include "common.h"


int main(int argc,
         char **argv)
{
    errval_t err;

    debug_printf("Xeon Phi Test started on the card.\n");

    err = xeon_phi_client_init(disp_xeon_phi_id());
    EXPECT_SUCCESS(err, "xeon_phi_client_init");

    xeon_phi_client_set_callbacks(&callbacks);

    alloc_local();

    wait_for_connection();

    char iface[30];
    snprintf(iface, 30, "xphi_ump_bench.%u", XPHI_BENCH_CORE_HOST);

    debug_printf("sending open to host domain..\n");
    err = xeon_phi_client_chan_open(disp_xeon_phi_id(), domainid, 0, local_frame, 2);
    EXPECT_SUCCESS(err, "xeon_phi_client_init");

#if XPHI_BENCH_INITIATOR_HOST
    debug_printf("giving time for host to initialize...\n");
    for (uint32_t i = 0; i < 10; ++i) {
        delay_ms(4000);
        thread_yield();
    }
#endif

#if XPHI_BENCH_INITIATOR_HOST
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_echo(&xphi_uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_echo(&xphi_uc_rev);
#else
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
#endif
}

