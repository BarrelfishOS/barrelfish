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
#include <xeon_phi/xeon_phi_domain.h>

#include "benchmark.h"

#include "common.h"


int main(int argc,
         char **argv)
{
    errval_t err;

    debug_printf("Xeon Phi Test started on the card %u.\n", disp_xeon_phi_id());

    xeon_phi_client_set_callbacks(&callbacks);

    err = xeon_phi_client_init(disp_xeon_phi_id());
    EXPECT_SUCCESS(err, "xeon_phi_client_init");

    alloc_local();

    if (disp_xeon_phi_id() == 0) {
        char *iface = xeon_phi_domain_build_iface(disp_name(), 1, 2);
        debug_printf("waiting for up: %s\n", iface);
        err = xeon_phi_domain_blocking_lookup(iface, &domainid);
        EXPECT_SUCCESS(err, "xeon_phi_domain_blocking_lookup");

        debug_printf("sending open message to %s on node 1\n", iface);
        err = xeon_phi_client_chan_open(1, domainid, 0xcafebabe, local_frame, 2);
        EXPECT_SUCCESS(err, "xeon_phi_client_chan_open");
    }

    wait_for_connection();

    if (disp_xeon_phi_id() != 0) {
        err = xeon_phi_client_chan_open(0, domainid, 0xdeadbeef, local_frame, 2);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "could not open channel");
        }
    }

    if (disp_xeon_phi_id() == 1) {
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
    } else {
        debug_printf("giving time for host to initialize...\n");
        for (uint32_t i = 0; i < 10; ++i) {
            delay_ms(4000);
            thread_yield();
        }
#ifndef XPHI_BENCH_THROUGHPUT
        debug_printf("---------------- normal run -----------------\n");
        xphi_bench_start_echo(&xphi_uc);
        debug_printf("---------------- reversed run -----------------\n");
        xphi_bench_start_echo(&xphi_uc_rev);
#else
        debug_printf("---------------- normal run -----------------\n");
        xphi_bench_start_processor( &xphi_uc);
        debug_printf("---------------- reversed run -----------------\n");
        xphi_bench_start_processor( &xphi_uc_rev);
#endif
    }


    while (1) {
        messages_wait_and_handle_next();
    }
}

