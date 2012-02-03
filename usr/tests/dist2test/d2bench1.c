/**
 * \file
 * \brief Benchmark publish / subscribe throughput.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <dist2/dist2.h>
#include <skb/skb.h>

#define EXP_RUNTIME_MS 10000

int main(int argc, char** argv)
{
	int clients = atoi(argv[1]);
	assert(clients > 0);

    dist_init();
    bench_init();

    char payload[256] = { 'a' };
    payload[255] = '\0';

    dist_set("rec { attr: '1' }");

    struct dist2_rpc_client* cl = get_dist_rpc_client();
    assert(cl != NULL);
    errval_t err = dist_set("rec { attr: '%s' }", payload);
    assert(err_is_ok(err));

    char* barrier = NULL;
    err = dist_barrier_enter("d2bench1", &barrier, clients);
    assert(err_is_ok(err));

    char* record;
    errval_t error_code;
    cycles_t server;
    uint8_t busy;

    if (strcmp(argv[2], "get") == 0) {
		while ( !stopped ) {
			cl->vtbl.get(cl, "rec", NOP_TRIGGER, &record, &error_code, &server, &busy);
		}
    }
    else if (strcmp(argv[2], "set") == 0) {
		while ( !stopped ) {
			//cl->vtbl.set(cl, "rec", NOP_TRIGGER, &record, &error_code, &server, &busy);
		}
    }
    else {
    	assert(!"Invalid argv[2]");
    }

    return EXIT_SUCCESS;
}
