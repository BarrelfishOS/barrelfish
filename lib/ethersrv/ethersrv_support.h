/*
 * Copyright (c) 2007-11 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ETHERSRV_BM_SUPPORT_H_
#define ETHERSRV_BM_SUPPORT_H_
#include <barrelfish/barrelfish.h>
#include <ethersrv/ethersrv.h>
#include <contmng/netbench.h>
#include <stdio.h>
#include <string.h>
#include "ethersrv_debug.h"
#include "ethersrv_local.h"

void benchmark_control_request(struct ether_binding *cc, uint8_t state,
        uint64_t trigger, uint64_t cl_data);


void reset_client_closure_stat(struct client_closure *cc);

#endif // ETHERSRV_BM_SUPPORT_H_
