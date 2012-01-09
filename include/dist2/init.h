/**
 * \file
 * \brief Header file for the dist2 initialization/general functions.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INIT_H_
#define INIT_H_

#include <barrelfish/barrelfish.h>
#include <if/dist2_defs.h>
#include <if/dist2_rpcclient_defs.h>

errval_t dist_init(void);
struct dist2_rpc_client* get_dist_rpc_client(void);
struct dist2_binding* get_dist_event_binding(void);

#endif /* INIT_H_ */
