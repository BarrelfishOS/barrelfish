/**
 * \file
 * \brief Queue for stack-ripped dist2 server-side handler code
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_QUEUE_H
#define DIST2_QUEUE_H

#include <barrelfish/barrelfish.h>
#include <if/dist2_defs.h>

#include <dist2_server/service.h>

void dist_rpc_enqueue_reply(struct dist2_binding *b,
        struct dist_reply_state* st);
struct dist_reply_state* dist_rpc_dequeue_reply(struct dist2_binding *b);

#endif // DIST2_QUEUE_H
