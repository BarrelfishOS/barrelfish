/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/types.h>
#include <barrelfish/caddr.h>
#include "queue.h"

#ifndef CAPOPS_TRANSPORT_H
#define CAPOPS_TRANSPORT_H

errval_t enqueue_send_target(coreid_t dest, struct msg_queue_elem *queue_elem);

errval_t enqueue_send_owner(struct capref capref, struct msg_queue_elem *queue_elem);

typedef void (*find_core_result_handler_t)(errval_t, coreid_t, void*);
errval_t find_core_with_cap(struct capability *cap, find_core_result_handler_t result_handler, void *st);

errval_t update_owner(struct capref capref, struct event_closure continuation);

#endif
