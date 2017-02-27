/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef E10K_DEVIF_H_
#define E10K_DEVIF_H_ 1

struct e10k_queue;
typedef void (*e10k_event_cb_t)(void* q);

errval_t e10k_queue_create(struct e10k_queue** q, e10k_event_cb_t cb, 
                           bool use_vf, bool interrupts);
errval_t e10k_queue_destroy(struct e10k_queue* q);
#endif
