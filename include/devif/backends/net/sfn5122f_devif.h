/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef SFN5122F_DEVIF_H_
#define SFN5122F_DEVIF_H_ 1

struct sfn5122f_queue;
typedef void (*sfn5122f_event_cb_t)(void* q);

errval_t sfn5122f_queue_create(struct sfn5122f_queue** q, sfn5122f_event_cb_t cb, 
                               bool userspace, bool interrupts);
errval_t sfn5122f_queue_destroy(struct sfn5122f_queue* q);
#endif
