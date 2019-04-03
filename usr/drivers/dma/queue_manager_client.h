/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef QUEUE_MANAGER_CLIENT_H_
#define QUEUE_MANAGER_CLIENT_H_ 1

typedef enum {
    QUEUE_TYPE_UNKNOWN  = 0,
    QUEUE_TYPE_E1K      = 1,
    QUEUE_TYPE_E10K     = 2,
    QUEUE_TYPE_SFxxxx   = 3,
    QUEUE_TYPE_IOAT_DMA = 4,
    QUEUE_TYPE_XPHI_DMA = 5,
    QUEUE_TYPE_AHCI     = 6,
    QUEUE_TYPE_MAX      = 7
} queue_t;



struct queue
{
    struct {
        struct capref registers;
        struct capref interrupt;
        struct capref vspace;
    } caps;
    queue_t type;
    uint64_t id;
};


errval_t qmng_init(void);

errval_t qmng_alloc_queue(queue_t type, nodeid_t proximity, struct queue *q);
errval_t qmng_alloc_queue_by_id(queue_t type, uint64_t id, struct queue *q);
errval_t qmng_release_queue(struct queue *q);

#endif
