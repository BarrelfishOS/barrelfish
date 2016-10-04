/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DESCQ_H_
#define DESCQ_H_ 1


#include <barrelfish/barrelfish.h>
#include <devif/queue_interface.h>

#define DESCQ_DEFAULT_SIZE 64
#define DESCQ_ALIGNMENT 64

struct descq {
    struct devq q;
    // Shared memory of the queue
    size_t slots;

    size_t local_head;
    size_t local_tail;
    // Queue pointers
    volatile union pointer* head;
    volatile union pointer* tail;

    // The queue itself
    struct desc* descs;
};
/**
 * @brief initialized a descriptor queue
 *
 * @param q                     Return pointer to the descriptor queue
 * @param slots                 Number of slots in the queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t descq_create(struct descq** q,
                      size_t slots);

/**
 * @brief Destroys a descriptor queue and frees its resources
 *
 * @param q                     The descriptor queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t descq_destroy(struct descq* q);


#endif /* DESCQ_H_ */
