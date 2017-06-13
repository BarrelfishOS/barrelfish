/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DEVIF_DEBUG_H_
#define DEVIF_DEBUG_H_ 1


#include <barrelfish/barrelfish.h>


struct debug_q;

/**
 */
errval_t debug_create(struct debug_q** q,
                      struct devq* other_q);

/**
 */
errval_t debug_destroy(struct debug_q* q, struct devq* other_q);


#endif /* DEVIF_DEBUG_H_ */
