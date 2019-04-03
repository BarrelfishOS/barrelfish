/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef NULL_H_
#define NULL_H_ 1


#include <barrelfish/barrelfish.h>

/*
 * Simple queue that does nothing but forward to another queue
 */

struct null_q;

/**
 */
errval_t null_create(struct null_q** q,
                     struct devq* other_q);
#endif /* NULL_H_ */
