/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _LOOPBACK_DEVQ_H_
#define _LOOPBACK_DEVQ_H_

struct loopback_queue;

errval_t loopback_queue_create(struct loopback_queue** q);


#endif // _LOOPBACK_DEVQ_H_
