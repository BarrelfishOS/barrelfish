/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAPLOCK_H
#define CAPLOCK_H

#include <barrelfish/caddr.h>
#include <barrelfish/waitset.h>
#include "capops.h"

struct event_queue_node;

void caplock_wait(struct domcapref cap,
                  struct event_queue_node *qn, struct event_closure cont);

void caplock_unlock(struct domcapref cap);

void caplock_init(struct waitset *ws);

#endif
