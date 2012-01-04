/**
 * \file
 * \brief Header file for triggers.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_TRIGGER_H_
#define DIST2_TRIGGER_H_

#include <barrelfish/barrelfish.h>

#include <if/dist2_defs.h>

#define MAX_TRIGGERS 256
#define NOP_TRIGGER     (dist2_trigger_t){ .in_case = 0, .m = 0, .trigger = 0, .st = 0 }

typedef void(*trigger_handler_fn)(char* object, void* state);

void trigger_handler(struct dist2_binding*, uint64_t, uint64_t, char*);

#endif /* DIST2_TRIGGER_H_ */
