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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_TRIGGER_H_
#define OCTOPUS_TRIGGER_H_

#include <barrelfish/barrelfish.h>

#include <octopus/definitions.h>
#include <octopus/getset.h>

#define NOP_TRIGGER (octopus_trigger_t){ /*in_case*/ 0, /*send_to*/ 0, \
                                         /*m*/ 0, /*trigger*/ 0, /*st*/ 0 }

#define TRIGGER_ALWAYS (OCT_PERSIST | OCT_ON_SET | OCT_ON_DEL | OCT_ALWAYS_SET)

typedef void(*trigger_handler_fn)(oct_mode_t mode, const char* record, void* state);
octopus_trigger_t oct_mktrigger(errval_t, octopus_binding_type_t, oct_mode_t,
        trigger_handler_fn, void*);
errval_t oct_remove_trigger(octopus_trigger_id_t);
errval_t oct_trigger_existing_and_watch(const char*,
        trigger_handler_fn, void*,
        octopus_trigger_id_t*);


#endif /* OCTOPUS_TRIGGER_H_ */
