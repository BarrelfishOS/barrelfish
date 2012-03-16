/**
 * \file
 * \brief Publish/Subscribe header file
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_PUBSUB_H_
#define DIST2_PUBSUB_H_

#include <stdint.h>
#include <if/octopus_defs.h>

#include <octopus/trigger.h>

typedef octopus_trigger_id_t subscription_t;
typedef trigger_handler_fn subscription_handler_fn;

errval_t oct_subscribe(subscription_handler_fn, const void*, subscription_t*,
        const char*, ...);
errval_t oct_unsubscribe(subscription_t);
errval_t oct_publish(const char*, ...);

#endif /* DIST2_PUBSUB_H_ */
