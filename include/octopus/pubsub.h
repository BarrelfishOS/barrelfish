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

#ifndef OCTOPUS_PUBSUB_H_
#define OCTOPUS_PUBSUB_H_

#include <stdint.h>

typedef uint64_t subscription_t;
typedef void(*subscription_handler_fn)(octopus_mode_t mode, const char* record, void* state);

errval_t oct_subscribe(subscription_handler_fn, const void*, subscription_t*,
        const char*, ...);
errval_t oct_unsubscribe(subscription_t);
errval_t oct_publish(const char*, ...);

#endif /* OCTOPUS_PUBSUB_H_ */
