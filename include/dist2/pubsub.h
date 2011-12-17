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
#include <if/dist2_defs.h>

#define MAX_SUBSCRIPTIONS 255

typedef uint64_t subscription_t;
typedef void(*subscription_handler_fn)(subscription_t id, char *object,
        void *state);

errval_t dist_subscribe(subscription_handler_fn, void*, subscription_t*, char*, ...);
errval_t dist_unsubscribe(subscription_t);
errval_t dist_publish(char*, ...);

void subscribed_message_handler(struct dist2_binding*, subscription_t, char*);
void dist_pubsub_init(void);

#endif /* DIST2_PUBSUB_H_ */
