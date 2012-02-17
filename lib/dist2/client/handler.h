/**
 * \file
 * \brief Handler functions for incoming messages.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef DIST2_HANDLER_H_
#define DIST2_HANDLER_H_

#include <if/dist2_defs.h>

#include <dist2/pubsub.h>

void trigger_handler(struct dist2_binding*, uint64_t, uint64_t, char*);
void subscribed_message_handler(struct dist2_binding*, subscription_t, char*);

void dist_pubsub_init(void);

#endif /* DIST2_HANDLER_H_ */
