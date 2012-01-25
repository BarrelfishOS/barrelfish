/**
 * \file
 * \brief dist2 service handler header file.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_SERVICE_H_
#define DIST2_SERVICE_H_

#include <barrelfish/barrelfish.h>
#include <if/dist2_defs.h>
#include <if/dist_event_defs.h>
#include <dist2/definitions.h>

#define BUFFER_SIZE (32 * 1024)

struct dist_reply_state;

struct skb_writer {
    char buffer[MAX_QUERY_LENGTH]; // TODO can be bigger than max query length...
    size_t length;
};

struct dist_query_state {
    struct skb_writer stdout;
    struct skb_writer stderr;
    int exec_res;
};

typedef void(*dist_reply_handler_fn)(struct dist2_binding*, struct dist_reply_state*);

struct dist_reply_state {
    struct dist2_binding* binding;
    dist_reply_handler_fn reply;

    struct dist_query_state query_state;
    bool return_record;
    errval_t error;

    dist2_trigger_t trigger;
    // For watch()
    uint64_t watch_id;
    uint64_t server_id;

    cycles_t time;
    uint8_t busy;

    struct dist_reply_state *next;
};

errval_t new_dist_reply_state(struct dist_reply_state**, dist_reply_handler_fn);

void get_names_handler(struct dist2_binding*, char*, dist2_trigger_t);
void get_handler(struct dist2_binding*, char*, dist2_trigger_t);
void set_handler(struct dist2_binding*, char*, uint64_t, dist2_trigger_t, bool);
void del_handler(struct dist2_binding*, char*, dist2_trigger_t);
void exists_handler(struct dist2_binding*, char*, dist2_trigger_t);

void subscribe_handler(struct dist2_binding*, char*, uint64_t);
void publish_handler(struct dist2_binding*, char*);
void unsubscribe_handler(struct dist2_binding*, uint64_t);

void get_identifier(struct dist2_binding*);
void identify_binding(struct dist2_binding*, uint64_t, dist2_binding_type_t);

#endif /* DIST2_SERVICE_H_ */
