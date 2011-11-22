/*
 * prolog_query.h
 *
 *  Created on: Nov 14, 2011
 *      Author: gz
 */

#ifndef SKB_QUERY_H_
#define SKB_QUERY_H_

#include <barrelfish/barrelfish.h>
#include <include/skb_server.h>

#include <if/skb_defs.h>

#include "ast.h"

errval_t set_events_binding(uint64_t, struct skb_events_binding*);
errval_t set_rpc_binding(uint64_t, struct skb_binding*);

errval_t get_record(struct ast_object*, struct skb_query_state*);
errval_t set_record(struct ast_object*, struct skb_query_state*);
errval_t del_record(struct ast_object*, struct skb_query_state*);

errval_t add_subscription(struct skb_binding*, struct ast_object*, uint64_t id, struct skb_query_state*);
errval_t del_subscription(struct skb_binding*, uint64_t, struct skb_query_state*);
errval_t find_subscribers(struct ast_object* ast, struct skb_query_state* sqs);

#endif /* SKB_QUERY_H_ */

