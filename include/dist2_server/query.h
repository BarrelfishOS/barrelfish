#ifndef DIST2_QUERY_H_
#define DIST2_QUERY_H_

#include <barrelfish/barrelfish.h>
#include <if/dist_defs.h>

#include <dist2_server/service.h>
#include <dist2/parser/ast.h>

errval_t set_events_binding(uint64_t, struct dist_event_binding*);
errval_t set_rpc_binding(uint64_t, struct dist_binding*);

errval_t get_record(struct ast_object*, struct dist_query_state*);
errval_t set_record(struct ast_object*, struct dist_query_state*);
errval_t del_record(struct ast_object*, struct dist_query_state*);

errval_t add_subscription(struct dist_binding*, struct ast_object*, uint64_t id, struct dist_query_state*);
errval_t del_subscription(struct dist_binding*, uint64_t, struct dist_query_state*);
errval_t find_subscribers(struct ast_object* ast, struct dist_query_state* sqs);

#endif /* DIST2_QUERY_H_ */

