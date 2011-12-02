#ifndef DIST2_QUERY_H_
#define DIST2_QUERY_H_

#include <barrelfish/barrelfish.h>
#include <if/dist_defs.h>

#include <dist2_server/service.h>
#include <dist2/parser/ast.h>

enum dist_trigger_type {
    TRIGGER_EXISTS,
    TRIGGER_NOT_EXISTS
};

enum dist_binding_type {
    DIST_BINDING_RPC,
    DIST_BINDING_EVENT
};

errval_t set_binding(enum dist_binding_type, uint64_t, void*);

errval_t get_record_names(struct ast_object*, struct dist_query_state*);
errval_t get_record(struct ast_object*, struct dist_query_state*);
errval_t set_record(struct ast_object*, struct dist_query_state*);
errval_t del_record(struct ast_object*, struct dist_query_state*);
errval_t set_trigger(enum dist_trigger_type type, struct ast_object*, struct dist_reply_state*);

errval_t add_subscription(struct dist_binding*, struct ast_object*, uint64_t id, struct dist_query_state*);
errval_t del_subscription(struct dist_binding*, uint64_t, struct dist_query_state*);
errval_t find_subscribers(struct ast_object* ast, struct dist_query_state* sqs);

#endif /* DIST2_QUERY_H_ */

