#ifndef DIST2_QUERY_H_
#define DIST2_QUERY_H_

#include <barrelfish/barrelfish.h>
#include <if/dist2_defs.h>

#include <dist2_server/service.h>
#include <dist2/parser/ast.h>

/**
 * Used to match the event binding and the rpc binding since in a regular
 * flounder interface RPC is on a different waitset. This is not needed
 * anymore once I fully switched over to using THC.
 */
errval_t set_binding(dist2_binding_type_t, uint64_t, void*);

/**
 * Given a query (as AST) returns a number of record names matching
 * the query. The record names are stored as a comma separated string
 * in dist_query_state.
 *
 * @param in ast Abstract Syntax Tree of the Query
 * @param in dqs Returned result of query invocation.
 *
 */
errval_t get_record_names(struct ast_object* ast, struct dist_query_state* dqs);

/**
 * Returns a record matching the given query.
 *
 * @param ast Query supplied by client converted to AST.
 * @param dqs Returned result of query invocation.
 */
errval_t get_record(struct ast_object*, struct dist_query_state*);

/**
 * Sets a record in the database.
 *
 * @param ast Record to set.
 * @param dqs Returned result of query invocation.
 */
errval_t set_record(struct ast_object* ast, uint64_t,
		struct dist_query_state* dqs);

/**
 * Deletes a record in the database.
 *
 * @param ast Record to delete.
 * @param dqs Returned result of query invocation.
 */
errval_t del_record(struct ast_object*, struct dist_query_state*);

/**
 * Sets a watch for a record(s) matching the given query. The Query Engine
 * is supposed to use the drs struct to reply to the client once the watch is
 * triggered.
 *
 * @param ast AST to watch for
 * @param mode When to trigger the watch (del or set).
 * @param drs Reply state used to reply to the client.
 */
errval_t set_watch(struct ast_object* ast, uint64_t mode,
		struct dist_reply_state* drs);

/**
 * Adds a subscription for the Publish/Subscribe API.
 *
 * @param b Binding of subscriber.
 * @param ast Subscription template (to match with published records).
 * @param id User supplied id to identify subscription.
 * @param dqs Returned result of query invocation.
 */
errval_t add_subscription(struct dist2_binding* b, struct ast_object* ast,
		uint64_t id, struct dist_query_state* dqs);

/**
 * Deletes a subscription for a given (Binding, Id) pair.
 *
 * @param b Binding identifying? the subscriber.
 * @param id Identifying? the subscription.
 * @param dqs Returned result of query invocation.
 */
errval_t del_subscription(struct dist2_binding* b, uint64_t id,
		struct dist_query_state* dqs);

/**
 * Returns a list of subscribers with a matching subscription for the given
 * AST.
 *
 * @param ast Record to match with stored subscription.
 * @param dqs Returned result of query invocation.
 */
errval_t find_subscribers(struct ast_object* ast, struct dist_query_state* dqs);


struct dist2_binding* get_event_binding(struct dist2_binding*);


#endif /* DIST2_QUERY_H_ */
