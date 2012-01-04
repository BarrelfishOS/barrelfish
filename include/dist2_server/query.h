/**
 * \file
 * \brief dist2 Query Interface Header file
 *
 * The server must implement this interface in order for dist2
 * to work accordingly.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_QUERY_H_
#define DIST2_QUERY_H_

#include <barrelfish/barrelfish.h>
#include <if/dist2_defs.h>

#include <dist2_server/service.h>
#include <dist2/parser/ast.h>

/**
 * \brief Stores a binding for the given id.
 *
 * Used to find the event binding for a given RPC connection
 * since in a regular flounder interface RPC calls are on on a
 * different waitset we currently need two binding.
 *
 * \param type Binding type (RPC or Event)
 * \param id Identifier unique per client
 * \param binding Pointer value of binding
 *
 * \retval SYS_ERR_OK
 */
errval_t set_binding(dist2_binding_type_t type, uint64_t id, void* binding);

/**
 * Given a query returns a number of record names matching
 * the query. The record names are stored as a comma separated string
 * in dist_query_state.
 *
 * \param ast Abstract Syntax Tree of query.
 * \param dqs Contains the result of the query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_NO_RECORD
 */
errval_t get_record_names(struct ast_object* ast, struct dist_query_state* dqs);

/**
 * \brief Returns a record matching the given query.
 *
 * \param ast Query supplied by client converted to AST.
 * \param dqs Contains the result of the query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_NO_RECORD
 */
errval_t get_record(struct ast_object* ast, struct dist_query_state* dqs);

/**
 * \brief Stores a record in the database.
 *
 * \param ast Record to set.
 * \param mode A combination of modes as defined in getset.h.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 */
errval_t set_record(struct ast_object* ast, uint64_t mode,
        struct dist_query_state* dqs);

/**
 * \brief Deletes a record in the database.
 *
 * \param ast Record to delete.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_NO_RECORD
 */
errval_t del_record(struct ast_object*, struct dist_query_state*);

/**
 * Sets a watch for a record(s) matching the given query. The Query Engine
 * is supposed to use the drs struct to reply to the client once the watch is
 * triggered.
 *
 * \param ast AST to watch for
 * \param mode When to trigger the watch (del or set).
 * \param drs Reply state used to reply to the client.
 *
 * \retval SYS_ERR_OK
 */
errval_t set_watch(struct ast_object* ast, uint64_t mode,
        struct dist_reply_state* drs);

/**
 * \brief Adds a subscription.
 *
 * \param b RPC binding of subscriber.
 * \param ast Subscription template (to match with published records).
 * \param id User supplied id to identify subscription.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 */
errval_t add_subscription(struct dist2_binding* b, struct ast_object* ast,
        uint64_t id, struct dist_query_state* dqs);

/**
 * \brief Deletes a subscription for a given (Binding, Id) pair.
 *
 * \param b RPC binding of subscriber.
 * \param id ID of the subscription.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 */
errval_t del_subscription(struct dist2_binding* b, uint64_t id,
        struct dist_query_state* dqs);

/**
 * Find all subscribers with a matching subscription for the given
 * AST.
 *
 * \param ast Record to match with stored subscription.
 * \param dqs Returned result of query invocation.
 *
 * \retval SYS_ERR_OK
 */
errval_t find_subscribers(struct ast_object* ast, struct dist_query_state* dqs);

/**
 * \brief Find the event binding of the client based on his RPC binding.
 *
 * \param binding RPC binding
 * \return Pointer of event binding or NULL if no corresponding binding found.
 */
struct dist2_binding* get_event_binding(struct dist2_binding* binding);

#endif /* DIST2_QUERY_H_ */
