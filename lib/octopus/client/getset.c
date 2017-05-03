/**
 * \file
 * \brief Get/Set client API implementation
 *
 * This file provides convenience functions to interface with the
 * octopus.if RPC calls.
 */

/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include <if/octopus_defs.h>
#include <if/octopus_thc.h>

#include <octopus/init.h>
#include <octopus/getset.h>
#include <octopus/parser/ast.h>
#include <octopus/trigger.h>

#include "common.h"

/**
 * \brief Retrieve all record names matching a given query.
 *
 * \param[out] names Names of all records matching the query.
 * Needs to be freed by the client (use oct_free_names) in
 * case of SYS_ERR_OK/LIB_ERR_MALLOC_FAIL.
 * \param[out] size Number of records matching the query. 0 in case of error.
 * \param[in] query Query sent to the server
 * \param ... Parameters used to build query with help of vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 * \retval LIB_ERR_MALLOC_FAIL
 */
errval_t oct_get_names(char*** names, size_t* len, const char* query, ...)
{
    assert(query != NULL);

    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    *len = 0;

    FORMAT_QUERY(query, args, buf); // buf

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();

    struct octopus_get_names_response__rx_args reply;
    err = cl->call_seq.get_names(cl, buf, NOP_TRIGGER, reply.output,
            &reply.tid, &reply.error_code);
    if (err_is_ok(err)) {
        err = reply.error_code;
    }

    if (err_is_ok(err)) {
        err = oct_parse_names(reply.output, names, len);
        //qsort(*names, *len, sizeof(char*), cmpstringp);
    }

    free(buf);
    return err;
}

/**
 * \brief Gets one record matching the given query.
 *
 * \param[out] data Record returned by the server.
 * \param[in] query The query sent to the server.
 * \param ... Additional arguments to format the query using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD
 * \retval OCT_ERR_AMBIGOUS_QUERY TODO!
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t oct_get(char** data, const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    assert(cl != NULL);

    struct octopus_get_response__rx_args reply;
    err = cl->call_seq.get(cl, buf, NOP_TRIGGER, reply.output,
            &reply.tid, &reply.error_code);

    if (err_is_ok(err)) {
        err = reply.error_code;
    }

    free(buf);

    if (err_is_fail(err)) {
        return err;
    }

    if (data) {
        *data = strdup(reply.output);
    }


    return err;
}

/**
 * \brief Sets a record.
 *
 * \param query The record to set.
 * \param ... Additional arguments to format the query using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD_NAME
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t oct_set(const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    // Send to Server
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();

    errval_t error_code;
    err = cl->call_seq.set(cl, buf, SET_DEFAULT, NOP_TRIGGER, false, NULL, NULL,
                           &error_code);

    if (err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}

/**
 * \brief Sets a record.
 *
 * \param query The record to set.
 * \param mode A combination of mode bits (see getset.h).
 * \param ... Additional arguments to format the query using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD_NAME
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t oct_mset(oct_mode_t mode, const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    // Send to Server
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();

    errval_t error_code;
    err = cl->call_seq.set(cl, buf, mode, NOP_TRIGGER, false, NULL, NULL,
                           &error_code);

    if (err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}

/**
 * \brief Sets a record and returns and in case of no error
 * returns it to the client.
 *
 * Additonally the mode how a record is set can be specified.
 *
 * \param mode A combination of mode bits (see getset.h).
 * \param[out] record The new record.
 * \param[in] query The record to set.
 * \param ... Additional arguments to format the query using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD_NAME
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 *
 * TODO maybe remove this function completely and let all use rpc call for set
 * directly if they want to a non-trivial set?
 */
errval_t oct_set_get(oct_mode_t mode, char** record, const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    // Send to Server
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    struct octopus_set_response__rx_args reply;
    err = cl->call_seq.set(cl, buf, mode, NOP_TRIGGER, true, reply.record,
                           &reply.tid, &reply.error_code);
    if (err_is_ok(err)) {
        err = reply.error_code;
    }

    free(buf);

    if (err_is_fail(err)) {
        return err;
    }

    if (record) {
        *record = strdup(reply.record);
    }

    return err;
}

/**
 * \brief Gets one record using the ID capability as the key/name.
 *
 * \param[out] data Record returned by the server.
 * \param[in] idcap ID capability used as the key/name of the record.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t oct_get_with_idcap(char **data, struct capref idcap)
{
    assert(!capref_is_null(idcap));
    errval_t err = SYS_ERR_OK;

    struct octopus_thc_client_binding_t *cl = oct_get_thc_client();
    assert(cl != NULL);
    struct octopus_get_with_idcap_response__rx_args reply;
    err = cl->call_seq.get_with_idcap(cl, idcap, NOP_TRIGGER, reply.output,
                                      &reply.tid, &reply.error_code);

    if (err_is_ok(err)) {
        err = reply.error_code;
    }

    if (err_is_fail(err)) {
        return err;
    }

    if (data) {
        *data = strdup(reply.output);
    }

    return err;
}

/**
 * \brief Sets a record using the ID capability as the name/key of the record.
 *
 * \param idcap      ID capability used as the name/key of the record.
 * \param attributes Attributes of the record.
 * \param ...        Additional arguments to format the attributes using
 *                   vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD_NAME
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 */
errval_t oct_set_with_idcap(struct capref idcap, const char *attributes, ...)
{
    assert(!capref_is_null(idcap));
    assert(attributes != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char *buf = NULL;
    FORMAT_QUERY(attributes, args, buf);

    // Send to Server
    struct octopus_thc_client_binding_t *cl = oct_get_thc_client();

    errval_t error_code;
    err = cl->call_seq.set_with_idcap(cl, idcap, buf, SET_DEFAULT, NOP_TRIGGER,
                                      false, NULL, NULL, &error_code);

    if (err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}

/**
 * \brief Deletes all records matching the given query.
 *
 * \param query Specifies the record(s) to be deleted.
 * \param ... Additional arguments to format the query using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD
 * \retval OCT_ERR_NO_RECORD_NAME
 * \retval OCT_ERR_ENGINE_FAIL
 * \retval OCT_ERR_PARSER_FAIL
 *
 * TODO: Atm only name of record is included in del query on server.
 */
errval_t oct_del(const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;
    err = cl->call_seq.del(cl, buf, NOP_TRIGGER, NULL, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}

/**
 * \brief Checks if a result for a given query exists.
 *
 * \param query Results are searched based on the query.
 * \param ... Additional arguments to format the query using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_NO_RECORD
 * \retval OCT_ERR_PARSER_FAIL
 * \retval OCT_ERR_ENGINE_FAIL
 **/
errval_t oct_exists(const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;
    err = cl->call_seq.exists(cl, buf, NOP_TRIGGER, NULL, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}

/**
 * \brief Waits until a given record exists.
 *
 * \param record  Record that matched the query, callee has to free this.
 * \param query Format of record to wait for.
 * \param ... Additional arguments to format query.
 *
 * \note This call blocks on the octopus RPC waitset if the record is not there yet.
 *
 * \retval SYS_ERR_OK
 */
errval_t oct_wait_for(char** record, const char *query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();

    struct octopus_wait_for_response__rx_args reply;
    err = cl->call_seq.wait_for(cl, buf, reply.record, &reply.error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = reply.error_code;

    if (err_is_fail(err)) {
        goto out;
    }

    if (record) {
        *record = strdup(reply.record);
    }

out:
    free(buf);
    return err;
}
