/**
 * \file
 * \brief Get/Set client API implementation
 *
 * This file provides convenience functions to interface with the
 * dist2.if RPC calls.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include <dist2/init.h>
#include <dist2/getset.h>
#include <dist2/parser/ast.h>
#include <dist2/trigger.h>

#include "strnatcmp.h"
#include "common.h"

static char* mystrdup(char* data)
{

    char *p = malloc(strlen(data) + 1);
    if (p == NULL) {
        return NULL;
    }

    strcpy(p, data);
    return p;
}

static int cmpstringp(const void *p1, const void *p2)
{
    return strnatcmp(*(char * const *) p1, *(char * const *) p2);
}

/**
 * \brief Retrieve all record names matching a given query.
 *
 * \param[out] names Names of all records matching the query.
 * Needs to be freed by the client (use dist_free_names) in
 * case of SYS_ERR_OK/LIB_ERR_MALLOC_FAIL.
 * \param[out] size Number of records matching the query. 0 in case of error.
 * \param[in] query Query sent to the server
 * \param ... Parameters used to build query with help of vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_NO_RECORD
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 * \retval LIB_ERR_MALLOC_FAIL
 */
errval_t dist_get_names(char*** names, size_t* len, const char* query, ...)
{
    assert(query != NULL);

    errval_t err = SYS_ERR_OK;
    va_list args;

    char* data = NULL;
    char* buf = NULL;
    *len = 0;

    FORMAT_QUERY(query, args, buf); // buf

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    DIST_LOCK_BINDING(rpc_client);
    err = rpc_client->vtbl.get_names(rpc_client, buf, NOP_TRIGGER, &data, // data
            &error_code);
    DIST_UNLOCK_BINDING(rpc_client);
    if (err_is_ok(err)) {
        err = error_code;
    }

    if (err_is_ok(err)) {
        char *p = mystrdup(data);
        if (p == NULL) {
            err = LIB_ERR_MALLOC_FAIL;
            goto out;
        }

        // first get the number of elements
        char* saveptr = NULL;
        size_t i;
        char* tok = p; // just make sure it's non-null
        char* first = p;
        for (i = 0; tok != NULL; i++, first = NULL) {
            tok = strtok(first, ",");
        }
        assert(p != NULL);
        free(p);
        p = NULL;
        *len = --i;

        *names = malloc(sizeof(char*) * i);
        if (*names == NULL) {
            *len = 0;
            err = LIB_ERR_MALLOC_FAIL;
            goto out;
        }
        memset(*names, 0, sizeof(char*) * i);

        // now get the actual elements
        saveptr = NULL;
        tok = data; // just make sure it's non-null
        first = data;
        for (i = 0; tok != NULL; i++, first = NULL) {
            tok = strtok(first, ", ");
            if (tok != NULL) {
                (*names)[i] = mystrdup(tok);
                if ((*names)[i] == NULL) {
                    dist_free_names(*names, i);
                    *names = NULL;
                    *len = 0;
                    err = LIB_ERR_MALLOC_FAIL;
                    goto out;
                }
            } else {
                break;
            }
        }
        qsort(*names, *len, sizeof(char*), cmpstringp);
    }

    // free(*data) on error? can be NULL?

out:
    free(buf);
    free(data);

    return err;
}

/**
 * \brief Helper function to free an array of strings.
 *
 * Frees all entries of the array and the array itself.
 *
 * \param names Non-null array of strings.
 * \param len Size of the names array

 * \see dist_get_names
 */
void dist_free_names(char** names, size_t len)
{
    //assert(names != NULL);
    for (size_t i = 0; i < len; i++) {
        free(names[i]);
    }

    free(names);
}

/**
 * \brief Gets one record matching the given query.
 *
 * \param[out] data Record returned by the server.
 * \param[in] query The query sent to the server.
 * \param ... Additional arguments to format the query using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_NO_RECORD
 * \retval DIST2_ERR_AMBIGOUS_QUERY TODO!
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 */
errval_t dist_get(char** data, const char* query, ...)
{
    assert(query != NULL);
    errval_t error_code;
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();
    DIST_LOCK_BINDING(rpc_client);
    err = rpc_client->vtbl.get(rpc_client, buf, NOP_TRIGGER, data,
            &error_code);
    DIST_UNLOCK_BINDING(rpc_client);

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
 * \param ... Additional arguments to format the query using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_NO_RECORD_NAME
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 */
errval_t dist_set(const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    // Send to Server
    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();
    char* record = NULL;

    errval_t error_code;
    DIST_LOCK_BINDING(rpc_client);
    err = rpc_client->vtbl.set(rpc_client, buf, SET_DEFAULT, NOP_TRIGGER, false,
            &record, &error_code);
    DIST_UNLOCK_BINDING(rpc_client);
    assert(record == NULL);

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
 * \retval DIST2_ERR_NO_RECORD_NAME
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 */
errval_t dist_mset(dist_mode_t mode, const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    // Send to Server
    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();
    char* record = NULL;

    errval_t error_code;
    DIST_LOCK_BINDING(rpc_client);
    err = rpc_client->vtbl.set(rpc_client, buf, mode, NOP_TRIGGER, false,
            &record, &error_code);
    DIST_UNLOCK_BINDING(rpc_client);
    assert(record == NULL);

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
 * \retval DIST2_ERR_NO_RECORD_NAME
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 *
 * TODO maybe remove this function completely and let all use rpc call for set
 * directly if they want to a non-trivial set?
 */
errval_t dist_set_get(dist_mode_t mode, char** record, const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    // Send to Server
    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    DIST_LOCK_BINDING(rpc_client);
    err = rpc_client->vtbl.set(rpc_client, buf, mode, NOP_TRIGGER, true, record,
            &error_code);
    DIST_UNLOCK_BINDING(rpc_client);
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
 * \retval DIST2_ERR_NO_RECORD
 * \retval DIST2_ERR_NO_RECORD_NAME
 * \retval DIST2_ERR_ENGINE_FAIL
 * \retval DIST2_ERR_PARSER_FAIL
 *
 * TODO: Atm only name of record is included in del query on server.
 */
errval_t dist_del(const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();
    errval_t error_code;
    DIST_LOCK_BINDING(rpc_client);
    err = rpc_client->vtbl.del(rpc_client, buf, NOP_TRIGGER, &error_code);
    DIST_UNLOCK_BINDING(rpc_client);


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
 * \retval DIST2_ERR_NO_RECORD
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 **/
errval_t dist_exists(const char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    DIST_LOCK_BINDING(rpc_client);
    err = rpc_client->vtbl.exists(rpc_client, buf, NOP_TRIGGER, &error_code);
    DIST_UNLOCK_BINDING(rpc_client);
    if (err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}
