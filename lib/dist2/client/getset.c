#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include <dist2/getset.h>
#include <dist2/parser/ast.h>

#include "strnatcmp.h"
#include "common.h"
/**
     #define STR(a) #a
     #define R(var, re)  static char var##_[] = STR(re);\
     const char * var = ( var##_[ sizeof(var##_) - 2] = '\0',  (var##_ + 1) );

     R(re, "\w\d");
     printf("Hello, world[%s]\n",  re);
**/


// Make sure args come right after query
#define FORMAT_QUERY(query, args, buf) do {                         \
    size_t length = 0;                                              \
    va_start(args, query);                                          \
    err = allocate_string(query, args, &length, &buf);              \
    va_end(args);                                                   \
    if(err_is_fail(err)) {                                          \
        return err;                                                 \
    }                                                               \
    va_start(args, query);                                          \
    size_t bytes_written = vsnprintf(buf, length+1, query, args);   \
    va_end(args);                                                   \
    assert(bytes_written == length);                                \
} while (0)



static char* mystrdup(char* data) {

    char *p = malloc(strlen(data) + 1);
    if (p == NULL) {
        return NULL;
    }

    strcpy(p, data);
    return p;
}


static int cmpstringp(const void *p1, const void *p2)
{
    return strnatcmp(* (char * const *) p1, * (char * const *) p2);
}


/**
 * Get names of all objects matching query
 */
errval_t dist_get_names(char*** names, size_t* len, char* query, ...)
{
    assert(query != NULL);

    errval_t err = SYS_ERR_OK;
    va_list  args;

    char* data = NULL;
    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    err = rpc_client->vtbl.get_names(rpc_client, buf, &data, &error_code);
    if(err_is_ok(err)) {
        err = error_code;

        if(err_is_ok(error_code)) {
            char *p = mystrdup(data);
            if (p == NULL) {
                err = LIB_ERR_MALLOC_FAIL;
                goto out;
            }

            // first get the number of elements
            char* saveptr = NULL;
            size_t i;
            char* tok = p;
            for (i = 0; tok != NULL; i++, p = NULL) {
                tok = strtok(p, ",");
            }
            free(p);
            p = NULL;
            *len = --i;

            *names = malloc(sizeof(char*) * i);
            memset(*names, 0, sizeof(char*) * i);
            if (*names == NULL) {
                *len = 0;
                err = LIB_ERR_MALLOC_FAIL;
                goto out;
            }

            // now get the actual elements
            saveptr = NULL;
            p = data;
            tok = p;
            for (i = 0; tok != NULL; i++, p = NULL) {
                tok = strtok(p, ", ");
                if (tok != NULL) {
                    (*names)[i] = mystrdup(tok);
                    if((*names)[i] == NULL) {
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
    }

    // free(*data) on error? can be NULL?

out:
    free(buf);
    free(data);

    return err;
}


void dist_free_names(char** names, size_t len)
{
    assert(names != NULL);
    for(size_t i=0; i<len; i++) {
        free(names[i]);
    }

    free(names);
}


/**
 * Gets one (the first?) found
 * returns error if ambigous query?
 */
errval_t dist_get(char* query, char** data)
{
	assert(query != NULL);

	char* error = NULL;
	errval_t error_code;
	errval_t err = SYS_ERR_OK;

	struct dist2_rpc_client* rpc_client = get_dist_rpc_client();
	err = rpc_client->vtbl.get(rpc_client, query, data, &error, &error_code);
	if(err_is_ok(err)) {
		err = error_code;
	}

	free(error); // TODO can this be NULL?
	// free(*data) on error? can be NULL?

	return err;
}


/**
 * sets one object
 */
errval_t dist_set(dist_mode_t mode, char* object, ...)
{
	assert(object != NULL);
	errval_t err = SYS_ERR_OK;
	va_list  args;

	char* buf = NULL;
    FORMAT_QUERY(object, args, buf);


	// Send to Server
    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    char* record = NULL;

	errval_t error_code;
	err = rpc_client->vtbl.set(rpc_client, buf, mode, false, &record, &error_code);
	//assert(record == NULL); TODO
	free(record);
	if(err_is_ok(err)) {
		err = error_code;
	}

	free(buf);
	return err;
}


errval_t dist_set_get(dist_mode_t mode, char** record, char* object, ...)
{
    assert(object != NULL);
    errval_t err = SYS_ERR_OK;
    va_list  args;

    char* buf = NULL;
    FORMAT_QUERY(object, args, buf);


    // Send to Server
    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    err = rpc_client->vtbl.set(rpc_client, buf, mode, true, record, &error_code);
    if(err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}


errval_t dist_del(char* object, ...)
{
	assert(object != NULL);
	errval_t err = SYS_ERR_OK;
	va_list  args;

	char* buf = NULL;
	FORMAT_QUERY(object, args, buf);

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();
	errval_t error_code;
	err = rpc_client->vtbl.del(rpc_client, buf, &error_code);

    if(err_is_ok(err)) {
        err = error_code;
    }

	free(buf);
	return err;
}


errval_t dist_exists(bool watch, char** record, char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    err = rpc_client->vtbl.exists(rpc_client, buf, NOP_TRIGGER, record, &error_code);
    if(err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}


errval_t dist_exists_not(bool watch, char* query, ...)
{
    assert(query != NULL);
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    err = rpc_client->vtbl.exists_not(rpc_client, buf, NOP_TRIGGER, &error_code);
    if(err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}




errval_t dist_wait_for(dist_mode_t mode, char** record, char* query, ...)
{
    errval_t err = SYS_ERR_OK;
    va_list args;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    struct dist2_rpc_client* rpc_client = get_dist_rpc_client();

    // IDs ignored for RPC
    uint64_t client_id = 0;
    watchid_t id = 0;

    errval_t error_code;
    err = rpc_client->vtbl.watch(rpc_client, buf, mode, dist2_BINDING_RPC, client_id, &client_id, &id, record, &error_code);
    if(err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}




