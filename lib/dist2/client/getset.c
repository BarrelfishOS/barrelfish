#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include <dist2/getset.h>
#include <dist2/parser/ast.h>

#include "strnatcmp.h"
#include "common.h"
extern int id;
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


/**
 * nice to have would be:
 * get("object { weight: %d }", &weight);
 *
 */

/*
static errval_t dist_del(char* name)
{

}
*/

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

    struct dist_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    err = rpc_client->vtbl.get_names(rpc_client, buf, id, &data, &error_code);
    if(err_is_ok(err)) {
        err = error_code;

        if(err_is_ok(error_code)) {
            char *p = mystrdup(data);
            if (p == NULL) {
                err = LIB_ERR_MALLOC_FAIL;
                goto out;
            }

            // first get the number of elements
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

	struct dist_rpc_client* rpc_client = get_dist_rpc_client();
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
    struct dist_rpc_client* rpc_client = get_dist_rpc_client();

    char* record = NULL;

	errval_t error_code;
	err = rpc_client->vtbl.set(rpc_client, buf, mode, false, id, &record, &error_code);
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
    struct dist_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    err = rpc_client->vtbl.set(rpc_client, buf, mode, true, id, record, &error_code);
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

    struct dist_rpc_client* rpc_client = get_dist_rpc_client();
	errval_t error_code;
	err = rpc_client->vtbl.del(rpc_client, buf, id, &error_code);

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

    struct dist_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    err = rpc_client->vtbl.exists(rpc_client, buf, watch, true, record, &error_code);
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

    struct dist_rpc_client* rpc_client = get_dist_rpc_client();

    errval_t error_code;
    err = rpc_client->vtbl.exists_not(rpc_client, buf, watch, id, &error_code);
    if(err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}


/**
 *
 * dist_read(record, "%s { attribute1: %s, attr2: %lu, attr3: %l, attr4: %f, attr5: '%s' }
 *
 * DIST2_ERR_ATTRIBUTE_NOT_FOUND
 * DIST2_ERR_TYPE_MISMATCH
 * DIST2_ERR_RECORD_NAME_MISMATCH
 * DIST2_ERR_ATTRIBUTE_MISMATCH
 */
errval_t dist_read(char* record, char* format, ...)
{
    errval_t err = SYS_ERR_OK;
    va_list args;

    char** s = NULL;
    int64_t* i = NULL;
    double* d = NULL;

    struct ast_object* ast = NULL;
    struct ast_object* format_ast = NULL;

    err = generate_ast(record, &ast);
    if(err_is_fail(err)) {
        goto out;
    }
    err = generate_ast(format, &format_ast);
    if(err_is_fail(err)) {
        goto out;
    }

    va_start(args, format);
    // Compare Name
    struct ast_object* format_name = format_ast->on.name;
    switch(format_name->type) {
        case nodeType_Ident:
            assert(ast->on.name->type == nodeType_Ident); // TODO err
            if(strcmp(format_name->in.str, ast->on.name->in.str) != 0) {
                err = DIST2_ERR_NAME_MISMATCH;
                goto out;
            }
        break;

        case nodeType_Scan:
            if(format_name->scn.c != 's') {
                err = DIST2_ERR_INVALID_FORMAT;
                goto out;
            }
            s = va_arg(args, char**);
            *s = ast->on.name->in.str;
            // Remove from AST so client has to free it
            ast->on.name->in.str = NULL;
        break;

        default:
            assert(!"Should not happen, check your parser!");
            err = DIST2_ERR_INVALID_FORMAT;
            goto out;
        break;
    }

    struct ast_object* attr = format_ast->on.attrs;
    for(; attr != NULL; attr = attr->an.next) {
        struct ast_object* format_attr = attr->an.attr;
        // Enforced by Parser
        assert(format_attr->type == nodeType_Pair);
        assert(format_attr->pn.left->type == nodeType_Ident);
        struct ast_object* record_attr = ast_find_attribute(ast, format_attr->pn.left->in.str);
        if(record_attr == NULL) {
            err = DIST2_ERR_UNKNOWN_ATTRIBUTE;
            goto out;
        }
        struct ast_object* value = record_attr->pn.right;


        bool type_matches = value->type == format_attr->pn.right->type || format_attr->pn.right->type == nodeType_Scan;
        if(!type_matches) {
            err = DIST2_ERR_TYPE_MISMATCH;
            goto out;
        }

        switch(format_attr->pn.right->type)
        {
            case nodeType_Scan:
            {
                switch(format_attr->pn.right->scn.c) {
                    case 's':
                        s = va_arg(args, char**);
                        if(value->type == nodeType_Ident) {
                            *s = value->in.str;
                            value->in.str = NULL;
                        }
                        else if(value->type == nodeType_String) {
                            *s = value->sn.str;
                            value->sn.str = NULL;
                        }
                        else {
                            err = DIST2_ERR_INVALID_FORMAT;
                            goto out;
                        }
                    break;

                    case 'd':
                        i = va_arg(args, int64_t*);
                        if(value->type == nodeType_Constant) {
                            *i = value->cn.value;
                        }
                        else {
                            *i = 0; // TODO handle errors like this?
                            err = DIST2_ERR_INVALID_FORMAT;
                            goto out;
                        }
                    break;

                    case 'f':
                        d = va_arg(args, double*);
                        if(value->type == nodeType_Float) {
                            *d = value->fn.value;
                        }
                        else {
                            *d = 0.0; // TODO handle errors like this?
                            err = DIST2_ERR_INVALID_FORMAT;
                            goto out;
                        }
                    break;

                    default:
                        err = DIST2_ERR_INVALID_FORMAT;
                        goto out;
                    break;
                }
            }
            break;

            case nodeType_Ident:
            {
                bool string_matches = strcmp(format_attr->pn.right->in.str, value->in.str) == 0;
                if(!type_matches || !string_matches) {
                    err = DIST2_ERR_ATTRIBUTE_MISMATCH;
                    goto out;
                }
            }
            break;

            case nodeType_String:
            {
                bool string_matches = strcmp(format_attr->pn.right->sn.str, value->sn.str) == 0;
                if(!string_matches) {
                    err = DIST2_ERR_ATTRIBUTE_MISMATCH;
                    goto out;
                }
            }
            break;

            case nodeType_Float:
            {
                bool double_matches = format_attr->pn.right->fn.value == value->fn.value;
                if(!double_matches) {
                    err = DIST2_ERR_ATTRIBUTE_MISMATCH;
                    goto out;
                }
            }
            break;

            case nodeType_Constant:
            {
                bool number_matches = format_attr->pn.right->cn.value == value->cn.value;
                if(!number_matches) {
                    err = DIST2_ERR_ATTRIBUTE_MISMATCH;
                    goto out;
                }
            }
            break;

            default:
                err = DIST2_ERR_INVALID_FORMAT;
                goto out;
            break;
        }
    }

    va_end(args);


out:
    free_ast(ast);
    free_ast(format_ast);
    return err;
}
