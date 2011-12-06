#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include <dist2/getset.h>
#include <dist2/parser/ast.h>

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
