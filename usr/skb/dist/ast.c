#include <stdio.h>
#include <assert.h>

#include "ast.h"
#include "flex.h"

extern void yyparse();

struct ast_object* dist2_parsed_ast = NULL;
errval_t dist2_parser_error = SYS_ERR_OK;

void free_ast(struct ast_object* p) {
    if (!p) return;

    switch(p->type) {
        case nodeType_Object:
            free_ast(p->on.name);
            free_ast(p->on.attrs);
        break;

        case nodeType_Attribute:
            free_ast(p->an.attr);
            free_ast(p->an.next);
        break;

        case nodeType_String:
            free(p->sn.str); // TODO: avoid leak memory if parser has error :-(
        	p->sn.str = NULL;
        break;

        case nodeType_Ident:
            free(p->in.str); // TODO mem leaks on parser error
        	p->in.str = NULL;
       	break;

        case nodeType_Constraint:
        	free_ast(p->cnsn.value);
		break;

        case nodeType_Pair:
            free_ast(p->pn.left);
            free_ast(p->pn.right);
        break;

        case nodeType_Unset:
        	assert(!"nodeType_Unset encountered in free_ast!");
        	abort();
        break;

        default:
        	// Nothing to do for value types
       	break;
    }

    free (p);
}


void append_attribute(struct ast_object* ast, struct ast_object* to_insert)
{
    struct ast_object** attr = &ast->on.attrs;
    for(; *attr != NULL; attr = &(*attr)->an.next) {
        // continue
    }

    struct ast_object* new_attr = alloc_node();
    new_attr->type = nodeType_Attribute;
    new_attr->an.attr = to_insert;
    new_attr->an.next = NULL;
    *attr = new_attr;
}


struct ast_object* find_attribute(struct ast_object* ast, char* name)
{
    struct ast_object** attr = &ast->on.attrs;

    for(; *attr != NULL; attr = &(*attr)->an.next) {

        assert((*attr)->type == nodeType_Attribute);
        if(strcmp((*attr)->an.attr->pn.left->in.str, name) == 0) {
            return (*attr)->an.attr;
        }

    }

    return NULL;
}


struct ast_object* remove_attribute(struct ast_object* ast, char* name)
{
    struct ast_object** attr = &ast->on.attrs;

    for(; *attr != NULL; attr = &(*attr)->an.next) {

        assert((*attr)->type == nodeType_Attribute);
        struct ast_object* pair = (*attr)->an.attr;
        struct ast_object* left = pair->pn.left;

        if(strcmp(left->in.str, name) == 0) {
            struct ast_object* current_attr = *attr;

            *attr = current_attr->an.next;

            current_attr->an.next = NULL;
            current_attr->an.attr = NULL;
            free_ast(current_attr);

            return pair;
        }

    }

    return NULL;
}


errval_t generate_ast(const char* input, struct ast_object** record)
{
	yy_scan_string(input);
    yyparse();
    yylex_destroy();

    errval_t err = dist2_parser_error;

    if(err_is_ok(err)) {
		*record = dist2_parsed_ast;
		dist2_parsed_ast = NULL;
    }

    return err;
}
