#include <stdio.h>

#include <barrelfish/barrelfish.h>

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
            free(p->in.str); // TODO: avoid leak memory if parser has error :-(
        break;

        case nodeType_Pair:
            free_ast(p->pn.left);
            free_ast(p->pn.right);
        break;

        default:
        break;
    }

    free (p);
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
