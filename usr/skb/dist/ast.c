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

errval_t generate_ast(const char* input, struct ast_object** record)
{
	printf("input before yacc is: %s\n", input);
	yy_scan_string(input);
    yyparse();
    yylex_destroy();
    printf("input after yacc is: %s\n", input);

    errval_t err = dist2_parser_error;

    if(err_is_ok(err)) {
    	printf("dist2_parsed_ast for input %s is %p\n", input, dist2_parsed_ast);
		*record = dist2_parsed_ast;
		dist2_parsed_ast = NULL;
    }

    return err;
}
