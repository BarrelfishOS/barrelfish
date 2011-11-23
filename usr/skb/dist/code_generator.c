#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include "code_generator.h"

#define INITIAL_LENGTH 256

// State used by tranlate()
static struct skb_record* sr;
static struct writer* w;
struct ast_object* attribute_name;


void emit(struct writer* w, const char* format, ...)
{
	assert(w != NULL);
	assert(format != NULL);
	va_list  args;

    if(w->output == NULL) {
        w->output = malloc(INITIAL_LENGTH);
        assert(w->output != NULL);
        w->pos = 0;
        w->length = INITIAL_LENGTH;
    }

	va_start(args, format);
	int append_len = vsnprintf(NULL, 0, format, args);
    va_end(args);

    size_t occupied = w->pos + append_len;
    if(w->length < occupied) {
        w->output = realloc(w->output, occupied+1);
        w->length = occupied;
    }

    va_start(args, format);
	int bytes_written = vsnprintf(w->output+w->pos, append_len+1, format, args);
    va_end(args);
    assert(bytes_written == append_len);

    w->pos = occupied;
}


static void translate(struct ast_object* p) {
	assert(sr != NULL);
    assert(p != NULL);

    switch(p->type) {
        case nodeType_Object:
            assert(p->on.name != NULL);
            w = &sr->name;
            
            translate(p->on.name);
    
            w = &sr->attributes;
			emit(w, "[ ");
			emit(&sr->constraints, "[ ");
            if(p->on.attrs) {
                translate(p->on.attrs);
            }
			emit(w, " ]");

            size_t len = strlen(sr->constraints.output);
            if(sr->constraints.output[len-2] == ',') {
				sr->constraints.output[len-2] = ' ';
				sr->constraints.output[len-1] = ']';
            }
            else {
            	emit(&sr->constraints, " ]");
            }

        break;

        case nodeType_Attribute:
            assert(p->an.attr != NULL);

            translate(p->an.attr);
            if(p->an.next != NULL) {
                emit(w, ", ");
                translate(p->an.next);
            }
        break;

        case nodeType_Pair:
            assert(p->pn.left != NULL);
            assert(p->pn.right != NULL);

            attribute_name = p->pn.left;

            translate(p->pn.left);
            emit(w, "::");
            translate(p->pn.right);
        break;

        case nodeType_Constraint:
            assert(p->cnsn.value != NULL);
            // prolog variable, dont care about result, just make sure it's set
            emit(w, "_");
        
            w = &sr->constraints;
            char* operator;
            switch(p->cnsn.op) {
                case constraint_GT:
                    operator = ">";
                break;
                case constraint_GE:
                    operator = ">=";
                break;
                case constraint_LT:
                    operator = "<";
                break;
                case constraint_LE:
                    operator = "=<";
                break;
                case constraint_EQ:
                    operator = "==";
                break;
                case constraint_NE:
                    operator = "=/=";
                break;
                case constraint_REGEX:
                	operator = "match";
				break;
                default:
                    assert(!"OP code not supported");
                break;
            }
            emit(w, "constraint(");
            translate(attribute_name);
            emit(w, ", ");
            emit(w, "'%s'", operator);
            emit(w, ", ");
            translate(p->cnsn.value);
            emit(w, "), ");    
            w = &sr->attributes;
        break;

        case nodeType_Float:
            emit(w, "%f", p->fn.value);
        break;

        case nodeType_Boolean:
            if(p->bn.value) {
            	emit(w, "true");
            }
            else {
            	emit(w, "false");
            }
        break;

        case nodeType_Constant:
            emit(w, "%d", p->cn.value);
        break;

        case nodeType_String:
            emit(w, "\'");
            emit(w, p->sn.str);
            emit(w, "\'");
        break;

        case nodeType_Ident:
            emit(w, p->in.str);
        break;

        case nodeType_Unset:
        	assert(!"nodeType_Unset");
		break;
   }

}


static void init_writer(struct writer* w) {
	w->pos = 0;
	w->length = 0;
	w->output = NULL;
}


static void init_record_writers(struct skb_record* po) {
	init_writer(&po->attributes);
	init_writer(&po->constraints);
	init_writer(&po->name);
}


void free_parsed_object(struct skb_record* po)
{
	free(po->name.output);
	free(po->attributes.output);
	free(po->constraints.output);
	free(po);
}


errval_t transform_record(struct ast_object* ast, struct skb_record** record)
{
	assert(ast != NULL);

	sr = NULL;
	sr = malloc(sizeof(struct skb_record));
	if(sr == NULL) {
		return LIB_ERR_MALLOC_FAIL;
	}

	init_record_writers(sr);
	translate(ast);
	*record = sr;

    return SYS_ERR_OK;
}


