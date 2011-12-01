#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include "code_generator.h"

// forward decl
static inline struct pword_pair create_constraint(struct ast_object*);

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

        default:
            assert(!"Not allowed for queries!"); // TODO return error in this case!
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


struct pword_pair {
    bool is_attribute;
    pword value;
    pword op;
};


static inline struct pword_pair visit_attribute_right(struct ast_object* p)
{
    struct pword_pair terms;
    terms.is_attribute = true;

    switch(p->type) {
        case nodeType_Ident:
            terms.value = ec_atom(ec_did(p->in.str, 0));
        break;

        case nodeType_String:
            assert(p->sn.str != NULL);
            terms.value = ec_string(p->sn.str);
        break;

        case nodeType_Float:
            terms.value = ec_double(p->fn.value);
        break;

        case nodeType_Constant:
            terms.value = ec_long(p->cn.value);
        break;

        case nodeType_Variable:
            terms.value = ec_newvar();
        break;

        case nodeType_Constraint:
            terms = create_constraint(p);
        break;

        default:
            assert(!"Should not happen, check your parser!");
        break;

    }

    return terms;
}


static inline struct pword_pair create_constraint(struct ast_object* p)
{
    assert(p != NULL);
    assert(p->type == nodeType_Constraint);

    struct pword_pair terms = visit_attribute_right(p->cnsn.value);
    terms.is_attribute = false;

    switch(p->cnsn.op) {
        case constraint_GT:
            terms.op = ec_atom(ec_did(">", 2));
        break;

        case constraint_GE:
            terms.op = ec_atom(ec_did(">=", 2));
        break;

        case constraint_LT:
            terms.op = ec_atom(ec_did("<", 2));
        break;

        case constraint_LE:
            terms.op = ec_atom(ec_did("=<", 2));
        break;

        case constraint_EQ:
            terms.op = ec_atom(ec_did("==", 2));
        break;

        case constraint_NE:
            terms.op = ec_atom(ec_did("=/=", 2));
        break;

        case constraint_REGEX:
            terms.op = ec_atom(ec_did("match", 2));
        break;

        default:
            assert(!"OP code not supported");
        break;
    }


    return terms;
}



static void translate2(struct ast_object* p, struct skb_ec_terms* ss)
{
    assert(p != NULL);
    assert(p->type == nodeType_Object);

    if(p->on.name->type == nodeType_Ident) {
        dident name_id = ec_did(p->on.name->in.str, 0);
        ss->name = ec_atom(name_id);
    }
    else if(p->on.name->type == nodeType_Variable) {
        ss->name = ec_newvar();
    }
    else {
        assert(!"Scan types not allowed here"); // TODO
    }

    ss->attribute_list = ec_nil();
    ss->constraint_list = ec_nil();

    struct ast_object* iter = p->on.attrs;
    for(; iter != NULL; iter = iter->an.next) {
        assert(iter->type == nodeType_Attribute);
        struct ast_object* left = iter->an.attr->pn.left;
        struct ast_object* right = iter->an.attr->pn.right;

        dident attr_id = ec_did(left->in.str, 0);
        pword left_term = ec_atom(attr_id);

        struct pword_pair right_terms = visit_attribute_right(right);

        if(right_terms.is_attribute) {
            pword entry = ec_term(ec_did("::", 2), left_term, right_terms.value);
            ss->attribute_list = ec_list(entry, ss->attribute_list);
        }
        else { // is constraint
            dident constraint = ec_did("constraint", 3);
            pword entry = ec_term(constraint, left_term, right_terms.op, right_terms.value);
            ss->constraint_list = ec_list(entry, ss->constraint_list);
        }
    }

}



errval_t transform_record2(struct ast_object* ast, struct skb_ec_terms* record)
{
    assert(ast != NULL);
    translate2(ast, record);

    return SYS_ERR_OK;
}


