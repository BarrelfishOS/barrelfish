#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "code_generator.h"
#include "ast.h"
#include "y.tab.h"
#include "flex.h"


static struct parsed_object* po;
static struct writer* w;

extern void yyparse();

#define INITIAL_OUTPUT_LEN 256

#ifdef PARSER_DEBUG
static void print_writer(struct writer* w) {
    printf("\tpos: %lu\n", w->pos);
    printf("\tlength: %lu\n", w->length);
    printf("\toutput: %s\n", w->output);
}
#endif


static void emit(struct writer* w, char* to_append) {
	assert(w != NULL);
	assert(to_append != NULL);

    if(w->output == NULL) {
        w->output = malloc(INITIAL_OUTPUT_LEN);
        assert(w->output != NULL);
        w->pos = 0;
        w->length = INITIAL_OUTPUT_LEN;
    }

    size_t append_len = strlen(to_append);
    size_t new_length = w->pos+append_len;

    if(new_length > w->length) {
        w->output = realloc(w->output, new_length + 1); // include \0
        w->length = new_length;
    }

    strcpy((w->output + w->pos), to_append);
    w->pos = w->pos + append_len;
}


int ex(struct nodeObject* p) {
    char buf[50];
    if (!p) return 0;

    switch(p->type) {
        case nodeType_Object:
            assert(p->on.name != NULL);
            w = &po->name;

            ex(p->on.name);
    
            w = &po->attributes;
            if(p->on.attrs) {
                emit(w, " [ ");
                ex(p->on.attrs);
            }
        break;

        case nodeType_Attribute:
            assert(p->an.attr != NULL);

            ex(p->an.attr);
            if(p->an.next != NULL) {
                emit(w, ", ");
                ex(p->an.next);
            }
            else {
                emit(w, " ]");
            }
        break;

        case nodeType_Pair:
            assert(p->pn.left != NULL);
            assert(p->pn.right != NULL);
            ex(p->pn.left);
            emit(w, "::");
            ex(p->pn.right);
        break;

        case nodeType_Float:
            sprintf(buf, "%f", p->fn.fl);
            emit(w, buf);
        break;

        case nodeType_Constant:
            sprintf(buf, "%d", p->cn.value);      
            emit(w, buf);
        break;

        case nodeType_String:
            emit(w, p->in.str);
        break;
   }
    return 0;
}

static void init_writer(struct writer* w) {
	w->pos = 0;
	w->length = 0;
	w->output = NULL;
}

static void init_po(struct parsed_object* po) {
	init_writer(&po->attributes);
	init_writer(&po->constraints);
	init_writer(&po->name);
}


struct parsed_object* transform_query(const char* input)
{
	po = malloc(sizeof(struct parsed_object));
	init_po(po);

    yy_scan_string(input);
    yyparse();
    yylex_destroy();

    //print_writer(&w);

    return po;
}


#ifdef TEST_PARSER
int main(int argc, char** argv) 
{
	transform_query("obj1");
	transform_query("obj2 {}");
	transform_query("obj3 { int: -11, fl: 12.0}");
	transform_query("obj4 { int: 12, fl: .0012321, fl2: .22123100 }");
	transform_query("obj5 { reference: bla, integer: 12, string: \
                     String, float: 12.0, bool: true }");

    return 0;
}
#endif
