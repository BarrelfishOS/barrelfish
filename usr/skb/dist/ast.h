#ifndef AST_H_
#define AST_H_

#include <stdlib.h>

#ifndef TEST_PARSER
#include <barrelfish/barrelfish.h>
#else

#define SYS_ERR_OK 0
typedef long errval_t;
typedef long int64_t;
static int err_is_ok(errval_t err) {
	return err == 0;
}
#endif



enum node_type {
	nodeType_Unset, // To detect errors
    nodeType_Float,
    nodeType_Constant,
    nodeType_Boolean,
    nodeType_String,
    nodeType_Ident,
    nodeType_Attribute,
    nodeType_Pair,
    nodeType_Constraint,
    nodeType_Object
};

struct node_record {
    struct ast_object* name;
    struct ast_object* attrs;
};

struct node_constant {
	int64_t value;
};

struct node_boolean {
    int value;
};

struct node_float {
    double value;
};

struct node_ident {
    char* str;
};

struct node_string {
    char* str;
};

struct node_constraint {
    size_t op;
    struct ast_object* value;
};

struct node_attribute {
    struct ast_object* attr;
    struct ast_object* next;
};

struct node_pair {
    struct ast_object* left;
    struct ast_object* right;
};

struct ast_object {
    enum node_type type;

    union {
        struct node_constant cn;
        struct node_constraint cnsn;
        struct node_boolean bn;
        struct node_float fn;
        struct node_ident in;
        struct node_string sn;
        struct node_attribute an;
        struct node_pair pn;
        struct node_record on;
    };
};

errval_t generate_ast(const char* input, struct ast_object** record);
void free_ast(struct ast_object* p);


static inline struct ast_object* alloc_node(void)
{
    struct ast_object* p = malloc(sizeof(struct ast_object));
    if (p == NULL) {
        //yyerror("out of memory");
    }

    return p;
}


static inline struct ast_object* boolean(int value)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_Boolean;
    p->bn.value = value;

    return p;
}


static inline struct ast_object* constraints(size_t op, struct ast_object* value)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_Constraint;
    p->cnsn.op = op;
    p->cnsn.value = value;

    return p;
}



static inline struct ast_object* floatingpoint(double value)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_Float;
    p->fn.value = value;

    return p;
}


static inline struct ast_object* object(struct ast_object* name, struct ast_object* attrs)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_Object;
    p->on.name = name;
    p->on.attrs = attrs;

    return p;
}


static inline struct ast_object* attribute(struct ast_object* attribute, struct ast_object* next)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_Attribute;
    p->an.attr = attribute;
    p->an.next = next;

    return p;
}


static inline struct ast_object* pair(struct ast_object* left, struct ast_object* right)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_Pair;
    p->pn.left = left;
    p->pn.right = right;

    return p;
}


static inline struct ast_object* ident(char* str)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_Ident;
    p->in.str = str;

    return p;
}


static inline struct ast_object* string(char* str)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_String;
    p->sn.str = str;

    return p;
}


static inline struct ast_object* num(int64_t value)
{
    struct ast_object* p = alloc_node();

    p->type = nodeType_Constant;
    p->cn.value = value;

    return p;
}


#endif // AST_H_
