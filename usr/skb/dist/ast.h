#ifndef AST_H_
#define AST_H_

#include <barrelfish/barrelfish.h>

enum node_type { 
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
    int value;
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


#endif // AST_H_

