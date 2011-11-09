#ifndef AST_H_
#define AST_H_

enum nodeType { 
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

struct objectNode {
    struct nodeObject* name;
    struct nodeObject* attrs;
};

struct constantNode {
    int value;
};

struct booleanNode {
    int value;
};

struct floatNode {
    double value;
};

struct identNode {
    char* str;
};

struct stringNode {
    char* str;
};

struct constraintNode {
    size_t op;
    struct nodeObject* value;
};

struct attributeNode {
    struct nodeObject* attr;
    struct nodeObject* next;
};

struct pairNode {
    struct nodeObject* left;
    struct nodeObject* right;
};

struct nodeObject {
    enum nodeType type;
    union {
        struct constantNode cn;
        struct constraintNode cnsn;
        struct booleanNode bn;
        struct floatNode fn;
        struct identNode in;
        struct stringNode sn;
        struct attributeNode an;
        struct pairNode pn;
        struct objectNode on;
    };
};

#endif // AST_H_

