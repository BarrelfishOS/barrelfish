#ifndef AST_H_
#define AST_H_

enum nodeType { 
    nodeType_Float,
    nodeType_Constant,
    nodeType_String,
    nodeType_Attribute,
    nodeType_Pair,
    nodeType_Object
};

struct objectNode {
    struct nodeObject* name;
    struct nodeObject* attrs;
};

struct constantNode {
    int value;
};

struct floatNode {
    float fl;
};

struct identNode{
    char* str;
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
        struct floatNode fn;
        struct identNode in;
        struct attributeNode an;
        struct pairNode pn;
        struct objectNode on;
    };
};

#endif // AST_H_

