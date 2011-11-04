%{
#define YYERROR_VERBOSE 1
#include<stdio.h>

#include "y.tab.h"
#include "ast.h"
#include "code_generator.h"

int yylex(void);
void yyerror(char *);

struct nodeObject* ident(char*);
struct nodeObject* num(int);
struct nodeObject* pair(struct nodeObject*, struct nodeObject*);
struct nodeObject* attribute(struct nodeObject*, struct nodeObject*);
struct nodeObject* object(struct nodeObject*, struct nodeObject*);
void free_nodes(struct nodeObject*);
%}

%union {
    int integer;
    float fl;
    char* str;
    struct nodeObject* nPtr;
};

%token RBRACKET
%token LBRACKET
%token RCURLY
%token LCURLY
%token COLON
%token COMMA
%token BOOL
%token <integer> NUMBER
%token <str> IDENT

%type <nPtr> value
%type <nPtr> attribute
%type <nPtr> attributes
%type <nPtr> object
%type <nPtr> program

%%
program: 
      object                         { ex($1); free_nodes($1); }
    | ;

object: 
      IDENT                          { $$ = object(ident($1), NULL); } 
    | IDENT RCURLY LCURLY            { $$ = object(ident($1), NULL); } 
    | IDENT RCURLY attributes LCURLY { $$ = object(ident($1), $3); }

attributes:
      attribute                      { $$ = attribute($1, NULL); }
    | attribute COMMA attributes     { $$ = attribute($1, $3); }

attribute:
      IDENT COLON value              { $$ = pair(ident($1), $3); }

value:
      IDENT                          { $$ = ident($1); }
    | NUMBER                         { $$ = num($1); }
    | BOOL                           {}
%%

void yyerror(char *s) 
{
    fprintf(stderr, "yyerror says: %s\n", s);
}

static struct nodeObject* alloc_node(void) 
{
    struct nodeObject* p = malloc(sizeof(struct nodeObject));
    if (p == NULL) {
        yyerror("out of memory");
    }

    return p;
}

struct nodeObject* object(struct nodeObject* name, struct nodeObject* attrs) 
{
    
    struct nodeObject* p = alloc_node();
    
    p->type = nodeType_Object;
    p->on.name = name;
    p->on.attrs = attrs;

    return p;
}


struct nodeObject* attribute(struct nodeObject* attribute, struct nodeObject* next) 
{
    
    struct nodeObject* p = alloc_node();
    
    p->type = nodeType_Attribute;
    p->an.attr = attribute;
    p->an.next = next;

    return p;
}


struct nodeObject* pair(struct nodeObject* left, struct nodeObject* right) 
{

    struct nodeObject* p = alloc_node();
    
    p->type = nodeType_Pair;
    p->pn.left = left;
    p->pn.right = right;

    return p;
}


struct nodeObject* ident(char* str) 
{

    struct nodeObject* p = alloc_node();

    p->type = nodeType_String;
    p->in.str = str;

    return p;
}


struct nodeObject* num(int value) 
{

    struct nodeObject* p = alloc_node();

    p->type = nodeType_Constant;
    p->cn.value = value;

    return p;
}


void free_nodes(struct nodeObject* p) 
{
    if (!p) return;

    switch(p->type) {
        case nodeType_Object:
            free_nodes(p->on.name);
            free_nodes(p->on.attrs);
        break;

        case nodeType_Attribute:
            free_nodes(p->an.attr);
            free_nodes(p->an.next);
        break;

        case nodeType_String:
            free(p->in.str); // TODO: avoid leak memory if parser has error :-(
        break;

        case nodeType_Pair:
            free_nodes(p->pn.left);
            free_nodes(p->pn.right);
        break;

        default:
        break;
    }

    free (p);
}


/*int main(void)
{ 
    yyparse(); 
    return 0;
}*/
