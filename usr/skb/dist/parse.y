%{
#include <stdio.h>

#include <barrelfish/barrelfish.h>

#include "y.tab.h"
#include "ast.h"
#include "ast.h"

int yylex(void);
void yyerror(char *);

static struct ast_object* ident(char*);
static struct ast_object* boolean(int);
static struct ast_object* floatingpoint(double);
static struct ast_object* num(int);
static struct ast_object* pair(struct ast_object*, struct ast_object*);
static struct ast_object* attribute(struct ast_object*, struct ast_object*);
static struct ast_object* object(struct ast_object*, struct ast_object*);
static struct ast_object* constraints(size_t, struct ast_object*);
static struct ast_object* string(char*);

void free_nodes(struct ast_object*);

extern struct ast_object* dist2_parsed_ast;
extern errval_t dist2_parser_error;
%}

%error-verbose
%union {
    int integer;
    double dl;
    char* str;
    struct ast_object* nPtr;
};

%token RBRACKET
%token LBRACKET
%token RCURLY
%token LCURLY
%token COLON
%token COMMA
%token GT
%token GE
%token LT
%token LE
%token EQ
%token NE
%token REGEX

%token <integer> BOOL
%token <dl> FLOAT
%token <integer> NUMBER
%token <str> IDENT
%token <str> REGEX
%token <str> STRING

%type <nPtr> value
%type <nPtr> attribute
%type <nPtr> attributes
%type <nPtr> object
%type <nPtr> constraint
%type <nPtr> program

%%
program: 
      object                         { dist2_parsed_ast = $1; }
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
    | IDENT COLON constraint         { $$ = pair(ident($1), $3); }

constraint:
      GT value                       { $$ = constraints(GT, $2);  }
    | GE value                       { $$ = constraints(GE, $2); }
    | LT value                       { $$ = constraints(LT, $2); }
    | LE value                       { $$ = constraints(LE, $2); }
    | EQ value                       { $$ = constraints(EQ, $2); }
    | NE value                       { $$ = constraints(NE, $2); }
    | REGEX                          { $$ = constraints(REGEX, string($1)); }

value:
      STRING                         { $$ = string($1); }
    | IDENT                          { $$ = ident($1); }
    | NUMBER                         { $$ = num($1); }
    | BOOL                           { $$ = boolean($1); }
    | FLOAT                          { $$ = floatingpoint($1); }
//    | VARIABLE                       { $$ = }
%%

void yyerror(char *s) 
{
    fprintf(stderr, "yyerror says: %s\n", s);
}


static struct ast_object* alloc_node(void) 
{
    struct ast_object* p = malloc(sizeof(struct ast_object));
    if (p == NULL) {
        yyerror("out of memory");
    }

    return p;
}

static struct ast_object* boolean(int value)
{
    struct ast_object* p = alloc_node();
    
    p->type = nodeType_Boolean;
    p->bn.value = value;

    return p;
}


static struct ast_object* constraints(size_t op, struct ast_object* value)
{
    struct ast_object* p = alloc_node();
    
    p->type = nodeType_Constraint;
    p->cnsn.op = op;
    p->cnsn.value = value;

    return p;
}



static struct ast_object* floatingpoint(double value)
{
    struct ast_object* p = alloc_node();
    
    p->type = nodeType_Float;
    p->fn.value = value;

    return p;
}


static struct ast_object* object(struct ast_object* name, struct ast_object* attrs) 
{
    
    struct ast_object* p = alloc_node();
    
    p->type = nodeType_Object;
    p->on.name = name;
    p->on.attrs = attrs;

    return p;
}


static struct ast_object* attribute(struct ast_object* attribute, struct ast_object* next) 
{
    
    struct ast_object* p = alloc_node();
    
    p->type = nodeType_Attribute;
    p->an.attr = attribute;
    p->an.next = next;

    return p;
}


static struct ast_object* pair(struct ast_object* left, struct ast_object* right) 
{

    struct ast_object* p = alloc_node();
    
    p->type = nodeType_Pair;
    p->pn.left = left;
    p->pn.right = right;

    return p;
}


static struct ast_object* ident(char* str) 
{

    struct ast_object* p = alloc_node();

    p->type = nodeType_Ident;
    p->in.str = str;

    return p;
}


static struct ast_object* string(char* str) 
{

    struct ast_object* p = alloc_node();

    p->type = nodeType_String;
    p->sn.str = str;

    return p;
}


static struct ast_object* num(int value) 
{

    struct ast_object* p = alloc_node();

    p->type = nodeType_Constant;
    p->cn.value = value;

    return p;
}


void free_nodes(struct ast_object* p) 
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
