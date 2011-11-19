%{
#include <stdio.h>

#include "ast.h"
#include "y.tab.h"
typedef long int64_t;

int yylex(void);
void yyerror(char *);

static struct ast_object* ident(char*);
static struct ast_object* boolean(int);
static struct ast_object* floatingpoint(double);
static struct ast_object* num(int64_t);
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
    long long int integer;
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
      IDENT                          { $$ = ident($1); printf("got ident: %s\n", $1); }
    | STRING                         { $$ = string($1); }
    | NUMBER                         { $$ = num($1); }
    | BOOL                           { $$ = boolean($1); }
    | FLOAT                          { $$ = floatingpoint($1); }
//    | VARIABLE                       { $$ = }
%%

void yyerror(char *s) 
{
    printf("yyerror says: %s\n", s);
}
