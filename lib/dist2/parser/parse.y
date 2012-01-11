%{
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#ifdef TEST_PARSER
#include "../../../include/dist2/parser/ast.h"
#else
#include <dist2/parser/ast.h>
#endif

#include "y.tab.h"

void yyerror(char *);

#define YYPARSE_PARAM data
#define YYLEX_PARAM   ((struct dist_parser_state*)data)->scanner

%}

%define api.pure
%error-verbose

%union {
    long long int integer;
    double dl;
    char* str;
    char c;
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
%token VARIABLE
%token EUNEXPECTED

%token <integer> BOOL
%token <dl> FLOAT
%token <integer> NUMBER
%token <str> IDENT
%token <str> REGEX
%token <str> STRING
%token <c> SCAN

%type <nPtr> value
%type <nPtr> attribute
%type <nPtr> attributes
%type <nPtr> object
%type <nPtr> constraint
%type <nPtr> name

%destructor { free_ast($$); } <nPtr>
%destructor { free($$); } <str>

%%
program: 
      object                         { ((struct dist_parser_state*) data)->ast = $1; YYACCEPT; }
    | ;

object: 
      name                           { $$ = ast_object($1, NULL);  } 
    | name RCURLY LCURLY             { $$ = ast_object($1, NULL);  } 
    | name RCURLY attributes LCURLY  { $$ = ast_object($1, $3);  }

name:
      IDENT                          { $$ = ast_ident($1); }
    | VARIABLE                       { $$ = ast_variable(); }
    | SCAN                           { $$ = ast_scan($1); }
    | REGEX                          { $$ = ast_constraints(constraint_REGEX, ast_string($1)); }

attributes:
      attribute                      { $$ = ast_attribute($1, NULL); }
    | attribute COMMA attributes     { $$ = ast_attribute($1, $3); }

attribute:
      IDENT COLON value              { $$ = ast_pair(ast_ident($1), $3); }
    | IDENT COLON constraint         { $$ = ast_pair(ast_ident($1), $3); }

constraint:
      GT value                       { $$ = ast_constraints(constraint_GT, $2);  }
    | GE value                       { $$ = ast_constraints(constraint_GE, $2); }
    | LT value                       { $$ = ast_constraints(constraint_LT, $2); }
    | LE value                       { $$ = ast_constraints(constraint_LE, $2); }
    | EQ value                       { $$ = ast_constraints(constraint_EQ, $2); }
    | NE value                       { $$ = ast_constraints(constraint_NE, $2); }
    | REGEX                          { $$ = ast_constraints(constraint_REGEX, ast_string($1)); }

value:
      IDENT                          { $$ = ast_ident($1); }
    | STRING                         { $$ = ast_string($1); }
    | NUMBER                         { $$ = ast_num($1); }
    | BOOL                           { $$ = ast_boolean($1); }
    | FLOAT                          { $$ = ast_floatingpoint($1); }
    | SCAN                           { $$ = ast_scan($1); }
    | VARIABLE                       { $$ = ast_variable(); }
    
%%

void yyerror(char *s)
{
#ifdef DIST2_DEBUG
    fprintf("dist2_parser: %s\n", s);
#endif
}
