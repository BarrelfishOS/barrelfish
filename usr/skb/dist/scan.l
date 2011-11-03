%{
#include <string.h>
#include "y.tab.h"
#include "ast.h"
int charcount=0,linecount=0;

char* strdup(const char*);
%}

%option noyywrap
%option nounput
%option noinput

RBRACKET \[
LBRACKET \]
RCURLY \{
LCURLY \}
COLON \:
COMMA ,

ALPHA  [A-Za-z_]
DIGIT  [0-9]
DIGIT19 [1-9]
DIGITS ({DIGIT})+

ALPHA_NUMERIC {ALPHA}|{DIGIT}
IDENT {ALPHA}({ALPHA_NUMERIC})*

INTEGER {DIGIT}|{DIGIT19}{DIGITS}|-{DIGIT}|-{DIGIT19}{DIGITS}
FRACTION \.{DIGITS}
FLOAT {INTEGER}{FRACTION}|{FRACTION}
NUMBER {INTEGER}|{FLOAT}
BOOL  true|false

SPACE ([\ \n\r\t\f])+

%%
{BOOL}      {
                return BOOL;
            }

{IDENT}     { 
                yylval.str = strdup(yytext); /* this may leak memory on parser error! */ 
                return IDENT; 
            }

{RCURLY}    {
                return RCURLY; 
            }

{LCURLY}    {
                return LCURLY;
            }

{RBRACKET}  { 
                return RBRACKET; 
            }

{COMMA}     {
                return COMMA;
            }

{LBRACKET}  { 
                return LBRACKET; 
            }

{COLON}     { 
                return COLON;
            }

{NUMBER}   {
                yylval.integer = atoi(yytext);
                return NUMBER;
            }


\n          { linecount++; }
{SPACE}     { /* eat up whitespace */ }

.           {
                printf("Unexpected: %c\n", *yytext);
                printf("Deal with it!\n");
            }
%%
