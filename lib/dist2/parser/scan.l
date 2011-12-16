%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "y.tab.h"

#ifdef TEST_PARSER
#include "../../../include/dist2/parser/ast.h"
#else
#include <dist2/parser/ast.h>
#endif

int charcount=0,linecount=0;
char string_buf[100];
char *string_buf_ptr;


// not in C99 :-(
char* strdup(const char*);
%}

%option reentrant
%option bison-bridge
%option noyywrap
%option nounput
%option noinput
%option never-interactive

RBRACKET \[
LBRACKET \]
RCURLY \{
LCURLY \}
COLON \:
COMMA ,

ALPHA  [A-Za-z_]
LALPHA [a-z]
DIGIT  [0-9]
DIGIT19 [1-9]
DIGITS ({DIGIT})+

ALPHA_NUMERIC {ALPHA}|{DIGIT}
IDENT {LALPHA}({ALPHA_NUMERIC})*
VARIABLE [_]

INTEGER {DIGIT}|{DIGIT19}{DIGITS}|-{DIGIT}|-{DIGIT19}{DIGITS}
FRACTION \.{DIGITS}
FLOAT  {INTEGER}{FRACTION}|{FRACTION}
NUMBER {INTEGER}
SCAN [%]{ALPHA}
BOOL  true|false


SPACE ([\ \n\r\t\f])+

UNESCAPEDCHAR [ -!#-\[\]-~]
ESCAPEDCHAR \\['"\\bfnrt/]
CHAR {UNESCAPEDCHAR}|{ESCAPEDCHAR}
CHARS {CHAR}+
QUOTE [']
REGEX_START r{QUOTE}

%x REG
%x STR

%%
{BOOL}      {
                yylval.integer = (strcmp(yytext, "true") == 0) ? 1 : 0;
                return BOOL;
            }

{FLOAT}     {
                yylval.dl = atof(yytext);
                return FLOAT;
            }


{IDENT}     { 
                yylval.str = strdup(yytext); /* TODO this may leak memory on parser error! */ 
                return IDENT; 
            }

{SCAN}      {
                yylval.c = yytext[1];
                return SCAN;
            }

{VARIABLE}  {
                return VARIABLE;
            }

{REGEX_START}      { string_buf_ptr = string_buf; BEGIN(REG); }

<REG>\'        { /* saw closing quote - all done */
     BEGIN(INITIAL);
     *string_buf_ptr = '\0';
     yylval.str = strdup(string_buf);
     return REGEX;
     }

<REG>\n        {
     /* error - unterminated string constant */
     /* generate error message */
     }

<REG>\\n  *string_buf_ptr++ = '\n';
<REG>\\t  *string_buf_ptr++ = '\t';
<REG>\\r  *string_buf_ptr++ = '\r';
<REG>\\b  *string_buf_ptr++ = '\b';
<REG>\\f  *string_buf_ptr++ = '\f';

<REG>\\(.|\n)  *string_buf_ptr++ = yytext[1];

<REG>[^\\\n\']+        {
     char *yptr = yytext;

     while ( *yptr )
             *string_buf_ptr++ = *yptr++;
     }


\'      string_buf_ptr = string_buf; BEGIN(STR);

<STR>\'        { /* saw closing quote - all done */
     BEGIN(INITIAL);
     *string_buf_ptr = '\0';
     yylval.str = strdup(string_buf);
     return STRING;
     }

<STR>\n     {
         /* error - unterminated string constant */
         /* generate error message */
     }

<STR>\\n  *string_buf_ptr++ = '\n';
<STR>\\t  *string_buf_ptr++ = '\t';
<STR>\\r  *string_buf_ptr++ = '\r';
<STR>\\b  *string_buf_ptr++ = '\b';
<STR>\\f  *string_buf_ptr++ = '\f';

<STR>\\(.|\n)  *string_buf_ptr++ = yytext[1];

<STR>[^\\\n\']+        {
     char *yptr = yytext;

     while ( *yptr )
             *string_buf_ptr++ = *yptr++;
     }

{NUMBER}   {
                yylval.integer = atoll(yytext);
                return NUMBER;
            }

{RCURLY}        return RCURLY; 
{LCURLY}        return LCURLY;
{RBRACKET}      return RBRACKET;
{COMMA}         return COMMA;
{LBRACKET}      return LBRACKET; 
{COLON}         return COLON;

">"             return GT;
">="            return GE;
"<"             return LT;
"<="            return LE;
"=="            return EQ;
"!="            return NE;


\n          { linecount++; }
{SPACE}     { /* eat up whitespace */ }

.           {
                printf("Unexpected: %c\n", *yytext);
                printf("Deal with it!\n");
            }
%%
