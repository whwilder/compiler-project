/* compile.l */
%{
extern int yychar;
extern int errcode;
void yyerror(char *s);
char *errtoken;
#include "compile.tab.h"
%}

%option noyywrap
%option nounput
%option noinput
%option yylineno
/* Declarations */

DIGIT [0-9]
LETTER [a-zA-Z]
CH    [[:print:]]{-}["]

%s COMMENT
%s COMMENTSTAR

%%

 /* Rules */
"/*"                       {BEGIN(COMMENT);}
<COMMENT>[^*]*
<COMMENT><<EOF>>           {yyerror("syntax error");exit(1);}
<COMMENT>"*"               {BEGIN(COMMENTSTAR);}
<COMMENTSTAR>"/"           {BEGIN(INITIAL);}
<COMMENTSTAR>"*"
<COMMENTSTAR>[^/]          {BEGIN(COMMENT);}

{DIGIT}{DIGIT}*              {yylval.intcon=atoi(yytext);return INTCON;}
'[[:print:]]{-}['\\]'      {yylval.charptr=strdup(yytext);return CHARCON;}
'\\n'                      {yylval.charptr=strdup(yytext);return CHARCON;}
'\\0'                      {yylval.charptr=strdup(yytext);return CHARCON;}
\"([[:print:]]{-}["])*\"   {yylval.charptr=strdup(yytext);return STRINGCON;}
"char"                     {yylval.charptr=strdup(yytext);return CHAR;}
"int"                      {yylval.charptr=strdup(yytext);return INT;}
"void"                     {yylval.charptr=strdup(yytext);return VOID;}
"extern"                   {yylval.charptr=strdup(yytext);return EXTERN;}
"if"                       {yylval.charptr=strdup(yytext);return IF;}
"else"                     {yylval.charptr=strdup(yytext);return ELSE;}
"while"                    {yylval.charptr=strdup(yytext);return WHILE;}
"for"                      {yylval.charptr=strdup(yytext);return FOR;}
"return"                   {yylval.charptr=strdup(yytext);return RETURN;}
[a-zA-Z]([a-zA-Z0-9_])*    {yylval.charptr=strdup(yytext);return ID;}
"&&"                       {yylval.charptr=strdup(yytext);return AND;}
"||"                       {yylval.charptr=strdup(yytext);return OR;}
"=="                       {yylval.charptr=strdup(yytext);return EQ;}
"!="                       {yylval.charptr=strdup(yytext);return NE;}
"<="                       {yylval.charptr=strdup(yytext);return LE;}
"<"                        {yylval.charptr=strdup(yytext);return LT;}
">="                       {yylval.charptr=strdup(yytext);return GE;}
">"                        {yylval.charptr=strdup(yytext);return GT;}
"("|")"|"{"|"}"|"["|"]"|";"|","|"="|"+"|"-"|"*"|"/"|"!" {yylval.ch=yytext[0]; return yytext[0];}

[[:space:]]
.                          {yylval.ch=yytext[0];return UNKNOWN;}

%%
