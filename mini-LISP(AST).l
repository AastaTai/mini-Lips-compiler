%option noyywrap
%{
    #include "y.tab.h"
%}
%%
"+"         {return ADD;}
"-"         {return SUB;}
"*"         {return MUL;}
"/"         {return DIV;}
"mod"       {return MOD;}
">"         {return GREATER;}
"<"         {return SMALLER;}
"="         {return EQUAL;}
"and"       {return AND;}
"or"        {return OR;}
"not"       {return NOT;}
"("         {return yytext[0];}
")"         {return yytext[0];}
"print-num"     {return PRINT_NUM;}
"print-bool"    {return PRINT_BOOL;}
"if"            {return IF;}
"define"        {return DEFINE;}
"fun"           {return FUN;}

[a-z]([a-z]|[0-9]|"-")*     {yylval.AST = newNode("string", strdup(yytext), 0, false, NULL, NULL); return ID;}
0|[1-9][0-9]*|-[1-9][0-9]*  {yylval.AST = newNode("integer", NULL, atoi(yytext), false, NULL, NULL); return NUM;}
"#f"|"#t"                   {yylval.AST = newNode("boolean", NULL, 0, yytext[1] == 't' ? true : false, NULL, NULL); return BOOL;}

"\t"|"\n"|"\r"|" "  {/*do nothing*/}
.                   {/*do nothing*/}
%%