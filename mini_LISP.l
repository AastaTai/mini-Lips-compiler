%option noyywrap
%{
    #include "y.tab.h"
%}
%%
"+"             {return ADD;}
"-"             {return SUB;}
"*"             {return MUL;}
"/"             {return DIV;}
"mod"           {return MOD;}
">"             {return GREATER;}
"<"             {return SMALLER;}
"="             {return EQUAL;}
"and"           {return AND;}
"or"            {return OR;}
"not"           {return NOT;}
"("             {return yytext[0];}
")"             {return yytext[0];}
"print-num"     {return PRINT_NUM;}
"print-bool"    {return PRINT_BOOL;}
"if"            {return IF;}
"define"        {return DEFINE;}
"fun"           {return FUN;}

[a-z]([a-z]|[0-9]|"-")*     {
                                yylval.choice.type = 1;
                                yylval.choice.name = strdup(yytext);
                                return ID;
                            }
0|[1-9][0-9]*|-[1-9][0-9]*  {
                                yylval.choice.type = 2;
                                yylval.choice.intVal = atoi(yytext);
                                return NUM;
                            }
"#f"|"#t"                   {
                                yylval.choice.type = 3;
                                yylval.choice.boolVal = yytext[1] == 't' ? true : false;
                                return BOOL;
                            }

"\t"|"\n"|"\r"|" "  {/*do nothing*/}
.                   {/*do nothing*/}
%%