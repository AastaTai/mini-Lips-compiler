// Q1~Q6 b2_1
%code requires
{
    #include <stdio.h>
    #include <string.h>

    #define bool int
    #define true 1
    #define false 0
    void yyerror(const char *message);

    struct Define{
        int type;   // 1:string, 2:num, 3:bool
        char* name;
        int intVal;
        bool boolVal;
    };

    struct Define getDefVar(char* varName);
    struct Define defVar[999];
    int defVarTop;

    //struct Define getDefFun(char* varName);
    //struct Define defFun[999];
    //int defFunTop;

    struct Choice{
        int type;   // 1:string, 2:num, 3:bool
        char* name;
        int intVal;
        bool boolVal;
        int add;
        int mul;
        bool equ;
        bool and;
        bool or;
    };
}
%define parse.error verbose  //give more details when syntax error happen, "simple" won't give details
%union
{
    struct Choice choice;
}
%token ADD SUB MUL DIV MOD
%token GREATER SMALLER EQUAL
%token AND OR NOT
%token PRINT_NUM PRINT_BOOL
%token IF DEFINE FUN
%token <choice> ID NUM BOOL

%type <choice> stmt stmts print_stmt
%type <choice> exp exps
%type <choice> num_op logical_op
%type <choice> def_stmt ids 
%type <choice> fun_exp variable fun_call fun_ids fun_body fun_name parameters
%type <choice> if_exp test_exp then_exp else_exp
%%
program     :   stmts       {}

stmts       :   stmt stmts  {}
            |   stmt        {}

stmt        :   exp         {}
            |   def_stmt    {}
            |   print_stmt  {}
            ;

print_stmt  :   '(' PRINT_NUM exp ')'
                {
                    if($3.type == 1){
                        struct Define tmp = getDefVar($3.name);
                        if(tmp.type == 2){
                            printf("%d\n", tmp.intVal);
                        }
                        else{
                            printf("Type error!\n");
                            return(0);
                        }
                    }
                    else if($3.type == 2){
                        printf("%d\n", $3.intVal);
                    }
                    else if($3.type == 3){
                        printf("Type error!\n");
                        return(0);
                    }
                }
            |   '(' PRINT_BOOL exp ')'
                {
                    if($3.type == 1){
                        struct Define tmp = getDefVar($3.name);
                        if(tmp.type == 3){
                            printf("%s\n", tmp.boolVal ? "#t" : "#f");
                        }
                        else{
                            printf("Type error!\n");
                            return(0);
                        }
                    }
                    else if($3.type == 2){
                        printf("Type error!\n");
                        return(0);
                    }
                    else if($3.type == 3){
                        printf("%s\n", $3.boolVal ? "#t" : "#f");
                    }
                }

exps        :   exp exps    {
                                $$ = $1;
                                if($1.type == 1){
                                    $$.add = $1.intVal + $2.add;
                                    $$.mul = $1.intVal * $2.mul;

                                    $$.equ = false;
                                    if($1.intVal == $2.intVal){
                                        $$.equ = true;
                                    }
                                    $$.equ = $$.equ && $2.equ;
                                    $$.and = $1.boolVal && $2.and;
                                    $$.or  = $1.boolVal || $2.or;
                                }
                                else if($$.type == 2){
                                    if($1.type != 2 || $2.type != 2){
                                        printf("Type error!\n");
                                        return (0);
                                    }
                                    $$.add = $1.intVal + $2.add;
                                    $$.mul = $1.intVal * $2.mul;
                                }
                                else if($1.type == 3){
                                    $$.equ = false;
                                    if($1.intVal == $2.intVal){
                                        $$.equ = true;
                                    }
                                    $$.equ = $$.equ && $2.equ;
                                    $$.and = $1.boolVal && $2.and;
                                    $$.or  = $1.boolVal || $2.or;
                                }
                            }
            |   exp         {
                                $$ = $1;
                                if($$.type == 1){
                                    printf("No match!\n");
                                    return(0);
                                }
                                else if($$.type == 2){
                                    if($1.type != 2){
                                        printf("Type error!\n");
                                        return (0);
                                    }
                                    $$.equ = true;
                                    $$.add = $1.intVal;
                                    $$.mul = $1.intVal;
                                }
                                else if($$.type == 3){
                                    $$.and = $1.boolVal;
                                    $$.or  = $1.boolVal;
                                    $$.add = $1.intVal;
                                    $$.mul = $1.intVal;
                                }
                            }

exp         :   BOOL        {
                                $$.type = 3;
                                $$.boolVal = $1.boolVal;
                            }
            |   NUM         {
                                $$.type = 2;
                                $$.intVal = $1.intVal;
                            }
            |   variable    {
                                struct Define tmp = getDefVar($1.name);
                                if(tmp.type == 1){
                                    printf("No match!\n");
                                    return(0);
                                }
                                else if(tmp.type == 2){
                                    $$.type = tmp.type;
                                    $$.boolVal = tmp.boolVal;
                                    $$.intVal = tmp.intVal;
                                    $$.equ = true;
                                    $$.add = tmp.intVal;
                                    $$.mul = tmp.intVal;
                                }
                                else if (tmp.type == 3){
                                    $$.type = tmp.type;
                                    $$.boolVal = tmp.boolVal;
                                    $$.intVal = tmp.intVal;
                                    $$.and = tmp.boolVal;
                                    $$.or = tmp.boolVal;
                                    $$.add = tmp.intVal;
                                    $$.mul = tmp.intVal;
                                }
            }
            |   num_op      {$$ = $1;}
            |   logical_op  {$$ = $1;}
            |   fun_exp     {$$ = $1;} //fail
            |   fun_call    {$$ = $1;} //fail
            |   if_exp      {$$ = $1;}
            ;

num_op      :   '(' ADD exp exps ')'
                {
                    $$.type = 2;
                    $$.intVal = $3.intVal + $4.add;
                }
            |   '(' SUB exp exp ')'
                {
                    $$.type = 2;
                    $$.intVal = $3.intVal - $4.intVal;
                }
            |   '(' MUL exp exps ')'
                {
                    $$.type = 2;
                    $$.intVal = $3.intVal * $4.mul;
                }
            |   '(' DIV exp exp ')'
                {
                    $$.type = 2;
                    $$.intVal = $3.intVal / $4.intVal;
                }
            |   '(' MOD exp exp ')'
                {
                    $$.type = 2;
                    $$.intVal = $3.intVal % $4.intVal;
                }
            |   '(' GREATER exp exp ')'
                {
                    $$.type = 3;
                    $$.boolVal = $3.intVal > $4.intVal ? true : false;
                }
            |   '(' SMALLER exp exp ')'
                {
                    $$.type = 3;
                    $$.boolVal = $3.intVal < $4.intVal ? true : false;
                }
            |   '(' EQUAL exp exps ')'
                {
                    $$.type = 3;
                    $$.boolVal = false;
                    if($3.intVal == $4.intVal){
                        $$.boolVal = true;
                    }
                    $$.boolVal = $$.boolVal && $4.equ;
                }

logical_op  :   '(' AND exp exps ')'
                {
                    $$.type = 3;
                    $$.boolVal = $3.boolVal && $4.and;
                }
            |   '(' OR exp exps ')'
                {
                    $$.type = 3;
                    $$.boolVal = $3.boolVal || $4.or;
                }
            |   '(' NOT exp ')'
                {
                    $$.type = 3;
                    $$.boolVal = !($3.boolVal);
                }

def_stmt    :   '(' DEFINE ID exp ')' {
                    defVarTop++;
                    defVar[defVarTop].type = $4.type;
                    defVar[defVarTop].name = $3.name;
                    defVar[defVarTop].boolVal = $4.boolVal;
                    defVar[defVarTop].intVal = $4.intVal;
                }
            ;

variable    :   ID
                {
                    $$ = $1;
                }
// function fail
fun_exp     :   '(' FUN fun_ids fun_body ')'
                {}

ids         :   ID ids  {
                            $$ = $1;
                        }
            |           {}

fun_ids     :   '(' ids ')'     {$$ = $2;}

fun_body    :   def_stmt fun_body   {}
            |   exp     {}

fun_call    :   '(' fun_exp parameters ')'  {}
            |   '(' fun_name parameters ')' {}

parameters  :   exp parameters  {}
            |                   {}

fun_name    :   ID  {
                        $$.type = 1;
                        $$.name = $1.name;
                    }
// function fail
if_exp      :   '(' IF test_exp then_exp else_exp ')'
                {
                    if($3.type == 3){
                        if($3.boolVal == true){
                            $$ = $4;
                        }
                        else{
                            $$ = $5;
                        }
                    }
                    else{
                        printf("Type error!\n");
                        return(0);
                    }
                }

test_exp    :   exp     {$$ = $1;}

then_exp    :   exp     {$$ = $1;}

else_exp    :   exp     {$$ = $1;}

%%

struct Define getDefVar(char* varName) {
    int i;
    for(i=0; i<=defVarTop; i++){
        if(strcmp(defVar[i].name, varName) == 0){
            return defVar[i];
        }
    }
}

void yyerror (const char *message) {
	printf("%s\n", message);
    return (0);
}

int main() {
    defVarTop = -1;
    //defFunTop = -1;
    yyparse();
    return 0;
}