%code requires
{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include <unistd.h>

    #define bool int
    #define true 1
    #define false 0
    #define DEBUG 1
    void yyerror(const char *message);

    struct ASTNode{
        char* type;
        char* name;
        int intVal;
        bool boolVal;

        struct ASTNode* left;
        struct ASTNode* right;
    };

    struct ASTNode* newNode(char* type, char* varName, int intVar, bool boolVar, struct ASTNode* left, struct ASTNode* right);
    struct ASTNode* emptyNode();

    void traverse(struct ASTNode* node, char* type, bool inFun);

    struct ASTNode* root;

    struct ASTNode* defVar[999];
    int defVarTop;

    void addDefVar(struct ASTNode* node);
    struct ASTNode* getDefVar(char* varName);
    
    struct Fun{
        char* funName;
        struct ASTNode* params;
        struct ASTNode* task;
    };

    struct Fun* defFun[999];
    int defFunTop;

    struct ASTNode* copyAST(struct ASTNode* node);
    void addFun(char* funName, struct ASTNode* funAdd);
    struct Fun* getFun(char* funName);
    void assign(struct ASTNode* paraName, struct ASTNode* paraAssign, struct ASTNode* funTask);
    void bind(struct ASTNode* taskNode, struct ASTNode* Replace);
    char* getType(char* type);
    void typeChecking(struct ASTNode* node, char* type);
    struct ASTNode* freeNode(struct ASTNode* node);

}
%define parse.error verbose  //give more details when syntax error happen, "simple" won't give details

%union
{
    struct ASTNode* AST;
}
%token ADD SUB MUL DIV MOD
%token GREATER SMALLER EQUAL
%token AND OR NOT
%token PRINT_NUM PRINT_BOOL
%token IF DEFINE FUN
%token <AST> ID NUM BOOL

%type <AST> stmt stmts print_stmt 
%type <AST> exp exps
%type <AST> num_op logical_op
%type <AST> def_stmt ids 
%type <AST> fun_exp variable fun_call fun_ids fun_body fun_name parameters
%type <AST> if_exp test_exp then_exp else_exp
%%
program     :   stmts       {root = $1;}

stmts       :   stmt stmts  {$$ = newNode("no_type", NULL, 0, false, $1, $2);}
            |   stmt        {$$ = $1;}
            
stmt        :   exp         {$$ = $1;}
            |   def_stmt    {$$ = $1;}
            |   print_stmt  {$$ = $1;}
            ;

print_stmt  :   '(' PRINT_NUM exp ')'
                {$$ = newNode("print_num", NULL, 0, false, $3, NULL);}
            |   '(' PRINT_BOOL exp ')'
                {$$ = newNode("print_bool", NULL, 0, false, $3, NULL);}

exps        :   exp exps    {$$ = newNode("equal_to_parent", NULL, 0, false, $1, $2);}
            |   exp         {$$ = $1;}

exp         :   BOOL        {$$ = $1;}
            |   NUM         {$$ = $1;}
            |   variable    {$$ = newNode("get_variable", $1->name, 0, false, NULL, NULL);}
            |   num_op      {$$ = $1;}
            |   logical_op  {$$ = $1;}
            |   fun_exp     {$$ = $1;}
            |   fun_call    {$$ = $1;}
            |   if_exp      {$$ = $1;}
            ;

num_op      :   '(' ADD exp exps ')'
                {$$ = newNode("add", NULL, 0, false, $3, $4);}
            |   '(' SUB exp exp ')'
                {$$ = newNode("sub", NULL, 0, false, $3, $4);}
            |   '(' MUL exp exps ')'
                {$$ = newNode("mul", NULL, 0, false, $3, $4);}
            |   '(' DIV exp exp ')'
                {$$ = newNode("div", NULL, 0, false, $3, $4);}
            |   '(' MOD exp exp ')'
                {$$ = newNode("mod", NULL, 0, false, $3, $4);}
            |   '(' GREATER exp exp ')'
                {$$ = newNode("greater", NULL, 0, false, $3, $4);}
            |   '(' SMALLER exp exp ')'
                {$$ = newNode("smaller", NULL, 0, false, $3, $4);}
            |   '(' EQUAL exp exps ')'
                {$$ = newNode("equal", NULL, 0, false, $3, $4);}

logical_op  :   '(' AND exp exps ')'
                {$$ = newNode("and", NULL, 0, false, $3, $4);}
            |   '(' OR exp exps ')'
                {$$ = newNode("or", NULL, 0, false, $3, $4);}
            |   '(' NOT exp ')'
                {$$ = newNode("not", NULL, 0, false, $3, NULL);}

def_stmt    :   '(' DEFINE ID exp ')' {
                    if(strcmp($4->type, "function") == 0){
                        $$ = newNode("define_function", NULL, 0, false, $3, $4);
                    }
                    else{
                        $$ = newNode("define_variable", NULL, 0, false, $3, $4);
                    }
                }
            ;

variable    :   ID
                {$$ = $1;}

fun_exp     :   '(' FUN fun_ids fun_body ')'
                {$$ = newNode("function", NULL, 0, false, $3, $4);}

ids         :   ID ids
                {$$ = newNode("function_parameters", NULL, 0, false, $1, $2);}
            |   
                {$$ = emptyNode();}

fun_ids     :   '(' ids ')'     {$$ = $2;}

fun_body    :   def_stmt fun_body
                {$$ = newNode("define_inside_function", NULL, 0, false, $1, $2);}
            |   exp     {$$ = $1;}

fun_call    :   '(' fun_exp parameters ')'
                {$$ = newNode("call_function", NULL, 0, false, $2, $3);}
            |   '(' fun_name parameters ')'
                {$$ = newNode("call_function", NULL, 0, false, $2, $3);}

parameters  :   exp parameters
                {$$ = newNode("function_parameters", NULL, 0, false, $1, $2);}
            |           
                {$$ = emptyNode();}

fun_name    :   ID  {$$ = $1;}

if_exp      :   '(' IF test_exp then_exp else_exp ')'
                {
                    struct ASTNode* ifState = newNode("if_stmts", NULL, 0, false, $4, $5);
                    $$ = newNode("if_else", NULL, 0, false, $3, ifState);
                }

test_exp    :   exp     {$$ = $1;}

then_exp    :   exp     {$$ = $1;}

else_exp    :   exp     {$$ = $1;}
%%
struct ASTNode* newNode(char* type, char* varName, int intVar, bool boolVar, struct ASTNode* left, struct ASTNode* right) {
    struct ASTNode* temp = (struct ASTNode*) malloc(sizeof(struct ASTNode));
    temp->type = type;
    temp->name = varName;
    temp->intVal = intVar;
    temp->boolVal = boolVar;

    temp->left = left;
    temp->right = right;
    //for debugging
    printf("type: %s, int: %d, bool %d\n", temp->type, temp->intVal, temp->boolVal);
    if(left != NULL){
        printf("middle: %s\n", temp->type);
        printf("left: %s\n", left->type);
    }
    if(right != NULL){
        printf("right: %s\n", right->type);
    }
    return temp;
}

struct ASTNode* emptyNode() {
    return newNode("no_type", NULL, 0, false, NULL, NULL);
}

void addDefVar(struct ASTNode* node) {
    defVar[++defVarTop] = node;
}

struct ASTNode* getDefVar(char* varName) {
    int i;
    for(i=0; i<=defVarTop; i++){
        if(strcmp(defVar[i]->name, varName) == 0){
            return copyAST(defVar[i]);
        }
    }
}

struct ASTNode* copyAST(struct ASTNode* node) {
    if(node == NULL) {
        return NULL;
    }
    
    struct ASTNode* copy = emptyNode();
    copy->type = node->type;
    copy->name = node->name;
    copy->intVal = node->intVal;
    copy->boolVal = node->boolVal;
    copy->left = copyAST(node->left);
    copy->right = copyAST(node->right);
    //for debugging
    printf("copy!\n");
    if(copy->left != NULL){
        printf("middle: %s\n", copy->type);
        printf("left: %s\n", copy->left->type);
    }
    if(copy->right != NULL){
        printf("right: %s\n", copy->right->type);
    }
    return copy; 
}

void addFun(char* funName, struct ASTNode* funAdd) {
    struct Fun* Add = (struct Fun*) malloc(sizeof(struct Fun));

    Add->funName = funName;
    Add->params = funAdd->left;
    Add->task = funAdd->right;

    defFun[++defFunTop] = Add;
}

struct Fun* getFun(char* funName) {
    int i;
    for(i=0; i<=defFunTop; i++){
        if(strcmp(defFun[i]->funName, funName) == 0){
            struct Fun* result = (struct Fun*) malloc(sizeof(struct Fun));
            result->funName = strdup(defFun[i]->funName);
            result->params = copyAST(defFun[i]->params);
            result->task = copyAST(defFun[i]->task);
            return result;
        }
    }
    return NULL;
}

void assign(struct ASTNode* paraName, struct ASTNode* paraAssign, struct ASTNode* funTask) {
    if(strcmp(paraName->type, "no_type") == 0){
        return;
    }
    else if(strcmp(paraName->type, "string") == 0){
        paraAssign->name = paraName->name;
        if(DEBUG){
            printf("to assign: %d (%s)\n", paraAssign->intVal, paraAssign->type);
        }
        bind(funTask, copyAST(paraAssign));
    }
    else if(strcmp(paraName->type, "function_parameters") == 0){
        paraAssign->left->name = paraName->left->name;
        if(DEBUG) {
            printf("to assign: %d (%s)\n", paraAssign->left->intVal, paraAssign->left->type);
        }
        bind(funTask, copyAST(paraAssign->left));
        assign(paraName->right, paraAssign->right, funTask);
    }
}

void bind(struct ASTNode* taskNode, struct ASTNode* Replace) {
    if(taskNode == NULL || strcmp(taskNode->type, "define_function") == 0) {
        return;
    }
    if(strcmp(taskNode->type, "string") == 0 && strcmp(Replace->type, "function") == 0){
        if(DEBUG) {
            printf("bind: %s -> ", taskNode->type);
        }
        taskNode->type = Replace->type;
        taskNode->intVal = Replace->intVal;
        taskNode->boolVal = Replace->boolVal;
        taskNode->left = copyAST(Replace->left);
        taskNode->right = copyAST(Replace->right);
        if(DEBUG) {
            printf("%s\n", taskNode->type);
        }
        return;
    }
    else if(strcmp(taskNode->type, "get_variable") == 0) {
        if(strcmp(taskNode->name, Replace->name) == 0){
            if(DEBUG) {
                printf("bind: %s -> ", taskNode->type);
            }
            taskNode->type = Replace->type;
            taskNode->intVal = Replace->intVal;
            taskNode->boolVal = Replace->boolVal;
            taskNode->left = copyAST(Replace->left);
            taskNode->right = copyAST(Replace->right);
            if(DEBUG) {
                printf("%s\n", taskNode->type);
            }
            return;
        }
    }
    bind(taskNode->left, Replace);
    bind(taskNode->right, Replace);
}

char* getType(char* type) {
    if(strcmp(type, "integer") == 0) {
        return strdup("number");
    }
    else if(strcmp(type, "boolean") == 0) {
        return strdup("boolean");
    }
    else if(strcmp(type, "function") == 0) {
        return strdup("function");
    }
}

void typeChecking(struct ASTNode* node, char* type) {
    if(strcmp(node->type, type) != 0){
        char errMsg[999];
        // 將結構字串化
        sprintf(errMsg, "Type Error: Expecting '%s', but got '%s'.", getType(type), getType(node->type));
        yyerror(errMsg);
    }
}

struct ASTNode* freeNode(struct ASTNode* node) {
    if(node == NULL){
        return NULL;
    }
    freeNode(node->left);
    freeNode(node->right);
    free(node);
    return NULL;
}

void traverse(struct ASTNode* node, char* type, bool inFun) {
    printf("traverse: %s\n", node->type);
    if(node == NULL){
        return;
    }
    if(strcmp(node->type, "no_type") == 0){
        traverse(node->left, node->left->type, inFun);
        traverse(node->right, node->right->type, inFun);
        return;
    }
    else if(strcmp(node->type, "equal_to_parent") == 0){
        node->type = type;
        traverse(node, node->type, inFun);
        if(DEBUG) {
            printf("Inherit parents type.\n");
        }
        return;
    }
    else if(strcmp(node->type, "print_num") == 0){
        traverse(node->left, node->left->type, inFun);
        typeChecking(node->left, "integer");
        if(DEBUG) {
            printf("print_num: %d\n", node->left->intVal);
        }
        printf("%d\n", node->left->intVal);
        return;
    }
    else if(strcmp(node->type, "print_bool") == 0){
        traverse(node->left, node->left->type, inFun);
        typeChecking(node->left, "boolean");
        if(DEBUG) {
            printf("print_bool: %s\n", node->left->boolVal ? "#t" : "#f");
        }
        printf("%s\n", node->left->boolVal ? "#t" : "#f");
        return;
    }
    else if(strcmp(node->type, "add") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);

        typeChecking(node->left, "integer");
        typeChecking(node->right, "integer");

        node->type = "integer";
        node->intVal = node->left->intVal + node->right->intVal;
        if(DEBUG) {
            printf("%d = %d + %d\n", node->intVal, node->left->intVal, node->right->intVal);
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "sub") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);

        typeChecking(node->left, "integer");
        typeChecking(node->right, "integer");

        node->type = "integer";
        node->intVal = node->left->intVal - node->right->intVal;
        if(DEBUG) {
            printf("%d = %d - %d\n", node->intVal, node->left->intVal, node->right->intVal);
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "mul") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);

        typeChecking(node->left, "integer");
        typeChecking(node->right, "integer");

        node->type = "integer";
        node->intVal = node->left->intVal * node->right->intVal;
        if(DEBUG) {
            printf("%d = %d * %d\n", node->intVal, node->left->intVal, node->right->intVal);
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "div") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);

        typeChecking(node->left, "integer");
        typeChecking(node->right, "integer");

        node->type = "integer";
        node->intVal = node->left->intVal / node->right->intVal;
        if(DEBUG) {
            printf("%d = %d / %d\n", node->intVal, node->left->intVal, node->right->intVal);
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "mod") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);

        typeChecking(node->left, "integer");
        typeChecking(node->right, "integer");

        node->type = "integer";
        node->intVal = node->left->intVal % node->right->intVal;
        if(DEBUG) {
            printf("%d = %d %% %d\n", node->intVal, node->left->intVal, node->right->intVal);
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "greater") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);

        typeChecking(node->left, "integer");
        typeChecking(node->right, "integer");

        node->type = "boolean";
        node->boolVal = node->left->intVal > node->right->intVal ? true : false;
        if(DEBUG) {
            printf("%s = %d > %d\n", node->boolVal ? "#t" : "#f", node->left->intVal, node->right->intVal);
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "smaller") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);

        typeChecking(node->left, "integer");
        typeChecking(node->right, "integer");

        node->type = "boolean";
        node->boolVal = node->left->intVal < node->right->intVal ? true : false;
        if(DEBUG) {
            printf("%s = %d < %d\n", node->boolVal ? "#t" : "#f", node->left->intVal, node->right->intVal);
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "equal") == 0){
        if(strcmp(node->right->type, "equal") != 0 || strcmp(node->right->type, "equal_to_parent") != 0){
            traverse(node->right, node->type, inFun);
            typeChecking(node->right, "integer");
            node->right->boolVal = 1;
        }

        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);
        typeChecking(node->left, "integer");
        if(strcmp(node->right->type, "boolean") != 0) {
            typeChecking(node->right, "integer");
        }
        node->type = "boolean";
        node->intVal = node->left->intVal;
        node->boolVal = (node->left->intVal == node->right->intVal)*node->right->boolVal ? true : false; //think
        if(DEBUG) {
            printf("%s = (%d == %d)*(%s)\n", node->boolVal ? "#t" : "#f", node->left->intVal, node->right->intVal, node->right->boolVal ? "#t" : "#f");
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "and") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);
        typeChecking(node->left, "boolean");
        typeChecking(node->right, "boolean");
        node->type = "boolean";
        node->boolVal = node->left->boolVal && node->right->boolVal;
        if(DEBUG) {
            printf("%s = %s && %s\n", node->boolVal ? "#t" : "#f", node->left->boolVal ? "#t" : "#f", node->right->boolVal ? "#t" : "#f");
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "or") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);
        typeChecking(node->left, "boolean");
        typeChecking(node->right, "boolean");
        node->type = "boolean";
        node->boolVal = node->left->boolVal || node->right->boolVal;
        if(DEBUG) {
            printf("%s = %s || %s\n", node->boolVal ? "#t" : "#f", node->left->boolVal ? "#t" : "#f", node->right->boolVal ? "#t" : "#f");
        }
        node->left = freeNode(node->left);
        node->right = freeNode(node->right);
        return;
    }
    else if(strcmp(node->type, "not") == 0){
        traverse(node->left, node->type, inFun);
        typeChecking(node->left, "boolean");
        node->type = "boolean";
        node->boolVal = !node->left->boolVal;
        if(DEBUG) {
            printf("%s = !%s\n", node->boolVal ? "#t" : "#f", node->left->boolVal ? "#t" : "#f");
        }
        node->left = freeNode(node->left);
        return;
    }
    else if(strcmp(node->type, "define_variable") == 0){
        node->right->name = node->left->name;
        addDefVar(node->right);
        return;
    }
    else if(strcmp(node->type, "get_variable") == 0){
        if(!inFun){
            struct ASTNode* Result = getDefVar(node->name);
            node->type = Result->type;
            node->intVal = Result->intVal;
            node->boolVal = Result->boolVal;
            node->left = copyAST(Result->left);
            node->right = copyAST(Result->right);
            traverse(node, node->type, inFun);
            if("DEBUG") {
                printf("Get variable: %s\n", node->name);
            }
        }
        return;
    }
    else if(strcmp(node->type, "define_function") == 0){
        addFun(node->left->name, node->right);
        return;
    }
    else if(strcmp(node->type, "define_inside_function") == 0){
        traverse(node->left, node->type, inFun);
        traverse(node->right, node->type, inFun);
        node->type = node->right->type;
        node->intVal = node->right->intVal;
        node->boolVal = node->right->boolVal;
        return;
    }
    else if(strcmp(node->type, "call_function") == 0){
        if(strcmp(node->left->type, "function") == 0) {
            assign(node->left->left, node->right, node->left->right);
            traverse(node->left->right, node->left->type, true);
            node->type = node->left->right->type;
            node->intVal = node->left->right->intVal;
            node->boolVal = node->left->right->boolVal;

            struct ASTNode* temp = copyAST(node->left);
            node->left = copyAST(node->left->right->left);
            node->right = copyAST(temp->right->right);
        }
        else if(strcmp(node->left->type, "string") == 0){
            struct Fun* funCall = getFun(node->left->name);
            if(funCall != NULL){
                assign(funCall->params, node->right, funCall->task);
                traverse(funCall->task, funCall->task->type, true);
                node->type = funCall->task->type;
                node->intVal = funCall->task->intVal;
                node->boolVal = funCall->task->boolVal;
                node->left = copyAST(funCall->task->left);
                node->right = copyAST(funCall->task->right);
            }
            else {
                struct ASTNode* Result = getDefVar(node->left->name);
                node->left->type = Result->type;
                node->left->intVal = Result->intVal;
                node->left->boolVal = Result->boolVal;
                node->left->left = copyAST(Result->left);
                node->left->right = copyAST(Result->right);
                traverse(node->left, node->left->type, true);
                traverse(node, node->type, true);
            }
        }
        return;
    }
    else if(strcmp(node->type, "if_else") == 0){
        traverse(node->left, node->type, inFun);
        if(node->left->boolVal){
            traverse(node->right->left, node->right->left->type, inFun);
            node->type = node->right->left->type;
            node->intVal = node->right->left->intVal;
            node->boolVal = node->right->left->boolVal;
        }
        else {
            traverse(node->right->right, node->right->right->type, inFun);
            node->type = node->right->right->type;
            node->intVal = node->right->right->intVal;
            node->boolVal = node->right->right->boolVal;
        }
        return;
    }
}

void yyerror (const char *message) {
	printf("%s\n", message);
    exit(0);
}

int main() {
    defFunTop = -1;
    defVarTop = -1;
    root = emptyNode();

    yyparse();
    traverse(root, root->type, false);

    return 0;
}