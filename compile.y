/* compile.y */

%{
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include "structs.h"
# include "proto.h"
extern int yychar, yydebug, yylineno;
extern char *yytext;
int errcode, scope;
int yylex();
void yyerror(char *s);


%}

%union{
   char *charptr;
   char ch;
   int intcon;
   int type;
   struct symTable *node;
   struct ParmList *parms;

   struct GlobNode *glob;
   struct StmtNode *stmt;
   struct ExprNode *expr;
}

/* tokens */

%token <charptr> INT ID CHARCON STRINGCON CHAR
%token <ch>      UNKNOWN
%token <ch>      '+' '-' '*' '/' '!'
%token <intcon>  INTCON
%token <charptr> IF ELSE WHILE FOR RETURN
%token <charptr> EXTERN
%token <type> VOID
%token <charptr> LT LE GE GT AND OR EQ NE

%type <type> type
%type <node> var_decl parm_decl loc_decl 
%type <glob> prog dcl decllist func loc_decllist varlist parm_types parmlist
%type <stmt> stmt stmtlist assg
%type <expr> expr exprlist
/* associativity */
%left error
%left OR
%left AND
%left EQ NE
%left LT LE GE GT
%left '+' '-'
%left '*' '/'
%right UMINUS '!'

/* expect 1 shift/reduce err for the dangling else */
%expect 1
%%

root : prog    {root = $1;}

prog  : dcl {tmpNum = 1; currOffset = 8;} prog  {$$ = addDclNode($1,$3);} 
      | func {tmpNum = 1;currOffset = 8;} prog {$$ = addGlobFuncNode($1,$3);}
      | /* epsilon */  {$$ = NULL;}
      ;

dcl      :  type decllist ';'    {$$ = $2;}
         |  error decllist ';'   {$$ = NULL;}
         |  EXTERN type ID addStack '(' parmlist ')' clearlocals 
            {addToTable($2, 0, $3, FUNCPROTO, 0, 0, $6); setExtern($3);}
            idlist ';'
               {$$ = newFuncProtoNode($3);}
         |  type ID addStack '(' parmlist ')' clearlocals
            {addToTable($1, 0, $2, FUNCPROTO, 0, 0, $5);}
                idlist ';'
            {$$ = newFuncProtoNode($2);}
         |  error ID '(' parmlist ')' idlist ';' {$$ = NULL;}
         |  EXTERN VOID ID addStack '(' parmlist ')' clearlocals
            {addToTable(VD, 0, $3, FUNCPROTO, 0, 0, $6); setExtern($3);}
            idlist ';'
              {$$ = newFuncProtoNode($3);}
         |  VOID ID addStack '(' parmlist ')' clearlocals
            {addToTable(VD, 0, $2, FUNCPROTO, 0, 0, $5);}
            idlist ';'
               {$$ = newFuncProtoNode($2);}
         ;
idlist   : idlist ',' ID addStack '(' parmlist ')'  clearlocals
            {addToTable(currType, 0, $3, FUNCPROTO, 0, 0, $6);}
         | idlist ',' ID addStack '(' parmlist error
         | /* epsilon */
         ;
var_decl : ID                  {$$ = addToTable(currType, 0, $1, VAR, scope, 0, NULL);}
         | ID '[' INTCON ']'   {$$ = addToTable(currType, 1, $1, VAR, scope, $3,NULL);}
         | ID '[' INTCON error {yyclearin; $$ = NULL;}
         | ID error INTCON ']' {yyclearin; $$ = NULL;}
         ;
decllist : var_decl ',' decllist {$$ = newDclNode($1, $3);}
         | var_decl  {$$ = newDclNode($1, NULL);}
         | error     {$$ = NULL;}
         ;
type     :  CHAR     {$$ = CHARACTER; currType = $$;}
         |  INT      {$$ = INTEGER;   currType = $$;}
         ;
parm_decl   : type ID '[' ']' {$$ = addToTable($1, 1, $2, VAR, 1, 0,NULL);}
            | type ID         {$$ = addToTable($1, 0, $2, VAR, 1, 0,NULL);}
            ;
parm_types: parm_decl    {$$ = newDclNode($1,NULL);}
          | parm_decl ',' parm_types {$$ = newDclNode($1,$3);}
          ;
parmlist: parm_types {$$ = $1;}
        | VOID       {$$ = NULL;}
        ;
func  :  type ID addStack '(' parmlist ')' 
         {checkExtern($2); addToTable($1, 0, $2, FUNC, 0, 0, $5); currOffset = -4;}
         '{' incscope varlist stmtlist '}'
         {
          $$ = newFuncNode($2, $5, $10, $11); checkHasReturn($2);
          clearLocals(); scope = 0; tmpNum = 1; currOffset = 8;}
      |  VOID ID addStack '(' parmlist ')'
         {checkExtern($2); addToTable(VD, 0, $2, FUNC, 0, 0, $5); currOffset = -4;}
         '{' incscope varlist stmtlist '}'
         {
          $$ = newFuncNode($2, $5, $10, $11); checkHasReturn($2);
            clearLocals(); scope = 0; tmpNum = 1; currOffset = 8;}
      |  error ID error parmlist ')' '{' varlist error '}' {$$ = NULL;}
      ;

addStack : /* epsilon */ {addStack();}
         ;
clearlocals: /* epsilon */ {clearLocals();}
           ;

incscope: /* epsilon */ {scope = 1;}
        ;

varlist : type loc_decllist ';' varlist {$$ = addDclNode($2,$4);}
        | /* epsilon */ {$$ = NULL;}
        ;
loc_decl : ID                  {$$ = addToTable(currType, 0, $1, VAR, scope, 0,NULL);}
         | ID '[' INTCON ']'   {$$ = addToTable(currType, 1, $1, VAR, scope, $3,NULL);}
         | ID '[' INTCON error {yyclearin; $$ = NULL;}
         | ID error INTCON ']' {yyclearin; $$ = NULL;}
         ;
loc_decllist : loc_decl ',' loc_decllist {$$ = newDclNode($1,$3);}
         | loc_decl  {$$ = newDclNode($1,NULL);}
         | error     {$$ = NULL;}
         ;
stmt:  IF '(' expr ')' stmt               {$$=newIfNode(IFSTMT,$3,$5,NULL);} 
    |  IF '(' expr ')' stmt ELSE stmt     {$$=newIfNode(IFSTMT,$3,$5,$7);}
    |  WHILE '(' expr ')' stmt      {$$=newWhileNode(WHILESTMT, $3, $5);}
    |  FOR '(' assg ';' expr ';' assg ')' stmt 
      {$$=newForNode(FORSTMT,$3,$5,$7,$9);}
    |  FOR '(' assg ';' expr ';'      ')' stmt 
      {$$=newForNode(FORSTMT, $3,$5,NULL,$8);}
    |  FOR '(' assg ';'      ';' assg ')' stmt 
      {$$=newForNode(FORSTMT, $3,NULL,$6,$8);}
    |  FOR '(' assg ';'      ';'      ')' stmt 
      {$$=newForNode(FORSTMT,$3,NULL,NULL,$7);}
    |  FOR '('      ';' expr ';' assg ')' stmt 
      {$$=newForNode(FORSTMT,NULL,$4,$6,$8);}
    |  FOR '('      ';'      ';' assg ')' stmt 
      {$$=newForNode(FORSTMT,NULL,NULL,$5,$7);}
    |  FOR '('      ';' expr ';'      ')' stmt 
      {$$=newForNode(FORSTMT,NULL,$4,NULL,$7);}
    |  FOR '('      ';'      ';'      ')' stmt 
      {$$=newForNode(FORSTMT,NULL,NULL,NULL,$6);}
    |  RETURN ';'    {$$ = newReturnNode(NULL); checkReturn(VD);}
    |  RETURN expr ';'  {$$ = newReturnNode($2); checkReturn($2->valType);}
    |  assg ';'      {$$ = $1;}
    |  ID '(' exprlist ')' ';'
         {$$ = stmtFuncCall($1, $3); checkFunc($1, $3, 1);}
    |  ID '(' ')' ';'     {$$ = stmtFuncCall($1, NULL); checkFunc($1, NULL, 1);}
    |  '{' stmtlist '}'    {$$ = $2;}
    |  ';'                 {$$ = newEmptyNode(EMPTYSTMT);}
    |  ID error ')' ';'      {$$ = NULL;}
    |  error      {$$ = NULL;}
    ;
stmtlist : stmt stmtlist {$$ = addStmtNode($1, $2);}
         | /* epsilon */ {$$ = NULL;}
         ;
assg  :  ID '[' expr ']'    '=' expr    {$$ = newAssgNode($1,$3,$6); checkElemAssg($1,$3->valType,$6->valType);}
      |  ID error expr ']'  '=' expr  {$$ = NULL;}
      |  ID '[' expr error  '=' expr  {$$ = NULL;}
      |  error '[' expr ']' '=' expr  {$$ = NULL;}
      |  ID '=' expr    {$$ = newAssgNode($1,NULL,$3); checkAssg($1, $3->valType);}
      |  ID error expr  {$$ = NULL;}
      ;
expr  :  '-' expr %prec UMINUS
         {$$ = newUnaNode(UNAMINUS, $2);
          $$->valType = checkUnaExpr($2,$1);}
      |  '!' expr %prec '!'
         {$$ = newUnaNode(UNANOT, $2);
          $$->valType = checkUnaLogExpr($2,$1);}
      |  error expr %prec '!' {$$ = NULL;}
      |  expr '+' expr
         {$$ = newOpNode(PLUS,  BIN, $1, $3);
          $$->valType = checkBinExpr($1,$3,$2);}
      |  expr '-' expr
         {$$ = newOpNode(MINUS, BIN, $1, $3);
          $$->valType = checkBinExpr($1,$3,$2);}
      |  expr '*' expr
         {$$ = newOpNode(MULT,  BIN, $1, $3);
          $$->valType = checkBinExpr($1,$3,$2);}
      |  expr '/' expr
         {$$ = newOpNode(DIV,   BIN, $1, $3);
          $$->valType = checkBinExpr($1,$3,$2);}
      |  expr AND expr
         {$$ = newOpNode(LOGAND,LOG, $1, $3);
          $$->valType = checkLogExpr($1,$3,$2);}
      |  expr OR expr
         {$$ = newOpNode(LOGOR, LOG, $1, $3);
          $$->valType = checkLogExpr($1,$3,$2);}
      |  expr EQ expr
         {$$ = newOpNode(RELEQ, REL, $1, $3);
          $$->valType = checkRelExpr($1,$3,$2);}
      |  expr NE expr
         {$$ = newOpNode(RELNE, REL, $1, $3);
          $$->valType = checkRelExpr($1,$3,$2);}
      |  expr LE expr
         {$$ = newOpNode(RELLE, REL, $1, $3);
          $$->valType = checkRelExpr($1,$3,$2);}
      |  expr LT expr
         {$$ = newOpNode(RELLT, REL, $1, $3);
          $$->valType = checkRelExpr($1,$3,$2);}
      |  expr GE expr
         {$$ = newOpNode(RELGE, REL, $1, $3);
          $$->valType = checkRelExpr($1,$3,$2);}
      |  expr GT expr
         {$$ = newOpNode(RELGT, REL, $1, $3);
          $$->valType = checkRelExpr($1,$3,$2);}
      |  expr error expr   {$$ = NULL;}
      |  ID            {$$ = newExprIdNode($1, NULL); checkIsVar($1);}
      |  ID '[' expr ']'            {$$ = newExprIdNode($1, $3); checkIsVar($1);}
         /*{$$ = checkIdExist($1,$3);}*/
      |  ID '[' expr error   {$$ = NULL;}
      |  ID '(' ')'     {$$ = exprFuncCall($1, NULL); checkFunc($1, NULL, 0);}
      |  ID '(' error   {$$ = NULL;}
      |  ID '(' exprlist ')'
         {$$ = exprFuncCall($1, $3); checkFunc($1, $3, 0);}
      |  '(' expr ')'  {$$ = $2;}
      |  INTCON      {$$ = newIntNode($1,INTEGER);}
      |  CHARCON      {$$ = newCharNode($1, CHARACTER);}
      |  STRINGCON      {$$ = newStringNode($1, STRING);}
      ;

exprlist : expr ',' exprlist  {$$ = addExprNode($1,$3);}
         | expr               {$$ = addExprNode($1,NULL);}
         ;

%%

void checkIsVar(char *id){
   symTabNode *tmp = findId(id);
   if (tmp != NULL && tmp->symType != VAR){
      semerror(27,id);
   }
}

void checkExtern(char *id){
   symTabNode *tmp = findId(id);
   if (tmp == NULL){
      return;
   }
   if(tmp->isExtern == 1){
      semerror(23, id);
   }
}

void setExtern(char *id){
   symTabNode *tmp = findId(id);
   tmp->isExtern = 1;
}

void checkHasReturn(char *id){
   symTabNode *tmp = getTopFunc();
   if (tmp->type == VD && tmp->hasReturn != 0){
      semerror(20,id);
   }
   if (tmp->type != VD && tmp->hasReturn == 0){
      semerror(21,id);
   }
}

void checkReturn(Type type){
   symTabNode *tmp = getTopFunc();
   if (tmp == NULL){
      semerror(-1,NULL);
      return;
   }
   /* If one Type is greater than 1, then it can't be an int or char, which means that it can't be compatible with any other type*/
   if(tmp->type != type && (tmp->type > 1 || type > 1)){
      semerror(19, tmp->id);
      return;
   }
   if(type != VD){
      tmp->hasReturn = 1;
   }
}

symTabNode *getTopFunc(){
   return tables->next->table;
   //symTabStack *stackptr = tables;
   //while (nextPtr != NULL){
   //   if (nextPtr->symType == FUNC && nextPtr->scope == 0)
   //      return nextPtr;
   //   nextPtr = nextPtr->next;
   //}
   //return nextPtr;
}

/* add new stack to symbol table when scope increases */
void addStack(){
   symTabNode *stackBottom = malloc(sizeof(symTabNode));
   stackBottom->type=0;
   stackBottom->defined=0;
   stackBottom->scope=0;
   stackBottom->symType=0;
   stackBottom->id = malloc(sizeof(char) * 2);
   stackBottom->id[0] = '$';
   stackBottom->id[1] = '\0';
   stackBottom->next=NULL;

   symTabStack *stack = malloc(sizeof(symTabStack));
   stack->table = stackBottom;
   stack->next = tables;
   tables = stack;
}

void checkCond(Type type, char *stmt){
   if(type != BOOL){
      semerror(17,stmt);
   }
}

Type checkAssg(char *id, Type type){
   symTabNode *tmp = findId(id);
   if( tmp == NULL){
      semerror(5,id);
      return type;
   }
   if (tmp->type != INTEGER && tmp->type != CHARACTER){
      semerror(25,id);
      return type;
   }
   if( tmp->symType != VAR){
      semerror(14,id);
      return type;
   }
   if(type == CHARACTER || type == INTEGER){
      return type;
   }
   else if (type == BOOL){
      semerror(18,id);
      return type;
   }
   else if(tmp->type != type){
      semerror(15,id);
      return type;
   }
   return type;
}
Type checkElemAssg(char *id, Type indexExpr, Type type){
   symTabNode *tmp = findId(id);
   if(indexExpr != INTEGER && indexExpr != CHARACTER){
      semerror(16,id);
      return type;
   }
   if( tmp == NULL){
      semerror(5,id);
      return type;
   }
   if( tmp->symType != VAR){
      semerror(14,id);
      return type;
   }
   if(tmp->type == INTEGER && type == CHARACTER){
      return type;
   }
   else if(tmp->type == CHARACTER && type == INTEGER){
      return type;
   }
   else if (type == BOOL){
      semerror(18,id);
      return type;
   }
   else if (tmp->type == INTARRAY && type == INTEGER)
      return type;
   else if (tmp->type == STRING && type == CHARACTER)
      return type;
   else if (tmp->type == INTARRAY && type == CHARACTER)
      return type;
   else if (tmp->type == STRING && type == INTEGER)
      return type;
   else if(tmp->type != type){
      semerror(15,id);
      return type;
   }
   return type;
}

Type checkFunc(char *id, exprNode *expr, int isStatement){
   symTabNode *node = findId(id);
   int numParms = countExprParms(expr);
   if (node == NULL){
      semerror(5,id);
      return ERR;
   }
   if (node->symType != FUNC && node->symType != FUNCPROTO){
      semerror(13, id);
      return node->type;
   }
   if (expr == NULL && node->numParms > 0){
      semerror(1,id);
      return node->type;
   }
   if (expr != NULL && node->numParms == 0){
      semerror(1,id);
      return node->type;
   }
   if (node->numParms != numParms){
      semerror(1,id);
      return node->type;
   }
   if (isStatement == 1 && node->type != VD){
      semerror(24,id);
      return node->type;
   }
   if (isStatement == 0 && node->type == VD){
      semerror(26,id);
      return node->type;
   }
   if (expr == NULL && node->numParms == 0){
      return node->type;
   }
   Type *funcParms = node->funcParms;
   exprNode *tmp = expr;
   int i = 0;
   while (tmp != NULL){
      if (funcParms[i] != tmp->valType){
         if(funcParms[i] == INTEGER && tmp->valType != CHARACTER)
            semerror(1, id);
         else if (funcParms[i] == CHARACTER && tmp->valType != INTEGER)
            semerror(1, id);
         else if (funcParms[i] == STRING || funcParms[i] == INTARRAY)
            semerror(1, id);
      }
      tmp = tmp->nextExpr;
      i++;
   }
   return node->type;
}

int countExprParms(exprNode *parms){
   int numParms = 0;
   exprNode *tmp = parms;
   while (tmp != NULL){
      numParms++;
      tmp = tmp->nextExpr;
   }
   return numParms;
}


Type checkUnaLogExpr(exprNode *e1, char op){
   Type t1 = e1->valType;
   if(t1 == BOOL){
      return BOOL;
   }
   else{
      semerror(12, &op);
      return BOOL;
   }
}
Type checkUnaExpr(exprNode *e1, char op){
   Type t1 = e1->valType;
   if(t1 == INTEGER || t1 == CHARACTER){
      return INTEGER;
   }
   else{
      semerror(11, &op);
      return INTEGER;
   }
}
Type checkRelExpr(exprNode *e1, exprNode *e2, char *op){
   Type t1 = e1->valType;
   Type t2 = e2->valType;
   if((t1 == t2) && (t1 == CHARACTER || t1 == INTEGER)){
      return BOOL;
   }
   else if (t1 == CHARACTER && t2 == INTEGER)
      return BOOL;
   else if (t1 == INTEGER && t2 == CHARACTER)
      return BOOL;
   else{
      semerror(10, op);
      return BOOL;
   }
}

Type checkLogExpr(exprNode *e1, exprNode *e2, char *op){
   Type t1 = e1->valType;
   Type t2 = e2->valType;
   if((t1 == t2) && (t1 == BOOL)){
      return BOOL;
   }
   else{
      semerror(9, op);
      return BOOL;
   }
}

Type checkBinExpr(exprNode *e1, exprNode *e2, char op){
   Type t1 = e1->valType;
   Type t2 = e2->valType;
   if(t1 == t2 && (t1 == CHARACTER || t1 == INTEGER)){
      return INTEGER;
   }
   else if (t1 == CHARACTER && t2 == INTEGER)
      return INTEGER;
   else if (t2 == CHARACTER && t1 == INTEGER)
      return INTEGER;
   else{
      semerror(8, &op);
      return INTEGER;
   }
}

symTabNode *checkIdExist(char *id, exprNode *expr){
   symTabNode *tmp = findId(id);
   if (tmp == NULL){
      semerror(5, id);
   }
   else if (expr != NULL && tmp->isArray == 0){
      semerror(7, id);
   }
   else if (expr != NULL && expr->valType != INTEGER && expr->valType != CHARACTER ){
      semerror(16, id);
   }
   else if(tmp->type == INTEGER || tmp->type == CHARACTER)
      return tmp;
   else if(tmp->type == STRING && expr != NULL)
      return tmp;
   else if(tmp->type == INTARRAY && expr != NULL)
      return tmp;
   else
      return tmp;
   return tmp;
}

symTabNode *addToTable(Type type, int isArray, char *id, SymType symType, int scope, int size, globNode *parms){
   symTabNode *tmp = findId(id);
   if (tmp != NULL){
      if (tmp->scope == scope && tmp->symType != FUNCPROTO ){
         semerror(3, tmp->id);
         return tmp;
      }
      else if(tmp->symType == FUNC && symType == FUNC){
         semerror(4, tmp->id);
         return tmp;
      }
      else if(tmp->symType == FUNCPROTO && symType == FUNCPROTO){
         semerror(4, tmp->id);
         return tmp;
      }
      else if(tmp->symType == FUNCPROTO && symType == FUNC){
         if (tmp->type != type){
            semerror(22,id);
         }
         int code = checkParms(tmp, parms);
         if(code != 0)
            return tmp;
      }
   }
   symTabNode *tmpNode;
   tmpNode = malloc(sizeof(symTabNode));
   if(isArray == 0)
      tmpNode->type = type;
   else{
      if(type == CHARACTER)
         tmpNode->type = STRING;
      else if(type == INTEGER)
         tmpNode->type = INTARRAY;
   }
   if (size > 0)
      tmpNode->arraySize = size;
   tmpNode->symType = symType;
   tmpNode->id = strdup(id);
   tmpNode->defined = 0;
   tmpNode->isArray = isArray;
   tmpNode->scope = scope;
   tmpNode->isExtern = 0;
   if(tmpNode->symType != FUNC && tmpNode->symType != FUNCPROTO)
      tmpNode->numParms = 0;
   if (symType == FUNC || symType == FUNCPROTO){
      addParmsToFunc(tmpNode, parms);
   }
   if(symType == FUNC){
      pushFuncDown(tmpNode);
      return tmpNode;
   }
   tmpNode->next = tables->table;
   tables->table = tmpNode;
   //if (scope == 0) {tmpNode->next = globalTable; globalTable = tmpNode;}
   //if (scope == 1) {tmpNode->next =  localTable; localTable  = tmpNode;}
   
   return tmpNode;
}

void pushFuncDown(symTabNode *tmpNode){
   symTabStack *stackPtr = tables;
   /*
      does not allow for nested functions; it should be possible to just stick the node in the next table down without complications, but the rest of the code would have to be aware of this
   */
   while (stackPtr->next != NULL){
      stackPtr = stackPtr->next;
   }
   tmpNode->next = stackPtr->table;
   stackPtr->table = tmpNode;

   //if(table->scope == 0){
   //   tmpNode->next = table;
   //   table = tmpNode;
   //   return;
   //}
   //
   //while (prevPtr->next != NULL && prevPtr->next->scope != 0){
   //   prevPtr = prevPtr->next;
   //}
   //if(prevPtr->next != NULL){
   //   nextPtr = prevPtr->next;
   //   tmpNode->next = nextPtr;
   //   prevPtr->next = tmpNode;
   //}
   //else{
   //   tmpNode->next = prevPtr;
   //   table = tmpNode;
   //}
}

int checkParms(symTabNode *node, globNode *parms){
   int numParms = 0;
   globNode *tmpParm = parms;
   while (tmpParm != NULL){
      numParms++;
      tmpParm = tmpParm->next;
   }
   if(node->numParms != numParms){
      semerror(1, node->id);
      return 1;
   }
   else if (node->numParms == 0 && numParms == 0)
      return 0;
   tmpParm = parms;
   int i = 0;
   while (tmpParm != NULL){
      if (tmpParm->dcl->type != node->funcParms[i]){
         semerror(2, node->id);
         return 1;
      }
      tmpParm = tmpParm->next;
   }
   return 0;
}

symTabNode *addParmsToFunc(symTabNode *tmpNode, globNode *parms){
   int i = 0;
   int numParms = 0;
   globNode *tmp = parms;
   if (parms == NULL){
      tmpNode->numParms = 0;
      tmpNode->funcParms = malloc(sizeof(Type));
      tmpNode->funcParms[0] = VD;
      return tmpNode;
   }
   while (tmp != NULL){
      numParms++;
      tmp = tmp->next;
   }
   tmpNode->funcParms = malloc(sizeof(Type) * numParms);
   tmpNode->numParms = numParms;
   tmp = parms;
   while (tmp != NULL){
      if (tmp->type == DCL){
         tmpNode->funcParms[i] = tmp->dcl->type;
      }
      else if (tmp->type == GLOBFUNC || tmp->type == GLOBFUNCPROTO){
         tmpNode->funcParms[i] = tmp->func->id->type;
      }
      tmp = tmp->next;
      i++;
   }
   return tmpNode;
}

/* local variables should always be at the top of the stack, so just pop nodes until we hit a global variable */
void clearLocals(){
   symTabNode *nextPtr = tables->table;
   while (nextPtr != NULL){
      symTabNode *tmp = nextPtr;
      nextPtr = nextPtr->next;
      tables->table = nextPtr;
      free(tmp->id);
      free(tmp->funcParms);
      free(tmp);
   }
   symTabStack *tmpStack = tables;
   tables = tables->next;
   free(tmpStack);
}

symTabNode *findId(char *id){
   symTabStack *stackPtr = tables;
   //if (scope == 0) {tmpPtr = globalTable;}
   //else if (scope == 1) {tmpPtr = localTable;}
   while (stackPtr != NULL){
      symTabNode *tmpPtr = stackPtr->table;
      while (tmpPtr->next != NULL){
         if(strcmp(tmpPtr->id,id) == 0){
            return tmpPtr;
         }
         tmpPtr = tmpPtr->next;
      }
      stackPtr = stackPtr->next;
   }
   return NULL;
}


static void print_tok(){
   if (yytext[0] == 0){
      fprintf(stderr, "EOF");
   } else if (yychar < 255){
      fprintf(stderr, "\"%c\"", yychar);
   } else {
      switch (yychar){
      case INTCON:
         fprintf(stderr,"\"%d\"", yylval.intcon);
         break;
      case '(': 
      case ')':
      case '{':
      case '}':
      case '[':
      case ']':
      case ';':
      case ',':
      case '=':
      case '+':
      case '-':
      case '*':
      case '/':
      case '!':
         fprintf(stderr,"\"%c\"", yylval.ch);
         break;
      case CHARCON:
      case STRINGCON:
      case CHAR:
      case INT:
      case VOID:
      case EXTERN:
      case IF:
      case ELSE:
      case WHILE:
      case FOR:
      case RETURN:
      case ID:
      case AND:
      case OR:
      case EQ:
      case NE:
      case LE:
      case LT:
      case GE:
      case GT:
         fprintf(stderr,"\"%s\"", yylval.charptr);
         break;
      default:
         fprintf(stderr,"\"%c\"", yylval.ch);
         break;
      }
   }
}


void yyerror(char *s){
   fprintf(stderr, "Line %d: %s: unexpected ", yylineno, s);
   print_tok();
   fprintf(stderr,"\n");
   errcode = 1;
}

int semerror(int code, char *str){
   errcode = 1;
   switch(code){
      case 1:
      fprintf(stderr,"Line %d: Parameter mismatch in function \"%s\".\n", yylineno, str);
      break;
      case 2:
      fprintf(stderr,"Line %d: Parameter mismatch in function \"%s\".\n", yylineno, str);
      break;
      case 3:
      fprintf(stderr, "Line %d: Symbol \"%s\" already declared.\n", yylineno, str);
      break;
      case 4:
      fprintf(stderr, "Line %d: Function \"%s\" already declared.\n", yylineno, str);
break;
      case 5:
      fprintf(stderr, "Line %d: Symbol \"%s\" undeclared.\n", yylineno, (char *)str);
      break;
      case 6:
      fprintf(stderr, "Line %d: Symbol \"%s\" declared as array.\n", yylineno,(char *) str);
      break;
      case 7:
      fprintf(stderr, "Line %d: Symbol \"%s\" not declared as array.\n", yylineno,(char *) str);
      break;
      case 8:
      fprintf(stderr, "Line %d: Invalid type in binary expression \"%c\".\n", yylineno, *str);
      break;
      case 9:
fprintf(stderr, "Line %d: Type mismatch in logical expression \"%s\".\n", yylineno, str);
      break;
      case 10:
      fprintf(stderr, "Line %d: Type mismatch in relational expression \"%s\".\n", yylineno, str);
      break;
case 11:
      fprintf(stderr, "Line %d: Type mismatch in unary expression \"%c\".\n", yylineno, *str);
      break;
      case 12:
      fprintf(stderr, "Line %d: Type mismatch in unary logical expression \"%c\".\n", yylineno, *str);
      break;
      case 13:
      fprintf(stderr, "Line %d: Symbol \"%s\" not declared as a function.\n", yylineno, str);
      break;
      case 14:
      fprintf(stderr, "Line %d: Symbol \"%s\" is not declared as a variable.\n", yylineno, str);
      break;
      case 15:
      fprintf(stderr, "Line %d: Assignment to variable \"%s\" is of incorrect type.\n", yylineno, str);
      break;
      case 16:
      fprintf(stderr, "Line %d: Index expression for array \"%s\" does not evaluate to an integer.\n", yylineno, str);
      break;
      case 17:
      fprintf(stderr, "Line %d: Expression in statement \"%s\" does not evaluate to boolean.\n", yylineno, str);
      break;
      case 18:
      fprintf(stderr, "Line %d: Assignment of boolean expression to variable \"%s\".\n", yylineno, str);
      break;
      case 19:
      fprintf(stderr, "Line %d: Return type does not match function type in function \"%s\".\n", yylineno, str);
      break;
      case 20:
      fprintf(stderr, "Line %d: Function \"%s\" has unexpected return statement.\n", yylineno, str);
      break;
      case 21:
      fprintf(stderr, "Line %d: Function \"%s\" does not have a return statement.\n", yylineno, str);
      break;
      case 22:
      fprintf(stderr, "Line %d: Type of function \"%s\" does not match prototype.\n", yylineno, str);
      break;
      case 23:
      fprintf(stderr, "Line %d: Function \"%s\" was declared as extern, but was defined in this program.\n", yylineno, str);
      break;
      case 24:
      fprintf(stderr, "Line %d: Function call to \"%s\" is statement, but return type is not void.\n", yylineno, str);
      break;
      case 25:
      fprintf(stderr, "Line %d: Invalid assignment to symbol \"%s\": \"%s\" is an array.\n", yylineno, str, str);
      break;
      case 26:
      fprintf(stderr, "Line %d: Function call to \"%s\" is not a statement, but return type is void.\n", yylineno, str);
      break;
      case 27:
      fprintf(stderr, "Line %d: Symbol \"%s\" is not a variable, but was used as one.\n", yylineno, str);
      break;

      default:
      fprintf(stderr, "Line %d: Unknown error code.\n", yylineno);
      break;
   }
   return code;
}

