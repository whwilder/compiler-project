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
%type <glob> prog dcl decllist func loc_decllist varlist parm_types
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
         |  EXTERN type ID '(' parm_types ')' clearlocals 
            {addToTable($2, 0, $3, FUNCPROTO, 0, 0); setExtern($3);}
            idlist ';'
               {$$ = newFuncProtoNode($3);}
         |  type ID '(' parm_types ')' clearlocals
            {addToTable($1, 0, $2, FUNCPROTO, 0, 0);}
                idlist ';'
            {$$ = newFuncProtoNode($2);}
         |  error ID '(' parm_types ')' idlist ';' {$$ = NULL;}
         |  EXTERN VOID ID '(' parm_types ')' clearlocals
            {addToTable(VD, 0, $3, FUNCPROTO, 0, 0); setExtern($3);}
            idlist ';'
              {$$ = newFuncProtoNode($3);}
         |  VOID ID '(' parm_types ')' clearlocals
            {addToTable(VD, 0, $2, FUNCPROTO, 0, 0);}
            idlist ';'
               {$$ = newFuncProtoNode($2);}
            
         ;
idlist   : idlist clearlocals ',' ID '(' parm_types ')'  clearlocals
            {addToTable(currType, 0, $4, FUNCPROTO, 0, 0);}
         | idlist clearlocals ',' ID '(' parm_types error
         | /* epsilon */
         ;
var_decl : ID                  {$$ = addToTable(currType, 0, $1, VAR, scope, 0);}
         | ID '[' INTCON ']'   {$$ = addToTable(currType, 1, $1, VAR, scope, $3);}
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
parm_decl   : type ID '[' ']' {$$ = addToTable($1, 1, $2, VAR, 1, 0);}
            | type ID         {$$ = addToTable($1, 0, $2, VAR, 1, 0);}
            ;
parm_types  : VOID         {$$ = NULL;}
            | parm_decl    {$$ = newDclNode($1,NULL);}
            | parm_decl ',' parm_types {$$ = newDclNode($1,$3);}
            ;
func  :  type ID '(' parm_types ')' 
         {checkExtern($2); addToTable($1, 0, $2, FUNC, 0, 0); currOffset = -4;}
         '{' incscope varlist stmtlist '}'
         {
          $$ = newFuncNode($2, $4, $9, $10);
          clearLocals(); scope = 0; tmpNum = 1; currOffset = 8;}
      |  VOID ID '(' parm_types ')'
         {checkExtern($2); addToTable(VD, 0, $2, FUNC, 0, 0); currOffset = -4;}
         '{' incscope varlist stmtlist '}'
         {
          $$ = newFuncNode($2, $4, $9, $10);
            clearLocals(); scope = 0; tmpNum = 1; currOffset = 8;}
      |  error ID error parm_types ')' '{' varlist error '}' {$$ = NULL;}
      ;

clearlocals: /* epsilon */ {clearLocals();}
           ;

incscope: /* epsilon */ {scope = 1;}
        ;

varlist : type loc_decllist ';' varlist {$$ = addDclNode($2,$4);}
        | /* epsilon */ {$$ = NULL;}
        ;
loc_decl : ID                  {$$ = addToTable(currType, 0, $1, VAR, scope, 0);}
         | ID '[' INTCON ']'   {$$ = addToTable(currType, 1, $1, VAR, scope, $3);}
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
    |  RETURN ';'    {$$ = newReturnNode(NULL);}
    |  RETURN expr ';'  {$$ = newReturnNode($2);}
    |  assg ';'      {$$ = $1;}
    |  ID addStack '(' exprlist ')' ';'
         {$$ = stmtFuncCall($1, $4); clearExprs();}
    |  ID addStack '(' ')' ';'     {$$ = stmtFuncCall($1, NULL); clearExprs();}
    |  '{' stmtlist '}'    {$$ = $2;}
    |  ';'                 {$$ = newEmptyNode(EMPTYSTMT);}
    |  ID error ')' ';'      {$$ = NULL;}
    |  error      {$$ = NULL;}
    ;
stmtlist : stmt stmtlist {$$ = addStmtNode($1, $2);}
         | /* epsilon */ {$$ = NULL;}
         ;
assg  :  ID '[' expr ']'    '=' expr    {$$ = newAssgNode($1,$3,$6);}
      |  ID error expr ']'  '=' expr  {$$ = NULL;}
      |  ID '[' expr error  '=' expr  {$$ = NULL;}
      |  error '[' expr ']' '=' expr  {$$ = NULL;}
      |  ID '=' expr    {$$ = newAssgNode($1,NULL,$3);}
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
      |  ID            {$$ = newExprIdNode($1, NULL);}
      |  ID '[' expr ']'            {$$ = newExprIdNode($1, $3);}
         /*{$$ = checkIdExist($1,$3);}*/
      |  ID '[' expr error   {$$ = NULL;}
      |  ID addStack '(' ')'     {$$ = exprFuncCall($1, NULL);clearExprs();}
      |  ID addStack '(' error   {$$ = NULL; clearExprs();}
      |  ID addStack '(' exprlist ')'
         {$$ = exprFuncCall($1, $4); clearExprs();}
      |  '(' expr ')'  {$$ = $2;}
      |  INTCON      {$$ = newIntNode($1,INTEGER);}
      |  CHARCON      {$$ = newCharNode($1, CHARACTER);}
      |  STRINGCON      {$$ = newStringNode($1, STRING);}
      ;

addStack : /* epsilon */ {addStack();}
         ;
exprlist : expr ',' exprlist  {$$ = addExprNode($1,$3);}
         | expr               {$$ = addExprNode($1,NULL);}
         ;

%%

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
   symTabNode *tmp = findId(id);
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
   symTabNode *nextPtr = table;
   while (nextPtr != NULL){
      if (nextPtr->symType == FUNC && nextPtr->scope == 0)
         return nextPtr;
      nextPtr = nextPtr->next;
   }
   return nextPtr;
}

void addStack(){
   exprStack *tmpStack = malloc(sizeof(exprStack));
   tmpStack->next = stacks;
   parmNode *tmpExprs = malloc(sizeof(parmNode));
   tmpExprs->type = -1;
   tmpExprs->next = NULL;
   tmpExprs->parmIndex = -1;
   tmpExprs->isArray = -1;
   tmpStack->exprs = tmpExprs;
   stacks = tmpStack;
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

Type checkFunc(char *id, parmNode *expr, int isStatement){
   symTabNode *node = findId(id);
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
   if (expr != NULL && node->numParms != expr->parmIndex+1){
      semerror(1,id);
      return node->type;
   }
   if (isStatement == 1 && node->type != VD){
      semerror(24,id);
      return node->type;
   }
   if (expr == NULL && node->numParms == 0){
      return node->type;
   }
   Type *funcParms = node->funcParms;
   parmNode *tmp = expr;
   while (tmp->next != NULL){
      int index = tmp->parmIndex;
      if (funcParms[index] != tmp->type){
         if(funcParms[index] == INTEGER && tmp->type != CHARACTER)
            semerror(1, id);
         else if (funcParms[index] == CHARACTER && tmp->type != INTEGER)
            semerror(1, id);
      }
      tmp = tmp->next;
   }
   return node->type;
}

void clearExprs(){
   parmNode *tmp = stacks->exprs;
   parmNode *tmpsp = stacks->exprs;
   while(tmp->next != NULL){
      tmp = tmp->next;
      free(tmpsp);
      tmpsp = tmp;
   }
   free(tmp);
   exprStack *tmpStack = stacks;
   stacks = stacks->next;
   free(tmpStack);
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

void addExpr(Type type){
   parmNode *tmp = malloc(sizeof(parmNode));
   tmp->type = type;
   tmp->next = stacks->exprs;
   tmp->parmIndex = tmp->next->parmIndex+1;
   stacks->exprs = tmp;
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

symTabNode *addToTable(Type type, int isArray, char *id, SymType symType, int scope, int size){
   symTabNode *tmp = findId(id);
   if (tmp != NULL){
      if (tmp->scope == scope && tmp->symType != FUNCPROTO ){
         semerror(3, tmp->id);
         return table;
      }
      else if(tmp->symType == FUNC && symType == FUNC){
         semerror(4, tmp->id);
         return table;
      }
      else if(tmp->symType == FUNCPROTO && symType == FUNCPROTO){
         semerror(4, tmp->id);
         return table;
      }
      else if(tmp->symType == FUNCPROTO && symType == FUNC){
         if (tmp->type != type){
            semerror(22,id);
         }
         int code = checkParms(tmp);
         if(code != 0)
            return table;
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
      addParmsToFunc(tmpNode);
   }
   if(symType == FUNC){
      pushFuncDown(tmpNode);
      return table;
   }
   tmpNode->next = table;
   table = tmpNode;
   //if (scope == 0) {tmpNode->next = globalTable; globalTable = tmpNode;}
   //if (scope == 1) {tmpNode->next =  localTable; localTable  = tmpNode;}
   
   return table;
}

void pushFuncDown(symTabNode *tmpNode){
   symTabNode *nextPtr;
   symTabNode *prevPtr;
   prevPtr = table;

   if(table->scope == 0){
      tmpNode->next = table;
      table = tmpNode;
      return;
   }
   
   while (prevPtr->next != NULL && prevPtr->next->scope != 0){
      prevPtr = prevPtr->next;
   }
   if(prevPtr->next != NULL){
      nextPtr = prevPtr->next;
      tmpNode->next = nextPtr;
      prevPtr->next = tmpNode;
   }
   else{
      tmpNode->next = prevPtr;
      table = tmpNode;
   }
}

int checkParms(symTabNode *node){
   if(node->numParms != (parms->parmIndex+1)){
      semerror(1, node->id);
      return 1;
   }
   parmNode *tmpParm = parms;
   while (tmpParm->next != NULL){
      int index = tmpParm->parmIndex;
      if (tmpParm->type != node->funcParms[index]){
         semerror(2, node->id);
         return 1;
      }
      tmpParm = tmpParm->next;
   }
   return 0;
}

symTabNode *addParmsToFunc(symTabNode *tmpNode){
   int numParms = parms->parmIndex+1;
   tmpNode->funcParms = malloc(sizeof(Type) * numParms);
   tmpNode->numParms = numParms;
   parmNode *tmp = parms;
   while (tmp->next != NULL){
      tmpNode->funcParms[tmp->parmIndex] = tmp->type;
      tmp = tmp->next;
      free(parms);
      parms = tmp;
   }
   return tmpNode;
}

/* local variables should always be at the top of the stack, so just pop nodes until we hit a global variable */
void clearLocals(){
   symTabNode *nextPtr = table;
   while (nextPtr->scope == 1){
      symTabNode *tmp = nextPtr;
      nextPtr = nextPtr->next;
      free(tmp);
   }
   table = nextPtr;
}

symTabNode *findId(char *id){
   symTabNode *tmpPtr = table;
   //if (scope == 0) {tmpPtr = globalTable;}
   //else if (scope == 1) {tmpPtr = localTable;}
   while (tmpPtr->next != NULL){
      if(strcmp(tmpPtr->id,id) == 0){
         return tmpPtr;
      }
      tmpPtr = tmpPtr->next;
   }
   return NULL;
}

parmNode *addParm(symTabNode *node){
   parmNode *tmp  = malloc(sizeof(parmNode));
   tmp->type      = node->type;
   tmp->isArray   = node->isArray;
   tmp->id        = strdup(node->id);
   tmp->next      = parms;
   tmp->parmIndex = parms->parmIndex+1;
   parms          = tmp;
   return parms;
}

void clearParms(){
   parmNode *tmp = parms;
   while (tmp->next != NULL){
      tmp = tmp->next;
      free(parms);
      parms = tmp;
   }
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

      default:
      fprintf(stderr, "Line %d: Something has gone horribly wrong!\n", yylineno);
      break;
   }
   return code;
}

