
#ifndef COMPILE_H_INCLUDED
#define COMPILE_H_INCLUDED
#ifndef SYNTREE_H_INCLUDED
#include "syntree.h"
#define SYNTREE_H_INCLUDED
#endif

enum type {INTEGER=0, CHARACTER=1, BOOL=2, STRING=3, VD=4, INTARRAY=5, ERR=6};
enum symtype {VAR=10, FUNC=11, FUNCPROTO=12};
typedef enum type Type;
typedef enum symtype SymType;

Type currType;

typedef struct symTable{
   int defined, symType, isArray, arraySize, numParms, hasReturn, isExtern;
   Type type;
   char *id;
   struct symTable *next;
   int scope;
   Type *funcParms;
} symTabNode;
symTabNode *findId(char *id);
Type checkIdExist(char *id, Type expr);
void pushFuncDown(symTabNode *tmpNode);
void setExtern(char *id);
void checkExtern(char *id);

typedef struct ParmList{
   Type type;
   int parmIndex;
   int isArray;
   int arraySize;
   char *id;
   struct ParmList *next;
} parmNode;
Type checkFunc(char *id, parmNode *expr, int isStatement);

typedef struct ExprStack{
   parmNode *exprs;
   struct ExprStack *next;
} exprStack;
exprStack *stacks;

void addStack();

parmNode *addParm(symTabNode *node);
int checkParms(symTabNode *node);
symTabNode *addParmsToFunc(symTabNode *tmpNode);

void addExpr(Type type);
void clearExprs();

void setIdValue(int val, char *id, SymType symType);
symTabNode *addToTable(Type type, int isArray, char *id, SymType symType, int scope, int size);
void clearLocals();
int semerror(int code, char *ptr);
int returnType;
Type checkBinExpr(exprNode *t1, exprNode *t2, char op);
Type checkLogExpr(Type t1, Type t2, char *op);
Type checkRelExpr(Type t1, Type t2, char *op);
Type checkUnaExpr(Type t1, char op);
Type checkUnaLogExpr(Type t1, char op);

Type checkAssg(char *id, Type type);
Type checkElemAssg(char *id, Type indexExpr, Type type);
void checkCond(Type type, char *stmt);
void checkReturn(Type type);
symTabNode *getTopFunc();
void checkHasReturn(char *id);

symTabNode *table;
parmNode *parms;
//parmNode *exprs;
//symTabNode *localTable;
#endif
