


#ifndef STRUCTS
#include "structs.h"
#endif

#ifndef PROTO
#define PROTO
/********************
 * TYPE CHECKING
 ********************/
exprStack *stacks;

void addStack();

symTabNode *findId(char *id);
symTabNode *checkIdExist(char *id, exprNode *expr);
void pushFuncDown(symTabNode *tmpNode);
void setExtern(char *id);
void checkExtern(char *id);

parmNode *addParm(symTabNode *node);
int checkParms(symTabNode *node, globNode *parms);
symTabNode *addParmsToFunc(symTabNode *tmpNode, globNode *parms);

void addExpr(Type type);
void clearExprs();

void setIdValue(int val, char *id, SymType symType);
symTabNode *addToTable(Type type, int isArray, char *id, SymType symType, int scope, int size, globNode *parms);
void clearLocals();
int semerror(int code, char *ptr);
int returnType;
Type checkBinExpr(exprNode *e1, exprNode *e2, char op);
Type checkLogExpr(exprNode *e1, exprNode *e2, char *op);
Type checkRelExpr(exprNode *e1, exprNode *e2, char *op);
Type checkUnaExpr(exprNode *e1, char op);
Type checkUnaLogExpr(exprNode *e1, char op);
Type checkFunc(char *id, exprNode *expr, int isStatement);
void checkIsVar(char *id);

Type checkAssg(char *id, Type type);
Type checkElemAssg(char *id, Type indexExpr, Type type);
void checkCond(Type type, char *stmt);
void checkReturn(Type type);
symTabNode *getTopFunc();
void checkHasReturn(char *id);

symTabNode  *table;
symTabStack *tables;
parmNode *parms;
//parmNode *exprs;
//symTabNode *localTable;

/********************
 * SYNTAX TREE
 ********************/

locTable *locals;
locTable *locEnd;
labTable *labels;
int labNum;
int tmpNum;
int currOffset;
char *newLabel();
void newGoto(char *label);

locTable *findLoc(char *id);
void addLocParms(globNode *parms);
void addLocVars(globNode *vars);
void clearLocTab();
locTable *loadTmp(exprNode *term);

// find using string contents, return id
char *findStr(char *str);
char *addStr(char *str);
void printStrings(strTable *strings);

//binOpNode *newBinOpNode(stNode *left, stNode *right, char* op);
//relOpNode *newRelOpNode(stNode *left, stNode *right, char* op);
stmtNode *newAssgNode(char *LHS, exprNode *index, exprNode *RHS);
idNode *newIdNode(char *id, exprNode *expr);
exprNode *newExprIdNode(char *id, exprNode *expr);
//stNode *newIdElemNode(char *id, stNode *right);
exprNode *newStringNode(char *str, Type type);
exprNode *newIntNode(int num, Type type);
exprNode *newCharNode(char *charcon, Type type);

globNode *newFuncNode(char *id, globNode *parms, globNode *varlist, stmtNode *stmtlist);
globNode *newFuncProtoNode(char *id);
globNode *newDclNode(symTabNode *node, globNode *next);
globNode *addGlobFuncNode(globNode *node, globNode *next);
globNode *addDclNode(globNode *node, globNode *next);
exprNode *addExprNode(exprNode *node, exprNode *next);
exprNode *newOpNode(opType type, exprType exprType, exprNode *left, exprNode *right);
exprNode *newUnaNode(opType type, exprNode *expr);
stmtNode *addStmtNode(stmtNode *node, stmtNode *next);
funcCallNode *newFuncCall(char *id, exprNode *parms);
exprNode *exprFuncCall(char *id, exprNode *parms);
stmtNode *stmtFuncCall(char *id, exprNode *parms);
stmtNode *newReturnNode(exprNode *expr);
stmtNode *newEmptyNode(stmtType type);
stmtNode *newWhileNode(stmtType type, exprNode *cond, stmtNode *stmt);
stmtNode *newForNode(stmtType type, stmtNode *init, exprNode *cond, stmtNode *inc, stmtNode *stmt);

void evalOp(exprNode *node);
void evalBin(exprNode *node);
void evalRel(exprNode *node, char *trueDest, char *falseDest);
void evalLog(exprNode *node, char *trueDest, char *falseDest);
void evalUnaExpr(exprNode *node, char *trueDest, char *falseDest);

void traverseTree(globNode *root);
void traverseStmt(stmtNode *stmts);
void getTerm(exprNode *term);
void traverseExprNode(exprNode *node, char *trueDest, char *falseDest);
int traverseParms(exprNode *parms);
void getId(exprNode *id);
void storeId(idNode *id, exprNode *RHS);

void codeGen_dcl(dclNode *dcl);
#endif
