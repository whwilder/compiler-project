
#ifndef SYNTREE_H_INCLUDED
#define SYNTREE_H_INCLUDED
#ifndef COMPILE_H_INCLUDED
#include "compile.h"
#define COMPILE_H_INCLUDED
#endif


typedef enum stmtType {ASSG, CALL, RET, STMTCALL, IFSTMT, WHILESTMT, FORSTMT, EMPTYSTMT} stmtType;
typedef enum exprType {TERM, EXPRID, EXPRCALL, BIN, REL, LOG, UNAOP} exprType;
typedef enum opType {PLUS, MINUS, MULT, DIV, LOGAND, LOGOR, RELEQ, RELNE, RELLE, RELLT, RELGE, RELGT, UNAMINUS, UNANOT} opType;
typedef enum globType {DCL, GLOBFUNC, GLOBFUNCPROTO} globType;

typedef struct StmtNode{
   stmtType type;
   struct AssgNode     *assgNode;
   struct whileNode    *whileNode;
   struct forNode      *forNode;
   struct ifNode       *ifNode;
   struct FuncCallNode *funcCall;
   struct ReturnNode   *ret;
   struct emptyNode    *empty;
   struct StmtNode     *next;
} stmtNode;

typedef struct emptyNode{
   stmtType type;
} emptyNode;

typedef struct ReturnNode{
   struct ExprNode *expr;
} returnNode;
typedef struct whileNode{
   struct ExprNode *cond;
   struct StmtNode *stmt;
} whileNode;
typedef struct forNode{
   struct StmtNode *init;
   struct ExprNode *cond;
   struct StmtNode *inc;
   struct StmtNode *stmt;
} forNode;
typedef struct ifNode{
   struct ExprNode *cond;
   struct StmtNode *stmtThen;
   struct StmtNode *stmtElse;
} ifNode;

typedef struct tmpNode {
   char *id;
   Type type;
   struct tmpNode *next;
} tmpNode;

typedef struct FuncNode{
   struct IdNode   *id;
   struct GlobNode *formParms;
   int        numParms;
   struct StmtNode *stmt;
   struct GlobNode *dcl;
   int stackSize;
} funcNode;

typedef struct FuncCallNode{
   char *id;
   int numParms;
   struct ExprNode *actParms;
} funcCallNode;

typedef struct AssgNode{
   struct IdNode *LHS;
   struct ExprNode *RHS;
} assgNode;


typedef struct ExprNode{
   exprType type;
   Type valType;
   struct opNode          *opNode;
   struct unaNode         *unaNode;
   struct IdNode          *idNode;
   struct FuncCallNode    *funcCall;
   struct TermNode         *termNode;
   struct ExprNode        *nextExpr;
   char *tmp;
   int offset;
   int touched;
} exprNode;

typedef struct opNode{
   opType op;
   exprNode *left;
   exprNode *right;
} opNode;
typedef struct unaNode{
   opType op;
   exprNode *expr;
} unaNode;

typedef struct IdNode{
   char *id;
   Type type;
   exprNode *expr;
} idNode;

typedef struct TermNode{
   Type type;
   int iVal;
   char *strcon;
} termNode;

typedef struct DclNode{
   char *id;
   Type type;
   int arraySize;
   int offset;
} dclNode;

typedef struct GlobNode{
   globType type;
   dclNode         *dcl;
   funcNode        *func;
   struct GlobNode *next;
} globNode;

globNode *root;


typedef struct StrTable {
   struct StrTable *next;
   char *id;
   char *str;
} strTable;

strTable *strings;
int strNum;

typedef struct locTable{
   struct locTable *next;
   int iVal;
   int offset;
   char *id;
   Type type;
} locTable;

typedef struct labelTable{
   char *labName;
   int labNum;
   struct labelTable *next;
} labTable;

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
