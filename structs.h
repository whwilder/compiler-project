#ifndef STRUCTS
#define STRUCTS
/*******************
 * TYPE CHECKING
 *******************/
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

typedef struct symTabStack{
   symTabNode *table;
   struct symTabStack *next;
} symTabStack;

typedef struct ParmList{
   Type type;
   int parmIndex;
   int isArray;
   int arraySize;
   char *id;
   struct ParmList *next;
} parmNode;

typedef struct ExprStack{
   parmNode *exprs;
   struct ExprStack *next;
} exprStack;


/*******************
 * SYNTAX TREE
 *******************/

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

/*******************
 * FINAL CODE GEN
 *******************/

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
#endif
