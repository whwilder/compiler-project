#include "structs.h"
#include "proto.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

globNode *addGlobFuncNode(globNode *node, globNode *next){
   node->next = next;
   return node;
}
globNode *addDclNode(globNode *node, globNode *next){
   globNode *tmp = node;
   while (tmp->next != NULL){
      tmp = tmp->next;
   }
   tmp->next = next;
   return node;
}

exprNode *addExprNode(exprNode *node, exprNode *next){
   if (node != NULL)
      node->nextExpr = next;
   return node;
}
stmtNode *addStmtNode(stmtNode *node, stmtNode *next){
   if (node == NULL)
      return NULL;
   node->next = next;
   return node;
}

stmtNode *newReturnNode(exprNode *expr){
   returnNode *ret = malloc(sizeof(returnNode));
   ret->expr = expr;

   stmtNode *stmt = malloc(sizeof(stmtNode));
   stmt->ret = ret;
   stmt->type = RET;
   return stmt;
}

globNode *newFuncProtoNode(char *id){
   funcNode *func = malloc(sizeof(funcNode));
   symTabNode *tmp = findId(id);

   int idLen = strlen(tmp->id)+1;

   idNode *idnode = malloc(sizeof(idNode));
   idnode->id = malloc(idLen * sizeof(char));
   strncpy(idnode->id, tmp->id, idLen);
   idnode->type = tmp->type;
   func->id = idnode;
   func->formParms = malloc(tmp->numParms * sizeof(Type));
   // use tmp->numParms*4 for # of bytes because an int is 4 bytes
   // TODO: keep an eye on this if something breaks
   memcpy(func->formParms, tmp->funcParms, tmp->numParms*4);
   func->numParms = tmp->numParms;

   globNode *glob = malloc(sizeof(globNode));
   glob->func = func;
   glob->type = GLOBFUNCPROTO;
   return glob;
}

stmtNode *newEmptyNode(stmtType type){
   emptyNode *empty = malloc(sizeof(emptyNode));
   empty->type = type;

   stmtNode *stmt = malloc(sizeof(stmtNode));
   stmt->empty = empty;
   stmt->type = EMPTYSTMT;
   return stmt;
}

stmtNode *newWhileNode(stmtType type, exprNode *cond, stmtNode *stmt){
   whileNode *node = malloc(sizeof(whileNode));
   node->cond = cond;
   node->stmt = stmt;

   stmtNode *snode = malloc(sizeof(stmtNode));
   snode->type = type;
   snode->whileNode = node;
   snode->next = NULL;
   return snode;
}
stmtNode *newForNode(stmtType type, stmtNode *init, exprNode *cond, stmtNode *inc, stmtNode *stmt){
   forNode *node = malloc(sizeof(forNode));
   node->init = init;
   node->cond = cond;
   node->inc = inc;
   node->stmt = stmt;

   stmtNode *snode = malloc(sizeof(stmtNode));
   snode->type = type;
   snode->forNode = node;
   snode->next = NULL;
   return snode;
}

stmtNode *newIfNode(stmtType type, exprNode *cond, stmtNode *stmtThen, stmtNode *stmtElse){
   ifNode *node = malloc(sizeof(ifNode));
   node->cond = cond;
   node->stmtThen = stmtThen;
   node->stmtElse = stmtElse;

   stmtNode *snode = malloc(sizeof(stmtNode));
   snode->type = type;
   snode->ifNode = node;
   snode->next = NULL;
   return snode;
}

globNode *newFuncNode(char *id, globNode *parms, globNode *varlist, stmtNode *stmtlist){
   funcNode *func = malloc(sizeof(funcNode));
   symTabNode *tmp = findId(id);

   int idLen = strlen(tmp->id)+1;

   idNode *idnode = malloc(sizeof(idNode));
   idnode->id = strdup(tmp->id);
   idnode->type = tmp->type;
   func->id = idnode;
   
   int numParms = 0;
   globNode *parm = parms;
   while (parm != NULL) {
      numParms++;
      parm = parm->next;
   }

   func->dcl = varlist;
   func->formParms = parms;

   func->numParms = numParms;
   func->stmt = stmtlist;
   
   stmtNode *ret = func->stmt;
   while (ret != NULL){
      if (ret->next == NULL)
         break;
      ret = ret->next;
   }
   if (ret == NULL){
      func->stmt = newReturnNode(NULL);
   }
   else if (ret->type != RET){
      ret->next = newReturnNode(NULL);
   }

   func->stackSize = tmpNum * -4;

   globNode *glob = malloc(sizeof(globNode));
   glob->func = func;
   glob->type = GLOBFUNC;
   return glob;
}

globNode *newDclNode(symTabNode *node, globNode *next){
   dclNode *dcl = malloc(sizeof(dclNode));
   dcl->id = node->id;
   dcl->type = node->type;
   dcl->arraySize = node->arraySize;
   tmpNum += 1 + (1*dcl->arraySize);
   if (currOffset < 0){
      currOffset -= 4;
      dcl->offset = tmpNum * -4;
   }
   else{
      currOffset += 4;
      dcl->offset = tmpNum * 4;
   }
   //fprintf(stderr, "offset of %s: %d\n", node->id, dcl->offset);

   globNode *glob = malloc(sizeof(globNode));
   glob->dcl = dcl;
   glob->next = next;
   glob->type = DCL;
   return glob;
}

exprNode *newExprIdNode(char *id, exprNode *expr){
   idNode *tmp = newIdNode(id, expr);
   if (tmp == NULL)
      return NULL;
   exprNode *e = malloc(sizeof(exprNode));
   e->nextExpr = NULL;
   e->touched = 0;
   e->idNode = tmp;
   e->type = EXPRID;
   e->offset = tmpNum * -4;
   e->tmp = malloc(10*sizeof(char));
   sprintf(e->tmp, "$tmp%d", tmpNum);
   currOffset -= 4;
   tmpNum++;

   if (expr == NULL){
      e->valType = tmp->type;
   } else if (tmp->type == INTARRAY){
      e->valType = INTEGER;
   } else if (tmp->type == STRING){
      e->valType = CHARACTER;
   }
   //fprintf(stderr, "offset of %s: %d\n", id, e->offset);
   return e;
}

exprNode *newOpNode(opType type, exprType exprType, exprNode *left, exprNode *right){
   opNode *node = malloc(sizeof(opNode));
   node->op = type;
   node->left = left;
   node->right = right;

   exprNode *expr = malloc(sizeof(exprNode));
   expr->touched = 0;
   expr->type = exprType;
   expr->opNode = node;
   expr->nextExpr = NULL;
   expr->tmp = malloc(10*sizeof(char));
   sprintf(expr->tmp, "$tmp%d", tmpNum);
   expr->offset = tmpNum * -4;
   currOffset -= 4;
   tmpNum++;
   return expr;
}
exprNode *newUnaNode(opType type, exprNode *expr){
   unaNode *node = malloc(sizeof(unaNode));
   node->op = type;
   node->expr = expr;

   exprNode *enode = malloc(sizeof(exprNode));
   enode->touched = 0;
   enode->type = UNAOP;
   enode->unaNode = node;
   enode->offset = tmpNum * -4;
   tmpNum++;
   enode->nextExpr = NULL;
   return enode;
}

idNode *newIdNode(char *name, exprNode *expr){
   symTabNode *node = checkIdExist(name, expr);
   idNode *tmpNode = malloc(sizeof(idNode));
   tmpNode->id = strdup(name);
   if (node == NULL){
      tmpNode->type = INTEGER;
   }
   else
      tmpNode->type = node->type;
   tmpNode->expr = expr;

   return tmpNode;
}

stmtNode *newAssgNode(char *LHS, exprNode *index, exprNode *RHS){
   assgNode *tmpNode = malloc(sizeof(assgNode));
   tmpNode->LHS = newIdNode(LHS,index);
   if (tmpNode->LHS == NULL)
      return NULL;
   tmpNode->RHS = RHS;
   stmtNode *stmt = malloc(sizeof(stmtNode));
   stmt->assgNode = tmpNode;
   stmt->type = ASSG;
   return stmt;
}

stmtNode *stmtFuncCall(char *id, exprNode *parms){
   funcCallNode *func = newFuncCall(id, parms);
   stmtNode *stmt = malloc(sizeof(stmtNode));
   stmt->funcCall = func;
   stmt->type = STMTCALL;
   return stmt;
}
exprNode *exprFuncCall(char *id, exprNode *parms){
   funcCallNode *func = newFuncCall(id, parms);
   exprNode *expr = malloc(sizeof(exprNode));
   expr->touched = 0;
   expr->funcCall = func;
   expr->type = EXPRCALL;
   expr->offset = tmpNum * -4;
   expr->nextExpr = NULL;
   tmpNum++;
   return expr;
}
funcCallNode *newFuncCall(char *id, exprNode *parms){
   funcCallNode *func = malloc(sizeof(funcCallNode));
   symTabNode *tmp = findId(id);
   func->id = strdup(id);
   func->actParms = parms;
   func->numParms = tmp->numParms;
   return func;
}

exprNode *newIntNode(int num, Type type){
   termNode *term = malloc(sizeof(termNode));
   term->iVal = num;
   term->type = type;

   exprNode *expr = malloc(sizeof(exprNode));
   expr->touched = 0;
   expr->termNode = term;
   expr->type = TERM;
   expr->tmp = malloc(10*sizeof(char));
   sprintf(expr->tmp, "$tmp%d", tmpNum);
   expr->offset = tmpNum * -4;
   expr->nextExpr = NULL;
   tmpNum++;
   expr->valType = type;
   //fprintf(stderr, "offset of %d: %d\n", num, expr->offset);
   return expr;
}
exprNode *newCharNode(char *charcon, Type type){
   termNode *term = malloc(sizeof(termNode));
   char ch = *(charcon+1);
   /* Deal with escape characters */
   if (ch == '\\'){
      char ch2 = *(charcon+2);
      switch(ch2){
         case '0':
            ch = '\0';
            break;
         case 'a':
            ch = '\a';
            break;
         case 'b':
            ch = '\b';
            break;
         case 'f':
            ch = '\f';
            break;
         case 'n':
            ch = '\n';
            break;
         case 'r':
            ch = '\r';
            break;
         case 't':
            ch = '\t';
            break;
         case 'v':
            ch = '\v';
            break;
         case '\\':
            ch = '\\';
            break;
         case '\'':
            ch = '\'';
            break;
         case '"':
            ch = '\"';
            break;
         case '?':
            ch = '\?';
            break;
      }
   } 
   term->iVal = ch;
   term->type = type;

   exprNode *expr = malloc(sizeof(exprNode));
   expr->touched = 0;
   expr->termNode = term;
   expr->type = TERM;
   expr->tmp = malloc(10*sizeof(char));
   sprintf(expr->tmp, "$tmp%d", tmpNum);
   expr->offset = tmpNum * -4;
   expr->nextExpr = NULL;
   tmpNum++;
   expr->valType = type;
   //fprintf(stderr, "offset of %d: %d\n", ch, expr->offset);
   return expr;
}
exprNode *newStringNode(char *str, Type type){
   termNode *term = malloc(sizeof(termNode));
   int strLen = strlen(str)-1;
   term->strcon = strndup(str+1,strLen-1);
   term->type = type;

   exprNode *expr = malloc(sizeof(exprNode));
   expr->termNode = term;
   expr->type = TERM;
   expr->nextExpr = NULL;
   expr->idNode = NULL;
   expr->touched = 0;
   expr->valType = type;
   return expr;
}


