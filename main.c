#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "structs.h"
#include "proto.h"

extern int yychar;
extern int yydebug;
int errcode;

/*
 *TODO: 
 */


int main (int argc, char **argv){
   table = malloc(sizeof(symTabNode));
   table->type=0;
   table->defined=0;
   table->scope=0;
   table->symType=0;
   table->id="$";
   table->next=NULL;
   
   parms = malloc(sizeof(parmNode));
   parms->type = -1;
   parms->parmIndex = -1;
   parms->next = NULL;

   //exprs = malloc(sizeof(parmNode));
   //exprs->type = -1;
   //exprs->parmIndex = -1;
   //exprs->next = NULL;

   stacks = malloc(sizeof(exprStack));
   stacks->next = NULL;
   stacks->exprs = NULL;

   root = malloc(sizeof(globNode));

   returnType = -1;
   tmpNum = 1;
   currOffset = 8;

   yydebug = 0;
   errcode = 0;
   yyparse();

   if(errcode != 0)
      return errcode;
   
   strNum = 0;
   labNum = 0;
   strings = malloc(sizeof(strTable));

printf("#####################################################\n");
printf("_print_int:\n");
printf("   li   $v0, 1  # escape code for printint\n");
printf("   lw   $a0, 0($sp)\n");
printf("   syscall      # print it\n");
printf("   jr $ra \n");
printf("#####################################################\n");
printf("_print_string:\n");
printf("   li   $v0, 4 # escape code for print string\n");
printf("   lw   $a0, 0($sp)\n");
printf("   syscall     # print it\n");
printf("   jr $ra \n");
printf("#####################################################\n");

   traverseTree(root);
}

void traverseTree(globNode *root){
   globNode *tmp = root;
   while (tmp != NULL){
      if(tmp->type == DCL){
 //        printf("dcl id: %s\n", tmp->dcl->id);
         printf(".data\n");
         codeGen_dcl(tmp->dcl);
      }
      else if(tmp->type == GLOBFUNC){
         if (tmp->func->formParms != NULL)
            addLocParms(tmp->func->formParms);
         printf(".text\n");
         printf("### enter %s\n", tmp->func->id->id);
         if (strcmp(tmp->func->id->id, "main") != 0)
            printf("_%s:\n", tmp->func->id->id);
         else
            printf("%s:\n", tmp->func->id->id);
         printf("# START PROLOGUE ###########################################\n");
         printf("    la  $sp, -8($sp)  # allocate space for old $fp and $ra\n");
         printf("    sw  $fp, 4($sp)   # Save old frame pointer\n");
         printf("    sw  $ra, 0($sp)   # save return address\n");
         printf("    la  $fp, 0($sp)   # set up frame pointer\n");
         printf("    la  $sp, %d($sp)   # allocate stack frame\n", tmp->func->stackSize);
         printf("# END PROLOGUE #############################################\n\n");
         if (tmp->func->dcl != NULL)
            addLocVars(tmp->func->dcl);
         traverseStmt(tmp->func->stmt);
         clearLocTab();
      }
         
      tmp = tmp->next;
   }
}

void clearLocTab(){
   locTable *tmp;
   while (locals != NULL){
      tmp = locals->next;
      free(locals->id);
      free(locals);
      locals = tmp;
   }
   tmpNum = 0;
}

void addLocParms(globNode *params){
   globNode *tmp = params;
   int offset = 8;
   while(tmp != NULL){
      locTable *local = malloc(sizeof(locTable));
      local->id = tmp->dcl->id;
      local->offset = offset;
      local->type = tmp->dcl->type;

      local->next = locals;
      locals = local;

      offset += 4;
      tmp = tmp->next;
   }
}

void addLocVars(globNode *vars){
   globNode *tmp = vars;
   while(tmp != NULL){
      locTable *local = malloc(sizeof(locTable));
      local->id = tmp->dcl->id;
      local->offset = tmp->dcl->offset;
      local->type = tmp->dcl->type;

      local->next = locals;
      locals = local;

      tmp = tmp->next;
   }
}

void traverseStmt(stmtNode *stmts){
   stmtNode *tmp = stmts;
   int numParms;
   char *labThen, *labElse;
   char *labTop, *labEval;
   char *labAfter;
   while (tmp != NULL){
      switch (tmp->type){
         case STMTCALL:
            numParms = traverseParms(tmp->funcCall->actParms);
            if(strcmp(tmp->funcCall->id,"main") != 0){
               printf("# call _%s, %d\n", tmp->funcCall->id, numParms);
               printf("    jal  _%s\n", tmp->funcCall->id);
               printf("    la $sp, %d($sp)   # Adjust stack from params\n\n", 4*numParms);
            }
            else{
               printf("# call %s, %d\n", tmp->funcCall->id, numParms);
               printf("    jal  %s\n", tmp->funcCall->id);
               printf("    la $sp, %d($sp)   # Adjust stack from params\n\n", 4*numParms);
            }
            break;
         case ASSG:
//            printf("$tmp%d := ", tmpNum);
            printf("# assg %s\n", tmp->assgNode->LHS->id);
            traverseExprNode(tmp->assgNode->RHS, NULL, NULL);
            //printf("    sw  $t0, _%s\n\n", tmp->assgNode->LHS->id);
            storeId(tmp->assgNode->LHS, tmp->assgNode->RHS);
            break;
         case RET:
            traverseExprNode(tmp->ret->expr, NULL, NULL);
            printf("    ### return\n");
            if (tmp->ret->expr != NULL){
               printf("    lw  $v0, %d($fp) # store return value\n", tmp->ret->expr->offset);
            }
            printf("    la $sp, 0($fp)   # deallocate locals\n");
            printf("    lw $ra, 0($sp)   # Restore RA\n");
            printf("    lw $fp, 4($sp)   # Restore SP\n");
            printf("    la $sp, 8($sp)   # Restore FP\n");
            printf("    jr $ra           # Return!\n\n");
            break;
         case IFSTMT:
            labThen  = newLabel();
            labElse  = newLabel();
            labAfter = newLabel();
            /* evaluate conditional */
            traverseExprNode(tmp->ifNode->cond, labThen, labElse);
            printf("%s:\n", labThen);
            /* evaluate if block */
            traverseStmt(tmp->ifNode->stmtThen);
            newGoto(labAfter);
            printf("%s:\n", labElse);
            traverseStmt(tmp->ifNode->stmtElse);
            printf("%s:\n", labAfter);
            break;
         case WHILESTMT:
            labTop   = newLabel();
            labEval  = newLabel();
            labAfter = newLabel();
            newGoto(labEval);
            printf("%s:\n", labTop);
            traverseStmt(tmp->whileNode->stmt);
            printf("%s:\n", labEval);
            traverseExprNode(tmp->whileNode->cond, labTop, labAfter);
            printf("%s:\n", labAfter);
            break;
         case FORSTMT:
            labTop   = newLabel();
            labEval  = newLabel();
            labAfter = newLabel();
            traverseStmt(tmp->forNode->init);
            newGoto(labEval);
            printf("%s:\n", labTop);
            traverseStmt(tmp->forNode->stmt);
            traverseStmt(tmp->forNode->inc);
            printf("%s:\n", labEval);
            if(tmp->forNode->cond != NULL)
               traverseExprNode(tmp->forNode->cond, labTop, labAfter);
            else{
               printf("# goto %s\n", labTop);
               printf("    j %s\n", labTop);
            }
            printf("%s:\n", labAfter);
            break;
         case EMPTYSTMT:
            break;
      }
      tmp = tmp->next;
   }
}

void newGoto(char *label){
   printf("# goto %s\n", label);
   printf("    j  %s\n", label);
}

char *newLabel(){
   labTable *label = malloc(sizeof(labTable));
   label->labName = malloc(15 * sizeof(char));
   sprintf(label->labName, "$label%d", labNum);
   labNum++;
   label->next = labels;
   labels = label;
   return label->labName;
}

void traverseExprNode(exprNode *node, char *trueDest, char *falseDest){
   if (node == NULL)
      return;
   int numParms;
   exprNode *parm;
   switch (node->type){
      case TERM:
         //addLocTmp(node);
         //loadTmp(node);
         getTerm(node);
         break;
      case EXPRID:
         getId(node);
         break;
      case BIN:
         evalBin(node);
         break;
      case REL:
         evalRel(node, trueDest, falseDest);
         break;
      case LOG:
         evalLog(node, trueDest, falseDest);
         break;
      case UNAOP:
         evalUnaExpr(node, trueDest, falseDest);
         break;
      case EXPRCALL:
         parm = node->funcCall->actParms;
         /* Don't look at the parameters if they've already been seen.
          * This is to prevent nested function calls from calling their nested functions over and over again */
         if (parm != NULL && parm->touched == 0){
            numParms = traverseParms(node->funcCall->actParms);
            if(strcmp(node->funcCall->id,"main") != 0){
               printf("# call _%s, %d\n", node->funcCall->id, numParms);
               printf("    jal  _%s\n", node->funcCall->id);
               printf("    la $sp, %d($sp)   # Adjust stack from params\n\n", 4*numParms);
            }
            else{
               printf("# call %s, %d\n", node->funcCall->id, numParms);
               printf("    jal  %s\n", node->funcCall->id);
               printf("    la $sp, %d($sp)   # Adjust stack from params\n\n", 4*numParms);
            }
            printf("    sw  $v0, %d($fp)\n\n", node->offset);
         }
         break;
   }
   traverseExprNode(node->nextExpr, trueDest, falseDest);
}

void evalUnaExpr(exprNode *node, char *trueDest, char *falseDest){
   unaNode *una = node->unaNode;
   opType type = una->op;;

   switch (type){
      case UNAMINUS:
         traverseExprNode(node->unaNode->expr, trueDest, falseDest);
         printf("    lw  $t0, %d($fp)\n", node->unaNode->expr->offset);
         printf("    neg $t1, $t0\n");
         printf("    sw  $t1, %d($fp)\n\n", node->offset);
         break;
      case UNANOT:
         traverseExprNode(node->unaNode->expr, falseDest, trueDest);
         break;
   }
}

void evalLog(exprNode *node, char *trueDest, char *falseDest){
   char *newDest = newLabel();
   opType type = node->opNode->op;
   switch (type){
      case LOGAND:
         traverseExprNode(node->opNode->left, newDest, falseDest);
         printf("%s:\n", newDest);
         traverseExprNode(node->opNode->right, trueDest, falseDest);
         break;
      case LOGOR:
         traverseExprNode(node->opNode->left, trueDest, newDest);
         printf("%s:\n", newDest);
         traverseExprNode(node->opNode->right, trueDest, falseDest);
         break;
   }
}

void evalRel(exprNode *node, char *trueDest, char *falseDest){
   if (node == NULL)
      return;

   opNode *op = node->opNode;
   opType type = op->op;
   traverseExprNode(op->left, trueDest, falseDest);
   traverseExprNode(op->right, trueDest, falseDest);

   switch (type){
      case RELEQ:
         printf("# if %s == %s goto %s\n", op->left->tmp, op->right->tmp, trueDest);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    beq  $t0, $t1, %s\n", trueDest);
         newGoto(falseDest);
         break;
      case RELNE:
         printf("# if %s != %s goto %s\n", op->left->tmp, op->right->tmp, trueDest);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    bne  $t0, $t1, %s\n", trueDest);
         newGoto(falseDest);
         break;
      case RELLE:
         printf("# if %s <= %s goto %s\n", op->left->tmp, op->right->tmp, trueDest);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    ble  $t0, $t1, %s\n", trueDest);
         newGoto(falseDest);
         break;
      case RELLT:
         printf("# if %s < %s goto %s\n", op->left->tmp, op->right->tmp, trueDest);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    blt  $t0, $t1, %s\n", trueDest);
         newGoto(falseDest);
         break;
      case RELGE:
         printf("# if %s >= %s goto %s\n", op->left->tmp, op->right->tmp, trueDest);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    bge  $t0, $t1, %s\n", trueDest);
         newGoto(falseDest);
         break;
      case RELGT:
         printf("# if %s > %s goto %s\n", op->left->tmp, op->right->tmp, trueDest);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    bgt  $t0, $t1, %s\n", trueDest);
         newGoto(falseDest);
         break;
   }
}

void evalBin(exprNode *node){
   opNode *op = node->opNode;
   opType type = op->op;
   traverseExprNode(op->left, NULL, NULL);
   traverseExprNode(op->right, NULL, NULL);

   switch (type){
      case PLUS:
         printf("# %s = %s + %s\n", node->tmp, op->left->tmp, op->right->tmp);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    add  $t2, $t0, $t1\n");
         printf("    sw  $t2, %d($fp)\n", node->offset);
         break;
      case MINUS:
         printf("# %s = %s - %s\n", node->tmp, op->left->tmp, op->right->tmp);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    sub  $t2, $t0, $t1\n");
         printf("    sw  $t2, %d($fp)\n", node->offset);
         break;
      case MULT:
         printf("# %s = %s * %s\n", node->tmp, op->left->tmp, op->right->tmp);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    mul  $t2, $t0, $t1\n");
         printf("    sw  $t2, %d($fp)\n", node->offset);
         break;
      case DIV:
         printf("# %s = %s / %s\n", node->tmp, op->left->tmp, op->right->tmp);
         printf("    lw  $t0, %d($fp)\n", op->left->offset);
         printf("    lw  $t1, %d($fp)\n", op->right->offset);
         printf("    div  $t2, $t0, $t1\n");
         printf("    sw  $t2, %d($fp)\n", node->offset);
         break;
   }
}

//locTable *loadTmp(exprNode *expr){
//   termNode *term = expr->termNode;
//   printf("# %s = %d\n", expr->tmp, term->iVal);
//   printf("    lui  $t2, %d\n", expr->termNode->iVal >> 16);
//   printf("    ori  $t2, %d\n", expr->termNode->iVal % (1 << 16));
//   printf("    sw   $t2, %d($fp)\n\n", expr->offset);
//   locTable *tmp = malloc(sizeof(locTable));
//   tmp->id = strdup(expr->tmp);
//   tmp->iVal = term->iVal;
//   tmp->next = locals;
//   tmp->offset = expr->offset;
//
//   printf("    la  $sp, -4($sp) # push %s onto stack\n", tmp->id);
//   printf("# %s = %d\n", tmp->id, tmp->iVal);
//   printf("    lui  $t2, %d\n", tmp->iVal >> 16);
//   printf("    ori  $t2, %d\n", tmp->iVal % (1 << 16));
//   printf("    sw   $t2, %d($fp)\n\n", expr->offset);
//   locals = tmp;
//   return tmp;
//}

void getTerm(exprNode *node){
   char *id;
   switch (node->termNode->type){
      case INTEGER:
         printf("# %s = %d\n", node->tmp, node->termNode->iVal);
         printf("    lui  $t0, %d\n", node->termNode->iVal >> 16);
         printf("    ori  $t0, $t0, %d\n", node->termNode->iVal % (1<<16));
         printf("    sw   $t0, %d($fp)\n\n", node->offset);
         break;
      case CHARACTER:
         printf("# %s = %d\n", node->tmp, node->termNode->iVal);
         printf("    lui  $t0, 0\n");
         printf("    ori  $t0, $t0, %d\n", node->termNode->iVal % (1<<8));
         printf("    sb   $t0, %d($fp)\n\n", node->offset);
         break;
      case STRING:
         id = findStr(node->termNode->strcon);
         if (id == NULL){
            /* add string to string table if it doesn't already exist*/
            id = addStr(node->termNode->strcon);
            printf("# %s = %s\n", id, node->termNode->strcon);
            printf(".data\n");
            printf("%s:  .asciiz \"%s\"\n", id, node->termNode->strcon);
            printf(".align 2\n");
            printf(".text\n");
         }
         printf("    la  $t0, %s\n", id);
         break;
   }
}

char *addStr(char *str){
   strNum++;
   strTable *tmp = malloc(sizeof(strTable));
   tmp->str = strdup(str);
   tmp->id = malloc(10 * sizeof(char));
   sprintf(tmp->id, "$str%d", strNum);
   tmp->next = strings;
   strings = tmp;
   return tmp->id;
}

char *findStr(char *str){
   strTable *tmp = strings;
   while (tmp->next != NULL){
      if(strcmp(str, tmp->str) == 0)
         return tmp->id;
      tmp = tmp->next;
   }
   return NULL;
}

void storeId(idNode *id, exprNode *RHS){
   locTable *loc = findLoc(id->id);
   Type rhsType;
   if (RHS->type == TERM)
      rhsType = RHS->termNode->type;
   else if (RHS->type == EXPRID)
      rhsType = RHS->idNode->type;
   else
      rhsType = INTEGER;
   printf("# %s = %s\n", id->id, RHS->tmp);
   if (loc == NULL){
      symTabNode *glob = findId(id->id);
      idNode *idNode = RHS->idNode;
      if (rhsType == CHARACTER){
         printf("    lb  $t0, %d($fp)\n", RHS->offset);
      }
      else {
         printf("    lw  $t0, %d($fp)\n", RHS->offset);
      }
      if (glob->type == INTEGER){
         printf("    sw  $t0, _%s\n\n", id->id);
      }
      else if (glob->type == INTARRAY){
         traverseExprNode(id->expr, NULL, NULL);
         printf("    la  $t0, _%s      # get A[0]\n", id->id);
         printf("    lw  $t4, %d($fp)  # get index value\n", id->expr->offset);
         printf("    sll $t2,  $t4, 2  # multiply index by 4\n");
         printf("    add $t3, $t0, $t2 # get addr of A[i]\n");
         printf("    lw  $t0, %d($fp)  # get value of RHS\n", RHS->offset);
         printf("    sw  $t0, 0($t3)   # store value of RHS at A[i]\n\n");
      }
      else if (glob->type == STRING){
         traverseExprNode(id->expr, NULL, NULL);
         printf("    la  $t0, _%s      # get A[0]\n", id->id);
         printf("    lw  $t4, %d($fp)  # get index value\n", id->expr->offset);
         printf("    add $t3, $t0, $t4 # get addr of A[i]\n");
         printf("    lb  $t0, %d($fp)  # get value of RHS\n", RHS->offset);
         printf("    sb  $t0, 0($t3)   # store value of RHS at A[i]\n\n");
      }
      else if (glob->type == CHARACTER){
         printf("    sb  $t0, _%s\n\n", id->id);
      }
      else if (glob->type == CHARACTER){
         printf("    sb  $t0, _%s\n\n", id->id);
      }
   } else{
      switch (loc->type){
         case INTEGER:
            if (rhsType == CHARACTER)
               printf("    lb  $t0, %d($fp)\n", RHS->offset);
            else
               printf("    lw  $t0, %d($fp)\n", RHS->offset);
            printf("    sw $t0, %d($fp)\n\n", loc->offset);
            break;
         case CHARACTER:
            if (rhsType == CHARACTER)
               printf("    lb  $t0, %d($fp)\n", RHS->offset);
            else
               printf("    lw  $t0, %d($fp)\n", RHS->offset);
            printf("    sb $t0, %d($fp)\n\n", loc->offset);
            break;
         case INTARRAY:
         traverseExprNode(RHS, NULL, NULL);
         traverseExprNode(id->expr, NULL, NULL);
         /* if the offset is < 0, it must be a local array; else, it must be an array passed as a parameter */
            printf("    la  $t0, %d($fp)      # get addr of 1st elem\n", loc->offset);
         if (id->expr != NULL){
               printf("    lw  $t4, %d($fp)  # get value of index\n",
                               id->expr->offset);
               printf("    sll $t2, $t4, 2   # multiply index by four\n");
               printf("    add $t3, $t0, $t2 # add index to addr of 1st elem\n");
               printf("    lw  $t4, %d($fp)   # get value of RHS\n", RHS->offset);
               printf("    sw  $t4, 0($t3)  # store value of RHS in elem\n\n");
            }
            break;
         case STRING:
         traverseExprNode(RHS, NULL, NULL);
         traverseExprNode(id->expr, NULL, NULL);
         /* if the offset is < 0, it must be a local array; else, it must be an array passed as a parameter */
            printf("    la  $t0, %d($fp)      # get addr of 1st elem\n", loc->offset);
         if (id->expr != NULL){
               printf("    lw  $t4, %d($fp)  # get value of index\n",
                               id->expr->offset);
               printf("    add $t3, $t0, $t4 # add index to addr of 1st elem\n");
               printf("    lw  $t4, %d($fp)   # get value of RHS\n", RHS->offset);
               printf("    sb  $t4, 0($t3)  # store value of RHS in elem\n\n");
            }
            break;
      }
   }
}
void getId(exprNode *expr){
   locTable *loc = findLoc(expr->idNode->id);
   /******************
    * Handle globals
    ******************/
   if (loc == NULL){
      symTabNode *glob = findId(expr->idNode->id);

      if (glob->type == INTEGER){
         printf("    la  $t0, _%s\n", expr->idNode->id);
         printf("    lw  $t0, 0($t0)\n");
         printf("    sw  $t0, %d($fp)\n", expr->offset);
      }
      else if (glob->type == CHARACTER){
         printf("    la  $t0, _%s\n", expr->idNode->id);
         printf("    lb  $t0, 0($t0)\n");
         printf("    sb  $t0, %d($fp)\n", expr->offset);
      }
      else if (glob->type == INTARRAY && expr->idNode->expr != NULL){
         traverseExprNode(expr->idNode->expr, NULL, NULL);
         printf("    la  $t0, _%s      # get addr of 1st elem\n", expr->idNode->id);
         printf("    lw  $t4, %d($fp)  # get value of index\n", expr->idNode->expr->offset);
         printf("    sll $t2, $t4, 2   # multiply index by four\n");
         printf("    add $t3, $t0, $t2 # get addr of A[i]\n");
         printf("    lw  $t3, 0($t3)   # get value of elem\n");
         printf("    sw  $t3, %d($fp)  # store elem\n", expr->offset);
      }
      else if (glob->type == STRING && expr->idNode->expr != NULL){
         traverseExprNode(expr->idNode->expr, NULL, NULL);
         printf("    la  $t0, _%s      # get addr of 1st elem\n", expr->idNode->id);
         printf("    lw  $t4, %d($fp)  # get value of index\n", expr->idNode->expr->offset);
         printf("    add $t3, $t0, $t4 # get addr of A[i]\n");
         printf("    lb  $t3, 0($t3)  # get value of elem\n");
         printf("    sb  $t3, %d($fp)   # store elem\n\n", expr->offset);
      }
      /* if a variable is not an int or char, and its expr node is null, then it must be a whole array */
      else{
         printf("    la  $t0, _%s\n", expr->idNode->id);
      }
   /******************
    * Handle locals
    ******************/
   } else{
      switch (loc->type){
         case INTEGER:
            printf("    lw  $t0, %d($fp)\n", loc->offset);
            printf("    sw  $t0, %d($fp)\n\n", expr->offset);
            break;
         case CHARACTER:
            printf("    lb  $t0, %d($fp)\n", loc->offset);
            printf("    sb  $t0, %d($fp)\n\n", expr->offset);
            break;
         case INTARRAY:
         traverseExprNode(expr->idNode->expr, NULL, NULL);
         /* if the offset is < 0, it must be a local array, and we can load the address directly; else, it must be an array passed as a parameter, and we need to use lw to get the address */
         if(loc->offset < 0)
            printf("    la  $t0, %d($fp)      # get addr of 1st elem\n", loc->offset);
         else
            printf("    lw  $t0, %d($fp)      # get addr of 1st elem\n", loc->offset);
         if (expr->idNode->expr != NULL){
               printf("    lw  $t4, %d($fp)  # get value of index\n",
                               expr->idNode->expr->offset);
               printf("    sll $t2, $t4, 2   # multiply index by four\n");
               printf("    add $t3, $t0, $t2 # add index to addr of 1st elem\n");
               printf("    lw  $t3, 0($t3)   # get value of elem\n");
               printf("    sw  $t3, %d($fp)  # store elem\n", expr->offset);
            }
            break;
         case STRING:
         traverseExprNode(expr->idNode->expr, NULL, NULL);
         /* if the offset is < 0, it must be a local array; else, it must be an array passed as a parameter */
         if(loc->offset < 0)
            printf("    la  $t0, %d($fp)      # get addr of 1st elem\n", loc->offset);
         else
            printf("    lw  $t0, %d($fp)      # get addr of 1st elem\n", loc->offset);
            if (expr->idNode->expr != NULL){
               printf("    lw  $t4, %d($fp)  # get value of index\n",
                               expr->idNode->expr->offset);
               printf("    add $t3, $t0, $t4 # add index to addr of 1st elem\n");
               printf("    lb  $t3, 0($t3)   # get value of elem\n");
               printf("    sb  $t3, %d($fp)  # store elem\n", expr->offset);
            }
            break;
      }
   }
}

locTable *findLoc(char *id){
   locTable *tmp = locals;
   while (tmp != NULL) {
      if (strcmp(id,tmp->id) == 0){
         return tmp;
      }
      tmp = tmp->next;
   }
   return NULL;
}

/* Go through parms of function call, push onto stack */
int traverseParms(exprNode *parm){
   if (parm == NULL)
      return 0;
   int numParms;
   parm->touched = 1;
   /* recursively call this function */
   numParms = traverseParms(parm->nextExpr)+1;
   Type type;
   if (parm->type == TERM)
      type = parm->termNode->type;
   else if (parm->type == EXPRID)
      type = parm->idNode->type;
   else if (parm->type == EXPRCALL){
      symTabNode *node = findId(parm->funcCall->id);
      type = node->type;
   }
   else
      type = INTEGER;
   traverseExprNode(parm, NULL, NULL);
   printf("# param %s\n", parm->tmp);
   switch (type){
      case INTARRAY:
         /* if we're passing an element of an array, store the value of the index */
         if (parm->idNode != NULL && parm->idNode->expr != NULL)
            printf("    lw  $t0, %d($fp)\n", parm->offset);
         break;
      case STRING:
         /* if we're passing an element of an array, store the value of the index */
         if (parm->idNode != NULL && parm->idNode->expr != NULL)
            printf("    lb  $t0, %d($fp)\n", parm->offset);
         break;
      case CHARACTER:
         printf("    lb  $t0, %d($fp)\n", parm->offset);
         break;
      default:
         printf("    lw  $t0, %d($fp)\n", parm->offset);

   }
   printf("    la  $sp, %d($sp)  # Move stack down 4\n", -4);
   printf("    sw  $t0, 0($sp)   # Stick param onto stack\n\n");
   return numParms;
}

void codeGen_dcl(dclNode *dcl){
   printf("_%s:\t", dcl->id);
   switch(dcl->type){
      case INTEGER:
         printf(".space 4\n");
         break;
      case CHARACTER:
         printf(".space 1\n");
         printf(".align 2\n");
         break;
      case INTARRAY:
         printf(".space %d\n", 4*dcl->arraySize);
         printf(".align 2\n");
         break;
      case STRING:
         printf(".space %d\n", 1*dcl->arraySize);
         printf(".align 2\n");
         break;
   }
}
