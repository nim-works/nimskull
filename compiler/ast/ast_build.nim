## This module provides simplified procedures for the AST construction

import ast, lineinfos

proc newIf*(cond, body: PNode, orElse: PNode = nil): PNode =
  ## Construct new `if` statement
  result = newTree(nkIfStmt, newTree(nkElifBranch, cond, body))
  if not isNil(orElse):
    result.add newTree(nkElse, orElse)

proc newWhen*(cond, body: PNode, orElse: PNode = nil): PNode =
  ## Create new `when cond: body` statement
  result = newTree(nkWhenStmt, newTree(nkElifBranch, cond, body))
  if not isNil(orElse):
    result.add newTree(nkElse, orElse)

proc newBreak*(target: PNode = newTree(nkEmpty)): PNode =
  ## Create new `break target` statement
  newTree(nkBreakStmt, target)

proc newContinue*(target: PNode = newTree(nkEmpty)): PNode =
  ## Create new `continue target` statement
  newTree(nkContinueStmt, target)

proc newWhile*(expr: PNode, body: varargs[PNode]): PNode =
  ## Create new `while expr: body` statement
  result = newTree(
    nkWhileStmt, expr, newTree(nkStmtList, body))

proc newExprColon*(lhs, rhs: PNode): PNode =
  ## Create new `lhs = rhs` expression
  newTree(nkExprColonExpr, lhs, rhs)

proc newExprEq*(lhs, rhs: PNode): PNode =
  ## Create new `lhs = rhs` expression
  newTree(nkExprEqExpr, lhs, rhs)

proc newDot*(self, id: PNode): PNode =
  ## Create new dot expression
  newTree(nkDotExpr, self, id)

proc newPar*(arg: PNode): PNode = newTree(nkPar, arg)

proc newSet*(elements: varargs[PNode]): PNode = newTree(nkCurly, elements)
proc newDot*(lhs, rhs: PNode): PNode = newTree(nkDotExpr, lhs, rhs)
proc newBracketExpr*(lhs: PNode, rhs: varargs[PNode]): PNode =
  ## Create new `lhs[rhs]` expression
  result = newTree(nkBracketExpr, lhs)
  for arg in rhs:
    result.add arg

proc newReturn*(expr: PNode): PNode =
  ## Create new `return` statement
  nkReturnStmt.newTree(@[expr])

proc newBlock*(args: varargs[PNode]): PNode =
  ## Construct `block` statement with `args` as content
  newTree(nkBlockStmt, newTree(nkEmpty), newTree(nkStmtList, args))

proc newCase*(expr: PNode): PNode =
  ## Construc empty case statement
  newTree(nkCaseStmt, expr)

proc newTry*(expr: PNode): PNode =
  ## Construct `try` statement
  newTree(nkTryStmt, expr)

proc newStmtList*(nodes: varargs[PNode]): PNode =
  ## Create new statementlist
  newTree(nkStmtList, nodes)

proc newFor*(forVars: openArray[PNode], expr: PNode, body: varargs[PNode]): PNode =
  ## construct `for <forVars> in <expr>: body` statement with multiple variables
  newTree(nkForStmt, @forVars & expr & newStmtList(body))

proc newFor*(forvar, expr: PNode, body: varargs[PNode]): PNode =
  ## Construct for statement with a single variable
  newTree(nkForStmt, forvar, expr, newStmtList(body))


