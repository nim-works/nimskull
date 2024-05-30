## Implements elimination of unreachable statements and expression within an
## AST already processed by early ``transf``. The transformation makes sure
## that a non-returning statement is not immediately followed by other code.
##
## For example, for:
##
## .. code-block:: nim
##
##   return 1
##   echo "a"
##
## The ``echo`` statement needs to be removed. This makes further processing
## easier, since detecting whether a block of code returns is now possible by
## inspecting just the trailing AST node.
##
## For later inspection, all non-returning statements (such as break, return,
## etc.) have the 'void' type assigned to them.

import
  compiler/ast/[
    ast_types,
    ast_query,
    ast,
    lineinfos
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/utils/[
    idioms
  ]

type
  PassContext = object
    blocks: seq[tuple[label: PSym, used: bool]]
    voidType: PType

iterator mitems(x: PNode): var PNode =
  for i in 0..<x.len:
    yield x[i]

iterator mpairs(x: PNode): (int, var PNode) =
  for i in 0..<x.len:
    yield (i, x[i])

func doesntReturn(x: PNode): bool =
  x.typ != nil and x.typ.kind == tyVoid

proc process(c: var PassContext, n: PNode): PNode =
  ## Transforms a single tree layer. The AST is mutated in-place, and the
  ## transformed node is returned.
  ##
  ## For signaling that the statement/expression doesn't return, it has the
  ## 'void' type assigned to it; non-returning expressions are turned into
  ## statements.
  template recurse(n: PNode): PNode =
    process(c, n)

  case n.kind
  of nkWithoutSons, nkSymChoices, nkNimNodeLit:
    result = n # nothing to do
  of nkCast, nkHiddenStdConv, nkHiddenSubConv, nkConv:
    # single operand expressions with the operand in the second slot
    let x = recurse(n[1])
    if doesntReturn(x):
      result = x
    else:
      result = n
      result[1] = x

  of nkHiddenAddr, nkAddr, nkHiddenDeref, nkDerefExpr, nkStringToCString,
     nkCStringToString, nkObjDownConv, nkObjUpConv, nkCheckedFieldExpr,
     nkReturnStmt, nkDiscardStmt, nkYieldStmt, nkRaiseStmt:
    # statements and expressions where only the first operand is relevant
    let x = recurse(n[0])
    if doesntReturn(x):
      result = x
    else:
      result = n
      result[0] = x

      if n.kind in {nkReturnStmt, nkRaiseStmt}:
        # mark as noreturn
        result.typ = c.voidType
  of nkAsgn, nkFastAsgn, nkBracketExpr, nkDotExpr, nkRange:
    # statements/expressions with two operands
    let lhs = recurse(n[0])
    if doesntReturn(lhs):
      result = lhs
    else:
      let rhs = recurse(n[1])
      if doesntReturn(rhs):
        result = nkStmtList.newTreeIT(n.info, c.voidType,
          nkDiscardStmt.newTreeI(lhs.info, lhs),
          rhs)
      else:
        result = n
        result[0] = lhs
        result[1] = rhs
  of nkStmtList:
    result = n
    for i, it in result.mpairs:
      it = recurse(it)
      if doesntReturn(it):
        result.sons.setLen(i + 1)
        result.typ = c.voidType
        return

  of nkStmtListExpr:
    result = n
    for i in 0..<n.len-1:
      result[i] = recurse(n[i])
      if doesntReturn(result[i]):
        result.transitionSonsKind(nkStmtList)
        # cut off the remaining statements
        result.sons.setLen(i + 1)
        result.typ = c.voidType
        return

    result[^1] = recurse(n[^1])
    if doesntReturn(result[^1]):
      # ends in a statement
      result.transitionSonsKind(nkStmtList)
      result.typ = c.voidType
  of nkCallKinds, nkBracket, nkCurly, nkClosure, nkTupleConstr, nkChckRange,
     nkChckRange64, nkChckRangeF, nkAsmStmt:
    # some call-like operation with left-to-right evaluation. If an operand
    # doesn't return, the remaining operands plus the operation itself are
    # removed
    for i, it in n.mpairs:
      # destructively omit the expr-colon-expr; it's not needed beyond this
      # point anyway
      it = recurse(it.skipColon)
      if doesntReturn(it):
        result = n
        result.transitionSonsKind(nkStmtList)
        result.typ = c.voidType
        # cut off the remaining expressions/statements; they're dead code
        result.sons.setLen(i + 1)
        # turn the expressions so far into statements by wrapping them in a
        # 'discard' statement
        for i in 0..i:
          # the argument might be a 'void' (unit, really) expression, no
          # discard must be used then
          if result[i].typ != nil:
            result[i] = newTreeI(nkDiscardStmt, n.info, result[i])

        return

    result = n
    if result.kind in nkCallKinds and result[0].kind == nkSym and
       result[0].sym.kind in routineKinds and
       sfNoReturn in result[0].sym.flags:
      # calls to noreturn procedures don't return
      result.typ = c.voidType
  of nkObjConstr:
    # similar to the call-like handling
    for i in 1..<n.len:
      n[i][1] = recurse(n[i][1])
      if doesntReturn(n[i][1]):
        # turn the operands so far into a valid statement list:
        result = newNodeIT(nkStmtList, n.info, c.voidType, i - 1)
        for j in 1..i:
          result[j - 1] = newTreeI(nkDiscardStmt, n[j][1].info, n[j][1])
        return

    result = n
  of nkLetSection, nkVarSection:
    for i, it in n.pairs:
      assert it.kind in {nkIdentDefs, nkVarTuple}
      let rhs = it[^1]
      if doesntReturn(rhs):
        if i == 0:
          return rhs # discard the rest
        else:
          # cut off the identdefs past and including the current one
          n.sons.setLen(i)
          # wrap the section in a statement list and append the non-returning
          # expression
          result = newTreeIT(nkStmtList, n.info, c.voidType, n, rhs)
          return

    result = n
  of nkBlockExpr, nkBlockStmt:
    c.blocks.add (n[0].sym, false) # push a new block
    let body = recurse(n[1])
    let info = c.blocks.pop() # pop the block again

    result = n
    if doesntReturn(body):
      # it's a statement
      result.transitionSonsKind(nkBlockStmt)
      if not info.used:
        # the block is never broken out of and the body doesn't return -> the
        # block doesn't return
        result.typ = c.voidType

    result[1] = body
  of nkIfExpr, nkIfStmt:
    result = n
    var exits = 0 ## number of branches that return
    for i, it in result.pairs:
      case it.kind
      of nkElifBranch, nkElifExpr:
        let cond = recurse(it[0])
        if doesntReturn(cond):
          # the condition expression doesn't return; everything that follows
          # is dead code
          if i == 0:
            return cond
          else:
            # turn into an else branch and cut off the remaining branches
            result[i] = newTreeI(nkElse, cond.info, cond)
            result.sons.setLen(i + 1)
            break

        it[1] = recurse(it[1])
        exits += ord(not doesntReturn(it[1]))
      of nkElse, nkElseExpr:
        it[0] = recurse(it[0])
        exits += ord(not doesntReturn(it[0]))
      else:
        unreachable(it.kind)

    if exits == 0 and result[^1].kind in {nkElse, nkElseExpr}:
      # exhaustive 'if' statement/expression and no branch returns -> it's a
      # noreturn statement
      result.transitionSonsKind(nkIfStmt)
      result.typ = c.voidType
  of nkCaseStmt:
    let x = recurse(n[0])
    if doesntReturn(x):
      return x # the body is dead code

    result = n
    result[0] = x

    # process all branches and count the number of exits:
    var exits = 0
    for i in 1..<n.len:
      result[i][^1] = recurse(n[i][^1])
      exits += ord(not doesntReturn(result[i][^1]))

    if exits == 0:
      # none of the branches return -> the case doesn't return
      result.typ = c.voidType
  of nkTryStmt, nkHiddenTryStmt:
    result = n
    var exits = 0
    for it in result.mitems:
      case it.kind
      of nkExceptBranch:
        it[^1] = recurse(it[^1])
        exits += ord(not doesntReturn(it[^1]))
      of nkFinally:
        it[0] = recurse(it[0])
      else:
        # must be the main body
        it = recurse(it)
        exits += ord(not doesntReturn(it))

    if exits == 0 or
       (result[^1].kind == nkFinally and doesntReturn(result[^1][0])):
      # the try/except or intercepting finally doesn't exit
      result.typ = c.voidType
  of nkWhileStmt:
    result = n
    result[^1] = recurse(n[1])
    result.typ = c.voidType # ``while true`` statements never return
  of nkBreakStmt:
    # mark the break target as being broken out of:
    for it in c.blocks.mitems:
      if it.label.id == n[0].sym.id:
        it.used = true
        break

    result = n
    result.typ = c.voidType # mark as noreturn
  of nkWhenStmt:
    result = n
    # process both branches of the ``when nimvm`` statement/expression
    result[0][1] = recurse(n[0][1])
    result[1][0] = recurse(n[1][0])
    if doesntReturn(result[0][1]) and doesntReturn(result[1][0]):
      # if both branches don't return, neither does the 'when'
      result.typ = c.voidType
    # XXX: if only one branches doesn't return, collapsing the 'when' to the
    #      one that doesn't results in the unreachable code persisting
  of nkPragmaBlock:
    result = n
    result[1] = recurse(n[1])
    if doesntReturn(result[1]):
      result.typ = c.voidType
  of callableDefs, nkConstSection, nkTypeSection, nkBindStmt, nkMixinStmt,
     nkIncludeStmt, nkImportStmt, nkImportExceptStmt, nkFromStmt, nkExportStmt,
     nkExportExceptStmt, nkPragma:
    # ignore declarative statements
    result = n
  else:
    unreachable(n.kind)

proc eliminateUnreachable*(graph: ModuleGraph, n: PNode): PNode =
  ## Entry point into the pass. Removes all unreachable statements/expression,
  ## making sure that a non-returning statement is always the last statement
  ## in a block of code.
  var c = PassContext(voidType: graph.getSysType(unknownLineInfo, tyVoid))
  result = process(c, n)
