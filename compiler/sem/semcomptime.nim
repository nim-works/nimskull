## Implements checks for whether otherwise valid code can be run at compile
## time or in NimScript contexts.

import
  std/[
    intsets
  ],
  compiler/ast/[
    ast_query,
    ast_types
  ]

type
  CheckCtx = object
    defs: IntSet ## IDs of the locals defined so far. Using a local not present
                 ## in this set is an error

proc checkAux(c: var CheckCtx, n: PNode): PNode =
  template check(n: PNode) =
    result = checkAux(c, n)
    # unwind in case an error is found:
    if result != nil:
      return

  case n.kind
  of nkSym:
    let s = n.sym
    case s.kind
    of skVar, skLet, skForVar, skTemp, skParam, skResult:
      if sfGlobal notin s.flags and s.id notin c.defs:
        # a local not available in the current context
        result = n
    else:
      discard "ignore"

  of nkForStmt:
    for it in forLoopDefs(n):
      c.defs.incl it.sym.id

    check(n[^2])
    check(n[^1])
  of nkIdentDefs, nkVarTuple:
    for it in n.names:
      c.defs.incl it.sym.id

    result = checkAux(c, n[^1])
  of nkExceptBranch:
    # an ``except Error as e`` branch also introduces a local
    if isInfixAs(n[0]):
      c.defs.incl n[0][2].sym.id
  of callableDefs, nkNimNodeLit, nkTypeSection, nkConstSection,
     nkTypeOfExpr, nkMixinStmt, nkBindStmt, nkImportStmt, nkImportExceptStmt,
     nkFromStmt, nkExportStmt:
    discard "ignore declarative nodes"
  of nkWhenStmt:
    # a 'when nimvm' statement; choose the first branch
    result = checkAux(c, n[0][1])
  of nkCast, nkConv, nkHiddenStdConv, nkHiddenSubConv:
    result = checkAux(c, n[1])
  else:
    for it in n.items:
      check(it)

proc check*(n: PNode): PNode =
  ## Given the untransformed AST `n` of a freestanding expression/statement,
  ## looks for access of locals that aren't available in the current compile-
  ## time context. Example:
  ##
  ## .. code-block:: nim
  ##
  ##   var x = 0
  ##   const y = x # <- 'x' cannot be used here
  ##
  ## If such illegal access is found, the first offending node is returned,
  ## 'nil' otherwise.
  var c = CheckCtx()
  result = checkAux(c, n)
