import
  compiler / ast / [ lineinfos, renderer, ast, ]


const
  DynamicSize = {nkBracket, nkCurly, nkRecList, nkTupleTy, nkPragma,
      nkGenericParams}
    ## syntax where an arbitrary number of children is allowed (including none)
  WithBody = callableDefs +
      {nkElifExpr, nkElifBranch, nkElse, nkElseExpr, nkWhileStmt, nkForStmt,
       nkBlockExpr, nkBlockStmt, nkPragmaBlock, nkOfBranch}
    ## syntax with a fixed number of children, where the last child represents
    ## a body (statement or expression)
  WithBodyReplaceable = callableDefs +
      {nkWhileStmt, nkForStmt, nkBlockExpr, nkBlockStmt, nkPragmaBlock}
    ## syntax that may be replace with its body (i.e., the last child)
  FixedFirstChild = {nkCall, nkCurlyExpr, nkBracketExpr, nkObjConstr} -
      {nkPostFix}
    ## syntax where the child has to always be present
  ListLike = {nkTupleConstr, nkTableConstr, nkVarSection, nkLetSection,
      nkConstSection, nkTypeSection, nkStmtList, nkStmtListExpr,
      nkFormalParams, nkBindStmt, nkMixinStmt, nkEnumTy, nkUsingStmt,
      nkImportStmt}
    ## syntax that must always have at least one element
  IfLike = {nkIfExpr, nkIfStmt, nkWhenStmt, nkRecWhen}
    ## syntax that has branches (e.g., `nkElse`, `nkElifBranch`, etc.) as children


proc numSteps*(n: PNode): int =
  ## Computes the number of distinct single modifications (i.e., removing a
  ## child node, replacing a child node, etc.) that can be performed
  ## for AST `n` (including its sub-trees).
  proc count(n: PNode): int =
    for it in n.items:
      result += numSteps(it)

  case n.kind
  of nkWithoutSons:
    # no steps are possible
    result = 0
  of nkIdentDefs:
    # the identdefs must always have at least one element
    result = count(n) + (if n.len > 3: n.len - 2 else: 0) +
      ord(n[^1].kind != nkEmpty)
  of nkConstDef:
    # similar to an identdefs, but the value expression cannot be removed
    result = count(n) + (if n.len > 3: n.len - 2 else: 0)
  of DynamicSize:
    result = count(n) + n.len
  of WithBody:
    result = count(n) + ord(n[^1].kind != nkEmpty) +
      ord(n.kind in WithBodyReplaceable)
  of nkCaseStmt, nkRecCase:
    # the selector must be kept, but replacing with branch bodies is possible
    result = count(n) + (n.len - 1) + (n.len - 1)
  of FixedFirstChild:
    # the first node must always stay
    result = count(n) + n.len - 1
  of ListLike:
    # no removals are possible when the list has a single element
    result = count(n) + (if n.len > 1: n.len else: 0)
  of IfLike:
    # the statement/expression can also be reduced by replacing it with
    # one of the bodies
    result = count(n) + (if n.len > 1: n.len else: 0) + n.len
  of nkCommand:
    result = count(n) + (if n.len > 2: (n.len - 1) else: 0)
  of nkPostfix:
    # the postfix can be replaced with its body
    result = count(n) + 1
  of nkPragmaExpr:
    result = count(n) + 1
  else:
    # everything else has to keep all their children
    result = count(n)


proc reduce*(n: PNode, index: var int): PNode =
  ## Computes the tree corresponding to a step.
  if index < 0:
    return n

  template doStep(): bool =
    let res = index == 0
    dec index
    res

  proc reduceAll(n: PNode, index: var int): PNode =
    # TODO: only copy on write
    result = shallowCopy(n)
    for i, it in n.pairs:
      result[i] = reduce(it, index)

  proc reduceWithRemove(n: PNode, start: int, index: var int): PNode =
    result = newNode(n.kind)
    for i in 0..<start:
      result.add reduce(n[i], index)
    for i in start..<n.len:
      if not doStep():
        result.add reduce(n[i], index)

  case n.kind
  of nkWithoutSons:
    result = n
  of nkIdentDefs, nkConstDef:
    result = reduceAll(n, index)
    if n.len > 3 and index >= 0:
      if index < n.len - 2:
        # remove the given identifier
        result.delSon(index)
      index -= (n.len - 2)
    if n.kind == nkIdentDefs and n[^1].kind != nkEmpty and doStep():
      # remove the value expression
      result[^1] = newNode(nkEmpty)
  of nkCaseStmt, nkRecCase:
    if index < n.len - 1:
      result = n[1 + index][^1]
      index -= (n.len - 1)
    else:
      index -= (n.len - 1)
      result = reduceWithRemove(n, 1, index) # the selector is always present
  of FixedFirstChild:
    result = reduceWithRemove(n, 1, index)
  of WithBody:
    result = reduceAll(n, index)
    if n[^1].kind != nkEmpty:
      if doStep():
        # remove the body
        result[^1] = nkDiscardStmt.newTree(nkEmpty.newNode())
    if n.kind in WithBodyReplaceable and doStep():
      # replace with the body
      result = result[^1]
  of DynamicSize:
    result = reduceWithRemove(n, 0, index)
  of ListLike:
    if n.len > 1:
      result = reduceWithRemove(n, 0, index)
    else:
      result = reduceAll(n, index)
  of IfLike:
    if index < n.len:
      # replace with the body of the given branch
      result = n[index][^1]
      index -= n.len
    else:
      index -= n.len
      if n.len > 1:
        result = reduceWithRemove(n, 0, index)
      else:
        result = reduceAll(n, index)
  of nkCommand:
    if n.len > 2:
      result = reduceWithRemove(n, 1, index)
    else:
      result = reduceAll(n, index)
  of nkPostfix:
    if doStep():
      result = n[1]
    else:
      result = reduceAll(n, index)
  of nkPragmaExpr:
    if doStep():
      result = n[0] # replace with the inner expression
    else:
      result = reduceAll(n, index)
  else:
    result = reduceAll(n, index)


iterator mutations*(n: PNode): PNode =
  for i in 0 ..< numSteps(n):
    var index = i
    yield reduce(n, index)
    assert index < 0 # sanity check
