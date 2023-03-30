#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This include implements the high level optimization pass.

proc hlo(c: PContext, n: PNode): PNode

proc evalPattern(c: PContext, n: PNode): PNode =
  internalAssert(
    c.config,
    n.kind == nkCall and n[0].kind == nkSym,
    "Expected call node")
  # we need to ensure that the resulting AST is semchecked. However, it's
  # awful to semcheck before macro invocation, so we don't and treat
  # templates and macros as immediate in this context.
  
  if n.isError:
    result = n
    return
  
  var original: PNode
  if c.config.hasHint(rsemPattern):
    original = n

  let s = n[0].sym
  result =
    case s.kind
    of skMacro:    semMacroExpr(c, n, s)
    of skTemplate: semTemplateExpr(c, n, s, {efFromHlo})
    else:          unreachable("not a macro/template: " & $s.kind)

  if c.config.hasHint(rsemPattern):
    c.config.localReport(n.info, SemReport(
      kind: rsemPattern,
      ast: n,
      expandedAst: result))


proc applyPatterns(c: PContext, n: PNode): PNode =
  result = n
  # we apply the last pattern first, so that pattern overriding is possible;
  # however the resulting AST would better not trigger the old rule then
  # anymore ;-)

  var hasError = false

  for i in countdown(c.patterns.len-1, 0):
    let pattern = c.patterns[i]
    
    if result.isError:
      return

    if not isNil(pattern):
      if pattern.isError:
        c.config.localReport(pattern.ast)

      let x = applyRule(c, pattern, result)

      if x.isNil:
        discard
      elif x.kind == nkError:
        result = x
      else:
        assert x.kind in {nkStmtList, nkCall}
        
        # better be safe than sorry, so check evalTemplateCounter too:
        inc(c.config.evalTemplateCounter)
        
        if c.config.evalTemplateCounter > evalTemplateLimit:
          globalReport(c.config, n.info, SemReport(
            kind: rsemTemplateInstantiationTooNested))

        # deactivate this pattern:
        c.patterns[i] = nil
        
        if x.kind == nkStmtList:
          assert x.len == 3
          x[1] = evalPattern(c, x[1])
        
          if x[1].isError:
            hasError = true
        
          result = flattenStmts(x)
        else:
          result = evalPattern(c, x)
        
        dec(c.config.evalTemplateCounter)
        # activate this pattern again:
        c.patterns[i] = pattern
  
  if hasError and result.kind != nkError:
    result = c.config.wrapError(result)


proc hlo(c: PContext, n: PNode): PNode =
  inc(c.hloLoopDetector)

  # simply stop and do not perform any further transformations:
  if c.hloLoopDetector > 300:
    return n

  case n.kind
  of nkError:
    result = n
  of nkMacroDef, nkTemplateDef, procDefs:
    # already processed (special cases in semstmts.nim)
    result = n
  else:
    if n.kind in {nkFastAsgn, nkAsgn, nkIdentDefs, nkVarTuple} and
        n[0].kind == nkSym and
        {sfGlobal, sfPure} * n[0].sym.flags == {sfGlobal, sfPure}:
      # do not optimize 'var g {.global} = re(...)' again!
      return n

    result = applyPatterns(c, n)
    
    if result.isError:
      return

    if result == n:
      # no optimization applied, try subtrees:
      for i in 0..<result.safeLen:
        let
          a = result[i]
          h = hlo(c, a)
        
        if h != a: result[i] = h
    else:
      # perform type checking, so that the replacement still fits:
      if isEmptyType(n.typ) and isEmptyType(result.typ):
        discard
      else:
        result = fitNode(c, n.typ, result, n.info)
      
      # optimization has been applied so check again:
      result = commonOptimizations(c.graph, c.idgen, c.module, result)
      result = hlo(c, result)
      result = commonOptimizations(c.graph, c.idgen, c.module, result)


proc hloBody(c: PContext, n: PNode): PNode =
  # fast exit:
  if c.patterns.len == 0 or optTrMacros notin c.config.options:
    return n
  
  c.hloLoopDetector = 0
  result = hlo(c, n)


proc hloStmt(c: PContext, n: PNode): PNode =
  # fast exit:
  if c.patterns.len == 0 or optTrMacros notin c.config.options:
    return n
  
  c.hloLoopDetector = 0
  result = hlo(c, n)
