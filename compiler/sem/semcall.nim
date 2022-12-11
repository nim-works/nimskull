#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements semantic checking for calls.
## included from sem.nim


proc sameMethodDispatcher(a, b: PSym): bool =
  result = false
  if a.kind == skMethod and b.kind == skMethod:
    var aa = lastSon(a.ast)
    var bb = lastSon(b.ast)
    if aa.kind == nkSym and bb.kind == nkSym:
      if aa.sym == bb.sym:
        result = true
    else:
      discard
      # generics have no dispatcher yet, so we need to compare the method
      # names; however, the names are equal anyway because otherwise we
      # wouldn't even consider them to be overloaded. But even this does
      # not work reliably! See tmultim6 for an example:
      # method collide[T](a: TThing, b: TUnit[T]) is instantiated and not
      # method collide[T](a: TUnit[T], b: TThing)! This means we need to
      # *instantiate* every candidate! However, we don't keep more than 2-3
      # candidates around so we cannot implement that for now. So in order
      # to avoid subtle problems, the call remains ambiguous and needs to
      # be disambiguated by the programmer; this way the right generic is
      # instantiated.

proc determineType(c: PContext, s: PSym)

proc initCandidateSymbols(c: PContext, headSymbol: PNode,
                          initialBinding: PNode,
                          filter: TSymKinds,
                          best, alt: var TCandidate,
                          o: var TOverloadIter): seq[tuple[s: PSym, scope: int]] =
  result = @[]
  var symx = initOverloadIter(o, c, headSymbol)
  while symx != nil:
    if symx.kind in filter:
      result.add((symx, o.lastOverloadScope))
    elif symx.isError:
      localReport(c.config, symx.ast)
    symx = nextOverloadIter(o, c, headSymbol)
  if result.len > 0:
    initCandidate(c, best, result[0].s, initialBinding,
                  result[0].scope)
    initCandidate(c, alt, result[0].s, initialBinding,
                  result[0].scope)
    best.state = csNoMatch

proc pickBestCandidate(c: PContext,
                       headSymbol: PNode,
                       n: PNode,
                       initialBinding: PNode,
                       filter: TSymKinds,
                       best, alt: var TCandidate,
                       errors: var seq[SemCallMismatch],
                       flags: TExprFlags) =
  var
    o: TOverloadIter
    sym = initOverloadIter(o, c, headSymbol)
    scope = o.lastOverloadScope
  # Thanks to the lazy semchecking for operands, we need to check whether
  # 'initCandidate' modifies the symbol table (via semExpr).
  # This can occur in cases like 'init(a, 1, (var b = new(Type2); b))'
  let counterInitial = c.currentScope.symbols.counter
  var
    syms: seq[tuple[s: PSym, scope: int]]
    noSyms = true
    nextSymIndex = 0
  
  if sym.isError:
    # xxx: this should be in the loop below but it's not that simple as we'll
    #      end up with lots of excessive reports, need a bigger rethink
    localReport(c.config, sym.ast)

  while sym != nil:
    if sym.kind in filter:
      # Initialise 'best' and 'alt' with the first available symbol
      initCandidate(c, best, sym, initialBinding, scope)
      initCandidate(c, alt, sym, initialBinding, scope)
      best.state = csNoMatch
      break
    else:
      sym = nextOverloadIter(o, c, headSymbol)
      scope = o.lastOverloadScope
  var z: TCandidate
  while sym != nil:
    if sym.kind notin filter:
      sym = nextOverloadIter(o, c, headSymbol)
      scope = o.lastOverloadScope
      continue
    determineType(c, sym)
    initCandidate(c, z, sym, initialBinding, scope)
    if c.currentScope.symbols.counter == counterInitial or syms.len != 0:
      matches(c, n, z)
      if z.state == csMatch:
        # little hack so that iterators are preferred over everything else:
        if sym.kind == skIterator:
          if not (efWantIterator notin flags and efWantIterable in flags):
            inc(z.exactMatches, 200)
          else:
            dec(z.exactMatches, 200)
        case best.state
        of csEmpty, csNoMatch: best = z
        of csMatch:
          var cmp = cmpCandidates(best, z)
          if cmp < 0: best = z   # x is better than the best so far
          elif cmp == 0: alt = z # x is as good as the best so far
      else:
        # XXX: Collect diagnostics for matching, but not winning overloads too?
        var err = z.error
        err.target = sym
        errors.add err

    else:
      # Symbol table has been modified. Restart and pre-calculate all syms
      # before any further candidate init and compare. SLOW, but rare case.
      syms = initCandidateSymbols(c, headSymbol, initialBinding, filter,
                                  best, alt, o)
      noSyms = false
    if noSyms:
      sym = nextOverloadIter(o, c, headSymbol)
      scope = o.lastOverloadScope
    elif nextSymIndex < syms.len:
      # rare case: retrieve the next pre-calculated symbol
      sym = syms[nextSymIndex].s
      scope = syms[nextSymIndex].scope
      nextSymIndex += 1
    else:
      break

proc maybeResemArgs*(c: PContext, n: PNode, startIdx: int = 1): seq[PNode] =
  # HACK original implementation of the `describeArgs` used `semOperand`
  # here, but until there is a clear understanding /why/ is it necessary to
  # additionall call sem on the arguments I will leave this as it is now.
  # This was introduced in commit 5b0d8246f79730a473a869792f12938089ecced6
  # that "made some tests green" (+98/-77)
  for i in startIdx ..< n.len:
    var arg = n[i]
    case n[i].kind
    of nkExprEqExpr:
      if arg.typ.isNil and arg.kind notin {nkStmtList, nkDo}:
        # XXX we really need to 'tryExpr' here!
        arg = c.semOperand(c, n[i][1])
        arg = n[i][1]
        n[i].typ = arg.typ
        n[i][1] = arg
    else:
      if arg.typ.isNil and arg.kind notin {
           nkStmtList, nkDo, nkElse, nkOfBranch, nkElifBranch, nkExceptBranch
         }:

        arg = c.semOperand(c, n[i])
        n[i] = arg

    if arg.typ != nil and arg.typ.kind == tyError:
      return

    result.add arg

proc notFoundError(c: PContext, n: PNode, errors: seq[SemCallMismatch]): PNode =
  ## Gives a detailed error message; this is separated from semOverloadedCall,
  ## as semOverloadedCall is already pretty slow (and we need this information
  ## only in case of an error).
  ## returns an nkError
  addInNimDebugUtils(c.config, "notFoundError", n, result)
  if c.config.m.errorOutputs == {}:
    # xxx: this is a hack to detect we're evaluating a constant expression or
    #      some other vm code, it seems
    # fail fast:
    result = c.config.newError(n, reportSem rsemRawTypeMismatch)
    return # xxx: under the legacy error scheme, this was a `msgs.globalReport`,
           #      which means `doRaise`, but that made sense because we did a
           #      double pass, now we simply return for fast exit.
  if errors.len == 0:
    # no further explanation available for reporting
    #
    # QUESTION I wonder if it makes sense to still attempt spelling
    # correction here.
    result = c.config.newError(n, reportSem rsemExpressionCannotBeCalled)
    return

  var report = reportAst(rsemCallTypeMismatch, n)

  # attempt to handle spelling
  var f = n[0]
  if f.kind == nkBracketExpr:
    f = f[0]

  if f.kind in {nkOpenSymChoice, nkClosedSymChoice}:
    f = f[0]

  if f.kind in {nkSym, nkIdent}:
    report.spellingCandidates = fixSpelling(
      c, if f.kind == nkSym: f.sym.name else: f.ident)

  discard maybeResemArgs(c, n, 1)
  report.callMismatches = errors

  result = newError(c.config, n, report)

proc bracketNotFoundError(c: PContext; n: PNode): PNode =
  var errors: seq[SemCallMismatch]
  var o: TOverloadIter
  let headSymbol = n[0]
  var symx = initOverloadIter(o, c, headSymbol)
  while symx != nil:
    if symx.kind in routineKinds:
      errors.add SemCallMismatch(target: symx, firstMismatch: MismatchInfo())
    elif symx.isError:
      # xxx: unlikely we ever get here; defensive code
      continue
    symx = nextOverloadIter(o, c, headSymbol)

  result = notFoundError(c, n, errors)

proc getMsgDiagnostic(
    c: PContext, flags: TExprFlags, n, origF: PNode): SemReport =
  ## Generate report for
  ## - `foo.bar()` in case of missing `bar()` overloads
  ## - `iter()` for inline iterators outside of the loop
  ## - `obj.field` for missing fields
  # for dotField calls, eg: `foo.bar()`, set f for nicer messages
  let f =
    if {nfDotField} <= n.flags and n.safeLen >= 3:
      n[2]
    else:
      origF

  # HACK apparently this call is still necessary to provide some additional
  # input validation and optionally raise the 'identifier expected but
  # found' error.
  discard legacyConsiderQuotedIdent(c, f, n)

  if c.compilesContextId > 0 and efExplain notin flags:
    # we avoid running more diagnostic when inside a `compiles(expr)`, to
    # errors while running diagnostic (see test D20180828T234921), and
    # also avoid slowdowns in evaluating `compiles(expr)`.
    result = SemReport(kind: rsemCompilesReport)

  else:
    var o: TOverloadIter
    if {nfDotField, nfExplicitCall} * n.flags == {nfDotField}:
      result = SemReport(
        typ: n[1].typ,
        str: $f,
        ast: n,
        explicitCall: false,
        kind: rsemCallNotAProcOrField)

    else:
      result = SemReport(
        str: $f,
        ast: n,
        explicitCall: true,
        kind: rsemCallNotAProcOrField)

    # store list of potential overload candidates that might be misuesd -
    # for example `obj.iterator()` call outside of the for loop.
    var sym = initOverloadIter(o, c, f)
    while sym != nil:
      if not sym.isError:
        # xxx: unlikely we need to do a localReport here, should be earlier
        result.unexpectedCandidate.add(sym)
      sym = nextOverloadIter(o, c, f)

    if f.kind == nkIdent:
      # Throw in potential typos - `obj.cull()` or `obj.lenghh` might
      # potentially be caused by this. This error is also called for `4
      # +2`, so command is not always an identifier.
      result.spellingCandidates = fixSpelling(c, f.ident)

proc resolveOverloads(c: PContext, n: PNode,
                      filter: TSymKinds, flags: TExprFlags,
                      errors: var seq[SemCallMismatch]): TCandidate =
  addInNimDebugUtils(c.config, "resolveOverloads", n, filter, errors, result)
  var
    initialBinding: PNode
    alt: TCandidate
    f = n[0]
  
  case f.kind
  of nkBracketExpr:
    # fill in the bindings:
    let hasError = semOpAux(c, f)
    initialBinding = f
    if hasError:
      f = c.config.wrapError(f)
    else:
      f = f[0]
  else:
    initialBinding = nil

  if f.isError:
    n[0] = f
    result.call = c.config.wrapError(n)
    return

  template pickBest(headSymbol) =
    pickBestCandidate(c, headSymbol, n, initialBinding,
                      filter, result, alt, errors, flags)
  pickBest(f)

  let overloadsState = result.state
  if overloadsState != csMatch:
    template tryOp(x) =
      let op = newIdentNode(getIdent(c.cache, x), n.info)
      n[0] = op
      pickBest(op)

    if nfDotField in n.flags:
      c.config.internalAssert f.kind == nkIdent and n.len >= 2

      if f.ident.s notin [".", ".()"]: # a dot call on a dot call is invalid
        # leave the op head symbol empty,
        # we are going to try multiple variants
        n.sons[0..1] = [nil, n[1], f]

        if nfExplicitCall in n.flags:
          tryOp ".()"

        if result.state in {csEmpty, csNoMatch}:
          tryOp "."

    elif nfDotSetter in n.flags and f.kind == nkIdent and n.len == 3:
      # we need to strip away the trailing '=' here:
      let calleeName = newIdentNode(getIdent(c.cache, f.ident.s[0..^2]), f.info)
      n.sons[0..1] = [nil, n[1], calleeName]
      tryOp ".="

    if overloadsState == csEmpty and result.state == csEmpty:
      if efNoUndeclared notin flags: # for tests/pragmas/tcustom_pragma.nim
        if n[0] != nil and n[0].kind == nkIdent and n[0].ident.s in [".", ".="] and n[2].kind == nkIdent:
          let sym = n[1].typ.typSym
          if sym == nil:
            let msg = getMsgDiagnostic(c, flags, n, f)
            result.call = c.config.newError(n, msg)
          else:
            n[2] = c.config.newError(n[2], SemReport(
              kind: rsemUndeclaredField, ast: n[2], sym: sym, typ: sym.typ))

            result.call = wrapError(c.config, n)
        else:
          let msg = getMsgDiagnostic(c, flags, n, f)
          result.call = c.config.newError(n, msg)

      return
    elif result.state != csMatch:
      if {nfDotField, nfDotSetter} * n.flags != {}:
        # clean up the inserted ops
        n.sons.delete(2)
        n[0] = f
      return
  if alt.state == csMatch and cmpCandidates(result, alt) == 0 and
      not sameMethodDispatcher(result.calleeSym, alt.calleeSym):
    c.config.internalAssert result.state == csMatch
    #writeMatches(result)
    #writeMatches(alt)
    if c.config.m.errorOutputs == {}:
      # quick error message for performance of 'compiles' built-in:
      globalReport(c.config, n.info, reportSem(rsemAmbiguous))

    elif c.config.errorCounter == 0:
      localReport(c.config, n.info, reportSymbols(
        rsemAmbiguous,
        @[result.calleeSym, alt.calleeSym],
        ast = n
      ))

proc instGenericConvertersArg*(c: PContext, a: PNode, x: TCandidate) =
  let a = if a.kind == nkHiddenDeref: a[0] else: a
  if a.kind == nkHiddenCallConv and a[0].kind == nkSym:
    let s = a[0].sym
    if s.isGenericRoutineStrict:
      let finalCallee = generateInstance(c, s, x.bindings, a.info)
      a[0].sym = finalCallee
      a[0].typ = finalCallee.typ
      #a.typ = finalCallee.typ[0]

proc instGenericConvertersSons*(c: PContext, n: PNode, x: TCandidate) =
  assert n.kind in nkCallKinds
  if x.genericConverter:
    for i in 1..<n.len:
      instGenericConvertersArg(c, n[i], x)

proc indexTypesMatch(c: PContext, f, a: PType, arg: PNode): PNode =
  var m = newCandidate(c, f)
  result = paramTypesMatch(m, f, a, arg)
  if m.genericConverter and result != nil:
    instGenericConvertersArg(c, result, m)

proc inferWithMetatype(c: PContext, formal: PType,
                       arg: PNode, coerceDistincts = false): PNode =
  var m = newCandidate(c, formal)
  m.coerceDistincts = coerceDistincts
  
  if arg.kind == nkError:
    result = arg
    return

  result = paramTypesMatch(m, formal, arg.typ, arg)

  if m.genericConverter and result != nil:
    instGenericConvertersArg(c, result, m)

  if result != nil:
    # This almost exactly replicates the steps taken by the compiler during
    # param matching. It performs an embarrassing amount of back-and-forth
    # type jugling, but it's the price to pay for consistency and correctness
    result.typ = generateTypeInstance(c, m.bindings, arg.info,
                                      formal.skipTypes({tyCompositeTypeClass}))
  else:
    result = typeMismatch(c.config, arg.info, formal, arg.typ, arg)
    if result.kind != nkError:
      # error correction:
      result = copyTree(arg)
      result.typ = formal

proc updateDefaultParams(call: PNode) =
  # In generic procs, the default parameter may be unique for each
  # instantiation (see tlateboundgenericparams).
  # After a call is resolved, we need to re-assign any default value
  # that was used during sigmatch. sigmatch is responsible for marking
  # the default params with `nfDefaultParam` and `instantiateProcType`
  # computes correctly the default values for each instantiation.
  let calleeParams = call[0].sym.typ.n
  for i in 1..<call.len:
    if nfDefaultParam in call[i].flags:
      let def = calleeParams[i].sym.ast
      if nfDefaultRefsParam in def.flags: call.flags.incl nfDefaultRefsParam
      call[i] = def

proc getCallLineInfo(n: PNode): TLineInfo =
  case n.kind
  of nkAccQuoted, nkBracketExpr, nkCall, nkCallStrLit, nkCommand:
    if len(n) > 0:
      return getCallLineInfo(n[0])
  of nkDotExpr:
    if len(n) > 1:
      return getCallLineInfo(n[1])
  else:
    discard
  result = n.info

proc semResolvedCall(c: PContext, x: TCandidate,
                     n: PNode, flags: TExprFlags): PNode =
  assert x.state == csMatch
  var finalCallee = x.calleeSym
  let info = getCallLineInfo(n)
  markUsed(c, info, finalCallee)
  onUse(info, finalCallee)
  assert finalCallee.ast != nil
  if x.hasFauxMatch:
    result = x.call
    result[0] = newSymNode(finalCallee, getCallLineInfo(result[0]))
    if containsGenericType(result.typ) or x.fauxMatch == tyUnknown:
      result.typ = newTypeS(x.fauxMatch, c)
      if result.typ.kind == tyError: incl result.typ.flags, tfCheckedForDestructor
    return
  let gp = finalCallee.ast[genericParamsPos]
  if gp.isGenericParams:
    if x.calleeSym.kind notin {skMacro, skTemplate}:
      if x.calleeSym.magic in {mArrGet, mArrPut}:
        finalCallee = x.calleeSym
      else:
        finalCallee = generateInstance(c, x.calleeSym, x.bindings, n.info)
    else:
      # For macros and templates, the resolved generic params
      # are added as normal params.
      for s in instantiateGenericParamList(c, gp, x.bindings):
        case s.kind
        of skConst:
          if not s.ast.isNil:
            x.call.add s.ast
          else:
            x.call.add c.graph.emptyNode
        of skType:
          x.call.add newSymNode(s, n.info)
        else:
          internalAssert(
            c.config, false,
            "Unexpected symbol kind for result of 'instantiateGenericParamList': " &
              $s.kind)

  result = x.call
  instGenericConvertersSons(c, result, x)
  result[0] = newSymNode(finalCallee, getCallLineInfo(result[0]))
  result.typ = finalCallee.typ[0]
  updateDefaultParams(result)

proc canDeref(n: PNode): bool {.inline.} =
  result = n.len >= 2 and (let t = n[1].typ;
    t != nil and t.skipTypes({tyGenericInst, tyAlias, tySink}).kind in {tyPtr, tyRef})

proc semOverloadedCall(c: PContext, n: PNode,
                       filter: TSymKinds, flags: TExprFlags): PNode {.nosinks.} =
  addInNimDebugUtils(c.config, "semOverloadedCall", n, result)
  var errors: seq[SemCallMismatch]

  var r = resolveOverloads(c, n, filter, flags, errors)

  if r.state != csMatch and implicitDeref in c.features and canDeref(n):
    # try to deref the first argument and then try overloading resolution again:
    #
    # XXX: why is this here?
    #      it could be added to the long list of alternatives tried
    #      inside `resolveOverloads` or it could be moved all the way
    #      into sigmatch with hidden conversion produced there

    n[1] = genDeref(n[1])
    r = resolveOverloads(c, n, filter, flags, errors)

  if r.state == csMatch:
    # this may be triggered, when the explain pragma is used
    if errors.len > 0:
      let errorsToReport =
        if efExplain in flags: # output everything
          errors
        else: # only report non-matching candidates that have sfExplain
          var filteredErrors: seq[SemCallMismatch]
          for e in errors:
            if e.diagnosticsEnabled and e.target != r.calleeSym:
              filteredErrors.add e
          filteredErrors

      if errorsToReport.len > 0:
        discard maybeResemArgs(c, n, 1)
        localReport(c.config, n.info, SemReport(
          ast: n,
          kind: rsemNonMatchingCandidates,
          callMismatches: errorsToReport
        ))

    result = semResolvedCall(c, r, n, flags)

  elif r.call.isError:
    result = r.call

  elif efNoUndeclared notin flags:
    result = notFoundError(c, n, errors)

  else:
    result = r.call

proc explicitGenericInstError(c: PContext; n: PNode): PNode =
  c.config.newError(n, reportAst(rsemCannotInstantiate, n), posInfo = getCallLineInfo(n))

proc explicitGenericSym(c: PContext, n: PNode, s: PSym): PNode =
  # binding has to stay 'nil' for this to work!
  var m = newCandidate(c, s, nil)

  for i in 1..<n.len:
    let formal = s.ast[genericParamsPos][i-1].typ
    var arg = n[i].typ
    # try transforming the argument into a static one before feeding it into
    # typeRel
    if formal.kind == tyStatic and arg.kind != tyStatic:
      let evaluated = c.semTryConstExpr(c, n[i])
      if evaluated != nil:
        arg = newTypeS(tyStatic, c)
        arg.sons = @[evaluated.typ]
        arg.n = evaluated
    let tm = typeRel(m, formal, arg)
    if tm in {isNone, isConvertible}: return nil
  var newInst = generateInstance(c, s, m.bindings, n.info)
  newInst.typ.flags.excl tfUnresolved
  let info = getCallLineInfo(n)
  markUsed(c, info, s)
  onUse(info, s)
  result = newSymNode(newInst, info)

proc explicitGenericInstantiation(c: PContext, n: PNode, s: PSym): PNode =
  assert n.kind == nkBracketExpr
  for i in 1..<n.len:
    let e = semExpr(c, n[i])
    if e.isError:
      n[i] = e
      result = c.config.wrapError(n)
      return
    elif e.typ == nil:
      n[i].typ = errorType(c)
    else:
      n[i].typ = e.typ.skipTypes({tyTypeDesc})
  var s = s
  var a = n[0]
  case a.kind
  of nkSym:
    # common case; check the only candidate has the right
    # number of generic type parameters:
    result =
      if s.ast[genericParamsPos].safeLen != n.len-1:
        let expected = s.ast[genericParamsPos].safeLen
        c.config.newError(
          n,
          SemReport(kind: rsemWrongNumberOfGenericParams,
                    ast: n,
                    countMismatch: (
                      expected: toInt128(expected),
                      got: toInt128(n.len - 1))),
          posInfo = getCallLineInfo(n))
      else:
        explicitGenericSym(c, n, s)
    
    if result.isNil:
      result = explicitGenericInstError(c, n)
  of nkClosedSymChoice, nkOpenSymChoice:
    # choose the generic proc with the proper number of type parameters.
    # XXX I think this could be improved by reusing sigmatch.paramTypesMatch.
    # It's good enough for now.
    result = newNodeI(a.kind, getCallLineInfo(n))
    for i in 0..<a.len:
      var candidate = a[i].sym
      
      if candidate.kind in {skProc, skMethod, skConverter,
                            skFunc, skIterator}:
        # it suffices that the candidate has the proper number of generic
        # type parameters:
        if candidate.ast[genericParamsPos].safeLen == n.len-1:
          let x = explicitGenericSym(c, n, candidate)
          if x != nil:
            result.add(x)
    
    # get rid of nkClosedSymChoice if not ambiguous:
    if result.len == 1 and a.kind == nkClosedSymChoice:
      result = result[0]
    elif result.len == 0:
      result = explicitGenericInstError(c, n)
    # candidateCount != 1: return explicitGenericInstError(c, n)
  else:
    result = explicitGenericInstError(c, n)

proc searchForBorrowProc(c: PContext, startScope: PScope, fn: PSym): PSym =
  ## Searches for the fn in the symbol table. If the parameter lists are suitable
  ## for borrowing the sym in the symbol table is returned, else nil.
  ## New approach: generate fn(x, y, z) where x, y, z have the proper types
  ## and use the overloading resolution mechanism:
  var call = newNodeI(nkCall, fn.info)
  var hasDistinct = false
  call.add(newIdentNode(fn.name, fn.info))
  for i in 1..<fn.typ.n.len:
    let param = fn.typ.n[i]
    const desiredTypes = abstractVar + {tyCompositeTypeClass} - {tyTypeDesc, tyDistinct}
    #[
      # We only want the type not any modifiers such as `ptr`, `var`, `ref` ...
      # tyCompositeTypeClass is here for
      # when using something like:
      type Foo[T] = distinct int
      proc `$`(f: Foo): string {.borrow.}
      # We want to skip `Foo` to get `int`
    ]#
    let t = skipTypes(param.typ, desiredTypes)
    if t.kind == tyDistinct or param.typ.kind == tyDistinct: hasDistinct = true
    var x: PType
    if param.typ.kind == tyVar:
      x = newTypeS(param.typ.kind, c)
      x.addSonSkipIntLit(t.baseOfDistinct(c.graph, c.idgen), c.idgen)
    else:
      x = t.baseOfDistinct(c.graph, c.idgen)
    call.add(newNodeIT(nkEmpty, fn.info, x))
  if hasDistinct:
    let filter = if fn.kind in {skProc, skFunc}: {skProc, skFunc} else: {fn.kind}
    var resolved = semOverloadedCall(c, call, filter, {})
    if resolved != nil:
      if resolved.kind == nkError:
        localReport(c.config, resolved)

      result = resolved[0].sym
      if not compareTypes(result.typ[0], fn.typ[0], dcEqIgnoreDistinct):
        result = nil
      elif result.magic in {mArrPut, mArrGet}:
        # cannot borrow these magics for now
        result = nil
