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

proc pickBestCandidate(c: PContext,
                       headSymbol: PNode,
                       n: PNode,
                       startScope: PScope,
                       filter: TSymKinds,
                       best, alt: var TCandidate,
                       errors: var seq[SemCallMismatch]) =
  var
    o: TOverloadIter
    sym = initOverloadIter(o, c, startScope, headSymbol)
    scope = o.lastOverloadScope
  
  if sym.isError:
    # xxx: this should be in the loop below but it's not that simple as we'll
    #      end up with lots of excessive reports, need a bigger rethink
    localReport(c.config, sym.ast)

  while sym != nil:
    if sym.kind in filter:
      # Initialise 'best' and 'alt' with the first available symbol
      initCallCandidate(c, best, sym, scope)
      initCallCandidate(c, alt, sym, scope)
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
    c.config.internalAssert(sym.typ != nil or sym.magic != mNone,
                            "missing type information")
    initCallCandidate(c, z, sym, scope)
    block:
      matches(c, n, z)
      if z.state == csMatch:
        # little hack so that iterators are preferred over everything else:
        if sym.kind == skIterator:
          inc(z.exactMatches, 200)
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

    sym = nextOverloadIter(o, c, headSymbol)
    scope = o.lastOverloadScope

proc maybeResemArgs*(c: PContext, n: PNode, startIdx: int = 1): seq[PNode] =
  # HACK original implementation of the `describeArgs` used `semOperand`
  # here and it's unclear /why/ it's necessary, leaving it as is for now.
  # This was introduced in commit 5b0d8246f79730a473a869792f12938089ecced6
  # that "made some tests green" (+98/-77)
  for i in startIdx ..< n.len:
    var arg = n[i]
    case n[i].kind
    of nkExprEqExpr:
      if arg.typ.isNil: # and arg.kind notin {nkStmtList, nkDo}:
        # XXX we really need to 'tryExpr' here!
        arg = c.semOperand(c, n[i][1])
        arg = n[i][1]
        n[i].typ = arg.typ
        n[i][1] = arg
    of nkStmtList, nkDo, nkElse, nkOfBranch, nkElifBranch, nkExceptBranch:
      discard "`semOperand` is not required... for some reason?"
    else:
      if arg.typ.isNil:
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
    result = c.config.newError(n, PAstDiag(kind: adSemRawTypeMismatch))
    return # xxx: under the legacy error scheme, this was a `msgs.globalReport`,
           #      which means `doRaise`, but that made sense because we did a
           #      double pass, now we simply return for fast exit.
  if errors.len == 0:
    # no further explanation available for reporting
    #
    # QUESTION I wonder if it makes sense to still attempt spelling
    # correction here.
    result = c.config.newError(n, PAstDiag(kind: adSemExpressionCannotBeCalled))
    return

  # attempt to handle spelling
  var f = n[0]
  if f.kind == nkBracketExpr:
    f = f[0]

  if f.kind in {nkOpenSymChoice, nkClosedSymChoice}:
    f = f[0]

  let
    spellingCandidates =
      if f.kind in {nkSym, nkIdent}:
        fixSpelling(c, if f.kind == nkSym: f.sym.name else: f.ident)
      else:
        @[]
    diag = PAstDiag(kind: adSemCallTypeMismatch,
                    callSpellingCandidates: spellingCandidates,
                    callMismatches: errors)
  
  discard maybeResemArgs(c, n, 1)

  result = c.config.newError(n, diag)

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
    c: PContext, flags: TExprFlags, n, origF: PNode): PAstDiag =
  ## Generate diagnostics for
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
  case f.kind
  of nkIdent, nkSym, nkAccQuoted, nkOpenSymChoice, nkClosedSymChoice:
    let (_, err) = considerQuotedIdent(c, f)
    if err != nil:
      return PAstDiag(kind: adSemExpectedIdentifierInExpr,
                      notIdent: if f.kind == nkAccQuoted and f.len == 1: f[0]
                                else: f,
                      instloc: instLoc())
  else:
    return PAstDiag(kind: adSemExpectedIdentifierInExpr,
                    notIdent: f,
                    instLoc: instLoc()) # set instLoc as it's being lost, see
                                        # tests/errmsgs/twrongcolon
  
  if c.compilesContextId > 0 and efExplain notin flags:
    # we avoid running more diagnostic when inside a `compiles(expr)`, to
    # errors while running diagnostic (see test D20180828T234921), and
    # also avoid slowdowns in evaluating `compiles(expr)`.
    result = PAstDiag(kind: adSemCallInCompilesContextNotAProcOrField)
  else:
    result =
      if {nfDotField, nfExplicitCall} * n.flags == {nfDotField}:
        PAstDiag(
          kind: adSemImplicitDotCallNotAProcOrField,
          notProcOrField: f,
          instLoc: instLoc())
      else:
        PAstDiag(
          kind: adSemCallNotAProcOrField,
          notProcOrField: f,
          instLoc: instLoc())

    # store list of potential overload candidates that might be misuesd -
    # for example `obj.iterator()` call outside of the for loop.
    var
      o: TOverloadIter
      sym = initOverloadIter(o, c, f)
    while sym != nil:
      if not sym.isError:
        # xxx: unlikely we need to do a localReport here, should be earlier
        result.unexpectedCandidates.add(sym)
      sym = nextOverloadIter(o, c, f)

    if f.kind == nkIdent:
      # Throw in potential typos - `obj.cull()` or `obj.lenghh` might
      # potentially be caused by this. This error is also called for `4
      # +2`, so command is not always an identifier.
      result.spellingAlts = fixSpelling(c, f.ident)

proc semGenericArgs(c: PContext, n: PNode): PNode =
  ## Semantically analyses the generic arguments passed to a routine invocation
  ## (i.e.: the bracket part in ``routine[A, B](a, b, c)``), producing the
  ## ``nkBracketExpr`` with the typed arguments, or an error.
  ##
  ## Since generic argument expressions must evaluate to either a type or a
  ## static value (both which can be disambiguated between without access to
  ## formal parameters), we fully analyse them here instead of deferring
  ## that to sigmatch. This has the benefit that we only need to analyse/
  ## evaluate each argument once, instead of for each matched against overload.
  assert n.kind == nkBracketExpr
  result = shallowCopy(n)
  result[0] = n[0]

  var hasError = false
  for i in 1..<n.len:
    # a generic argument must either evaluate to a type (``tyTypedesc``) or
    # static value
    # XXX: ``semOperand`` is the procedure that best matches what we want, but
    #      it's still not entirely correct. A dedicated ``semTypeExpr`` (that
    #      also handles ``tyStatic``) is likely needed here
    let opr = c.semOperand(c, n[i], {})
    case opr.typ.kind
    of tyError:
      hasError = true
      result[i] = opr
    of tyTypeDesc:
      # the operand is a type
      result[i] = opr
    else:
      let evaluated = evalConstExpr(c, opr)
      case evaluated.kind
      of nkError:
        # not a constant expression
        hasError = true
        result[i] = evaluated
      of nkType:
        result[i] = evaluated
      else:
        # sigmatch expects the non-type generic arugments to use ``tyStatic``
        let opr = copyTree(evaluated)
        opr.typ = newTypeS(tyStatic, c)
        opr.typ.sons = @[evaluated.typ]
        opr.typ.n = evaluated

        result[i] = opr

  if hasError:
    result = c.config.wrapError(result)

proc resolveOverloads(c: PContext, n: PNode,
                      filter: TSymKinds, flags: TExprFlags,
                      errors: var seq[SemCallMismatch]): TCandidate =
  addInNimDebugUtils(c.config, "resolveOverloads", n, filter, errors, result)
  var
    alt: TCandidate
    f = n[0]
  
  case f.kind
  of nkBracketExpr:
    # the call has explicit generic arguments specified
    let args = semGenericArgs(c, f)
    if args.kind == nkError:
      f = args
    else:
      n[0] = args
      f = args[0]
  else:
    discard

  if f.isError:
    n[0] = f
    result.call = c.config.wrapError(n)
    return

  let scope = c.currentScope
  # all symbols created during lazy semchecking of operands first get
  # committed into a shadow scope, and iff there was a match are merged into
  # the original scope
  c.openShadowScope()

  template pickBest(headSymbol) =
    pickBestCandidate(c, headSymbol, n, scope, filter, result, alt, errors)
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
            n[2] = c.config.newError(n[2], PAstDiag(
              kind: adSemUndeclaredField, givenSym: sym, symTyp: sym.typ))

            result.call = wrapError(c.config, n)
        else:
          let msg = getMsgDiagnostic(c, flags, n, f)
          result.call = c.config.newError(n, msg)

      c.closeShadowScope()
      return
    elif result.state != csMatch:
      if {nfDotField, nfDotSetter} * n.flags != {}:
        # clean up the inserted ops
        n.sons.delete(2)
        n[0] = f
      c.closeShadowScope()
      return

  # a match was found; commit the created symbols to the symbol table. Note
  # that for the sake of error correction, we still do so even if the call is
  # ambiguous
  assert result.state == csMatch
  c.mergeShadowScope()

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
  addInNimDebugUtils(c.config, "inferWithMetatype", arg, result)

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
    # XXX: overwriting the type of `result` like it's done here is not correct.
    #      At least for the ``coerceDistincts == true`` case (currently only
    #      used by ``semConv``), it makes more sense to return both the fitted
    #      node *and* the inferred formal type, and let the callsite handle it
    #      from there
    if formal.kind == tyCompositeTypeClass:
      # passing the composite type-class to ``generateTypeInstance`` would
      # get us the matched source type, which is not what we want here
      # (especially in the presense of ``distinct``s). We want the instantiated
      # base type.
      doAssert formal[0].kind == tyGenericBody
      let inst = formal[1] ## the fully instantiated meta type
      assert inst.kind == tyGenericInst

      var invocation = newTypeS(tyGenericInvocation, c)
      invocation.sons = @[formal[0]] # don't propagte the flags
      # add the instance arguments as the invocation parameters:
      for i in 1..<inst.len-1:
        invocation.rawAddSon(inst[i])

      # evaluate the invocation:
      result.typ = generateTypeInstance(c, m.bindings, arg.info, invocation)
    else:
      result.typ = generateTypeInstance(c, m.bindings, arg.info, formal)
  else:
    result = typeMismatch(c.config, arg.info, formal, arg.typ, arg)
    if result.kind != nkError:
      # error correction:
      result = copyTree(arg)
      result.typ = formal

proc updateDefaultParams(conf: ConfigRef, call: PNode): PNode =
  # In generic procs, the default parameter may be unique for each
  # instantiation (see tlateboundgenericparams).
  # After a call is resolved, we need to re-assign any default value
  # that was used during sigmatch. sigmatch is responsible for marking
  # the default params with `nfDefaultParam` and `instantiateProcType`
  # computes correctly the default values for each instantiation.
  let calleeParams = call[0].sym.typ.n
  var hasError = false
  for i in 1..<call.len:
    if nfDefaultParam in call[i].flags:
      let def = calleeParams[i].sym.ast
      if nfDefaultRefsParam in def.flags: call.flags.incl nfDefaultRefsParam
      call[i] =
        case def.kind
        of nkEmpty:
          # ``instantiateProcType`` uses ``nkEmpty`` to signal an incompatible
          # default expression
          hasError = true
          conf.newError(calleeParams[i],
                        PAstDiag(kind: adSemIncompatibleDefaultExpr,
                                 formal: calleeParams[i].sym))
        else:
          def

  result =
    if hasError: conf.wrapError(call)
    else:        call

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
  result = updateDefaultParams(c.config, result)

proc canDeref(n: PNode): bool {.inline.} =
  result = n.len >= 2 and (let t = n[1].typ;
    t != nil and t.skipTypes({tyGenericInst, tyAlias, tySink}).kind in {tyPtr, tyRef})

proc semOverloadedCall(c: PContext, n: PNode,
                       filter: TSymKinds, flags: TExprFlags): PNode {.nosinks.} =
  addInNimDebugUtils(c.config, "semOverloadedCall", n, result)
  var errors: seq[SemCallMismatch]

  var r = resolveOverloads(c, n, filter, flags, errors)
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

    result =
      case r.calleeSym.ast.kind
      of nkError:
        # the symbol refers to an erroneous entity
        c.config.newError(r.call):
          PAstDiag(kind: adSemCalleeHasAnError, callee: r.calleeSym)
      else:
        semResolvedCall(c, r, n, flags)

  elif r.call.isError:
    result = r.call

  elif efNoUndeclared notin flags:
    result = notFoundError(c, n, errors)

  else:
    result = r.call

proc explicitGenericInstError(c: PContext; n: PNode): PNode =
  c.config.newError(n, PAstDiag(kind: adSemCannotInstantiate,
                                callLineInfo: getCallLineInfo(n)))

proc explicitGenericSym(c: PContext, n: PNode, s: PSym): PNode =
  ## Tries to create an instance of the generic routine `s`. If `s` is not a
  ## generic routine, or the provided arguments `n` don't fit the parameters,
  ## ``nil`` is returned. Otherwise, a node with the symbol of the instance is
  ## returned
  let params = s.ast[genericParamsPos]
  if params.kind == nkEmpty:
    # not a generic routine
    return nil

  var m = newCallCandidate(c, s)
  matchesGenericParams(c, n, m)
  if m.state != csMatch:
    return nil

  # a match doesn't mean that the routine can be instantiated. We still need
  # to check for missing arguments

  for i in (n.len-1)..<params.len:
    if params[i].sym.ast == nil:
      # a parameter has no binding -> cannot instantiate
      return nil

  # generate an instance with the bindings as filled in by
  # ``matchesGenericParams``
  var newInst = generateInstance(c, s, m.bindings, n.info)
  let info = getCallLineInfo(n)
  markUsed(c, info, s)
  newInst.typ.flags.excl tfUnresolved
  newSymNode(newInst, info)

proc explicitGenericInstantiation(c: PContext, n: PNode): PNode =
  assert n.kind == nkBracketExpr
  # analyse the operands first
  let args = semGenericArgs(c, n)
  if args.kind == nkError:
    return args

  let a = n[0]
  case a.kind
  of nkSym:
    # common case; check the only candidate has the right
    # number of generic type parameters:
    let s = a.sym
    result = explicitGenericSym(c, args, s)
    if result.isNil:
      # TODO: provide the diagnostic taken from the ``TCandidate`` instead
      result =
        if n.len-1 < s.requiredGenericParams:
          # provide a better diagnostic for argument mismatch
          c.config.newError(
            args,
            PAstDiag(kind: adSemWrongNumberOfGenericParams,
                      gnrcCallLineInfo: getCallLineInfo(n),
                      countMismatch: (
                        expected: s.requiredGenericParams,
                        got: n.len - 1)))
        else:
          explicitGenericInstError(c, args)
  of nkClosedSymChoice, nkOpenSymChoice:
    # collect all matching generic routines into a symbol choice
    result = newNodeI(a.kind, getCallLineInfo(n))
    result.typ = newTypeS(tyNone, c)

    for i in 0..<a.len:
      let candidate = a[i].sym
      if candidate.kind in {skProc, skMethod, skConverter,
                            skFunc, skIterator}:
        let x = explicitGenericSym(c, args, candidate)
        if x != nil:
          result.add(x)

    # get rid of nkClosedSymChoice if not ambiguous:
    if result.len == 1 and a.kind == nkClosedSymChoice:
      result = result[0]
    elif result.len == 0:
      result = explicitGenericInstError(c, n)
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
