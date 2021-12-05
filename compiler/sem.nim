#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module implements the semantic checking pass.

import
  ast, strutils, options, astalgo, trees,
  wordrecg, ropes, msgs, idents, renderer, types, platform, math,
  magicsys, nversion, nimsets, semfold, modulepaths, importer,
  procfind, lookups, pragmas, passes, semdata, semtypinst, sigmatch,
  intsets, transf, vmdef, vm, aliases, cgmeth, lambdalifting,
  evaltempl, patterns, parampatterns, sempass2, linter, semmacrosanity,
  lowerings, plugins/active, lineinfos, strtabs, int128,
  isolation_check, typeallowed, modulegraphs, enumtostr, concepts, astmsgs,
  errorhandling, errorreporting

when defined(nimfix):
  import nimfix/prettybase

when not defined(leanCompiler):
  import spawn

# implementation

proc semExpr(c: PContext, n: PNode, flags: TExprFlags = {}): PNode
proc semExprWithType(c: PContext, n: PNode, flags: TExprFlags = {}): PNode
proc semExprNoType(c: PContext, n: PNode): PNode
proc semExprNoDeref(c: PContext, n: PNode, flags: TExprFlags = {}): PNode
proc semProcBody(c: PContext, n: PNode): PNode

proc fitNode(c: PContext, formal: PType, arg: PNode; info: TLineInfo): PNode
proc changeType(c: PContext; n: PNode, newType: PType, check: bool)

proc semTypeNode(c: PContext, n: PNode, prev: PType): PType
proc semStmt(c: PContext, n: PNode; flags: TExprFlags): PNode
proc semOpAux(c: PContext, n: PNode)
proc semParamList(c: PContext, n, genericParams: PNode, s: PSym)
proc addParams(c: PContext, n: PNode, kind: TSymKind)
proc maybeAddResult(c: PContext, s: PSym, n: PNode)
proc tryExpr(c: PContext, n: PNode, flags: TExprFlags = {}): PNode
proc activate(c: PContext, n: PNode)
proc semQuoteAst(c: PContext, n: PNode): PNode
proc finishMethod(c: PContext, s: PSym)
proc evalAtCompileTime(c: PContext, n: PNode): PNode
proc indexTypesMatch(c: PContext, f, a: PType, arg: PNode): PNode
proc semStaticExpr(c: PContext, n: PNode): PNode
proc semStaticType(c: PContext, childNode: PNode, prev: PType): PType
proc semTypeOf(c: PContext; n: PNode): PNode
proc computeRequiresInit(c: PContext, t: PType): bool
proc defaultConstructionError(c: PContext, t: PType, info: TLineInfo)
proc hasUnresolvedArgs(c: PContext, n: PNode): bool
proc isArrayConstr(n: PNode): bool {.inline.} =
  result = n.kind == nkBracket and
    n.typ.skipTypes(abstractInst).kind == tyArray

proc deltaTrace(stopProc, indent: string, entries: seq[StackTraceEntry])
  {.inline.} =
  # find the actual StackTraceEntry index based on the name
  let endsWith = entries.len - 1
  var startFrom = 0
  for i in countdown(endsWith, 0):
    let e = entries[i]
    if i != endsWith and $e.procname == stopProc: # found the previous
      startFrom = i + 1
      break                                       # skip the rest

  # print the trace oldest (startFrom) to newest (endsWith)
  for i in startFrom..endsWith:
    let e = entries[i]
    echo:
      "$1| $2 $3($4)" % [indent, $e.procname, $e.filename, $e.line]

template addInNimDebugUtilsAux(conf: ConfigRef; prcname: string;
                                enterMsg, leaveMsg, getInfo: untyped) =
  ## used by one of the dedicated templates in order to output compiler trace
  ## data, use a dedicated template (see below) for actual output. this is a
  ## helper that takes three templates, `enterMsg`, `leaveMsg`, and `getInfo`
  ## that will emit a message when entering and leaving a proc, and getting
  ## the string out of some lineinfo, respectively.
  ##
  ## The dedicate templates take specific parameters and pass in the above
  ## templates with the following signatures:
  ## * enterMsg: indent: string -> string
  ## * leaveMsg: indent: string -> string
  ## * getInfo: void -> string
  ##
  ## once a specialized template exists, again see below, use at the start of a
  ## proc, typically a high traffic one such as `semExpr` and then this will
  ## output partial traces through the compiler.
  ##
  ## The output is roughly:
  ## 1. begin message with starting location
  ##    a.  a full stacktrace for context
  ## 2. for each proc (nests):
  ##    a. `>prcname plus useful info...`
  ##    b. delta stack trace `| procname filepath(line, col)`
  ##    c. `<prcname plus useful change info...`
  ## 3. end message

  # xxx: as this template develops, eventually all the delta traces will be
  #      replaced with useful debugging output from here so we can inspect
  #      what the compiler is doing. eventually, even that should be superceded
  #      as that sort of transformation and observability should be first class

  when defined(nimDebugUtils): # see `debugutils`

    # do all this at the start of any proc we're debugging
    let
      isDebug = conf.isCompilerDebug()
        ## see if we're in compiler debug mode and also use the fact that we know
        ## this early to see if we just entered or just left

      # determine indentitation levels for output
      indentString = "  "
      indentLevel = conf.debugUtilsStack.len
      indent = indentString.repeat(indentLevel)
      info = getInfo()

    if isDebug:
      conf.debugUtilsStack.add prcname # use this to track deltas
      echo enterMsg(indent)
      if indentLevel != 0: # print a delta stack
        # try to print only the part of the stacktrace since the last time,
        # this is done by looking for any previous calls in `debugUtilsStack`
        {.line.}: # stops the template showing up in the StackTraceEntries
          let
            stopProc =
              if indentLevel == 1: prcname  # we're the only ones
              else: conf.debugUtilsStack[^2] # the one before us
            entries = getStackTraceEntries()
            endsWith = entries.len - 1

        # find the actual StackTraceEntry index based on the name
        var startFrom = 0
        for i in countdown(endsWith, 0):
          let e = entries[i]
          if i != endsWith and $e.procname == stopProc: # found the previous
            startFrom = i + 1
            break                                       # skip the rest

        # print the trace oldest (startFrom) to newest (endsWith)
        for i in startFrom..endsWith:
          let e = entries[i]
          echo:
            "$1| $2 $3($4)" % [indent, $e.procname, $e.filename, $e.line]

    # upon leaving the proc being debugged (`defer`), let's see what changed
    defer:
      if not isDebug and conf.isCompilerDebug():
        # meaning we just analysed a `{.define(nimCompilerDebug).}`
        # it started of as false, now after the proc's work (`semExpr`) this
        # `defer`red logic is seeing `true`, so we must have just started.
        echo "compiler debug start at (initial stacktrace follows): ", info
        {.line.}:           # don't let the template show up in the StackTrace
          writeStackTrace() # gives context to the rest of the partial traces
                            # we do a full one instead
      elif isDebug and not conf.isCompilerDebug():
        # meaning we just analysed an `{.undef(nimCompilerDebug).}`
        # it started of as true, now in the `defer` it's false
        discard conf.debugUtilsStack.pop()
        echo "compiler debug end: ", info
      elif isDebug:
        discard conf.debugUtilsStack.pop()
        echo leaveMsg(indent)
  else:
    discard # noop if undefined

template addInNimDebugUtils(c: ConfigRef; name: string; n, r: PNode;
                            flags: TExprFlags) =
  ## add tracing to procs that are primarily `PNode -> PNode`, with expr flags
  ## and can determine the type
  template enterMsg(indent: string): string =
    "$1>$2: $3, $4, $5" % [indent, name, $n.kind, c$n.info, $flags]
  template leaveMsg(indent: string): string =
    "$1<$2: $3, $4, $5" %
      [indent, name, $r.kind, c$r.info, if r != nil: $r.typ else: ""]
  template getInfo(): string =
    c$n.info

  addInNimDebugUtilsAux(c, name, enterMsg, leaveMsg, getInfo)

template addInNimDebugUtils(c: ConfigRef; name: string; n, r: PNode) =
  ## add tracing to procs that are primarily `PNode -> PNode`, and can
  ## determine the type

  template enterMsg(indent: string): string =
    "$1>$2: $3, $4" % [indent, name, $n.kind, c$n.info]
  template leaveMsg(indent: string): string =
    "$1<$2: $3, $4, $5" %
      [indent, name, $r.kind, c$r.info, if r != nil: $r.typ else: ""]
  template getInfo(): string =
    c$n.info

  addInNimDebugUtilsAux(c, name, enterMsg, leaveMsg, getInfo)

template addInNimDebugUtils(c: ConfigRef; name: string; n: PNode;
                            prev, r: PType) =
  ## add tracing to procs that are primarily `PNode, PType|nil -> PType`,
  ## determining a type node, with a possible previous type.
  template enterMsg(indent: string): string =
    "$1>$2: $3, $4, prev: $5" %
      [indent, name, $n.kind, c$n.info, $prev]
  template leaveMsg(indent: string): string =
    "$1<$2: $3, $4, $5, %6, prev: $7" %
      [indent, name, $n.kind, c$n.info, $n.typ, $r, $prev]
  template getInfo(): string =
    c$n.info
  addInNimDebugUtilsAux(c, name, enterMsg, leaveMsg, getInfo)

template addInNimDebugUtils(c: ConfigRef; name: string; x: PType; n: PNode;
                            r: PType) =
  ## add tracing to procs that are primarily `PType, PNode -> PType`, looking
  ## for a common type
  template enterMsg(indent: string): string =
    "$1>$2: $3, $4, $5, $6" %
      [indent, name, $x.kind, c$n.info, $n.kind, $n.typ]
  template leaveMsg(indent: string): string =
    "$1<$2: $3, $4, $5, %6, $7" %
      [indent, name, $x.kind, c$n.info, $x, $r, $n.typ]
  template getInfo(): string =
    c$n.info
  addInNimDebugUtilsAux(c, name, enterMsg, leaveMsg, getInfo)

template addInNimDebugUtils(c: ConfigRef; name: string; x, y, r: PType) =
  ## add tracing to procs that are primarily `PType, PType -> PType`, looking
  ## for a common type
  template enterMsg(indent: string): string =
    "$1>$2: $3, $4" % [indent, name, $x.kind, $y.kind]
  template leaveMsg(indent: string): string =
    "$1<$2: $3, $4, $5, $6" % [indent, name, $x.kind, $x, $y, $r]
  template getInfo(): string =
    ""
  addInNimDebugUtilsAux(c, name, enterMsg, leaveMsg, getInfo)

template semIdeForTemplateOrGenericCheck(conf, n, requiresCheck) =
  # we check quickly if the node is where the cursor is
  when defined(nimsuggest):
    if n.info.fileIndex == conf.m.trackPos.fileIndex and n.info.line == conf.m.trackPos.line:
      requiresCheck = true

template semIdeForTemplateOrGeneric(c: PContext; n: PNode;
                                    requiresCheck: bool) =
  # use only for idetools support; this is pretty slow so generics and
  # templates perform some quick check whether the cursor is actually in
  # the generic or template.
  when defined(nimsuggest):
    if c.config.cmd == cmdIdeTools and requiresCheck:
      #if optIdeDebug in gGlobalOptions:
      #  echo "passing to safeSemExpr: ", renderTree(n)
      discard safeSemExpr(c, n)

proc fitNodePostMatch(c: PContext, formal: PType, arg: PNode): PNode =
  let x = arg.skipConv
  if (x.kind == nkCurly and formal.kind == tySet and formal.base.kind != tyGenericParam) or
    (x.kind in {nkPar, nkTupleConstr}) and formal.kind notin {tyUntyped, tyBuiltInTypeClass}:
    changeType(c, x, formal, check=true)
  result = arg
  result = skipHiddenSubConv(result, c.graph, c.idgen)


proc fitNode(c: PContext, formal: PType, arg: PNode; info: TLineInfo): PNode =
  if arg.typ.isNil:
    c.config.report(arg.info, SemReport(
      kind: rsemExpressionHasNoType
      expression: renderTree(arg, {renderNoComments})))

    # error correction:
    result = copyTree(arg)
    result.typ = formal
  elif arg.kind in nkSymChoices and formal.skipTypes(abstractInst).kind == tyEnum:
    # Pick the right 'sym' from the sym choice by looking at 'formal' type:
    for ch in arg:
      if sameType(ch.typ, formal):
        return getConstExpr(c.module, ch, c.idgen, c.graph)

    # XXX: why don't we set the `typ` field to formal like above and below?
    result = typeMismatch(c.config, info, formal, arg.typ, arg)

  else:
    result = indexTypesMatch(c, formal, arg.typ, arg)
    if result == nil:
      result = typeMismatch(c.config, info, formal, arg.typ, arg)
      if result.kind != nkError:
        # error correction:
        # XXX: is this "error correction" or actually "fitting" the node?
        result = copyTree(arg)
        result.typ = formal
    else:
      result = fitNodePostMatch(c, formal, result)

proc fitNodeConsiderViewType(c: PContext, formal: PType, arg: PNode; info: TLineInfo): PNode =
  let a = fitNode(c, formal, arg, info)
  if formal.kind in {tyVar, tyLent}:
    #classifyViewType(formal) != noView:
    result = newNodeIT(nkHiddenAddr, a.info, formal)
    result.add a
    formal.flags.incl tfVarIsPtr
  else:
   result = a

proc inferWithMetatype(c: PContext, formal: PType,
                       arg: PNode, coerceDistincts = false): PNode

template commonTypeBegin*(): PType = PType(kind: tyUntyped)

proc commonType*(c: PContext; x, y: PType): PType =
  # new type relation that is used for array constructors,
  # if expressions, etc.:
  addInNimDebugUtils(c.config, "commonType", x, y, result)
  if x == nil: return x
  if y == nil: return y
  var a = skipTypes(x, {tyGenericInst, tyAlias, tySink})
  var b = skipTypes(y, {tyGenericInst, tyAlias, tySink})
  result = x
  if a.kind in {tyUntyped, tyNil}: result = y
  elif b.kind in {tyUntyped, tyNil}: result = x
  elif a.kind == tyTyped: result = a
  elif b.kind == tyTyped: result = b
  elif a.kind == tyTypeDesc:
    # turn any concrete typedesc into the abstract typedesc type
    if a.len == 0: result = a
    else:
      result = newType(tyTypeDesc, nextTypeId(c.idgen), a.owner)
      rawAddSon(result, newType(tyNone, nextTypeId(c.idgen), a.owner))
  elif b.kind in {tyArray, tySet, tySequence} and
      a.kind == b.kind:
    # check for seq[empty] vs. seq[int]
    let idx = ord(b.kind == tyArray)
    if a[idx].kind == tyEmpty: return y
  elif a.kind == tyTuple and b.kind == tyTuple and a.len == b.len:
    var nt: PType
    for i in 0..<a.len:
      let aEmpty = isEmptyContainer(a[i])
      let bEmpty = isEmptyContainer(b[i])
      if aEmpty != bEmpty:
        if nt.isNil:
          nt = copyType(a, nextTypeId(c.idgen), a.owner)
          copyTypeProps(c.graph, c.idgen.module, nt, a)

        nt[i] = if aEmpty: b[i] else: a[i]
    if not nt.isNil: result = nt
    #elif b[idx].kind == tyEmpty: return x
  elif a.kind == tyRange and b.kind == tyRange:
    # consider:  (range[0..3], range[0..4]) here. We should make that
    # range[0..4]. But then why is (range[0..4], 6) not range[0..6]?
    # But then why is (2,4) not range[2..4]? But I think this would break
    # too much code. So ... it's the same range or the base type. This means
    #  typeof(if b: 0 else 1) == int and not range[0..1]. For now. In the long
    # run people expect ranges to work properly within a tuple.
    if not sameType(a, b):
      result = skipTypes(a, {tyRange}).skipIntLit(c.idgen)
    when false:
      if a.kind != tyRange and b.kind == tyRange:
        # XXX This really needs a better solution, but a proper fix now breaks
        # code.
        result = a #.skipIntLit
      elif a.kind == tyRange and b.kind != tyRange:
        result = b #.skipIntLit
      elif a.kind in IntegralTypes and a.n != nil:
        result = a #.skipIntLit
  elif a.kind == tyProc and b.kind == tyProc:
    if a.callConv == ccClosure and b.callConv != ccClosure:
      result = x
    elif compatibleEffects(a, b) != efCompat or
        (b.flags * {tfNoSideEffect, tfGcSafe}) < (a.flags * {tfNoSideEffect, tfGcSafe}):
      result = y
  else:
    var k = tyNone
    if a.kind in {tyRef, tyPtr}:
      k = a.kind
      if b.kind != a.kind: return x
      # bug #7601, array construction of ptr generic
      a = a.lastSon.skipTypes({tyGenericInst})
      b = b.lastSon.skipTypes({tyGenericInst})
    if a.kind == tyObject and b.kind == tyObject:
      result = commonSuperclass(a, b)
      # this will trigger an error later:
      if result.isNil or result == a: return x
      if result == b: return y
      # bug #7906, tyRef/tyPtr + tyGenericInst of ref/ptr object ->
      # ill-formed AST, no need for additional tyRef/tyPtr
      if k != tyNone and x.kind != tyGenericInst:
        let r = result
        result = newType(k, nextTypeId(c.idgen), r.owner)
        result.addSonSkipIntLit(r, c.idgen)

proc endsInNoReturn(n: PNode): bool =
  # check if expr ends in raise exception or call of noreturn proc
  var it = n
  while it.kind in {nkStmtList, nkStmtListExpr} and it.len > 0:
    it = it.lastSon
  result = it.kind in nkLastBlockStmts or
    it.kind in nkCallKinds and it[0].kind == nkSym and sfNoReturn in it[0].sym.flags

proc commonType*(c: PContext; x: PType, y: PNode): PType =
  # ignore exception raising branches in case/if expressions
  addInNimDebugUtils(c.config, "commonType", x, y, result)
  result = x
  if endsInNoReturn(y): return
  result = commonType(c, x, y.typ)

proc newSymS(kind: TSymKind, n: PNode, c: PContext): PSym =
  result = newSym(kind, considerQuotedIdent(c, n), nextSymId c.idgen, getCurrOwner(c), n.info)
  when defined(nimsuggest):
    suggestDecl(c, n, result)

proc newSymG*(kind: TSymKind, n: PNode, c: PContext): PSym =
  # like newSymS, but considers gensym'ed symbols
  if n.kind == nkSym:
    # and sfGenSym in n.sym.flags:
    result = n.sym
    if result.kind notin {kind, skTemp}:
      localError(c.config, n.info, "cannot use symbol of kind '$1' as a '$2'" %
        [result.kind.toHumanStr, kind.toHumanStr])
    when false:
      if sfGenSym in result.flags and result.kind notin {skTemplate, skMacro, skParam}:
        # declarative context, so produce a fresh gensym:
        result = copySym(result)
        result.ast = n.sym.ast
        put(c.p, n.sym, result)
    # when there is a nested proc inside a template, semtmpl
    # will assign a wrong owner during the first pass over the
    # template; we must fix it here: see #909
    result.owner = getCurrOwner(c)
  else:
    result = newSym(kind, considerQuotedIdent(c, n), nextSymId c.idgen, getCurrOwner(c), n.info)
  #if kind in {skForVar, skLet, skVar} and result.owner.kind == skModule:
  #  incl(result.flags, sfGlobal)
  when defined(nimsuggest):
    suggestDecl(c, n, result)

proc semIdentVis(c: PContext, kind: TSymKind, n: PNode,
                 allowed: TSymFlags): PSym
  # identifier with visibility
proc semIdentWithPragma(c: PContext, kind: TSymKind, n: PNode,
                        allowed: TSymFlags): PSym

proc typeAllowedCheck(c: PContext; info: TLineInfo; typ: PType; kind: TSymKind;
                      flags: TTypeAllowedFlags = {}) =
  let t = typeAllowed(typ, kind, c, flags)
  if t != nil:
    var err: string
    if t == typ:
      err = "invalid type: '$1' for $2" % [typeToString(typ), toHumanStr(kind)]
      if kind in {skVar, skLet, skConst} and taIsTemplateOrMacro in flags:
        err &= ". Did you mean to call the $1 with '()'?" % [toHumanStr(typ.owner.kind)]
    else:
      err = "invalid type: '$1' in this context: '$2' for $3" % [typeToString(t),
              typeToString(typ), toHumanStr(kind)]
    localError(c.config, info, err)

proc paramsTypeCheck(c: PContext, typ: PType) {.inline.} =
  typeAllowedCheck(c, typ.n.info, typ, skProc)

proc expectMacroOrTemplateCall(c: PContext, n: PNode): PSym
proc semDirectOp(c: PContext, n: PNode, flags: TExprFlags): PNode
proc semWhen(c: PContext, n: PNode, semCheck: bool = true): PNode
proc semTemplateExpr(c: PContext, n: PNode, s: PSym,
                     flags: TExprFlags = {}): PNode
proc semMacroExpr(c: PContext, n, nOrig: PNode, sym: PSym,
                  flags: TExprFlags = {}): PNode

proc symFromType(c: PContext; t: PType, info: TLineInfo): PSym =
  if t.sym != nil: return t.sym
  result = newSym(skType, getIdent(c.cache, "AnonType"), nextSymId c.idgen, t.owner, info)
  result.flags.incl sfAnon
  result.typ = t

proc symNodeFromType(c: PContext, t: PType, info: TLineInfo): PNode =
  result = newSymNode(symFromType(c, t, info), info)
  result.typ = makeTypeDesc(c, t)

when false: # xxx: deprecate me
  proc createEvalContext(c: PContext, mode: TEvalMode): PEvalContext =
    result = newEvalContext(c.module, mode)
    result.getType = proc (n: PNode): PNode =
      result = tryExpr(c, n)
      if result == nil:
        result = newSymNode(errorSym(c, n))
      elif result.typ == nil:
        result = newSymNode(getSysSym"void")
      else:
        result.typ = makeTypeDesc(c, result.typ)

    result.handleIsOperator = proc (n: PNode): PNode =
      result = isOpImpl(c, n)

proc hasCycle(n: PNode): bool =
  incl n.flags, nfNone
  for i in 0..<n.safeLen:
    if nfNone in n[i].flags or hasCycle(n[i]):
      result = true
      break
  excl n.flags, nfNone

proc fixupTypeAfterEval(c: PContext, evaluated, eOrig: PNode): PNode =
  # recompute the types as 'eval' isn't guaranteed to construct types nor
  # that the types are sound:
  when true:
    if eOrig.typ.kind in {tyUntyped, tyTyped, tyTypeDesc}:
      result = semExprWithType(c, evaluated)
    else:
      result = evaluated
      let expectedType = eOrig.typ.skipTypes({tyStatic})
      if hasCycle(result):
        result = localErrorNode(c, eOrig, "the resulting AST is cyclic and cannot be processed further")
      else:
        semmacrosanity.annotateType(result, expectedType, c.config)
  else:
    result = semExprWithType(c, evaluated)
    #result = fitNode(c, e.typ, result) inlined with special case:
    let arg = result
    result = indexTypesMatch(c, eOrig.typ, arg.typ, arg)
    if result == nil:
      result = arg
      # for 'tcnstseq' we support [] to become 'seq'
      if eOrig.typ.skipTypes(abstractInst).kind == tySequence and
         isArrayConstr(arg):
        arg.typ = eOrig.typ

proc tryConstExpr(c: PContext, n: PNode): PNode =
  var e = semExprWithType(c, n)
  if e == nil: return

  result = getConstExpr(c.module, e, c.idgen, c.graph)
  if result != nil: return

  let oldErrorCount = c.config.errorCounter
  let oldErrorMax = c.config.errorMax
  let oldErrorOutputs = c.config.m.errorOutputs

  c.config.m.errorOutputs = {}
  c.config.errorMax = high(int) # `setErrorMaxHighMaybe` not appropriate here

  try:
    result = evalConstExpr(c.module, c.idgen, c.graph, e)
    if result == nil or result.kind == nkEmpty:
      result = nil
    else:
      result = fixupTypeAfterEval(c, result, e)

  except ERecoverableError:
    result = nil

  c.config.errorCounter = oldErrorCount
  c.config.errorMax = oldErrorMax
  c.config.m.errorOutputs = oldErrorOutputs

const
  errConstExprExpected = "constant expression expected"

proc semConstExpr(c: PContext, n: PNode): PNode =
  var e = semExprWithType(c, n)
  if e == nil:
    localError(c.config, n.info, errConstExprExpected)
    return n
  if e.kind in nkSymChoices and e[0].typ.skipTypes(abstractInst).kind == tyEnum:
    return e
  result = getConstExpr(c.module, e, c.idgen, c.graph)
  if result == nil:
    #if e.kind == nkEmpty: globalError(n.info, errConstExprExpected)
    result = evalConstExpr(c.module, c.idgen, c.graph, e)
    if result == nil or result.kind == nkEmpty:
      if e.info != n.info:
        pushInfoContext(c.config, n.info)
        localError(c.config, e.info, errConstExprExpected)
        popInfoContext(c.config)
      else:
        localError(c.config, e.info, errConstExprExpected)
      # error correction:
      result = e
    else:
      result = fixupTypeAfterEval(c, result, e)

proc semExprFlagDispatched(c: PContext, n: PNode, flags: TExprFlags): PNode =
  if efNeedStatic in flags:
    if efPreferNilResult in flags:
      return tryConstExpr(c, n)
    else:
      return semConstExpr(c, n)
  else:
    result = semExprWithType(c, n, flags)
    if efPreferStatic in flags:
      var evaluated = getConstExpr(c.module, result, c.idgen, c.graph)
      if evaluated != nil: return evaluated
      evaluated = evalAtCompileTime(c, result)
      if evaluated != nil: return evaluated

when not defined(nimHasSinkInference):
  {.pragma: nosinks.}

include hlo, seminst, semcall

proc resetSemFlag(n: PNode) =
  if n != nil:
    excl n.flags, nfSem
    for i in 0..<n.safeLen:
      resetSemFlag(n[i])

proc semAfterMacroCall(c: PContext, call, macroResult: PNode,
                       s: PSym, flags: TExprFlags): PNode =
  ## Semantically check the output of a macro.
  ## This involves processes such as re-checking the macro output for type
  ## coherence, making sure that variables declared with 'let' aren't
  ## reassigned, and binding the unbound identifiers that the macro output
  ## contains.
  inc(c.config.evalTemplateCounter)
  if c.config.evalTemplateCounter > evalTemplateLimit:
    globalError(c.config, s.info, "template instantiation too nested")
  c.friendModules.add(s.owner.getModule)
  result = macroResult
  resetSemFlag result
  if s.typ[0] == nil:
    result = semStmt(c, result, flags)
  else:
    var retType = s.typ[0]
    if retType.kind == tyTypeDesc and tfUnresolved in retType.flags and
        retType.len == 1:
      # bug #11941: template fails(T: type X, v: auto): T
      # does not mean we expect a tyTypeDesc.
      retType = retType[0]
    case retType.kind
    of tyUntyped:
      # Not expecting a type here allows templates like in ``tmodulealias.in``.
      result = semExpr(c, result, flags)
    of tyTyped:
      # More restrictive version.
      result = semExprWithType(c, result, flags)
    of tyTypeDesc:
      if result.kind == nkStmtList: result.transitionSonsKind(nkStmtListType)
      var typ = semTypeNode(c, result, nil)
      if typ == nil:
        localError(c.config, result.info, "expression has no type: " &
                   renderTree(result, {renderNoComments}))
        result = newSymNode(errorSym(c, result))
      else:
        result.typ = makeTypeDesc(c, typ)
      #result = symNodeFromType(c, typ, n.info)
    else:
      if s.ast[genericParamsPos] != nil and retType.isMetaType:
        # The return type may depend on the Macro arguments
        # e.g. template foo(T: typedesc): seq[T]
        # We will instantiate the return type here, because
        # we now know the supplied arguments
        var paramTypes = newIdTable()
        for param, value in genericParamsInMacroCall(s, call):
          idTablePut(paramTypes, param.typ, value.typ)

        retType = generateTypeInstance(c, paramTypes,
                                       macroResult.info, retType)

      result = semExpr(c, result, flags)
      result = fitNode(c, retType, result, result.info)
      #globalError(s.info, errInvalidParamKindX, typeToString(s.typ[0]))
  dec(c.config.evalTemplateCounter)
  discard c.friendModules.pop()

const
  errMissingGenericParamsForTemplate = "'$1' has unspecified generic parameters"
  errFloatToString = "cannot convert '$1' to '$2'"

proc semMacroExpr(c: PContext, n, nOrig: PNode, sym: PSym,
                  flags: TExprFlags = {}): PNode =
  rememberExpansion(c, nOrig.info, sym)
  pushInfoContext(c.config, nOrig.info, sym.detailedInfo)

  let info = getCallLineInfo(n)
  markUsed(c, info, sym)
  onUse(info, sym)
  if sym == c.p.owner:
    globalError(c.config, info, "recursive dependency: '$1'" % sym.name.s)

  let genericParams = sym.ast[genericParamsPos].len
  let suppliedParams = max(n.safeLen - 1, 0)

  if suppliedParams < genericParams:
    globalError(
      c.config, info, errMissingGenericParamsForTemplate % n.renderTree)

  let reportTraceExpand = c.config.macrosToExpand.hasKey(sym.name.s)
  var original: string
  if reportTraceExpand:
    original = renderTree(n)

  result = evalMacroCall(
    c.module, c.idgen, c.graph, c.templInstCounter, n, nOrig, sym)

  if efNoSemCheck notin flags:
    result = semAfterMacroCall(c, n, result, sym, flags)

  if reportTraceExpand:
    c.config.report(SemReport(
      kind: rsemExpandMacro,
      location: some toReportLinePoint(nOrig.info),
      originalExpr: original,
      expandedExpr: renderTree(result)))

  result = wrapInComesFrom(nOrig.info, sym, result)
  popInfoContext(c.config)

proc forceBool(c: PContext, n: PNode): PNode =
  result = fitNode(c, getSysType(c.graph, n.info, tyBool), n, n.info)
  if result == nil: result = n

proc semConstBoolExpr(c: PContext, n: PNode): PNode =
  result = forceBool(c, semConstExpr(c, n))
  if result.kind != nkIntLit:
    localError(c.config, n.info, errConstExprExpected)

proc semGenericStmt(c: PContext, n: PNode): PNode
proc semConceptBody(c: PContext, n: PNode): PNode

include semtypes

proc setGenericParamsMisc(c: PContext; n: PNode) =
  ## used by call defs (procs, templates, macros, ...) to analyse their generic
  ## params, and store the originals in miscPos for better error reporting.
  let orig = n[genericParamsPos]

  doAssert orig.kind in {nkEmpty, nkGenericParams}

  if n[genericParamsPos].kind == nkEmpty:
    n[genericParamsPos] = newNodeI(nkGenericParams, n.info)
  else:
    # we keep the original params around for better error messages, see
    # issue https://github.com/nim-lang/Nim/issues/1713
    n[genericParamsPos] = semGenericParamList(c, orig)

  if n[miscPos].kind == nkEmpty:
    n[miscPos] = newTree(nkBracket, c.graph.emptyNode, orig)
  else:
    n[miscPos][1] = orig

include semtempl, semgnrc, semstmts, semexprs

proc isImportSystemStmt(g: ModuleGraph; n: PNode): bool =
  ## true if `n` is an import statement referring to the system module
  if g.systemModule == nil: return false
  case n.kind
  of nkImportStmt:
    for x in n:
      if x.kind == nkIdent:
        let f = checkModuleName(g.config, x, false)
        if f == g.systemModule.info.fileIndex:
          return true
  of nkImportExceptStmt, nkFromStmt:
    if n[0].kind == nkIdent:
      let f = checkModuleName(g.config, n[0], false)
      if f == g.systemModule.info.fileIndex:
        return true
  else: discard

proc isEmptyTree(n: PNode): bool =
  ## true if `n` is empty that shouldn't count as a top level statement
  case n.kind
  of nkStmtList:
    for it in n:
      if not isEmptyTree(it): return false
    result = true
  of nkEmpty, nkCommentStmt: result = true
  else: result = false

proc semStmtAndGenerateGenerics(c: PContext, n: PNode): PNode =
  ## given top level statements from a module, carries out semantic analysis:
  ## - per module, ensure system module is improted first unless in system
  ## - semantic analysis of the AST and high level optimizations
  ## - minor module transforms for interactive mode and idetools
  ## - delegates further semantic analysis to `sempass2`, see sempass2.nim
  ##
  ## the return value is valid AST for further compilation passes on a per top
  ## level statement basis, with the `PContext` parameter `c` acting as an
  ## accumulator across the various top level statements, modules, and overall
  ## program compilation.

  if c.isfirstTopLevelStmt and not isImportSystemStmt(c.graph, n):
    if sfSystemModule notin c.module.flags and not isEmptyTree(n):
      assert c.graph.systemModule != nil
      c.moduleScope.addSym c.graph.systemModule # import the system module
      importAllSymbols(c, c.graph.systemModule)
      inc c.topStmts
    else:
      # do not increment `c.topStmts`, as we want to ignore empty/trivial nodes
      # at the start of a module.
      discard
  else:
    inc c.topStmts

  # xxx: can noforward be deprecated? might be repurposed for IC, not sure.
  if sfNoForward in c.module.flags:
    result = semAllTypeSections(c, n)
  else:
    result = n

  result = semStmt(c, result, {})
  result = hloStmt(c, result)

  case c.config.cmd
  of cmdInteractive:
    if not isEmptyType(result.typ):
      result = buildEchoStmt(c, result)
  of cmdIdeTools:
    appendToModule(c.module, result)
  else:
    discard
  trackStmt(c, c.module, result, isTopLevel = true)

# All the code below here is for the pass machinery, the code above is the
# actual work.

# -- code-myopen

proc myOpen(graph: ModuleGraph; module: PSym;
            idgen: IdGenerator): PPassContext {.nosinks.} =
  var c = newContext(graph, module)
  c.idgen = idgen
  c.enforceVoidContext = newType(tyTyped, nextTypeId(idgen), nil)
  c.voidType = newType(tyVoid, nextTypeId(idgen), nil)

  if c.p != nil: internalError(graph.config, module.info, "sem.myOpen")
  c.semConstExpr = semConstExpr
  c.semExpr = semExpr
  c.semTryExpr = tryExpr
  c.semTryConstExpr = tryConstExpr
  c.computeRequiresInit = computeRequiresInit
  c.semOperand = semOperand
  c.semConstBoolExpr = semConstBoolExpr
  c.semOverloadedCall = semOverloadedCall
  c.semInferredLambda = semInferredLambda
  c.semGenerateInstance = generateInstance
  c.semTypeNode = semTypeNode
  c.instTypeBoundOp = sigmatch.instTypeBoundOp
  c.hasUnresolvedArgs = hasUnresolvedArgs
  c.templInstCounter = new int

  pushProcCon(c, module)
  pushOwner(c, c.module)

  c.moduleScope = openScope(c)
  c.moduleScope.addSym(module) # a module knows itself

  if sfSystemModule in module.flags:
    graph.systemModule = module
  c.topLevelScope = openScope(c)
  result = c

# -- code-myprocess

proc recoverContext(c: PContext) =
  # clean up in case of a semantic error: We clean up the stacks, etc. This is
  # faster than wrapping every stack operation in a 'try finally' block and
  # requires far less code.
  c.currentScope = c.topLevelScope
  while getCurrOwner(c).kind != skModule: popOwner(c)
  while c.p != nil and c.p.owner.kind != skModule: c.p = c.p.next

proc myProcess(context: PPassContext, n: PNode): PNode {.nosinks.} =
  ## Entry point for the semantic analysis pass, this proc is part of the
  ## compiler graph `passes` interface. This adapts that interface to the sem
  ## implementation by wrapping `semStmtAndGenerateGenerics`.
  ## This will be called with top level nodes
  ## from a module as it's parsed and uses the context to accumulate data.
  var c = PContext(context)
  # no need for an expensive 'try' if we stop after the first error anyway:
  if c.config.errorMax <= 1:
    result = semStmtAndGenerateGenerics(c, n)
  else:
    let oldContextLen = msgs.getInfoContextLen(c.config)
    let oldInGenericInst = c.inGenericInst
    try:
      result = semStmtAndGenerateGenerics(c, n)
    except ERecoverableError, ESuggestDone:
      recoverContext(c)
      c.inGenericInst = oldInGenericInst
      msgs.setInfoContextLen(c.config, oldContextLen)
      if getCurrentException() of ESuggestDone:
        c.suggestionsMade = true
        result = nil
      else:
        result = newNodeI(nkEmpty, n.info)
  storeRodNode(c, result)

# -- code-myclose

proc reportUnusedModules(c: PContext) =
  for i in 0..high(c.unusedImports):
    if sfUsed notin c.unusedImports[i][0].flags:
      message(c.config, c.unusedImports[i][1], warnUnusedImportX, c.unusedImports[i][0].name.s)

proc addCodeForGenerics(c: PContext, n: PNode) =
  for i in c.lastGenericIdx..<c.generics.len:
    var prc = c.generics[i].inst.sym
    if prc.kind in {skProc, skFunc, skMethod, skConverter} and prc.magic == mNone:
      if prc.ast == nil or prc.ast[bodyPos] == nil:
        internalError(c.config, prc.info, "no code for " & prc.name.s)
      else:
        n.add prc.ast
  c.lastGenericIdx = c.generics.len

proc myClose(graph: ModuleGraph; context: PPassContext, n: PNode): PNode =
  var c = PContext(context)
  if c.config.cmd == cmdIdeTools and not c.suggestionsMade:
    suggestSentinel(c)
  closeScope(c)         # close module's scope
  rawCloseScope(c)      # imported symbols; don't check for unused ones!
  reportUnusedModules(c)
  result = newNode(nkStmtList)
  if n != nil:
    internalError(c.config, n.info, "n is not nil") #result := n;
  addCodeForGenerics(c, result)
  if c.module.ast != nil:
    result.add(c.module.ast)
  popOwner(c)
  popProcCon(c)
  sealRodFile(c)

const semPass* = makePass(myOpen, myProcess, myClose,
                          isFrontend = true)
