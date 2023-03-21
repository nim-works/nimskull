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
  std/[
    strutils,
    math,
    strtabs,
    intsets,
    sets,       # used for markOwnerModuleAsUsed
    tables,     # used for markOwnerModuleAsUsed
  ],
  compiler/ast/[
    ast,
    astalgo,
    trees,
    wordrecg,
    renderer,
    types,
    nimsets,
    errorreporting,
    errorhandling,
    astmsgs,
    lineinfos,
    idents,
    enumtostr,
    linter
  ],
  compiler/modules/[
    magicsys,
    modulepaths,
    importer,
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    ropes,
    platform,
    nversion,
    debugutils,
    int128,
    astrepr,
    idioms
  ],
  compiler/sem/[
    semfold,
    typeallowed,
    isolation_check,
    procfind,
    lookups,
    pragmas,
    passes,
    semdata,
    semtypinst,
    sigmatch,
    transf,
    aliases,
    sempass2,
    patterns,
    parampatterns,
    evaltempl,
    lowerings,
  ],
  compiler/backend/[
    cgmeth
  ],
  compiler/plugins/[
    active
  ],
  compiler/vm/[
    compilerbridge,
    vmdef,
  ]

from std/options as std_options import some, none

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport,
  reportAst,
  reportSem,       # xxx: used by `semcall` at least
  reportStr,       # xxx: used by `semtypes` at least
  reportSym,
  reportSymbols,   # xxx: used by `semcall` at least
  reportTyp        # xxx: used by `semtypes` at least

# TODO: `semtypes` misuses `VMReport` to indicate a compile time error, it's a
#       semantic analysis error born of compile time evaluation
from compiler/ast/reports_vm import VMReport
from compiler/ast/report_enums import ReportKind

when defined(nimsuggest):
  # TODO: used in `semexprs.tryIt` for the report hook, it's far too broad and
  #       it's silly that the compiler hook looks so broadly
  from compiler/ast/reports import Report

import compiler/tools/suggest

when defined(nimfix):
  import compiler/nimfix/prettybase

# implementation

proc semExpr(c: PContext, n: PNode, flags: TExprFlags = {}): PNode
proc semExprWithType(c: PContext, n: PNode, flags: TExprFlags = {}): PNode
proc semExprNoType(c: PContext, n: PNode): PNode
proc semExprNoDeref(c: PContext, n: PNode, flags: TExprFlags = {}): PNode
proc semProcBody(c: PContext, n: PNode): PNode

proc fitNode(c: PContext, formal: PType, arg: PNode; info: TLineInfo): PNode
proc changeType(c: PContext; n: PNode, newType: PType, check: bool): PNode

proc semTypeNode(c: PContext, n: PNode, prev: PType): PType
proc semStmt(c: PContext, n: PNode; flags: TExprFlags): PNode
proc semOpAux(c: PContext, n: PNode): bool
proc semParamList(c: PContext, n, genericParams: PNode, kind: TSymKind): PType
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
proc defaultConstructionError(c: PContext, t: PType, n: PNode): PNode
proc hasUnresolvedArgs(c: PContext, n: PNode): bool
proc isArrayConstr(n: PNode): bool {.inline.} =
  result = n.kind == nkBracket and
    n.typ.skipTypes(abstractInst).kind == tyArray

proc wrapErrorAndUpdate(c: ConfigRef, n: PNode, s: PSym): PNode =
  ## Wraps the erroneous AST `n` in an error node, sets it as the AST of `s`,
  ## and returns the wrapped node. Note that `s` itself is not transitioned to
  ## an ``skError``.
  result = c.wrapError(n)
  s.ast = result

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
  var
    a = arg
    x = a.mutableSkipConv
  if (x.kind == nkCurly and formal.kind == tySet and formal.base.kind != tyGenericParam) or
    (x.kind in {nkPar, nkTupleConstr}) and formal.kind notin {tyUntyped, tyBuiltInTypeClass}:
    x = changeType(c, x, formal, check=true)
    
    if x.isError:
      result = c.config.wrapError(a)
      return

  result = a
  result = skipHiddenSubConv(result, c.graph, c.idgen)


proc fitNode(c: PContext, formal: PType, arg: PNode; info: TLineInfo): PNode =
  if arg.kind == nkError:
    result = arg
    return

  if arg.typ.isNil:
    c.config.localReport(arg.info, reportAst(rsemExpressionHasNoType, arg))

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

    if a.kind == nkError:
      result = c.config.wrapError(result)
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
  addInNimDebugUtils(c.config, "commonType", y, x, result)
  result = x
  if endsInNoReturn(y): return
  result = commonType(c, x, y.typ)

proc newSymS(kind: TSymKind, n: PNode, c: PContext): PSym =
  let (ident, err) = considerQuotedIdent(c, n)
  if err != nil:
    localReport(c.config, err)
  result = newSym(kind, ident, nextSymId c.idgen, getCurrOwner(c), n.info)
  when defined(nimsuggest):
    suggestDecl(c, n, result)

proc newSymG*(kind: TSymKind, n: PNode, c: PContext): PSym =
  # like newSymS, but considers gensym'ed symbols
  case n.kind
  of nkSym:
    # and sfGenSym in n.sym.flags:
    result = n.sym
    if result.kind notin {kind, skTemp}:
      localReport(c.config, n.info, SemReport(
        kind: rsemSymbolKindMismatch,
        sym: result,
        expectedSymbolKind: {kind}))

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
  else: # xxx: should know the kinds and error out if not valid
    let (ident, err) = considerQuotedIdent(c, n)
    if err != nil:
      localReport(c.config, err)
    result = newSym(kind, ident, nextSymId c.idgen, getCurrOwner(c), n.info)
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
    # var err: string
    # if t == typ:
    #   err = "invalid type: '$1' for $2" % [typeToString(typ), toHumanStr(kind)]
    #   if kind in {skVar, skLet, skConst} and taIsTemplateOrMacro in flags:
    #     err &= ". Did you mean to call the $1 with '()'?" % [toHumanStr(typ.owner.kind)]
    # else:
    #   err = "invalid type: '$1' in this context: '$2' for $3" % [typeToString(t),
    #           typeToString(typ), toHumanStr(kind)]

    localReport(c.config, info, SemReport(
      kind: rsemTypeNotAllowed,
      allowedType: (
        allowed: t,
        actual: typ,
        kind: kind,
        allowedFlags: flags)))

proc paramsTypeCheck(c: PContext, typ: PType) {.inline.} =
  typeAllowedCheck(c, typ.n.info, typ, skProc)

proc semDirectOp(c: PContext, n: PNode, flags: TExprFlags): PNode
proc semWhen(c: PContext, n: PNode, semCheck: bool = true): PNode
proc semTemplateExpr(c: PContext, n: PNode, s: PSym,
                     flags: TExprFlags = {}): PNode
proc semMacroExpr(c: PContext, n: PNode, sym: PSym,
                  flags: TExprFlags = {}): PNode

proc symFromType(c: PContext; t: PType, info: TLineInfo): PSym =
  if t.sym != nil: return t.sym
  result = newSym(skType, getIdent(c.cache, "AnonType"), nextSymId c.idgen, t.owner, info)
  result.flags.incl sfAnon
  result.typ = t

proc symNodeFromType(c: PContext, t: PType, info: TLineInfo): PNode =
  result = newSymNode(symFromType(c, t, info), info)
  result.typ = makeTypeDesc(c, t)

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
  # XXX: `fixupTypeAfterEval` is not really needed anymore
  when true:
    if eOrig.typ.kind in {tyUntyped, tyTyped, tyTypeDesc}:
      # XXX: is this case still used now?
      result = semExprWithType(c, evaluated)
    else:
      result = evaluated
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
  addInNimDebugUtils(c.config, "tryConstExpr", n, result)
  let e = semExprWithType(c, n)
  if e.isError:
    return

  # XXX: ``getConstExpr`` can and does report errors via ``localReport``. In
  #      case it happens, ``tryConstExpr`` doesn't return nil but compilation
  #      fails. This behaviour does make sense, but non-vmgen error should
  #      also not be ignored here then
  result = getConstExpr(c.module, e, c.idgen, c.graph)
  if result != nil: return

  let oldErrorCount = c.config.errorCounter
  let oldErrorMax = c.config.errorMax
  let oldErrorOutputs = c.config.m.errorOutputs

  c.config.m.errorOutputs = {}
  c.config.errorMax = high(int) # `setErrorMaxHighMaybe` not appropriate here

  result = evalConstExpr(c.module, c.idgen, c.graph, e)
  case result.kind
  of nkEmpty, nkError:
    result = nil
  else:
    discard

  c.config.errorCounter = oldErrorCount
  c.config.errorMax = oldErrorMax
  c.config.m.errorOutputs = oldErrorOutputs

proc evalConstExpr(c: PContext, n: PNode): PNode =
  ## Tries to turn the expression `n` into AST that represents a concrete
  ## value. If this fails, an `nkError` node is returned
  addInNimDebugUtils(c.config, "evalConstExpr", n, result)
  assert not n.isError

  # this happens when the overloadableEnums is enabled. We short-circuit
  # evaluation in this case, as neither ``vmgen`` nor ``semfold`` know what to
  # do with the sym-choice
  # TODO: this should be handled at the callsite instead
  if n.kind in nkSymChoices and n[0].typ.skipTypes(abstractInst).kind == tyEnum:
    return n

  result = getConstExpr(c.module, n, c.idgen, c.graph)
  if result != nil:
    # constant folding was successful
    return

  # evaluate the expression with the VM:
  let res = evalConstExpr(c.module, c.idgen, c.graph, n)
  assert res != nil

  result =
    case res.kind
    of nkEmpty:
      c.config.newError(n, PAstDiag(kind: adSemConstExprExpected))
    of nkError:
      # pass the error on
      res
    else:
      res

proc semConstExpr(c: PContext, n: PNode): PNode =
  ## Analyses the expression `n` and, on success, tries to evaluate it (i.e.
  ## materialize it into AST that represents a concrete value). If the analysis
  ## part fails, returns `n` -- if the evaluation fails returns the sem-checked
  ## expression
  addInNimDebugUtils(c.config, "semConstExpr", n, result)
  # TODO: propagate the error upwards instead of reporting it here. Also
  #       remove the error correction -- that should be done at the callsite,
  #       if needed

  let e = semExprWithType(c, n)
  if e.isError:
    localReport(c.config, e)
    return n

  result = evalConstExpr(c, e)
  if result.isError:
    localReport(c.config, result)
    result = e # error correction

proc semRealConstExpr(c: PContext, n: PNode): PNode =
  ## Semantically analyses the expression `n` and evaluates it. An error is
  ## returned if the expression either contains an error or is not a constant
  ## expression.
  addInNimDebugUtils(c.config, "semRealConstExpr", n, result)
  assert not n.isError

  result = semExprWithType(c, n)
  if result.kind != nkError:
    result = evalConstExpr(c, result)

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
    case n.kind
    of nkError:
      discard
    else:
      for i in 0..<n.safeLen:
        resetSemFlag(n[i])

proc semAfterMacroCall(c: PContext, call, macroResult: PNode,
                       s: PSym, flags: TExprFlags): PNode =
  ## Semantically check the output of a macro.
  ## This involves processes such as re-checking the macro output for type
  ## coherence, making sure that variables declared with 'let' aren't
  ## reassigned, and binding the unbound identifiers that the macro output
  ## contains.
  c.config.addInNimDebugUtils("semAfterMacroCall", s, macroResult, result)
  inc(c.config.evalTemplateCounter)
  if c.config.evalTemplateCounter > evalTemplateLimit:
    globalReport(c.config, s.info, SemReport(kind: rsemTemplateInstantiationTooNested))
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
        let err = newError(c.config, result, PAstDiag(kind: adSemExpressionHasNoType))
        localReport(c.config, err)
        result = newSymNode(errorSym(c, result, err))
      else:
        result.typ = makeTypeDesc(c, typ)
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
  dec(c.config.evalTemplateCounter)
  discard c.friendModules.pop()

proc semMacroExpr(c: PContext, n: PNode, sym: PSym,
                  flags: TExprFlags = {}): PNode =
  c.config.addInNimDebugUtils("semMacroExpr", sym, n, result)
  rememberExpansion(c, n.info, sym)
  pushInfoContext(c.config, n.info, sym)

  let info = getCallLineInfo(n)
  markUsed(c, info, sym)
  if sym == c.p.owner:
    globalReport(c.config, info, reportSym(rsemCyclicDependency, sym))

  let
    genericParams = sym.ast[genericParamsPos].safeLen
    suppliedParams = max(n.safeLen - 1, 0)

  if suppliedParams < genericParams:
    globalReport(
      c.config, info, reportAst(
        rsemMissingGenericParamsForTemplate, n, sym = sym))

  let reportTraceExpand = c.config.macrosToExpand.hasKey(sym.name.s)
  var original: PNode
  if reportTraceExpand:
    original = n

  result = evalMacroCall(
    c.module, c.idgen, c.graph, c.templInstCounter, n, sym)

  if efNoSemCheck notin flags:
    result = semAfterMacroCall(c, n, result, sym, flags)

  if reportTraceExpand:
    c.config.localReport(n.info, SemReport(
      sym: sym,
      kind: rsemExpandMacro,
      ast: original,
      expandedAst: result))

  result = wrapInComesFrom(n.info, sym, result)
  popInfoContext(c.config)

proc forceBool(c: PContext, n: PNode): PNode =
  result = fitNode(c, getSysType(c.graph, n.info, tyBool), n, n.info)
  if result == nil: result = n

proc semConstBoolExpr(c: PContext, n: PNode): PNode =
  result = forceBool(c, semConstExpr(c, n))
  if result.kind != nkIntLit:
    localReport(c.config, n, reportSem rsemConstExprExpected)

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
  addInNimDebugUtils(c.config, "semStmtAndGenerateGenerics", n, result)

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

  graph.config.internalAssert(c.p == nil, module.info, "sem.myOpen")

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
      localReport(c.config, c.unusedImports[i][1], reportSym(
        rsemUnusedImport, c.unusedImports[i][0]))

proc addCodeForGenerics(c: PContext, n: PNode) =
  for i in c.lastGenericIdx..<c.generics.len:
    var prc = c.generics[i].inst.sym
    if prc.kind in {skProc, skFunc, skMethod, skConverter} and prc.magic == mNone:
      c.config.internalAssert(prc.ast != nil and prc.ast[bodyPos] != nil, prc.info, "no code for " & prc.name.s)

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
  c.config.internalAssert(n == nil, n.info, "n is not nil") #result := n;
  addCodeForGenerics(c, result)
  if c.module.ast != nil:
    result.add(c.module.ast)
  popOwner(c)
  popProcCon(c)
  sealRodFile(c)

const semPass* = makePass(myOpen, myProcess, myClose,
                          isFrontend = true)
