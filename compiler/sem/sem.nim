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
    checked_ast,
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
proc addParams(c: PContext, n: PNode)
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

proc wrapErrorAndUpdate(c: ConfigRef, n: PNode, s: PSym): PNode =
  ## Wraps the erroneous AST `n` in an error node, sets it as the AST of `s`,
  ## and returns the wrapped node. Note that `s` itself is not transitioned to
  ## an ``skError``.
  result = c.wrapError(n)
  s.ast = result

template semIdeForTemplateOrGenericCheck(conf, n, cursorInBody) =
  # use only for idetools support; detecting cursor in generic or template body
  # if so call `semIdeForTemplateOrGeneric` for semantic checking
  when defined(nimsuggest):
    if conf.ideCmd in IdeLocCmds and
       n.info.fileIndex == conf.m.trackPos.fileIndex and
       n.info.line == conf.m.trackPos.line:
      cursorInBody = true

template semIdeForTemplateOrGeneric(c: PContext; n: PNode;
                                    cursorInBody: bool) =
  # provide incomplete information for idetools support in generic or template
  when defined(nimsuggest):
    if c.config.cmd == cmdIdeTools and cursorInBody:
      #if optIdeDebug in gGlobalOptions:
      #  echo "passing to safeSemExpr: ", renderTree(n)
      discard safeSemExpr(c, n)

proc fitNodePostMatch(c: PContext, n: PNode): PNode =
  ## Performs post-processing on the result of a ``paramTypesMatch``
  ## invocation. This processing is required for the expression AST to be
  ## proper typed AST!
  case n.kind
  of nkHiddenSubConv, nkHiddenStdConv:
    if n.typ.kind in {tyVar, tyLent}:
      # drop the modifier
      # XXX: this is a workaround. ``sigmatch`` shouldn't produce these
      #      conversions in the first place
      n.typ = n.typ.base

    if n[1].kind in {nkCurly, nkTupleConstr, nkNilLit}:
      # special handling for these nodes: the conversion is treated as an
      # instruction to re-type the expression
      let x = changeType(c, n[1], n.typ, check=true)
      if x.isError:
        # keep the conversion
        n[1] = x
        c.config.wrapError(n)
      else:
        x # skip the conversion node; it was applied
    elif n.typ.kind == tyTuple and n[1].typ.skipTypes({tySink}).kind == tyTuple:
      # conversion between named/unnamed tuples -> apply the type directly
      n[1].typ = n.typ
      n[1]
    elif n.typ.kind in {tyOpenArray, tyVarargs, tySequence} and
         n[1].typ.isEmptyContainer:
      # fixup empty container types
      let
        arg = n[1].typ
        elem = elemType(n.typ)
        typ = copyType(arg.skipTypes({tyGenericInst, tyAlias}),
                       nextTypeId(c.idgen), arg.owner)
        # XXX: ^^ the type skipping looks dangerous

      copyTypeProps(c.graph, c.idgen.module, typ, arg)
      typ[ord(arg.kind == tyArray)] = elem
      propagateToOwner(typ, elem)
      n[1].typ = typ

      if n.typ.kind in {tyOpenArray, tyVarargs}:
        # don't drop the conversion just yet, later processing still needs it
        n
      else:
        n[1] # the conversion was applied; drop it
    else:
      # something else; keep the conversion
      n
  of nkHiddenCallConv:
    # the argument of an injected hidden call conversion needs to be fitted
    # to!
    n[1] = fitNodePostMatch(c, n[1])
    n
  else:
    n


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
      result = fitNodePostMatch(c, result)

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

proc exprNotGenericRoutine(c: PContext, n: PNode): PNode =
  ## Checks that the analysed expression `n` is *not* an uninstantiated generic
  ## routine, and returns an error node if it is one. In the case of no error,
  ## `n` is returned.
  if n.typ != nil and n.typ.kind == tyError:
    return n

  # skip all statement list wrappers:
  var it {.cursor.} = n
  while it.kind in {nkStmtListExpr, nkBlockExpr}:
    it = it.lastSon

  if (it.kind == nkSym and it.sym.isGenericRoutineStrict) or
     it.isGenericRoutine:
    c.config.newError(n, PAstDiag(kind: adSemProcHasNoConcreteType))
  else:
    n

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

func defNameErrorNodeAllowsSymUpdate*(n: PNode): bool {.inline.} =
  ## true if `n` is an `nkError` of kind `adSemSymNameSym` and the error is
  ## minor enough to allow the recovery sym to be updated. Used to see if
  ## progress is allowed inspite of errors.
  n.kind == nkError and n.diag.kind == adSemDefNameSym and
    n.diag.defNameSymData.kind == adSemDefNameSymExpectedKindMismatch

func getDefNameSymOrRecover*(n: PNode): PSym {.inline.} =
  ## extracts the symbol from `n`, which must be an `nkSym` or `nkError` with
  ## diagnostic kind of `adSemDefNameSym`. If `n` is an error, a
  ## recovery symbol is extracted, allowing progress to be made.
  case n.kind
  of nkSym: n.sym
  of nkError:
    case n.diag.kind
    of adSemDefNameSym: n.diag.defNameSym
    else: unreachable("all error cases must be covered, got: " & $n.diag.kind)
  else:
    unreachable("no other cases supported, got: " & $n.kind)

proc newSymGNode*(kind: TSymKind, n: PNode, c: PContext): PNode =
  ## like newSymS, but considers gensym'ed symbols, analyses `n` producing a
  ## canonical symbol node (`nkSym`), or if unsuccessful an `nkError` with an
  ## `adSemDefNameSym` diagnostic.
  ## 
  ## Symbol canonicalization is is as follows:
  ## 1. `nkSym` -> match `kind` (or `skTemp`) and updates the sym owner to the
  ##               current context (gensym specific)
  ## 2. `nkIdentKinds` -> instantiate a symbol considering quoted identifiers
  ## 3. `nkError` -> simply returns the same error
  ## 4. other kinds -> new error
  # TODO: come up with a better name for this proc
  let
    info = n.info
    currOwner = getCurrOwner(c)

  template makeError(config: ConfigRef, n: PNode, sym: PSym,
                     dataKindSuffix: untyped): PNode =
    let
      dataKind = `adSemDefNameSym dataKindSuffix`
      (wrongNode, defData) =
        case dataKind
        of adSemDefNameSymExpectedKindMismatch:
          (n, AdSemDefNameSym(kind: dataKind, expectedKind: kind))
        of adSemDefNameSymIdentGenFailed:
          (n.diag.wrongNode,
          AdSemDefNameSym(kind: dataKind, identGenErr: n))
        of adSemDefNameSymExistingError,
            adSemDefNameSymIllformedAst:
          (n, AdSemDefNameSym(kind: dataKind))

    config.newError(wrongNode, PAstDiag(kind: adSemDefNameSym,
                                        defNameSym: sym,
                                        defNameSymData: defData))

  case n.kind
  of nkSym:
    result =
      # xxx: we really should guard on `sfGenSym`; but macros can transplant
      #      symbols from pretty much anywhere, so we don't know where gensym
      #      really came from.
      if n.sym.kind in {kind, skTemp}:
        n.sym.owner = currOwner # xxx: modifying the sym owner is suss
        n
      else:
        # a symbol with the wrong kind is transplanted into a definition
        # position. Create a new symbol instead of a copy in order to prevent
        # follow-up errors due to the symbol's flags, type, etc.
        let recoverySym = newSym(kind, n.sym.name, nextSymId c.idgen,
                                 currOwner, info)
        c.config.makeError(n, recoverySym, ExpectedKindMismatch)
  of nkIdent, nkAccQuoted:
    # xxx: sym choices qualify here, but shouldn't those be errors in
    #      definition positions?
    let
      (ident, err) = considerQuotedIdent(c, n)
      sym = newSym(kind, ident, nextSymId c.idgen, currOwner, info)
    result =
      if err.isNil:
        newSymNode(sym)
      else:
        c.config.makeError(err, sym, IdentGenFailed)
  of nkError:
    result = 
      case n.diag.kind
      of adSemDefNameSym:
        # TODO: this branch needs to cover all `adSemDefinitionName...` diag
        #       kinds, so `newSymGNode`, `semIdentVis`, `semIdentWithPragma` etc
        #       get along and we don't overwrap
        n
      else:
        # TODO: once known kinds are covered (above) and can differentiate
        #       from errors copied around by macros/tempaltes, then we can safely
        #       wrap this in `adSemDefinitionNameExistingError`; for now we have
        #       this defensive check.
        var innerNode = n.diag.wrongNode
        while innerNode.kind == nkError and
              innerNode.diag.kind != adSemDefNameSym:
          innerNode = n.diag.wrongNode
        doAssert innerNode.kind != nkError: # false means we're rewrapping
          "this can only happen due to a bug/potential infinite loop"

        let recoverySym = newSym(kind, c.cache.getNotFoundIdent(),
                                nextSymId c.idgen, currOwner, info)
        c.config.makeError(n, recoverySym, ExistingError)
  of nkAllNodeKinds - nkIdentKinds - {nkError} + nkSymChoices:
    let recoverySym = newSym(kind, c.cache.getNotFoundIdent(),
                             nextSymId c.idgen, currOwner, info)
    result = c.config.makeError(n, recoverySym, IllformedAst)
  when defined(nimsuggest):
    case result.kind
    of nkError:
      suggestDecl(c, n, result.diag.defNameSym)
    of nkSym:
      suggestDecl(c, n, result.sym)
    else:
      unreachable("only produces `nkSym` or `nkError`")

proc semIdentVis(c: PContext, kind: TSymKind, n: PNode,
                 allowed: TSymFlags): PSym
  # identifier with visibility
proc semIdentWithPragma(c: PContext, kind: TSymKind, n: PNode,
                        allowed: TSymFlags): PSym

proc paramsTypeCheck(c: PContext, typ: PType) {.inline.} =
  let
    kind = skProc
    t = typeAllowed(typ, kind, c)
    info = typ.n.info
  if t != nil:
    # var err: string
    # if t == typ:
    #   err = "invalid type: '$1' for $2" % [typeToString(typ), toHumanStr(kind)]
    #   if kind in {skVar, skLet, skConst}:
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
        allowedFlags: {})))

proc semDirectOp(c: PContext, n: PNode, flags: TExprFlags): PNode
proc semWhen(c: PContext, n: PNode, semCheck: bool = true): PNode
proc semTemplateExpr(c: PContext, n: PNode, s: PSym,
                     flags: TExprFlags = {}): PNode
proc semMacroExpr(c: PContext, n: PNode, sym: PSym,
                  flags: TExprFlags = {}): PNode

proc tryConstExpr(c: PContext, n: PNode): PNode =
  addInNimDebugUtils(c.config, "tryConstExpr", n, result)
  pushExecCon(c, {})
  let e = semExprWithType(c, n)
  popExecCon(c)
  if e.isError:
    return

  # XXX: returning errors here makes sense (they're unrelated to whether or
  #      not the expression is constant), but it is inconsitent with the
  #      above, where errors are treated as "not a constant expression"
  result = foldInAst(c.module, e, c.idgen, c.graph)
  if (let f = getConstExprError(c.module, result, c.idgen, c.graph); f != nil):
    return f

  proc containsUnresolvedTypeVar(n: PNode): bool {.nimcall.} =
    ## Returns whether the expression `n` contains an unresolved generic
    ## parameter. Only considers imperative contexts.
    case n.kind
    of nkSym:
      if n.sym.kind == skGenericParam:
        return true
    of routineDefs, nkImportStmt, nkImportExceptStmt, nkExportStmt,
       nkExportExceptStmt, nkFromStmt, nkBindStmt, nkMixinStmt, nkTypeSection,
       nkConstSection:
      result = false
    of nkConv, nkCast, nkHiddenSubConv, nkHiddenStdConv, nkIdentDefs,
       nkVarTuple:
      result = containsUnresolvedTypeVar(n[^1])
    else:
      for it in n.items:
        if containsUnresolvedTypeVar(it):
          return true

  if e.typ.kind == tyFromExpr or containsUnresolvedTypeVar(e):
    # XXX: a work around for unresolved generic expressions reaching here. They
    #      shouldn't, but until they don't, we at least prevent them from
    #      reaching into the compile-time evaluation machinery. Known places
    #      from which this case is triggered:
    #      - ``paramTypesMatchAux``
    return nil

  let oldErrorCount = c.config.errorCounter
  let oldErrorMax = c.config.errorMax
  let oldErrorOutputs = c.config.m.errorOutputs

  c.config.m.errorOutputs = {}
  c.config.errorMax = high(int) # `setErrorMaxHighMaybe` not appropriate here

  result = evalConstExpr(c.module, c.idgen, c.graph, result)
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

  # we first perform constant folding in the AST and then attempt to fold
  # the expression. This is a bit more efficient than doing it the other way
  # around
  result = foldInAst(c.module, n, c.idgen, c.graph)
  if (let f = getConstExprError(c.module, result, c.idgen, c.graph); f != nil):
    # constant folding was successful or resulted in an error
    return f

  # evaluate the expression with the VM:
  let res = evalConstExpr(c.module, c.idgen, c.graph, result)
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
  pushExecCon(c, {})
  let e = semExprWithType(c, n)
  popExecCon(c)
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

when not defined(nimHasSinkInference):
  {.pragma: nosinks.}

include hlo, seminst, semcall

template resultTypeIsInferrable(typ: PType): untyped =
  typ.isMetaType and typ.kind != tyTypeDesc and
    (typ.kind notin tyUserTypeClasses or not typ.isResolvedUserTypeClass)

proc semAfterMacroCall(c: PContext, call, macroResult: PNode,
                       s: PSym, flags: TExprFlags): PNode =
  ## Semantically check the output of a macro.
  ## This involves processes such as re-checking the macro output for type
  ## coherence, making sure that variables declared with 'let' aren't
  ## reassigned, and binding the unbound identifiers that the macro output
  ## contains.
  c.config.addInNimDebugUtils("semAfterMacroCall", s, macroResult, result)

  proc resetSemFlag(n: PNode) {.nimcall.} =
    if n != nil:
      excl n.flags, nfSem
      case n.kind
      of nkError:
        discard
      else:
        for i in 0..<n.safeLen:
          resetSemFlag(n[i])

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
      if s.ast.isGenericRoutine and retType.isMetaType:
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
      if resultTypeIsInferrable(retType):
        # this is a "return type inference" scenario. There's no return type
        # to infer, but the expression still needs to use the proper type
        result = inferWithMetatype(c, retType, result)
      else:
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

  # XXX: also check for ``efFromHlo`` like ``semTemplateExpr`` does?
  # XXX: using ``evalTemplateArgs`` (for both macros and templates) should not
  #      be needed. If the routine doesn't match the provided arguments (both
  #      generic and normal), invoking it should simply not be attempted.
  let args = evalTemplateArgs(n, sym, c.config, fromHlo=false)
  if args.kind == nkError:
    return args

  let reportTraceExpand = c.config.macrosToExpand.hasKey(sym.name.s)
  var original: PNode
  if reportTraceExpand:
    original = n

  result = evalMacroCall(
    c.module, c.idgen, c.graph, c.templInstCounter, n, args, sym)

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

include semtypes, semtempl, semgnrc, semstmts, semexprs

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

  proc isImportSystemStmt(g: ModuleGraph; n: PNode): bool {.nimcall.} =
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

  proc isEmptyTree(n: PNode): bool {.nimcall.} =
    ## true if `n` is empty that shouldn't count as a top level statement
    case n.kind
    of nkStmtList:
      for it in n:
        if not isEmptyTree(it): return false
      result = true
    of nkEmpty, nkCommentStmt: result = true
    else: result = false

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

  result = foldInAst(c.module, result, c.idgen, c.graph)
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

  if module.position >= graph.libs.len:
    graph.libs.setLen(module.position + 1)

  if module.position < graph.transformed.len:
    # discard the cached transformed bodies
    graph.transformed[module.position].reset()

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
  c.executionCons.setLen(1)

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

proc appendInstancedGenericRuntimeRoutines(c: PContext, n: PNode) =
  ## appends to the `n`ode all run-time generic routines instantiated due to
  ## the semantic analysis for the current module in `PContext.generics`
  ## starting from `PContext.lastGenericIdx`. This will append all non-magic
  ## procedure, function, method, and converter symbols, and errors. Updates
  ## `lastGenericIdx` to allow for the next module's generic instantiations.
  for i in c.lastGenericIdx..<c.generics.len:
    var prc = c.generics[i].inst.sym
    if prc.kind in {skProc, skFunc, skMethod, skConverter} and prc.magic == mNone:
      c.config.internalAssert(
        prc.ast != nil and (prc.ast.kind == nkError or prc.ast[bodyPos] != nil),
        prc.info,
        "no code for " & prc.name.s)
      case prc.ast.kind
      of nkError:
        # xxx: should error reporting happen here or in `myClose`? Might be
        #      even better than sempass2.
        n.add prc.ast
      of nkProcDef, nkFuncDef, nkMethodDef, nkConverterDef:
        n.add prc.ast
      else:
        unreachable()
  c.lastGenericIdx = c.generics.len

proc sealRodFile(c: PContext) =
  if c.config.symbolFiles != disabledSf:
    if c.graph.vm != nil:
      for (m, n) in PEvalContext(c.graph.vm).vm.vmstateDiff:
        if m == c.module:
          addPragmaComputation(c, n)
    c.idgen.sealed = true # no further additions are allowed

proc myClose(graph: ModuleGraph; context: PPassContext, n: PNode): PNode =
  var c = PContext(context)
  if c.config.cmd == cmdIdeTools and not c.suggestionsMade:
    suggestSentinel(c)

  # setup the symbols for the globals that store the handles of loaded
  # dynamic libraries:
  for id, it in c.libs:
    if it.kind == libDynamic:
      let
        info = c.module.info
        s = newSym(skVar, c.cache.getIdent("lib" & $id.index),
                   nextSymId(c.idgen), c.module, info)
      s.typ = graph.getSysType(info, tyPointer)
      s.flags.incl sfGlobal
      it.name = s

  storeLibs(graph, c.idgen.module)
  closeScope(c)         # close module's scope
  rawCloseScope(c)      # imported symbols; don't check for unused ones!
  reportUnusedModules(c)
  result = newNode(nkStmtList)
  c.config.internalAssert(n == nil, n.info, "n is not nil") #result := n;
  appendInstancedGenericRuntimeRoutines(c, result)
  if c.module.ast != nil:
    result.add(c.module.ast)
  popOwner(c)
  popProcCon(c)
  sealRodFile(c)

const semPass* = makePass(myOpen, myProcess, myClose,
                          isFrontend = true)
