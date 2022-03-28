#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements semantic checking for pragmas

import
  std/[
    strutils,
    math,
    os
  ],
  ast/[
     ast,
     astalgo,
     idents,
     renderer,
     wordrecg,
     trees,
     linter,
     errorhandling,
     reports,
     lineinfos
  ],
  modules/[
    magicsys
  ],
  front/[
    msgs,
    options
  ],
  utils/[
    ropes,
    pathutils,
    debugUtils
  ],
  sem/[
    semdata,
    lookups
  ],
  backend/[
    extccomp
  ]



from ic/ic import addCompilerProc

const
  FirstCallConv* = wNimcall
  LastCallConv* = wNoconv

const
  declPragmas = {wImportc, wImportObjC, wImportCpp, wImportJs, wExportc, wExportCpp,
    wExportNims, wExtern, wDeprecated, wNodecl, wError, wUsed}
    ## common pragmas for declarations, to a good approximation
  procPragmas* = declPragmas + {FirstCallConv..LastCallConv,
    wMagic, wNoSideEffect, wSideEffect, wNoreturn, wNosinks, wDynlib, wHeader,
    wCompilerProc, wNonReloadable, wCore, wProcVar, wVarargs, wCompileTime, wMerge,
    wBorrow, wImportCompilerProc, wThread,
    wAsmNoStackFrame, wDiscardable, wNoInit, wCodegenDecl,
    wGensym, wInject, wRaises, wEffectsOf, wTags, wLocks, wDelegator, wGcSafe,
    wConstructor, wLiftLocals, wStackTrace, wLineTrace, wNoDestroy,
    wEnforceNoRaises}
  converterPragmas* = procPragmas
  methodPragmas* = procPragmas+{wBase}-{wImportCpp}
  templatePragmas* = {wDeprecated, wError, wGensym, wInject, wDirty,
    wDelegator, wExportNims, wUsed, wPragma}
  macroPragmas* = declPragmas + {FirstCallConv..LastCallConv,
    wMagic, wNoSideEffect, wCompilerProc, wNonReloadable, wCore,
    wDiscardable, wGensym, wInject, wDelegator}
  iteratorPragmas* = declPragmas + {FirstCallConv..LastCallConv, wNoSideEffect, wSideEffect,
    wMagic, wBorrow,
    wDiscardable, wGensym, wInject, wRaises, wEffectsOf,
    wTags, wLocks, wGcSafe}
  exprPragmas* = {wLine, wLocks, wNoRewrite, wGcSafe, wNoSideEffect}
  stmtPragmas* = {wChecks, wObjChecks, wFieldChecks, wRangeChecks,
    wBoundChecks, wOverflowChecks, wNilChecks, wStaticBoundchecks,
    wStyleChecks, wAssertions,
    wWarnings, wHints,
    wLineDir, wStackTrace, wLineTrace, wOptimization, wHint, wWarning, wError,
    wFatal, wDefine, wUndef, wCompile, wLink, wLinksys, wPure, wPush, wPop,
    wPassl, wPassc, wLocalPassc,
    wDeadCodeElimUnused,  # deprecated, always on
    wDeprecated,
    wFloatChecks, wInfChecks, wNanChecks, wPragma, wEmit, wUnroll,
    wLinearScanEnd, wPatterns, wTrMacros, wEffects, wNoForward, wReorder, wComputedGoto,
    wExperimental, wThis, wUsed, wAssert}
  lambdaPragmas* = {FirstCallConv..LastCallConv,
    wNoSideEffect, wSideEffect, wNoreturn, wNosinks, wDynlib, wHeader,
    wThread, wAsmNoStackFrame,
    wRaises, wLocks, wTags, wEffectsOf,
    wGcSafe, wCodegenDecl, wNoInit, wCompileTime}
  typePragmas* = declPragmas + {wMagic, wAcyclic,
    wPure, wHeader, wCompilerProc, wCore, wFinal, wSize, wShallow,
    wIncompleteStruct, wCompleteStruct, wByCopy, wByRef,
    wInheritable, wGensym, wInject, wRequiresInit, wUnchecked, wUnion, wPacked,
    wCppNonPod, wBorrow, wGcSafe, wPartial, wExplain, wPackage}
  fieldPragmas* = declPragmas + {wGuard, wBitsize, wCursor,
    wRequiresInit, wNoalias, wAlign} - {wExportNims, wNodecl} # why exclude these?
  varPragmas* = declPragmas + {wVolatile, wRegister, wThreadVar,
    wMagic, wHeader, wCompilerProc, wCore, wDynlib,
    wNoInit, wCompileTime, wGlobal,
    wGensym, wInject, wCodegenDecl,
    wGuard, wGoto, wCursor, wNoalias, wAlign}
  constPragmas* = declPragmas + {wHeader, wMagic,
    wGensym, wInject,
    wIntDefine, wStrDefine, wBoolDefine, wCompilerProc, wCore}
  paramPragmas* = {wNoalias, wInject, wGensym}
  letPragmas* = varPragmas
  procTypePragmas* = {FirstCallConv..LastCallConv, wVarargs, wNoSideEffect,
                      wThread, wRaises, wEffectsOf, wLocks, wTags, wGcSafe}
  forVarPragmas* = {wInject, wGensym}
  allRoutinePragmas* = methodPragmas + iteratorPragmas + lambdaPragmas
  enumFieldPragmas* = {wDeprecated}

proc getPragmaVal*(procAst: PNode; name: TSpecialWord): PNode =
  let p = procAst[pragmasPos]
  if p.kind == nkEmpty: return nil
  for it in p:
    if it.kind in nkPragmaCallKinds and it.len == 2 and it[0].kind == nkIdent and
        it[0].ident.id == ord(name):
      return it[1]

proc pragma*(c: PContext, sym: PSym, n: PNode, validPragmas: TSpecialWords;
            isStatement: bool = false): PNode {.discardable.}

proc recordPragma(c: PContext; n: PNode; args: varargs[string]) =
  var recorded = newNodeI(nkReplayAction, n.info)
  for i in 0..args.high:
    recorded.add newStrNode(args[i], n.info)
  addPragmaComputation(c, recorded)

proc invalidPragma*(c: PContext; n: PNode): PNode =
  ## create an error node (`nkError`) for an invalid pragma error
  c.config.newError(n, reportAst(rsemInvalidPragma, n))

proc illegalCustomPragma*(c: PContext; n: PNode, s: PSym): PNode =
  ## create an error node (`nkError`) for an illegal custom pragma error
  c.config.newError(
    n,
    reportSym(rsemIllegalCustomPragma, s, ast = n),
    @[newSymNode(s)],
    n.info
  )

proc pragmaAsm*(c: PContext, n: PNode): tuple[marker: char, err: PNode] =
  ## Gets the marker out of an asm stmts pragma list
  ## Returns ` as the default marker if no other markers are found
  result.marker = '`'
  if n != nil:
    for i in 0..<n.len:
      let it = n[i]
      if it.kind in nkPragmaCallKinds and it.len == 2 and it[0].kind == nkIdent:
        case whichKeyword(it[0].ident)
        of wSubsChar:
          if it[1].kind == nkCharLit: result.marker = chr(it[1].intVal)
          else: result.err = invalidPragma(c, it)
        else: result.err = invalidPragma(c, it)
      else: result.err = invalidPragma(c, it)

# xxx: the procs returning `SetExternNameStatus` names were introduced in order
#      to avoid carrying out IO/error effects within, instead signaling the
#      state and allowing the caller to deal with them. The pattern is a bit
#      noisey given the current state of nimskull. fixing it would entail a bit
#      more design time and work than was avaiable.

type
  SetExternNameStatus = enum
    ## used by `setExternName` and procs that depend upon it to signal extern
    ## name handling succcess/failure
    ExternNameSet       # successfully set the name, default
    ExternNameSetFailed # failed to set the name

proc setExternName(c: PContext; s: PSym, ext: string): SetExternNameStatus =
  ## sets an `ext`ern name, on `s`ymbol and returns a success or failure status

  result = ExternNameSet # we're optimistic, because most paths are successful

  # xxx: only reason we have to handle errors is because of the name lookup
  #      that can fail, instead if we separate that out it'll clean-up this and
  #      the call-sites

  # special cases to improve performance:
  if ext == "$1":
    s.loc.r = rope(s.name.s)
  elif '$' notin ext:
    s.loc.r = rope(ext)
  else:
    try:
      s.loc.r = rope(ext % s.name.s)
    except ValueError:
      result = ExternNameSetFailed
  if c.config.cmd == cmdNimfix and '$' notin ext:
    # note that '{.importc.}' is transformed into '{.importc: "$1".}'
    s.loc.flags.incl(lfFullExternalName)

proc makeExternImport(c: PContext; s: PSym, ext: string): SetExternNameStatus =
  ## produces (mutates) `s`'s `loc`ation setting the import name, marks it as
  ## an import and notes it as not forward declared, then returns a
  ## success/failure
  result = setExternName(c, s, ext)
  incl(s.flags, sfImportc)
  excl(s.flags, sfForward)

proc makeExternExport(c: PContext; s: PSym, ext: string): SetExternNameStatus =
  ## produces (mutates) `s`'s `loc`ation setting the export name, marks it as
  ## an export c, and returns a success/failure
  result = setExternName(c, s, ext)
  incl(s.flags, sfExportc)

proc processImportCompilerProc(c: PContext; s: PSym, ext: string): SetExternNameStatus =
  ## produces (mutates) `s`'s `loc`ation setting the imported compiler proc
  ## name `ext`. marks it as import c and no forward declaration, sets the
  ## location as a compiler proc import, and returns a success/failure
  result = setExternName(c, s, ext)
  incl(s.flags, sfImportc)
  excl(s.flags, sfForward)
  incl(s.loc.flags, lfImportCompilerProc)

proc processImportCpp(c: PContext; s: PSym, ext: string): SetExternNameStatus =
  ## produces (mutates) `s`'s `loc`ation setting the imported cpp proc
  ## name `ext`. marks it as import c, an infix call (dot), and no forward
  ## declaration. If the backend is configured to C, marks the symbol as
  ## compiles to C++, and sets the global options to generate mixed C/C++ code,
  ## and returns a success/failure
  result = setExternName(c, s, ext)
  s.flags.incl {sfImportc, sfInfixCall}
  s.flags.excl sfForward
  if c.config.backend == backendC:
    s.getModule().flags.incl sfCompileToCpp
  incl c.config, optMixedMode

proc processImportObjC(c: PContext; s: PSym, ext: string): SetExternNameStatus =
  ## produces (mutates) `s`'s `loc`ation setting the imported objc proc
  ## name `ext`. marks it as import c, named param call, and no forward
  ## declaration, sets the current module to comiple to objc, and
  ## returns a success/failure.
  result = setExternName(c, s, ext)
  s.flags.incl {sfImportc, sfNamedParamCall}
  s.flags.excl sfForward
  s.getModule().flags.incl sfCompileToObjc

proc newEmptyStrNode(c: PContext; n: PNode): PNode {.noinline.} =
  result = newNodeIT(nkStrLit, n.info, getSysType(c.graph, n.info, tyString))
  result.strVal = ""

proc getStrLitNode(c: PContext, n: PNode): PNode =
  ## returns a PNode that's either an error or a string literal node
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    c.config.newError(n, SemReport(kind: rsemStringLiteralExpected))
  else:
    n[1] = c.semConstExpr(c, n[1])
    case n[1].kind
    of nkStrLit, nkRStrLit, nkTripleStrLit:
      n[1]
    else:
      # xxx: this is a potential bug, but requires a lot more tests in place
      #      for pragmas prior to changing, but we're meant to return n[1], yet
      #      on error we return a wrapped `n`, that's the wrong level of AST.
      c.config.newError(n, SemReport(kind: rsemStringLiteralExpected))


proc strLitToStrOrErr(c: PContext, n: PNode): (string, PNode) =
  ## extracts the string from an string literal, or errors if it's not a string
  ## literal or doesn't evaluate to one
  let r = getStrLitNode(c, n)
  case r.kind
  of nkStrLit, nkRStrLit, nkTripleStrLit:
    (r.strVal, nil)
  of nkError:
    ("", r)
  else:
    ("", c.config.newError(n, SemReport(kind: rsemStringLiteralExpected)))

proc intLitToIntOrErr(c: PContext, n: PNode): (int, PNode) =
  ## extracts the int from an int literal, or errors if it's not an int
  ## literal or doesn't evaluate to one
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    (-1, c.config.newError(n, SemReport(kind: rsemIntLiteralExpected)))
  else:
    n[1] = c.semConstExpr(c, n[1])
    case n[1].kind
    of nkIntLit..nkInt64Lit:
      (int(n[1].intVal), nil)
    else:
      (-1, c.config.newError(n, SemReport(kind: rsemIntLiteralExpected)))

proc getOptionalStrLit(c: PContext, n: PNode, defaultStr: string): PNode =
  ## gets an optional strlit node, used for optional arguments to pragmas,
  ## will error out if an option's value expression produces an error
  if n.kind in nkPragmaCallKinds: result = getStrLitNode(c, n)
  else: result = newStrNode(defaultStr, n.info)

proc processCodegenDecl(c: PContext, n: PNode, sym: PSym): PNode =
  ## produces (mutates) sym using the `TSym.constraint` field (xxx) to store
  ## the string literal from `n`
  result = getStrLitNode(c, n)
  sym.constraint = result

proc processMagic(c: PContext, n: PNode, s: PSym): PNode =
  ## produces an error if `n` is not a pragmacall kinds, otherwise `n` is
  ## returned as is and production (mutation) is carried out on `s`, updating
  ## the `magic` field with the name of the magic in `n` as a string literal.
  result = n
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    result = c.config.newError(n, SemReport(kind: rsemStringLiteralExpected))
  else:
    var v: string
    if n[1].kind == nkIdent:
      v = n[1].ident.s
    else:
      var (s, err) = strLitToStrOrErr(c, n)
      if err.isNil:
        v = s
      else:
        result = err
        return
    for m in TMagic:
      if substr($m, 1) == v:
        s.magic = m
        break
    if s.magic == mNone:
      c.config.localReport(n.info, reportStr(rsemUnknownMagic, v))

proc wordToCallConv(sw: TSpecialWord): TCallingConvention =
  # this assumes that the order of special words and calling conventions is
  # the same
  TCallingConvention(ord(ccNimCall) + ord(sw) - ord(wNimcall))

proc isTurnedOn(c: PContext, n: PNode): (bool, PNode) =
  # default to false as a "safe" value
  if n.kind in nkPragmaCallKinds and n.len == 2:
    let x = c.semConstBoolExpr(c, n[1])
    n[1] = x
    if x.kind == nkIntLit:
      (x.intVal != 0, nil)
    else:
      (false, c.config.newError(n, SemReport(kind: rsemOnOrOffExpected)))
  else:
    (false, c.config.newError(n, SemReport(kind: rsemOnOrOffExpected)))

proc onOff(c: PContext, n: PNode, op: TOptions, resOptions: var TOptions): PNode =
  ## produces an error, or toggles the setting in `resOptions` param
  let (r, err) = isTurnedOn(c, n)
  result = if err.isNil: n
           else:         err
  if r: resOptions.incl op
  else: resOptions.excl op

proc pragmaNoForward(c: PContext, n: PNode; flag=sfNoForward): PNode =
  ## `n` must be a callable pragma of length two, or an error is produced,
  ## otherwise produces (mutates) the boolean arg (2nd) in `n` and the
  ## current modules flags, enabling no forward, disables code re-ordering.
  var (isOn, err) = isTurnedOn(c, n)
  result =
    if err.isNil:
      if isOn:
        incl(c.module.flags, flag)
        c.features.incl codeReordering
      else:
        excl(c.module.flags, flag)
      n
    else:
      err

proc processCallConv(c: PContext, n: PNode): PNode =
  ## sets the calling convention on the the `c`ontext's option stack, and upon
  ## failure, eg: lack of calling convention, produces an error over `n`.
  result = n
  if n.kind in nkPragmaCallKinds and n.len == 2 and n[1].kind == nkIdent:
    let sw = whichKeyword(n[1].ident)
    case sw
    of FirstCallConv..LastCallConv:
      c.optionStack[^1].defaultCC = wordToCallConv(sw)
    else:
      result = c.config.newError(n, SemReport(kind: rsemCallconvExpected))
  else:
    result = c.config.newError(n, SemReport(kind: rsemCallconvExpected))

proc getLib(c: PContext, kind: TLibKind, path: PNode): PLib =
  for it in c.libs:
    if it.kind == kind and trees.exprStructuralEquivalent(it.path, path):
      return it

  result = newLib(kind)
  result.path = path
  c.libs.add result
  if path.kind in {nkStrLit..nkTripleStrLit}:
    result.isOverriden = options.isDynlibOverride(c.config, path.strVal)

proc expectDynlibNode(c: PContext, n: PNode): PNode =
  ## `n` must be a callable, this will produce the ast for the callable or
  ## produce a `StringLiteralExpected` error node.
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    result = c.config.newError(n, SemReport(kind: rsemStringLiteralExpected))
  else:
    # For the OpenGL wrapper we support:
    # {.dynlib: myGetProcAddr(...).}
    result = c.semExpr(c, n[1])
    if result.kind == nkSym and result.sym.kind == skConst:
      result = result.sym.ast # look it up
    if result.typ == nil or result.typ.kind notin {tyPointer, tyString, tyProc}:
      result = c.config.newError(n, SemReport(kind: rsemStringLiteralExpected))

proc processDynLib(c: PContext, n: PNode, sym: PSym): PNode =
  ## produces (mutates) the `sym` with all the dynamic libraries specified in
  ## the pragma `n`, finally return `n` as is (maybe?) or an error wrapping `n`
  result = n
  if (sym == nil) or (sym.kind == skModule):
    let libNode = expectDynlibNode(c, n)
    case libNode.kind
    of nkError:
      result = libNode
    else:
      let lib = getLib(c, libDynamic, libNode)
      if not lib.isOverriden:
        c.optionStack[^1].dynlib = lib
  else:
    if n.kind in nkPragmaCallKinds:
      let libNode = expectDynlibNode(c, n)
      case libNode.kind
      of nkError:
        result = libNode
      else:
        var lib = getLib(c, libDynamic, libNode)
        if not lib.isOverriden:
          addToLib(lib, sym)
          incl(sym.loc.flags, lfDynamicLib)
    else:
      incl(sym.loc.flags, lfExportLib)
    # since we'll be loading the dynlib symbols dynamically, we must use
    # a calling convention that doesn't introduce custom name mangling
    # cdecl is the default - the user can override this explicitly
    if sym.kind in routineKinds and sym.typ != nil and
       tfExplicitCallConv notin sym.typ.flags:
      sym.typ.callConv = ccCDecl

proc processNote(c: PContext, n: PNode): PNode =
  ## process a single pragma "note" `n`
  ## xxx: document this better, this is awful
  proc handleNote(enumVals: ReportKinds, notes: ConfNoteSet): PNode =
    let x = findStr(enumVals, n[0][1].ident.s, repNone)
    case x:
      of repNone:
        invalidPragma(c, n)

      else:
        let nk = x
        let x = c.semConstBoolExpr(c, n[1])
        n[1] = x

        if x.kind == nkIntLit and x.intVal != 0:
          incl(c.config, notes, nk)
        else:
          excl(c.config, notes, nk)

        n

  let
    validPragma = n.kind in nkPragmaCallKinds and n.len == 2
    exp =
      if validPragma: n[0]
      else:           invalidPragma(c, n)
    isBracketExpr = exp.kind == nkBracketExpr and exp.len == 2
    useExp = isBracketExpr or exp.kind == nkError
    bracketExpr =
      if useExp: exp
      else:      invalidPragma(c, n)

  result =
    if isBracketExpr:
      let cw = whichKeyword(n[0][0].ident)
      case cw:
        of wHint:           handleNote(repHintKinds,    cnCurrent)
        of wWarning:        handleNote(repWarningKinds, cnCurrent)
        of wWarningAsError: handleNote(repWarningKinds, cnWarnAsError)
        of wHintAsError:    handleNote(repHintKinds,    cnHintAsError)
        else: invalidPragma(c, n)
    else:
      bracketExpr

proc pragmaToOptions(w: TSpecialWord): TOptions {.inline.} =
  ## some pragmas are 1-to-1 mapping of options, this does that
  case w
  of wChecks: ChecksOptions
  of wObjChecks: {optObjCheck}
  of wFieldChecks: {optFieldCheck}
  of wRangeChecks: {optRangeCheck}
  of wBoundChecks: {optBoundsCheck}
  of wOverflowChecks: {optOverflowCheck}
  of wFloatChecks: {optNaNCheck, optInfCheck}
  of wNanChecks: {optNaNCheck}
  of wInfChecks: {optInfCheck}
  of wStaticBoundchecks: {optStaticBoundsCheck}
  of wStyleChecks: {optStyleCheck}
  of wAssertions: {optAssert}
  of wWarnings: {optWarns}
  of wHints: {optHints}
  of wLineDir: {optLineDir}
  of wStackTrace: {optStackTrace}
  of wLineTrace: {optLineTrace}
  of wDebugger: {optNone}
  of wProfiler: {optProfiler, optMemTracker}
  of wMemTracker: {optMemTracker}
  of wByRef: {optByRef}
  of wImplicitStatic: {optImplicitStatic}
  of wPatterns, wTrMacros: {optTrMacros}
  of wSinkInference: {optSinkInference}
  else: {}

proc processExperimental(c: PContext; n: PNode): PNode =
  ## experimental pragmas, produces (mutates) `n`, analysing the call param, or
  ## returns an error, wrapping n, and further child errors for the arg.
  result = n
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    c.features.incl oldExperimentalFeatures
  else:
    n[1] = c.semConstExpr(c, n[1])
    case n[1].kind
    of nkStrLit, nkRStrLit, nkTripleStrLit:
      try:
        let feature = parseEnum[Feature](n[1].strVal)
        c.features.incl feature
        if feature == codeReordering:
          if not isTopLevel(c):
            result = c.config.newError(n, reportSem(rsemInnerCodeReordering))
          c.module.flags.incl sfReorder
      except ValueError:
        n[1] = c.config.newError(
          n[1], reportAst(rsemUnknownExperimental, n[1]))

        result = wrapErrorInSubTree(c.config, n)
    of nkError:
      result = wrapErrorInSubTree(c.config, n)
    else:
      result = c.config.newError(n, reportSem(rsemStringLiteralExpected))

proc tryProcessOption(c: PContext, n: PNode, resOptions: var TOptions): (bool, PNode) =
  ## try to process callable pragmas that are also compiler options, the value
  ## of which is in the first part of the tuple, and any errors in the second.
  ## If the second part of the tuple is nil, then the value is trust worthy
  ##
  ## for pragmas that are options, they must be a pragma call kind, we produce
  ## (mutate) `n` with it's children analysed, and using the values update
  ## `resOptions` appropriately. Upon error, instead of the `n` production, an
  ## error node wrapping n is produced.
  result = (true, nil)
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    result = (false, nil)
  elif n[0].kind == nkBracketExpr:
    let err = processNote(c, n)
    result = (true, if err.kind == nkError: err else: nil)
  elif n[0].kind != nkIdent:
    result = (false, nil)
  else:
    let sw = whichKeyword(n[0].ident)
    if sw == wExperimental:
      let e = processExperimental(c, n)
      result = (true, if e.kind == nkError: e else: nil)
      return
    let opts = pragmaToOptions(sw)
    if opts != {}:
      let e = onOff(c, n, opts, resOptions)
      result = (true, if e.kind == nkError: e else: nil)
    else:
      case sw
      of wCallconv:
        let e = processCallConv(c, n)
        result = (true, if e.kind == nkError: e else: nil)
      of wDynlib:
        let e = processDynLib(c, n, nil)
        result = (true, if e.kind == nkError: e else: nil)
      of wOptimization:
        # debug n
        if n[1].kind != nkIdent:
          result = (false, invalidPragma(c, n))
        else:
          case n[1].ident.s.normalize
          of "speed":
            incl(resOptions, optOptimizeSpeed)
            excl(resOptions, optOptimizeSize)
          of "size":
            excl(resOptions, optOptimizeSpeed)
            incl(resOptions, optOptimizeSize)
          of "none":
            excl(resOptions, optOptimizeSpeed)
            excl(resOptions, optOptimizeSize)
          else:
            result = (false, c.config.newError(n, SemReport(
              kind: rsemWrongIdent,
              expectedIdents: @["none", "speed", "size"])))
      else:
        result = (false, nil)

proc processOption(c: PContext, n: PNode, resOptions: var TOptions): PNode =
  ## wraps `tryProcessOption`, the difference that the return is either an
  ## error or `n`.
  let (opt, err) = tryProcessOption(c, n, resOptions)
  result =
    if err.isNil or opt:
      n
    else:
      # calling conventions (boring...):
      c.config.newError(n, SemReport(kind: rsemPragmaOptionExpected))

proc processPush(c: PContext, n: PNode, start: int): PNode =
  ## produces (mutates) `n`, or an error, `start` indicates which of the
  ## current pushed options (child of n) are being produced. will wrap the
  ## child and `n` each in errors.
  result = n
  if n[start-1].kind in nkPragmaCallKinds:
    result = c.config.newError(n, reportSem(rsemUnexpectedPushArgument))
    return
  var x = pushOptionEntry(c)
  for i in start..<n.len:
    var tmp = c.config.options
    let (opt, err) = tryProcessOption(c, n[i], tmp)
    c.config.options = tmp

    if err.isNil:
      if not opt:
        # simply store it somewhere:
        if x.otherPragmas.isNil:
          x.otherPragmas = newNodeI(nkPragma, n.info)
        x.otherPragmas.add n[i]
    else:
      n[i] = err
      result = wrapErrorInSubTree(c.config, n)
      assert not cyclicTree(result)
      return

  # If stacktrace is disabled globally we should not enable it
  if optStackTrace notin c.optionStack[0].options:
    c.config.excl(optStackTrace)

  c.config.localReport(n.info, DebugReport(
    kind: rdbgOptionsPush, optionsNow: c.config.options))

proc processPop(c: PContext, n: PNode): PNode =
  # process a pop pragma, produces (mutates) `n` or an error wrapping `n`
  result = n
  if c.optionStack.len <= 1:
    result = c.config.newError(n, SemReport(kind: rsemMismatchedPopPush))
  else:
    popOptionEntry(c)

  c.config.localReport(n.info, DebugReport(
    kind: rdbgOptionsPop, optionsNow: c.config.options))

proc processDefine(c: PContext, n: PNode): PNode =
  ## processes pragma defines
  ## does not affect `n`, will either return it or `n` wrapped in an error if
  ## `n` is not a pragma callable, and its argument isn't an identifier.
  if (n.kind in nkPragmaCallKinds and n.len == 2) and (n[1].kind == nkIdent):
    let str = n[1].ident.s
    if defined(nimCompilerDebug) and str == "nimCompilerDebug":
      c.config.localReport(
        n.info, DebugReport(kind: rdbgTraceDefined))

    defineSymbol(c.config, str)
    n
  else:
    invalidPragma(c, n)

proc processUndef(c: PContext, n: PNode): PNode =
  ## processes pragma undefines
  ## does not affect `n`, will either return it or `n` wrapped in an error if
  ## `n` is not a pragma callable, and its argument isn't an identifier.
  if (n.kind in nkPragmaCallKinds and n.len == 2) and (n[1].kind == nkIdent):
    let str = n[1].ident.s
    if defined(nimCompilerDebug) and str == "nimCompilerDebug":
      c.config.localReport(
        n.info, DebugReport(kind: rdbgTraceUndefined))

    undefSymbol(c.config, str)
    n
  else:
    invalidPragma(c, n)

proc relativeFile(c: PContext; name: string, info: TLineInfo;
                  ext = ""): AbsoluteFile =
  ## helper proc to determine the file path given, `name`, `info`, and optional
  ## `ext`ension
  let s =
    if ext.len > 0 and splitFile(name).ext == "":
      addFileExt(name, ext)
    else:
      name
  result = AbsoluteFile parentDir(toFullPath(c.config, info)) / s
  if not fileExists(result):
    if isAbsolute(s): result = AbsoluteFile s
    else:
      result = findFile(c.config, s)
      if result.isEmpty: result = AbsoluteFile s

proc processCompile(c: PContext, n: PNode): PNode =
  ## compile pragma
  ## produces (mutates) `n`, which must be a callable, analysing its arg, or returning
  ## `n` wrapped in an error.
  result = n
  proc docompile(c: PContext; it: PNode; src, dest: AbsoluteFile; customArgs: string) =
    var cf = Cfile(nimname: splitFile(src).name,
                   cname: src, obj: dest, flags: {CfileFlag.External},
                   customArgs: customArgs)
    extccomp.addExternalFileToCompile(c.config, cf)
    recordPragma(c, it, "compile", src.string, dest.string, customArgs)

  proc getStrLit(c: PContext, n: PNode; i: int): (string, PNode) =
    n[i] = c.semConstExpr(c, n[i])
    case n[i].kind
    of nkStrLit, nkRStrLit, nkTripleStrLit:
      shallowCopy(result[0], n[i].strVal)
      result[1] = nil
    else:
      result = ("", c.config.newError(
        n, SemReport(kind: rsemStringLiteralExpected)))

  let it = if n.kind in nkPragmaCallKinds and n.len == 2: n[1] else: n
  if it.kind in {nkPar, nkTupleConstr} and it.len == 2:
    let
      (s, sErr) = getStrLit(c, it, 0)
      (dest, destErr) = getStrLit(c, it, 1)

    if sErr != nil:
      result = sErr
    elif destErr != nil:
      result = destErr
    else:
      var found = parentDir(toFullPath(c.config, n.info)) / s
      for f in os.walkFiles(found):
        let obj = completeCfilePath(c.config, AbsoluteFile(dest % extractFilename(f)))
        docompile(c, it, AbsoluteFile f, obj, "")
  else:
    var
      s = ""
      customArgs = ""
      err: PNode
    if n.kind in nkCallKinds:
      (s, err) = getStrLit(c, n, 1)
      if err.isNil:
        if n.len <= 3:
          (customArgs, err) = getStrLit(c, n, 2)
          if err != nil:
            result = err
            return
        else:
          result = c.config.newError(n, SemReport(
            kind: rsemExcessiveCompilePragmaArgs))
          return
      else:
        result = err
        return
    else:
      (s, err) = strLitToStrOrErr(c, n)
      if err != nil:
        result = err
        return

    var found = AbsoluteFile(parentDir(toFullPath(c.config, n.info)) / s)
    if not fileExists(found):
      if isAbsolute(s): found = AbsoluteFile s
      else:
        found = findFile(c.config, s)
        if found.isEmpty: found = AbsoluteFile s
    let obj = toObjFile(c.config, completeCfilePath(c.config, found, false))
    docompile(c, it, found, obj, customArgs)

proc processLink(c: PContext, n: PNode): PNode =
  result = n
  let (name, err) = strLitToStrOrErr(c, n)
  if err.isNil:
    let found = relativeFile(c, name, n.info, CC[c.config.cCompiler].objExt)
    extccomp.addExternalFileToLink(c.config, found)
    recordPragma(c, n, "link", found.string)
  else:
    result = err

proc semAsmOrEmit*(con: PContext, n: PNode, marker: char): PNode =
  case n[1].kind
  of nkStrLit, nkRStrLit, nkTripleStrLit:
    result = newNodeI(if n.kind == nkAsmStmt: nkAsmStmt else: nkArgList, n.info)
    var str = n[1].strVal
    if str == "":
      result = con.config.newError(n, SemReport(kind: rsemEmptyAsm))
      return
    # now parse the string literal and substitute symbols:
    var a = 0
    while true:
      var b = strutils.find(str, marker, a)
      var sub = if b < 0: substr(str, a) else: substr(str, a, b - 1)
      if sub != "": result.add newStrNode(nkStrLit, sub)
      if b < 0: break
      var c = strutils.find(str, marker, b + 1)
      if c < 0: sub = substr(str, b + 1)
      else: sub = substr(str, b + 1, c - 1)
      if sub != "":
        var amb = false
        var e = searchInScopes(con, getIdent(con.cache, sub), amb)
        # XXX what to do here if 'amb' is true?
        if e != nil:
          incl(e.flags, sfUsed)
          result.add newSymNode(e)
        else:
          result.add newStrNode(nkStrLit, sub)
      else:
        # an empty '``' produces a single '`'
        result.add newStrNode(nkStrLit, $marker)
      if c < 0: break
      a = c + 1
  else:
    result = con.config.newError(n, SemReport(
      ast: n,
      kind: rsemIllformedAst,
      str: "Expected string string literal for asm/emit statement [1] " &
        "but AST contains '$1'" % [$n[1].kind]))

proc pragmaEmit(c: PContext, n: PNode): PNode =
  result = n
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    result = c.config.newError(n, SemReport(kind: rsemStringLiteralExpected))
  else:
    let n1 = n[1]
    if n1.kind == nkBracket:
      var b = newNodeI(nkBracket, n1.info, n1.len)
      for i in 0..<n1.len:
        b[i] = c.semExpr(c, n1[i])
      n[1] = b
    else:
      n[1] = c.semConstExpr(c, n1)
      case n[1].kind
      of nkStrLit, nkRStrLit, nkTripleStrLit:
        n[1] = semAsmOrEmit(c, n, '`')
      else:
        result = c.config.newError(n, SemReport(kind: rsemStringLiteralExpected))

proc noVal(c: PContext; n: PNode): PNode =
  ## ensure that this pragma does not produce a value
  if n.kind in nkPragmaCallKinds and n.len > 1:
    invalidPragma(c, n)
  else:
    n

proc pragmaUnroll(c: PContext, n: PNode): PNode =
  result = n
  if c.p.nestedLoopCounter <= 0:
    result = invalidPragma(c, n)
  elif n.kind in nkPragmaCallKinds and n.len == 2:
    var (unrollFactor, err) = intLitToIntOrErr(c, n)
    if err.isNil:
      if unrollFactor <% 32:
        n[1] = newIntNode(nkIntLit, unrollFactor)
      else:
        result = invalidPragma(c, n)
    else:
      result = err

proc pragmaLine(c: PContext, n: PNode): PNode =
  result = n
  if n.kind in nkPragmaCallKinds and n.len == 2:
    n[1] = c.semConstExpr(c, n[1])
    let a = n[1]
    if a.kind in {nkPar, nkTupleConstr}:
      # unpack the tuple
      var x = a[0]
      var y = a[1]
      if x.kind == nkExprColonExpr: x = x[1]
      if y.kind == nkExprColonExpr: y = y[1]
      if x.kind != nkStrLit:
        result = c.config.newError(n, reportSem(rsemStringLiteralExpected))
      elif y.kind != nkIntLit:
        result = c.config.newError(n, reportSem(rsemIntLiteralExpected))
      else:
        n.info.fileIndex = fileInfoIdx(c.config, AbsoluteFile(x.strVal))
        n.info.line = uint16(y.intVal)
    else:
      result = c.config.newError(
        n, reportAst(rsemLinePragmaExpectsTuple, n))
  else:
    # sensible default:
    n.info = getInfoContext(c.config, -1)

proc processPragma(c: PContext, n: PNode, i: int): PNode =
  let it = n[i]
  result = it
  if it.kind notin nkPragmaCallKinds and it.safeLen == 2 or
     it.safeLen != 2 or it[0].kind != nkIdent or it[1].kind != nkIdent:
    n[i] = invalidPragma(c, it)
    result = n[i]
    return

  var userPragma = newSym(skTemplate, it[1].ident, nextSymId(c.idgen), nil,
                          it.info, c.config.options)
  userPragma.ast = newTreeI(nkPragma, n.info, n.sons[i+1..^1])
  strTableAdd(c.userPragmas, userPragma)
  result = it

proc pragmaRaisesOrTags(c: PContext, n: PNode): PNode =
  result = n
  proc processExc(c: PContext, x: PNode): PNode =
    result = x
    if c.hasUnresolvedArgs(c, x):
      x.typ = makeTypeFromExpr(c, x)
    else:
      var t = skipTypes(c.semTypeNode(c, x, nil), skipPtrs)
      if t.kind != tyObject and not t.isMetaType:
        # xxx: was errGenerated
        result = c.config.newError(x, reportTyp(
          rsemRaisesPragmaExpectsObject, t))

        return
      x.typ = t

  if n.kind in nkPragmaCallKinds and n.len == 2:
    let it = n[1]
    if it.kind notin {nkCurly, nkBracket}:
      let r = processExc(c, it)
      if r.kind == nkError:
        n[1] = r
        result = wrapErrorInSubTree(c.config, n)
    else:
      for i, e in it.pairs:
        let r = processExc(c, e)
        if r.kind == nkError:
          n[i] = r
          result = wrapErrorInSubTree(c.config, n)
          return
  else:
    result = invalidPragma(c, n)

proc pragmaLockStmt(c: PContext; it: PNode): PNode =
  result = it
  if it.kind notin nkPragmaCallKinds or it.len != 2:
    result = invalidPragma(c, it)
  else:
    let n = it[1]
    if n.kind != nkBracket:
      # xxx: was errGenerated
      it[1] = c.config.newError(n, reportSem(rsemLocksPragmaExpectsList))
      result = wrapErrorInSubTree(c.config, it)
    else:
      for i in 0..<n.len:
        n[i] = c.semExpr(c, n[i])

proc pragmaLocks(c: PContext, it: PNode): (TLockLevel, PNode) =
  if it.kind notin nkPragmaCallKinds or it.len != 2:
    result = (UnknownLockLevel, invalidPragma(c, it))
  else:
    case it[1].kind
    of nkStrLit, nkRStrLit, nkTripleStrLit:
      if it[1].strVal == "unknown":
        result = (UnknownLockLevel, nil)
      else:
        it[1] = c.config.newError(it[1], reportStr(
          rsemLocksPragmaBadLevel,
          "invalid string literal for locks pragma (only allowed string is \"unknown\")"))
        result = (UnknownLockLevel, wrapErrorInSubTree(c.config, it))
    else:
      let (x, err) = intLitToIntOrErr(c, it)
      if err.isNil:
        if x < 0 or x > MaxLockLevel:
          it[1] = c.config.newError(it[1], reportStr(
            rsemLocksPragmaBadLevel,
            "integer must be within 0.." & $MaxLockLevel))
          result = (UnknownLockLevel, wrapErrorInSubTree(c.config, it))
        else:
          result = (TLockLevel(x), nil)

proc typeBorrow(c: PContext; sym: PSym, n: PNode): PNode =
  result = n
  if n.kind in nkPragmaCallKinds and n.len == 2:
    let it = n[1]
    if it.kind != nkAccQuoted:
      result = c.config.newError(n, SemReport(kind: rsemBorrowPragmaNonDot))
      return
  incl(sym.typ.flags, tfBorrowDot)

proc markCompilerProc(c: PContext; s: PSym): PNode =
  result = nil
  # minor hack ahead: FlowVar is the only generic .compilerproc type which
  # should not have an external name set.
  # xxx: like all hacks, they incur penalties and now the error handling is
  #      ugly and this proc wants to know far more than it should... sigh
  if s.kind != skType or s.name.s != "FlowVar":
    let name = "$1"
    case makeExternExport(c, s, name)
    of ExternNameSet:
      discard
    of ExternNameSetFailed:
      result = c.config.newError(
        newSymNode(s),
        SemReport(kind: rsemInvalidExtern, sym: s, externName: name))

  s.flags.incl {sfCompilerProc, sfUsed}
  registerCompilerProc(c.graph, s)
  if c.config.symbolFiles != disabledSf:
    addCompilerProc(c.encoder, c.packedRepr, s)

proc deprecatedStmt(c: PContext; outerPragma: PNode): PNode =
  result = outerPragma
  let pragma = outerPragma[1]
  if pragma.kind in {nkStrLit..nkTripleStrLit}:
    incl(c.module.flags, sfDeprecated)
    c.module.constraint = getStrLitNode(c, outerPragma)
    if c.module.constraint.kind == nkError:
      result = wrapErrorInSubTree(c.config, outerPragma)
    return
  elif pragma.kind != nkBracket:
    result = c.config.newError(pragma, reportStr(
      rsemBadDeprecatedArgs, "list of key:value pairs expected"))
    return
  for n in pragma:
    if n.kind in nkPragmaCallKinds and n.len == 2:
      let dest = qualifiedLookUp(c, n[1], {checkUndeclared})
      if dest == nil or dest.kind in routineKinds or dest.kind == skError:
        # xxx: warnings need to be figured out, also this is just silly, why
        #      are they unreliable?
        localReport(c.config, n.info, SemReport(kind: rsemUserWarning))
      let (src, err) = considerQuotedIdent2(c, n[0])
      if err.isNil:
        let alias = newSym(skAlias, src, nextSymId(c.idgen), dest, n[0].info, c.config.options)
        incl(alias.flags, sfExported)
        if sfCompilerProc in dest.flags:
          let e = markCompilerProc(c, alias)
          if e != nil:
            result = e
            return
        addInterfaceDecl(c, alias)
        n[1] = newSymNode(dest)
      else:
        result = err
        return
    else:
      result = c.config.newError(n, reportStr(
        rsemBadDeprecatedArgs, "key:value pair expected"))
      return

proc pragmaGuard(c: PContext; it: PNode; kind: TSymKind): PSym =
  if it.kind notin nkPragmaCallKinds or it.len != 2:
    result = newSym(skError, getIdent(c.cache, "err:" & renderTree(it)),
                    nextSymId(c.idgen), getCurrOwner(c), it.info, {})
    result.ast = invalidPragma(c, it)
    return
  let n = it[1]
  if n.kind == nkSym:
    result = n.sym
  elif kind == skField:
    # First check if the guard is a global variable:
    result = qualifiedLookUp(c, n, {})
    if result.isError:
      # this is an error propagate it
      return
    elif result.isNil or result.kind notin {skLet, skVar} or
        sfGlobal notin result.flags:
      # We return a dummy symbol; later passes over the type will repair it.
      # Generic instantiation needs to know about this too. But we're lazy
      # and perform the lookup on demand instead.
      result = newSym(skUnknown, considerQuotedIdent(c, n), nextSymId(c.idgen), nil, n.info,
        c.config.options)
  else:
    result = qualifiedLookUp(c, n, {checkUndeclared})

proc semCustomPragma(c: PContext, n: PNode): PNode =
  var callNode: PNode

  if n.kind in {nkIdent, nkSym}:
    # pragma -> pragma()
    callNode = newTree(nkCall, n)
  elif n.kind == nkExprColonExpr:
    # pragma: arg -> pragma(arg)
    callNode = newTree(nkCall, n[0], n[1])
  elif n.kind in nkPragmaCallKinds:
    callNode = n
  else:
    result = invalidPragma(c, n)
    return

  let r = c.semOverloadedCall(c, callNode, {skTemplate}, {efNoUndeclared})
  if r.isNil or sfCustomPragma notin r[0].sym.flags:
    result = invalidPragma(c, n)
    return

  result = r
  # Transform the nkCall node back to its original form if possible
  if n.kind == nkIdent and r.len == 1:
    # pragma() -> pragma
    result = result[0]
  elif n.kind == nkExprColonExpr and r.len == 2:
    # pragma(arg) -> pragma: arg
    result.transitionSonsKind(n.kind)

proc processEffectsOf(c: PContext, n: PNode; owner: PSym): PNode =
  proc processParam(c: PContext; n: PNode): PNode =
    # xxx: this should use the nkError node form the semExpr?
    let r = c.semExpr(c, n)
    result =
      if r.kind == nkSym and r.sym.kind == skParam:
        if r.sym.owner == owner:
          incl r.sym.flags, sfEffectsDelayed
          n
        else:
          # xxx: was errGenerated for error handling
          c.config.newError(n, reportAst(rsemMisplacedEffectsOf, n))
      else:
        # xxx: was errGenerated for error handling
        c.config.newError(n, reportAst(rsemMissingPragmaArg, n))

  if n.kind notin nkPragmaCallKinds or n.len != 2:
    # xxx: was errGenerated for error handling
    result = c.config.newError(n, reportAst(rsemMissingPragmaArg, n))
  else:
    let it = n[1]
    if it.kind in {nkCurly, nkBracket}:
      for x in items(it):
        let e = processParam(c, x)
        if e.kind == nkError:
          return e
    else:
      result = processParam(c, it)

proc prepareSinglePragma(
    c: PContext; sym: PSym, n: PNode, i: var int, validPragmas: TSpecialWords,
    comesFromPush, isStatement: bool
  ): PNode =
  ## given a `sym`bol with pragmas `n`, check and prepare `i`'th pragma, if
  ## it's a single valid pragma, where valid is a kind within `validPragmas`.
  ##
  ## With special handling for:
  ## * comes from a push
  ## * whether it's `isStatement`
  ##
  ## what this does:
  ## * flag with nfImplicitPragma if it's an implcit pragma :D
  ## * return the pragma after prep and it's good to go
  var
    it = n[i]
    key = if it.kind in nkPragmaCallKinds and it.len > 1: it[0] else: it

  case key.kind
  of nkBracketExpr:
    result = processNote(c, it)
    return
  of nkCast:
    result =
      if comesFromPush:
        c.config.newError(n, reportAst(rsemCannotPushCast, nil))
      elif not isStatement:
        c.config.newError(n, reportAst(rsemCastRequiresStatement, nil))
      elif whichPragma(key[1]) in {wRaises, wTags}:
        pragmaRaisesOrTags(c, key[1])
      else:
        c.graph.emptyNode
    return
  of nkIdentKinds:
    # this is fine, continue processing
    result = it
  else:
    n[i] = semCustomPragma(c, it)
    result = c.graph.emptyNode
    return

  if result == nil or result.kind == nkError:
    # we already know it's not a single pragma
    return

  let (ident, error) = considerQuotedIdent2(c, key)
  if error != nil:
    result = error
    return
  var userPragma = strTableGet(c.userPragmas, ident)
  if userPragma != nil and userPragma.kind != skError:
    if {optStyleHint, optStyleError} * c.config.globalOptions != {}:
      styleCheckUse(c.config, key.info, userPragma)

    # number of pragmas increase/decrease with user pragma expansion
    inc c.instCounter
    if c.instCounter > 100:
      result = c.config.newError(
        it, reportSym(rsemPragmaRecursiveDependency, userPragma))

      return # xxx: under the legacy error scheme, this was a
             #      `msgs.globalReport`, which means `doRaise`, or throw an
             #      exception on error, so we return. The rest of the code will
             #      have to respsect this somewhat.

    let p = pragma(c, sym, userPragma.ast, validPragmas, isStatement)
    n.sons[i..i] = userPragma.ast.sons # expand user pragma with its content
    i.inc(userPragma.ast.len - 1) # inc by -1 is ok, user pragmas was empty
    dec c.instCounter

    result = if p != nil and p.kind == nkError: p else: it
  else:
    let k = whichKeyword(ident)
    if k in validPragmas:
      if {optStyleHint, optStyleError} * c.config.globalOptions != {}:
        checkPragmaUse(c.config, key.info, k, ident.s)
      case k
      of wExportc, wExportCpp:
        let extLit = getOptionalStrLit(c, it, "$1")
        if extLit.kind == nkError:
          result = it
        else:
          let ext = extLit.strVal
          case makeExternExport(c, sym, ext)
          of ExternNameSet:
            if k == wExportCpp:
              if c.config.backend != backendCpp:
                result = c.config.newError(it, reportSem rsemExportcppRequiresCpp)
                return
              else:
                incl(sym.flags, sfMangleCpp)
            result = it
          of ExternNameSetFailed:
            result = c.config.newError(
              it, SemReport(kind: rsemInvalidExtern, externName: ext))

        incl(sym.flags, sfUsed) # avoid wrong hints
      of wImportc:
        let nameLit = getOptionalStrLit(c, it, "$1")
        case nameLit.kind
        of nkError:
          result = nameLit
        else:
          let name = nameLit.strVal
          cppDefine(c.config, name)
          recordPragma(c, it, "cppdefine", name)
          result =
            case makeExternImport(c, sym, name)
            of ExternNameSet:
              it
            of ExternNameSetFailed:
              c.config.newError(
                it, SemReport(kind: rsemInvalidExtern, externName: name))
      of wImportCompilerProc:
        let nameLit = getOptionalStrLit(c, it, "$1")
        case nameLit.kind
        of nkError:
          result = nameLit
        else:
          let name = nameLit.strVal
          cppDefine(c.config, name)
          recordPragma(c, it, "cppdefine", name)
          result =
            case processImportCompilerProc(c, sym, name)
            of ExternNameSet:
              it
            of ExternNameSetFailed:
              c.config.newError(
                it, SemReport(kind: rsemInvalidExtern, externName: name))
      of wExtern:
        let (name, err) = strLitToStrOrErr(c, it)
        if err.isNil:
          result =
            case setExternName(c, sym, name)
            of ExternNameSet:
              it
            of ExternNameSetFailed:
              c.config.newError(
                it, SemReport(kind: rsemInvalidExtern, externName: name))
        else:
          result = err
      of wDirty:
        result =
          if sym.kind == skTemplate:
            incl(sym.flags, sfDirty)
            it
          else:
            invalidPragma(c, it)
      of wImportCpp:
        let nameLit = getOptionalStrLit(c, it, "$1")
        case nameLit.kind
        of nkError:
          result = nameLit
        else:
          let name = nameLit.strVal
          result =
            case processImportCpp(c, sym, name)
            of ExternNameSet:
              it
            of ExternNameSetFailed:
              c.config.newError(
                it, SemReport(kind: rsemInvalidExtern, externName: name))
      of wCppNonPod:
        incl(sym.flags, sfCppNonPod)
        result = it
      of wImportJs:
        let nameLit = getOptionalStrLit(c, it, "$1")
        case nameLit.kind
        of nkError:
          result = nameLit
        else:
          let name = nameLit.strVal
          result =
            if c.config.backend != backendJs:
              c.config.newError(it, reportSem rsemImportjsRequiresJs)
            elif sym.kind in skProcKinds and {'(', '#', '@'} notin name:
              c.config.newError(it, reportSem rsemImportjsRequiresPattern)
            else:
              sym.flags.incl {sfImportc, sfInfixCall}
              case setExternName(c, sym, name)
              of ExternNameSet:
                it
              of ExternNameSetFailed:
                c.config.newError(
                  it, SemReport(kind: rsemInvalidExtern, externName: name))
      of wImportObjC:
        let nameLit = getOptionalStrLit(c, it, "$1")
        case nameLit.kind
        of nkError:
          result = nameLit
        else:
          let name = nameLit.strVal
          result =
            case processImportObjC(c, sym, name)
            of ExternNameSet:
              it
            of ExternNameSetFailed:
              c.config.newError(
                it, SemReport(kind: rsemInvalidExtern, externName: name))
      of wSize:
        result =
          if sym.typ == nil:
            invalidPragma(c, it)
          else:
            it
        var (size, err) = intLitToIntOrErr(c, it)
        result =
          case size
          of -1:
            err
          of 1, 2, 4:
            sym.typ.size = size
            sym.typ.align = int16 size
            it
          of 8:
            sym.typ.size = 8
            sym.typ.align = floatInt64Align(c.config)
            it
          else:
            c.config.newError(it, reportSem rsemBitsizeRequires1248)
      of wAlign:
        let (alignment, err) = intLitToIntOrErr(c, it)
        result =
          case alignment
          of -1:
            err
          of 0:
            c.config.newError(it, reportSem rsemAlignRequiresPowerOfTwo)
          elif isPowerOfTwo(alignment):
            sym.alignment = max(sym.alignment, alignment)
            it
          else:
            c.config.newError(it, reportSem rsemAlignRequiresPowerOfTwo)
      of wNodecl:
        result = noVal(c, it)
        incl(sym.loc.flags, lfNoDecl)
      of wPure, wAsmNoStackFrame:
        result = noVal(c, it)
        if sym != nil:
          if k == wPure and sym.kind in routineKinds:
            result = invalidPragma(c, it)
          else:
            incl(sym.flags, sfPure)
      of wVolatile:
        result = noVal(c, it)
        incl(sym.flags, sfVolatile)
      of wCursor:
        result = noVal(c, it)
        incl(sym.flags, sfCursor)
      of wRegister:
        result = noVal(c, it)
        incl(sym.flags, sfRegister)
      of wNoalias:
        result = noVal(c, it)
        incl(sym.flags, sfNoalias)
      of wEffectsOf:
        result = processEffectsOf(c, it, sym)
      of wThreadVar:
        result = noVal(c, it)
        incl(sym.flags, {sfThread, sfGlobal})
      of wDeadCodeElimUnused: discard  # xxx: deprecated, dead code elim always on
      of wNoForward:
        result = pragmaNoForward(c, it)
      of wReorder:
        result = pragmaNoForward(c, it, flag = sfReorder)
      of wMagic:
        result = processMagic(c, it, sym)
      of wCompileTime:
        result = noVal(c, it)
        if comesFromPush:
          if sym.kind in {skProc, skFunc}:
            incl(sym.flags, sfCompileTime)
        else:
          incl(sym.flags, sfCompileTime)
      of wGlobal:
        result = noVal(c, it)
        sym.flags.incl {sfGlobal, sfPure}
      of wMerge:
        # only supported for backwards compat, doesn't do anything anymore
        result = noVal(c, it)
      of wConstructor:
        result = noVal(c, it)
        incl(sym.flags, sfConstructor)
      of wHeader:
        result = getStrLitNode(c, it) # the path or an error
        var lib = getLib(c, libHeader, result)
        addToLib(lib, sym)
        sym.flags.incl sfImportc
        sym.loc.flags.incl {lfHeader, lfNoDecl}
        # implies nodecl, because otherwise header would not make sense
        if sym.loc.r == nil: sym.loc.r = rope(sym.name.s)
      of wNoSideEffect:
        result = noVal(c, it)
        if sym != nil:
          incl(sym.flags, sfNoSideEffect)
          if sym.typ != nil: incl(sym.typ.flags, tfNoSideEffect)
      of wSideEffect:
        result = noVal(c, it)
        incl(sym.flags, sfSideEffect)
      of wNoreturn:
        result = noVal(c, it)
        # Disable the 'noreturn' annotation when in the "Quirky Exceptions" mode!
        if c.config.exc != excQuirky:
          incl(sym.flags, sfNoReturn)
        if sym.typ[0] != nil:
          # xxx: the info for this node used to be: sym.ast[paramsPos][0].info
          result = c.config.newError(it, reportSem rsemNoReturnHasReturn)
      of wNoDestroy:
        result = noVal(c, it)
        incl(sym.flags, sfGeneratedOp)
      of wNosinks:
        result = noVal(c, it)
        incl(sym.flags, sfWasForwarded)
      of wDynlib:
        result = processDynLib(c, it, sym)
      of wCompilerProc, wCore:
        result = noVal(c, it)           # compilerproc may not get a string!
        cppDefine(c.graph.config, sym.name.s)
        recordPragma(c, it, "cppdefine", sym.name.s)
        if sfFromGeneric notin sym.flags:
          let e = markCompilerProc(c, sym)
          result = if e.isNil: it else: e
      of wNonReloadable:
        result = it
        sym.flags.incl sfNonReloadable
      of wProcVar:
        result = noVal(c, it)
        incl(sym.flags, sfProcvar)
      of wExplain:
        result = it
        sym.flags.incl sfExplain
      of wDeprecated:
        if sym != nil and sym.kind in routineKinds + {skType, skVar, skLet}:
          if it.kind in nkPragmaCallKinds:
            let e = getStrLitNode(c, it)
            if e.kind == nkError:
              result = e
          incl(sym.flags, sfDeprecated)
        elif sym != nil and sym.kind != skModule:
          # We don't support the extra annotation field
          if it.kind in nkPragmaCallKinds:
            result = c.config.newError(it, reportSem rsemMisplacedDeprecation)
          incl(sym.flags, sfDeprecated)
          # At this point we're quite sure this is a statement and applies to the
          # whole module
        elif it.kind in nkPragmaCallKinds:
          result = deprecatedStmt(c, it)
        else:
          incl(c.module.flags, sfDeprecated)
      of wVarargs:
        result = noVal(c, it)
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else:
          incl(sym.typ.flags, tfVarargs)
      of wBorrow:
        if sym.kind == skType:
          result = typeBorrow(c, sym, it)
        else:
          result = noVal(c, it)
          incl(sym.flags, sfBorrow)
      of wFinal:
        result = noVal(c, it)
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else: incl(sym.typ.flags, tfFinal)
      of wInheritable:
        result = noVal(c, it)
        if sym.typ == nil or tfFinal in sym.typ.flags:
          result = invalidPragma(c, it)
        else: incl(sym.typ.flags, tfInheritable)
      of wPackage:
        result = noVal(c, it)
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else: incl(sym.flags, sfForward)
      of wAcyclic:
        result = noVal(c, it)
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else: incl(sym.typ.flags, tfAcyclic)
      of wShallow:
        result = noVal(c, it)
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else: incl(sym.typ.flags, tfShallow)
      of wThread:
        result = noVal(c, it)
        sym.flags.incl {sfThread, sfProcvar}
        if sym.typ != nil:
          incl(sym.typ.flags, tfThread)
          if sym.typ.callConv == ccClosure: sym.typ.callConv = ccNimCall
      of wGcSafe:
        result = noVal(c, it)
        if sym != nil:
          if sym.kind != skType: incl(sym.flags, sfThread)
          if sym.typ != nil:
            incl(sym.typ.flags, tfGcSafe)
          else:
            result = invalidPragma(c, it)
        else:
          discard "no checking if used as a code block"
      of wPacked:
        result = noVal(c, it)
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else:
          incl(sym.typ.flags, tfPacked)
      of wHint:
        let (s, err) = strLitToStrOrErr(c, it)
        result =
          if err.isNil:
            recordPragma(c, it, "hint", s)
            c.config.localReport(it.info, reportStr(rsemUserHint, s))
            it
          else:
            err
      of wWarning:
        let (s, err) = strLitToStrOrErr(c, it)
        result =
          if err.isNil:
            recordPragma(c, it, "warning", s)
            c.config.localReport(it.info, reportStr(rsemUserWarning, s))
            it
          else:
            err
      of wError:
        result = it
        if sym != nil and (sym.isRoutine or sym.kind == skType) and not isStatement:
          # This is subtle but correct: the error *statement* is only
          # allowed when 'wUsed' is not in validPragmas. Here this is the easiest way to
          # distinguish properly between
          # ``proc p() {.error}`` and ``proc p() = {.error: "msg".}``
          if it.kind in nkPragmaCallKinds:
            let e = getStrLitNode(c, it)
            if e.kind == nkError:
              result = e
          incl(sym.flags, sfError)
          excl(sym.flags, sfForward)
        else:
          let s = getStrLitNode(c, it)
          case s.kind:
            of nkError:
              result = s # err
            else:
              recordPragma(c, it, "error", s.strVal)
              result = c.config.newError(
                it, reportStr(rsemCustomUserError, s.strVal))
      of wFatal:
        result = c.config.newError(
          it, SemReport(kind: rsemFatalError), args = @[getStrLitNode(c, it)])
      of wDefine:
        result = processDefine(c, it)
      of wUndef:
        result = processUndef(c, it)
      of wCompile:
        result = processCompile(c, it)
      of wLink:
        result = processLink(c, it)
        result = it
      of wPassl:
        let (s, err) = strLitToStrOrErr(c, it)
        result =
          if err.isNil:
            extccomp.addLinkOption(c.config, s)
            recordPragma(c, it, "passl", s)
            it
          else:
            err
      of wPassc:
        let (s, err) = strLitToStrOrErr(c, it)
        result =
          if err.isNil:
            extccomp.addCompileOption(c.config, s)
            recordPragma(c, it, "passc", s)
            it
          else:
            err
      of wLocalPassc:
        assert sym != nil and sym.kind == skModule
        let (s, err) = strLitToStrOrErr(c, it)
        result =
          if err.isNil:
            extccomp.addLocalCompileOption(
              c.config, s, toFullPathConsiderDirty(c.config, sym.info.fileIndex))
            recordPragma(c, it, "localpassl", s)
            it
          else:
            err
      of wPush:
        result = processPush(c, n, i + 1)
        result.flags.incl nfImplicitPragma # xxx: legacy singlepragma=true
      of wPop:
        result = processPop(c, it)
        result.flags.incl nfImplicitPragma # xxx: legacy singlepragma=true
      of wPragma:
        if not sym.isNil and sym.kind == skTemplate:
          sym.flags.incl sfCustomPragma
        else:
          result = processPragma(c, n, i)
          result.flags.incl nfImplicitPragma # xxx: legacy singlepragma=true
      of wDiscardable:
        result = noVal(c, it)
        if sym != nil: incl(sym.flags, sfDiscardable)
      of wNoInit:
        result = noVal(c, it)
        if sym != nil: incl(sym.flags, sfNoInit)
      of wCodegenDecl:
        result = processCodegenDecl(c, it, sym)
      of wChecks, wObjChecks, wFieldChecks, wRangeChecks, wBoundChecks,
         wOverflowChecks, wNilChecks, wAssertions, wWarnings, wHints,
         wLineDir, wOptimization, wStaticBoundchecks, wStyleChecks,
         wCallconv, wDebugger, wProfiler,
         wFloatChecks, wNanChecks, wInfChecks, wPatterns, wTrMacros:
        var tmp = c.config.options
        result = processOption(c, it, tmp)
        c.config.options = tmp
      of wStackTrace, wLineTrace:
        if sym.kind in {skProc, skMethod, skConverter}:
          result = processOption(c, it, sym.options)
        else:
          var tmp = c.config.options
          result = processOption(c, it, tmp)
          c.config.options = tmp
      of FirstCallConv..LastCallConv:
        assert(sym != nil)
        result = it
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else:
          sym.typ.callConv = wordToCallConv(k)
          sym.typ.flags.incl tfExplicitCallConv
      of wEmit:
        result = pragmaEmit(c, it)
      of wUnroll:
        result = pragmaUnroll(c, it)
      of wLinearScanEnd, wComputedGoto:
        result = noVal(c, it)
      of wEffects:
        # is later processed in effect analysis:
        result = noVal(c, it)
      of wIncompleteStruct:
        result = noVal(c, it)
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else:
          incl(sym.typ.flags, tfIncompleteStruct)
      of wCompleteStruct:
        result = noVal(c, it)
        if sym.typ == nil:
          result = invalidPragma(c, it)
        else:
          incl(sym.typ.flags, tfCompleteStruct)
      of wUnchecked:
        result = noVal(c, it)
        if sym.typ == nil or sym.typ.kind notin {tyArray, tyUncheckedArray}:
          result = invalidPragma(c, it)
        else:
          sym.typ.kind = tyUncheckedArray
      of wUnion:
        if c.config.backend == backendJs:
          result = c.config.newError(it, reportSem rsemNoUnionForJs)
        else:
          result = noVal(c, it)
          if sym.typ == nil:
            result = invalidPragma(c, it)
          else:
            incl(sym.typ.flags, tfUnion)
      of wRequiresInit:
        result = noVal(c, it)
        if sym.kind == skField:
          sym.flags.incl sfRequiresInit
        elif sym.typ != nil:
          incl(sym.typ.flags, tfNeedsFullInit)
        else:
          result = invalidPragma(c, it)
      of wByRef:
        result = noVal(c, it)
        if sym == nil or sym.typ == nil:
          var tmp = c.config.options
          result = processOption(c, it, tmp)
          c.config.options = tmp
        else:
          incl(sym.typ.flags, tfByRef)
      of wByCopy:
        result = noVal(c, it)
        if sym.kind != skType or sym.typ == nil:
          result = invalidPragma(c, it)
        else:
          incl(sym.typ.flags, tfByCopy)
      of wPartial:
        result = noVal(c, it)
        if sym.kind != skType or sym.typ == nil:
          result = invalidPragma(c, it)
        else:
          incl(sym.typ.flags, tfPartial)
      of wInject, wGensym:
        # We check for errors, but do nothing with these pragmas otherwise
        # as they are handled directly in 'evalTemplate'.
        result = noVal(c, it)
        if sym == nil: result = invalidPragma(c, it)
      of wLine:
        result = pragmaLine(c, it)
      of wRaises, wTags:
        result = pragmaRaisesOrTags(c, it)
      of wLocks:
        if sym == nil:
          result = pragmaLockStmt(c, it)
        elif sym.typ == nil:
          result = invalidPragma(c, it)
        else:
          (sym.typ.lockLevel, result) = pragmaLocks(c, it)
          if result.isNil:
            result = it
      of wBitsize:
        if sym == nil or sym.kind != skField:
          result = invalidPragma(c, it)
        else:
          (sym.bitsize, result) = intLitToIntOrErr(c, it)
          if result.isNil: result = it
          if sym.bitsize <= 0:
            result = c.config.newError(it, reportSem rsemBitsizeRequiresPositive)
      of wGuard:
        result = it
        if sym == nil or sym.kind notin {skVar, skLet, skField}:
          result = invalidPragma(c, it)
        else:
          sym.guard = pragmaGuard(c, it, sym.kind)
          if sym.guard != nil and sym.guard.kind == skError and sym.guard.ast != nil and sym.guard.ast.kind == nkError:
            result = sym.guard.ast
          else:
            result = it
      of wGoto:
        result = it
        if sym == nil or sym.kind notin {skVar, skLet}:
          result = invalidPragma(c, it)
        else:
          sym.flags.incl sfGoto
      of wExportNims:
        result = it
        if sym == nil:
          result = invalidPragma(c, it)
        else:
          result = magicsys.registerNimScriptSymbol2(c.graph, sym)
          if result.kind != nkError:
            result = it
      of wExperimental:
        if not isTopLevel(c):
          result = c.config.newError(it, reportSem rsemExperimentalRequiresToplevel)
        result = processExperimental(c, it)
      of wThis:
        if it.kind in nkPragmaCallKinds and it.len == 2:
          (c.selfName, result) = considerQuotedIdent2(c, it[1])
          if result == nil:
            result = it
            localReport(
              c.config, n.info,
              reportStr(rsemDeprecated, "'.this' pragma is deprecated"))
          else:
            it[1] = result # we retrieved it above from `it[1]`, so making sure return the same node
            result = wrapErrorInSubTree(c.config, it)
        elif it.kind == nkIdent or it.len == 1:
          c.selfName = getIdent(c.cache, "self")
          localReport(
            c.config, n.info,
            reportStr(rsemDeprecated, "'.this' pragma is deprecated"))
        else:
          result = c.config.newError(it, reportSem rsemThisPragmaRequires01Args)
      of wNoRewrite:
        result = noVal(c, it)
      of wBase:
        result = noVal(c, it)
        sym.flags.incl sfBase
      of wIntDefine:
        result = it
        sym.magic = mIntDefine
      of wStrDefine:
        result = it
        sym.magic = mStrDefine
      of wBoolDefine:
        result = it
        sym.magic = mBoolDefine
      of wUsed:
        result = noVal(c, it)
        if sym == nil:
          result = invalidPragma(c, it)
        else:
          sym.flags.incl sfUsed
      of wLiftLocals:
        result = it
      of wEnforceNoRaises:
        sym.flags.incl sfNeverRaises
      else:
        result = invalidPragma(c, it)
    elif comesFromPush and whichKeyword(ident) != wInvalid:
      discard "ignore the .push pragma; it doesn't apply"
    else:
      if sym == nil or (sym.kind in {skVar, skLet, skParam,
                        skField, skProc, skFunc, skConverter, skMethod, skType}):
        n[i] = semCustomPragma(c, it)
        result = n[i]
      elif sym != nil:
        result = illegalCustomPragma(c, it, sym)
      else:
        result = invalidPragma(c, it)

proc overwriteLineInfo(n: PNode; info: TLineInfo) =
  n.info = info
  for i in 0..<n.safeLen:
    overwriteLineInfo(n[i], info)

proc mergePragmas(n, pragmas: PNode) =
  var pragmas = copyTree(pragmas)
  overwriteLineInfo pragmas, n.info
  if n[pragmasPos].kind == nkEmpty:
    n[pragmasPos] = pragmas
  else:
    for p in pragmas: n[pragmasPos].add p

proc pragmaRec(c: PContext, sym: PSym, n: PNode, validPragmas: TSpecialWords;
               isStatement: bool): PNode =
  result = n
  assert not cyclicTree(n)
  if n == nil: return
  var i = 0
  while i < n.len:
    let p = prepareSinglePragma(c, sym, n, i, validPragmas, false, isStatement)
    assert not cyclicTree(p)

    if p.isErrorLike:
      assert not cyclicTree(result)
      if p.isError and p[0] == result:
        # This can happen because processPush for example may
        # return the whole pragma node wrapped in an error.
        # We don't want to accidently create a cycle in that case.
        result = p
      else:
        result[i] = p
      assert not cyclicTree(result)
      result = wrapErrorInSubTree(c.config, result)
      return
    elif p != nil and nfImplicitPragma in p.flags:
      break
    inc i

proc hasPragma*(n: PNode, pragma: TSpecialWord): bool =
  ## true if any of `n`'s children are of `pragma` special words
  result = false
  if n == nil:
    return

  for p in n:
    var key = if p.kind in nkPragmaCallKinds and p.len > 1: p[0] else: p
    if key.kind == nkIdent and whichKeyword(key.ident) == pragma:
      result = true
      return

proc implicitPragmas*(c: PContext, sym: PSym, info: TLineInfo,
                      validPragmas: TSpecialWords): PSym {.discardable.} =
  result = sym
  if sym != nil and sym.kind != skModule:
    for it in c.optionStack:
      let o = it.otherPragmas
      if not o.isNil and sfFromGeneric notin sym.flags: # see issue #12985
        pushInfoContext(c.config, info)
        var i = 0
        while i < o.len:
          let p = prepareSinglePragma(c, sym, o, i, validPragmas, true, false)
          if p.kind == nkError:
            result = newSym(skError, sym.name, nextSymId(c.idgen), sym.owner, sym.info)
            result.typ = c.errorType
            result.ast = c.config.newError(
              p, reportSem rsemImplicitPragmaError, args = @[newSymNode(sym)])
            return
          c.config.internalAssert(nfImplicitPragma notin p.flags, info, "implicitPragmas")
          inc i
        popInfoContext(c.config)
        if sym.kind in routineKinds and sym.ast != nil:
          mergePragmas(sym.ast, o)

    if lfExportLib in sym.loc.flags and sfExportc notin sym.flags:
      result = newSym(skError, sym.name, nextSymId(c.idgen), sym.owner, sym.info)
      result.typ = c.errorType
      result.ast = c.config.newError(newSymNode(sym), reportSem rsemPragmaDynlibRequiresExportc)
      return
    var lib = c.optionStack[^1].dynlib
    if {lfDynamicLib, lfHeader} * sym.loc.flags == {} and
        sfImportc in sym.flags and lib != nil:
      incl(sym.loc.flags, lfDynamicLib)
      addToLib(lib, sym)
      if sym.loc.r == nil: sym.loc.r = rope(sym.name.s)

proc pragma*(c: PContext, sym: PSym, n: PNode, validPragmas: TSpecialWords;
            isStatement: bool): PNode {.discardable.} =
  addInNimDebugUtils(c.config, "pragma", sym, n, result)
  if n == nil or n.kind == nkError: return
  result = pragmaRec(c, sym, n, validPragmas, isStatement)
  if result != nil and result.kind == nkError:
    return
  # XXX: in the case of a callable def, this should use its info
  let s = implicitPragmas(c, sym, n.info, validPragmas)
  if s != nil and s.kind == skError:
    result = s.ast

proc pragmaCallable*(c: PContext, sym: PSym, n: PNode,
                     validPragmas: TSpecialWords): PNode {.discardable.} =
  if n == nil or n.kind == nkError: return
  result = n
  if n[pragmasPos].kind != nkEmpty:
    let p = pragmaRec(c, sym, n[pragmasPos], validPragmas, false)
    if p.kind == nkError:
      n[pragmasPos] = p
      result = wrapErrorInSubTree(c.config, n)
