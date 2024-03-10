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
  compiler/ast/[
    ast,
    astalgo,
    idents,
    renderer,
    wordrecg,
    trees,
    linter,
    errorhandling,
    lineinfos
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/utils/[
    pathutils,
    debugutils,
    idioms
  ],
  compiler/sem/[
    semdata,
    semfold,
    lookups
  ],
  compiler/backend/[
    extccomp
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport,
  reportAst,
  reportSem,
  reportStr,
  reportSym,
  reportTyp
from compiler/ast/reports_debug import DebugReport
from compiler/ast/report_enums import ReportKind,
  ReportKinds,
  repHintKinds,
  repWarningKinds

from compiler/ic/ic import addCompilerProc

const
  FirstCallConv* = wNimcall
  LastCallConv* = wNoconv

const
  declPragmas = {wImportc, wImportJs, wExportc, wExportNims, wExtern,
    wDeprecated, wNodecl, wError, wUsed}
    ## common pragmas for declarations, to a good approximation
  procPragmas* = declPragmas + {FirstCallConv..LastCallConv,
    wMagic, wNoSideEffect, wSideEffect, wNoreturn, wNosinks, wDynlib, wHeader,
    wCompilerProc, wCore, wProcVar, wVarargs, wCompileTime,
    wBorrow, wImportCompilerProc, wThread,
    wAsmNoStackFrame, wDiscardable, wNoInit, wCodegenDecl,
    wGensym, wInject, wRaises, wEffectsOf, wTags, wLocks, wGcSafe,
    wStackTrace, wLineTrace, wNoDestroy}
  converterPragmas* = procPragmas
  methodPragmas* = procPragmas+{wBase}-{wOverride}
  templatePragmas* = {wDeprecated, wError, wGensym, wInject, wDirty,
    wExportNims, wUsed, wPragma}
  macroPragmas* = declPragmas + {FirstCallConv..LastCallConv,
    wMagic, wNoSideEffect, wCompilerProc, wCore,
    wDiscardable, wGensym, wInject}
  iteratorPragmas* = declPragmas + {FirstCallConv..LastCallConv, wNoSideEffect, wSideEffect,
    wMagic, wBorrow,
    wDiscardable, wGensym, wInject, wRaises, wEffectsOf,
    wStackTrace, wLineTrace,
    wTags, wLocks, wGcSafe}
  exprPragmas* = {wLine, wLocks, wNoRewrite, wGcSafe, wNoSideEffect}
  stmtPragmas* = {wChecks, wObjChecks, wFieldChecks, wRangeChecks,
    wBoundChecks, wOverflowChecks, wNilChecks, wStaticBoundchecks,
    wStyleChecks, wAssertions,
    wWarnings, wHints,
    wLineDir, wStackTrace, wLineTrace, wOptimization, wHint, wWarning, wError,
    wFatal, wDefine, wUndef, wCompile, wLink, wPush, wPop,
    wPassl, wPassc, wLocalPassc,
    wDebugger, wProfiler,
    wDeprecated,
    wFloatChecks, wInfChecks, wNanChecks, wPragma, wEmit,
    wTrMacros, wEffects, wComputedGoto,
    wExperimental, wUsed, wByRef, wCallconv}
  lambdaPragmas* = {FirstCallConv..LastCallConv,
    wNoSideEffect, wSideEffect, wNoreturn, wNosinks, wDynlib, wHeader,
    wThread, wAsmNoStackFrame,
    wRaises, wLocks, wTags, wEffectsOf,
    wGcSafe, wCodegenDecl, wNoInit, wCompileTime}
  typePragmas* = declPragmas + {wMagic, wAcyclic,
    wPure, wHeader, wCompilerProc, wCore, wFinal, wSize,
    wIncompleteStruct, wCompleteStruct, wByCopy, wByRef,
    wInheritable, wGensym, wInject, wRequiresInit, wUnion, wPacked,
    wBorrow, wGcSafe, wExplain, wPackage}
  fieldPragmas* = declPragmas + {wGuard, wBitsize, wCursor,
    wRequiresInit, wNoalias, wAlign} - {wExportNims, wNodecl} # why exclude these?
  varPragmas* = declPragmas + {wVolatile, wRegister, wThreadVar,
    wMagic, wHeader, wCompilerProc, wCore, wDynlib,
    wNoInit, wCompileTime, wGlobal,
    wGensym, wInject,
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

  allowsCustomPragma = {skVar, skLet, skParam, skField, skProc, skFunc,
                        skConverter, skMethod, skIterator, skType}
    ## the set of symbol kinds that allow attached custom pragmas

proc getPragmaVal*(procAst: PNode; name: TSpecialWord): PNode =
  let p = procAst[pragmasPos]
  if p.kind == nkEmpty: return nil
  for it in p:
    if it.kind in nkPragmaCallKinds and it.len == 2 and it[0].kind == nkIdent and
        it[0].ident.id == ord(name):
      return it[1]

proc recordPragma(c: PContext; n: PNode; args: varargs[string]) =
  var recorded = newNodeI(nkReplayAction, n.info)
  for i in 0..args.high:
    recorded.add newStrNode(args[i], n.info)
  addPragmaComputation(c, recorded)

proc invalidPragma*(c: PContext; n: PNode): PNode =
  ## create an error node (`nkError`) for an invalid pragma error
  c.config.newError(n, PAstDiag(kind: adSemInvalidPragma))

proc illegalCustomPragma*(c: PContext; n: PNode, s: PSym): PNode =
  ## create an error node (`nkError`) for an illegal custom pragma error
  c.config.newError(
    n,
    PAstDiag(kind: adSemIllegalCustomPragma, customPragma: s))

func isLocal(s: PSym): bool =
  ## Returns whether `s` is a parameter or local `var`/`let`.
  # XXX: ideally, we could rely on ``sfGlobal``, but for declarations
  #      outside of routines, the flag is currently only included after pragma
  #      processing
  s.kind in skLocalVars and sfGlobal notin s.flags and s.owner.kind != skModule

proc disallowedExternalLocal(c: PContext, n: PNode): PNode =
  c.config.newError(n, PAstDiag(kind: adSemExternalLocalNotAllowed))

proc pragmaAsm*(c: PContext, n: PNode): tuple[marker: char, err: PNode] =
  ## Gets the marker out of an asm stmts pragma list
  ## Returns ` as the default marker if no other markers are found
  result.marker = '`'
  if n != nil:
    for it in n:
      if it.kind in nkPragmaCallKinds and it.len == 2 and it[0].kind == nkIdent:
        case whichKeyword(it[0].ident)
        of wSubsChar:
          if it[1].kind == nkCharLit: result.marker = chr(it[1].intVal)
          else: result.err = invalidPragma(c, it)
        else: result.err = invalidPragma(c, it)
      else: result.err = invalidPragma(c, it)

proc semExternName(c: ConfigRef, n: PNode): PNode =
  ## Verifies that the given string-literal node `n` may be used as an external
  ## name. Returns either `n` or an error.
  if n.isError:
    return n

  assert n.kind in nkStrLiterals

  # the only valid interpolation sequence in external names is "$1". Make sure
  # that the input string adheres to the rule:
  var i = 0
  while i < n.strVal.len:
    let c = n.strVal[i]
    case c
    of '$':
      if i + 1 >= n.strVal.len or n.strVal[i + 1] != '1':
        break # error
    else:
      discard "ok"

    inc i

  if i == n.strVal.len and i != 0:
    n # no error
  else:
    c.newError(n, PAstDiag(kind: adSemInvalidExtern, externName: n.strVal))

proc setExternName(c: PContext; s: PSym, ext: string) =
  ## sets an `ext`ern name, on `s`ymbol and returns a success or failure status

  # special cases to improve performance:
  if ext == "$1":
    s.extname = s.name.s
  elif '$' notin ext:
    s.extname = ext
  else:
    s.extname = ext % s.name.s
  if c.config.cmd == cmdNimfix and '$' notin ext:
    # note that '{.importc.}' is transformed into '{.importc: "$1".}'
    s.extFlags.incl(exfFullExternalName)

proc makeExternImport(c: PContext; s: PSym, ext: string) =
  ## produces (mutates) `s`'s `loc`ation setting the import name, marks it as
  ## an import and notes it as not forward declared, then returns a
  ## success/failure
  setExternName(c, s, ext)
  incl(s.flags, sfImportc)
  excl(s.flags, sfForward)

proc makeExternExport(c: PContext; s: PSym, ext: string) =
  ## produces (mutates) `s`'s `loc`ation setting the export name, marks it as
  ## an export c, and returns a success/failure
  setExternName(c, s, ext)
  incl(s.flags, sfExportc)

proc processImportCompilerProc(c: PContext; s: PSym, ext: string) =
  ## produces (mutates) `s`'s `loc`ation setting the imported compiler proc
  ## name `ext`. marks it as import c and no forward declaration, sets the
  ## location as a compiler proc import, and returns a success/failure
  setExternName(c, s, ext)
  incl(s.flags, sfImportc)
  excl(s.flags, sfForward)
  incl(s.extFlags, exfImportCompilerProc)

proc getStrLitNode(c: PContext, n: PNode): PNode =
  ## returns a PNode that's either an error or a string literal node
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected))
  else:
    n[1] = c.semConstExpr(c, n[1])
    case n[1].kind
    of nkStrLit, nkRStrLit, nkTripleStrLit:
      n[1]
    else:
      # xxx: this is a potential bug, but requires a lot more tests in place
      #      for pragmas prior to changing, but we're meant to return n[1], yet
      #      on error we return a wrapped `n`, that's the wrong level of AST.
      c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected))


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
    ("", c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected)))

proc intLitToIntOrErr(c: PContext, n: PNode): (int, PNode) =
  ## extracts the int from an int literal, or errors if it's not an int
  ## literal or doesn't evaluate to one
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    (-1, c.config.newError(n, PAstDiag(kind: adSemIntLiteralExpected)))
  else:
    n[1] = c.semConstExpr(c, n[1])
    case n[1].kind
    of nkSIntLiterals:
      (int(n[1].intVal), nil)
    else:
      (-1, c.config.newError(n, PAstDiag(kind: adSemIntLiteralExpected)))

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
    result = c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected))
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
      (false, c.config.newError(n, PAstDiag(kind: adSemOnOrOffExpected)))
  else:
    (false, c.config.newError(n, PAstDiag(kind: adSemOnOrOffExpected)))

proc onOff(c: PContext, n: PNode, op: TOptions, resOptions: var TOptions): PNode =
  ## produces an error, or toggles the setting in `resOptions` param
  let (r, err) = isTurnedOn(c, n)
  result = if err.isNil: n
           else:         err
  if r: resOptions.incl op
  else: resOptions.excl op

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
      result = c.config.newError(n, PAstDiag(kind: adSemCallconvExpected))
  else:
    result = c.config.newError(n, PAstDiag(kind: adSemCallconvExpected))

proc getLib(c: PContext, kind: TLibKind, path: PNode): LibId =
  for id, it in c.libs:
    if it.kind == kind and trees.exprStructuralEquivalent(it.path, path):
      # comment equality SHOULDN'T matter here (demo: TODO)
      return id

  var lib = initLib(kind)
  lib.path = path
  if path.kind in nkStrLiterals:
    lib.isOverriden = options.isDynlibOverride(c.config, path.strVal)

  result = c.addLib(lib)

proc expectDynlibNode(c: PContext, n: PNode): PNode =
  ## `n` must be a callable, this will produce the ast for the callable or
  ## produce a `StringLiteralExpected` error node.
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    result = c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected))
  else:
    # For the OpenGL wrapper we support:
    # {.dynlib: myGetProcAddr(...).}
    result = c.semExpr(c, n[1])
    # this is AST that later gets passed to code generation, so we have to
    # perform constant folding
    result = foldInAst(c.module, result, c.idgen, c.graph)
    # XXX: sempass2 is missing here...
    if result.typ == nil or result.typ.kind notin {tyPointer, tyString, tyProc}:
      result = c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected))

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
      if not c[lib].isOverriden:
        c.optionStack[^1].dynlib = lib
  else:
    if n.kind in nkPragmaCallKinds:
      let libNode = expectDynlibNode(c, n)
      case libNode.kind
      of nkError:
        result = libNode
      else:
        var lib = getLib(c, libDynamic, libNode)
        if not c[lib].isOverriden:
          addToLib(lib, sym)
          incl(sym.extFlags, exfDynamicLib)
    else:
      incl(sym.extFlags, exfExportLib)
    # since we'll be loading the dynlib symbols dynamically, we must use
    # a calling convention that doesn't introduce custom name mangling
    # cdecl is the default - the user can override this explicitly
    if sym.kind in routineKinds and sym.typ != nil and
       tfExplicitCallConv notin sym.typ.flags:
      sym.typ.callConv = ccCDecl

proc processNote(c: PContext, n: PNode): PNode =
  ## Analyzes the pragma expression `n`, verifying that its of the form
  ## ``note[name]: val``.
  ##
  ## If `note` is the name of a valid note set, `name` is the name of a valid
  ## enum part of that set, and `val` evaluates to a bool value, includes or
  ## excludes `val` from the `note` set and returns the typed AST. An error is
  ## returned otherwise.

  proc handleNote(warning: bool, notes: ConfNoteSet, n: PNode): PNode =
    let
      enumVals =
        if warning: repWarningKinds
        else:       repHintKinds
      # search for an enum value that has the provided name:
      nk = findStr(enumVals, n[0][1].ident.s, repNone)

    if nk == repNone:
      # not part of the allowed set
      localReport(c.config, n[0][1].info):
        reportStr(if warning: rsemUnknownWarning else: rsemUnknownHint,
                  n[0][1].ident.s)

    # always evaluate the boolean expression:
    let enable = c.semConstBoolExpr(c, n[1])

    if nk != repNone and enable.kind == nkIntLit:
      if enable.intVal != 0:
        incl(c.config, notes, nk)
      else:
        excl(c.config, notes, nk)

    # setup the production:
    result = shallowCopy(n)
    result[0] = n[0]
    result[1] = enable

    if enable.kind == nkError:
      result = c.config.wrapError(result)

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
      of wHint:           handleNote(false, cnCurrent,     n)
      of wWarning:        handleNote(true,  cnCurrent,     n)
      of wWarningAsError: handleNote(true,  cnWarnAsError, n)
      of wHintAsError:    handleNote(false, cnHintAsError, n)
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
  of wTrMacros: {optTrMacros}
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
      except ValueError:
        n[1] = c.config.newError(
          n[1], PAstDiag(kind: adSemUnknownExperimental))

        result = wrapError(c.config, n)
    of nkError:
      result = wrapError(c.config, n)
    else:
      result = c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected))

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
            result = (false, c.config.newError(n, PAstDiag(
              kind: adSemWrongIdent,
              allowedIdents: @["none", "speed", "size"])))
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
      c.config.newError(n, PAstDiag(kind: adSemPragmaOptionExpected))

proc processPush(c: PContext, info: TLineInfo, pragmas: openArray[PNode], r: PNode): bool =
  ## Processes each pragma in `n`, detecting for each whether it is a statement
  ## modifying the environment (i.e. an option) or a pragma that applies to a
  ## declaration -- both are recorded to a new entry in the option stack.
  ## Returns 'false' if an error was encountered, and 'true' if not.
  ##
  ## Each processed pragma is added to `r`, or an error node, if pre-processing
  ## for the pragma failed.
  result = true # start with success

  var x = pushOptionEntry(c)
  for it in pragmas.items:
    var tmp = c.config.options
    var (opt, err) = tryProcessOption(c, it, tmp)
    c.config.options = tmp

    if err.isNil and it.kind == nkCast:
      # disallow ``cast(x)`` pragmas being pushed to the option stack
      err = c.config.newError(it, PAstDiag(kind: adSemCannotPushCast))

    if err.isNil:
      r.add it
      if not opt:
        # simply store it somewhere:
        if x.otherPragmas.isNil:
          x.otherPragmas = newNodeI(nkPragma, info)
        x.otherPragmas.add it

    else:
      r.add err
      result = false

  # If stacktrace is disabled globally we should not enable it
  if optStackTrace notin c.optionStack[0].options:
    # TODO: generalize this logic (i.e. in the form of global option overrides)
    c.config.excl(optStackTrace)

  c.config.localReport(info, DebugReport(
    kind: rdbgOptionsPush, optionsNow: c.config.options))

proc processPop(c: PContext, n: PNode): PNode =
  # process a pop pragma, produces (mutates) `n` or an error wrapping `n`
  result = n
  if c.optionStack.len <= 1:
    result = c.config.newError(n, PAstDiag(kind: adSemMismatchedPopPush))
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
    if defined(nimDebugUtils) and
       cmpIgnoreStyle(str, "nimCompilerDebug") == 0:
      c.config.outputTrace(CompilerTrace(kind: compilerTraceDefined,
                                         srcLoc: n.info))

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
    if defined(nimDebugUtils) and
       cmpIgnoreStyle(str, "nimCompilerDebug") == 0:
      c.config.outputTrace(CompilerTrace(kind: compilerTraceUndefined,
                                         srcLoc: n.info))

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
        n, PAstDiag(kind: adSemStringLiteralExpected)))

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
          result = c.config.newError(n, PAstDiag(
            kind: adSemExcessiveCompilePragmaArgs))
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
      result = con.config.newError(n, PAstDiag(kind: adSemEmptyAsm))
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
    result = con.config.newError(n, PAstDiag(
      kind: adSemAsmEmitExpectsStringLiteral,
      unexpectedKind: n[1].kind))

proc pragmaEmit(c: PContext, n: PNode): PNode =
  result = n
  if n.kind notin nkPragmaCallKinds or n.len != 2:
    result = c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected))
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
        result = c.config.newError(n, PAstDiag(kind: adSemStringLiteralExpected))

proc noVal(c: PContext; n: PNode): PNode =
  ## ensure that this pragma does not produce a value
  if n.kind in nkPragmaCallKinds and n.len > 1:
    invalidPragma(c, n)
  else:
    n

proc pragmaLine(c: PContext, n: PNode): PNode =
  result = n
  if n.kind in nkPragmaCallKinds and n.len == 2:
    n[1] = c.semConstExpr(c, n[1])
    let a = n[1]
    if a.kind == nkTupleConstr and a.len == 3:
      # unpack the tuple
      let
        path = a[0].skipColon
        line = a[1].skipColon
        col  = a[2].skipColon

      if path.kind == nkStrLit and line.kind == nkIntLit and
         col.kind == nkIntLit:
        n.info = newLineInfo(fileInfoIdx(c.config, AbsoluteFile(path.strVal)),
                             line.intVal.int,
                             col.intVal.int)
      else:
        n[1] = c.config.newError(
          a, PAstDiag(kind: adSemLinePragmaExpectsTuple))
    elif a.kind != nkError:
      n[1] = c.config.newError(
        a, PAstDiag(kind: adSemLinePragmaExpectsTuple))

    if n[1].kind == nkError:
      result = c.config.wrapError(n)
  else:
    # sensible default:
    n.info = getInfoContext(c.config, -1)

proc processPragma(c: PContext, info: TLineInfo, pragmas: openArray[PNode]): PNode =
  ## Processes the user-pragma definition described by the `pragmas` list. On
  ## success, creates and registers a new user-pragma with `pragmas[1..^1]` as the pragmas it
  ## expands to on use, and returns the first item from `pragmas` (that is, the
  ## name part). On failure, an error is returned.
  let it = pragmas[0]
  result = it
  if it.kind notin nkPragmaCallKinds and it.safeLen == 2 or
     it.safeLen != 2 or it[0].kind != nkIdent or it[1].kind != nkIdent:
    return invalidPragma(c, it)

  var userPragma = newSym(skTemplate, it[1].ident, nextSymId(c.idgen), nil,
                          it.info, c.config.options)
  userPragma.ast = newTreeI(nkPragma, info, pragmas[1..^1])
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
        result = c.config.newError(x, PAstDiag(
          kind: adSemRaisesPragmaExpectsObject, wrongType: t))

        return
      x.typ = t

  if n.kind in nkPragmaCallKinds and n.len == 2:
    let it = n[1]
    if it.kind notin {nkCurly, nkBracket}:
      let r = processExc(c, it)
      if r.kind == nkError:
        n[1] = r
        result = wrapError(c.config, n)
    else:
      for i, e in it.pairs:
        let r = processExc(c, e)
        if r.kind == nkError:
          n[i] = r
          result = wrapError(c.config, n)
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
      it[1] = c.config.newError(n, PAstDiag(kind: adSemLocksPragmaExpectsList))
      result = wrapError(c.config, it)
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
        it[1] = c.config.newError(it[1], PAstDiag(
          kind: adSemLocksPragmaBadLevelString))
        result = (UnknownLockLevel, wrapError(c.config, it))
    else:
      let (x, err) = intLitToIntOrErr(c, it)
      if err != nil:
        result = (UnknownLockLevel, err)
      elif x < 0 or x > MaxLockLevel:
        it[1] = c.config.newError(it[1], PAstDiag(
          kind: adSemLocksPragmaBadLevelRange))
        result = (UnknownLockLevel, wrapError(c.config, it))
      else:
        result = (TLockLevel(x), nil)

proc typeBorrow(c: PContext; sym: PSym, n: PNode): PNode =
  result = n
  if n.kind in nkPragmaCallKinds and n.len == 2:
    let it = n[1]
    if it.kind != nkAccQuoted:
      result = c.config.newError(n, PAstDiag(kind: adSemBorrowPragmaNonDot))
      return
  incl(sym.typ.flags, tfBorrowDot)

proc markCompilerProc(c: PContext; s: PSym) =
  makeExternExport(c, s, "$1")

  s.flags.incl {sfCompilerProc, sfUsed}
  registerCompilerProc(c.graph, s)
  if c.config.symbolFiles != disabledSf:
    addCompilerProc(c.encoder, c.packedRepr, s)

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
      let (ident, err) = considerQuotedIdent(c, n)
      internalAssert(c.config, err.isNil,
        "the qualifiedLookup above should have caught any issues")
      result = newSym(skUnknown, ident, nextSymId(c.idgen), nil, n.info,
        c.config.options)
  else:
    result = qualifiedLookUp(c, n, {checkUndeclared})

proc semCustomPragma(c: PContext, n: PNode): PNode =
  var callNode: PNode

  case n.kind
  of nkIdent, nkSym:
    # pragma -> pragma()
    callNode = newTree(nkCall, n)
  of nkExprColonExpr:
    # pragma: arg -> pragma(arg)
    callNode = newTree(nkCall, n[0], n[1])
  of nkPragmaCallKinds - {nkExprColonExpr}:
    callNode = n
  else:
    result = invalidPragma(c, n)
    return

  let r = c.semOverloadedCall(c, callNode, callNode, {skTemplate}, {efNoUndeclared})
  if r.isError:
    return r
  elif r.isNil or sfCustomPragma notin r[0].sym.flags:
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
          nil
        else:
          # xxx: was errGenerated for error handling
          c.config.newError(n, PAstDiag(kind: adSemMisplacedEffectsOf))
      else:
        # xxx: was errGenerated for error handling
        c.config.newError(n, PAstDiag(kind: adSemMissingPragmaArg))

  if n.kind notin nkPragmaCallKinds or n.len != 2:
    # xxx: was errGenerated for error handling
    result = c.config.newError(n, PAstDiag(kind: adSemMissingPragmaArg))
  else:
    let it = n[1]
    if it.kind in {nkCurly, nkBracket}:
      for x in items(it):
        result = processParam(c, x)
        if result.isError:
          break

    else:
      result = processParam(c, it)

func key(n: PNode): PNode =
  if n.kind in nkPragmaCallKinds and n.len > 1:
    n[0]
  else:
    n

proc applySymbolPragma(c: PContext, sym: PSym, it: PNode): PNode =
      ## Applies the pragma expression `it` to the symbol `sym`. Application of
      ## the pragma consists of two steps:
      ## 1.) make sure they're semantically valid (which sometimes depends on
      ##     the symbol kind)
      ## 2.) apply them
      ##
      ## XXX: this also means that there are two kinds of errors
      let k = whichPragma(it)
      case k
      of wExportc:
        if isLocal(sym):
          return disallowedExternalLocal(c, it)

        let extLit = semExternName(c.config, getOptionalStrLit(c, it, "$1"))

        result =
          if extLit.kind == nkError:
            extLit
          else:
            makeExternExport(c, sym, extLit.strVal)
            it

        incl(sym.flags, sfUsed) # avoid wrong hints
      of wImportc:
        if isLocal(sym):
          return disallowedExternalLocal(c, it)

        let nameLit = semExternName(c.config, getOptionalStrLit(c, it, "$1"))
        case nameLit.kind
        of nkError:
          result = nameLit
        else:
          let name = nameLit.strVal
          # XXX: this is not a good idea, we're modifying global state. Adding
          #      a C pre-processor define should be the responsibility of the C
          #      code generator
          cppDefine(c.config, name)
          recordPragma(c, it, "cppdefine", name)
          makeExternImport(c, sym, name)
          result = it
      of wImportCompilerProc:
        let nameLit = semExternName(c.config, getOptionalStrLit(c, it, "$1"))
        case nameLit.kind
        of nkError:
          result = nameLit
        else:
          let name = nameLit.strVal
          cppDefine(c.config, name)
          recordPragma(c, it, "cppdefine", name)
          processImportCompilerProc(c, sym, name)
          result = it
      of wExtern:
        if isLocal(sym):
          return disallowedExternalLocal(c, it)

        let name = semExternName(c.config, getStrLitNode(c, it))
        result =
          if name.isError:
            name
          else:
            setExternName(c, sym, name.strVal)
            it
      of wDirty:
        result = noVal(c, it)
        assert sym.kind == skTemplate
        incl(sym.flags, sfDirty)
      of wImportJs:
        if isLocal(sym):
          return disallowedExternalLocal(c, it)

        # note: don't use ``semExternName`` here. The operand is *not* an
        # external name, but rather a *pattern* string
        let nameLit = getOptionalStrLit(c, it, "$1")
        case nameLit.kind
        of nkError:
          result = nameLit
        else:
          result =
            if c.config.backend != backendJs:
              c.config.newError(it, PAstDiag(kind: adSemImportjsRequiresJs))
            else:
              sym.flags.incl {sfImportc, sfInfixCall}
              setExternName(c, sym, nameLit.strVal)
              it
      of wSize:
        assert sym.typ != nil
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
            c.config.newError(it, PAstDiag(kind: adSemBitsizeRequires1248))
      of wAlign:
        let (alignment, err) = intLitToIntOrErr(c, it)
        result =
          case alignment
          of -1:
            err
          of 0:
            c.config.newError(it, PAstDiag(kind: adSemAlignRequiresPowerOfTwo))
          elif isPowerOfTwo(alignment):
            sym.alignment = max(sym.alignment, alignment)
            it
          else:
            c.config.newError(it, PAstDiag(kind: adSemAlignRequiresPowerOfTwo))
      of wNodecl:
        if isLocal(sym):
          result = disallowedExternalLocal(c, it)
        else:
          result = noVal(c, it)
          incl(sym.extFlags, exfNoDecl)
      of wPure, wAsmNoStackFrame:
        result = noVal(c, it)
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
        if result.isNil: # was there no error?
          result = it
      of wThreadVar:
        result = noVal(c, it)
        incl(sym.flags, {sfThread, sfGlobal})
      of wMagic:
        result = processMagic(c, it, sym)
      of wCompileTime:
        result = noVal(c, it)
        incl(sym.flags, sfCompileTime)
      of wGlobal:
        result = noVal(c, it)
        sym.flags.incl {sfGlobal, sfPure}
      of wHeader:
        if isLocal(sym):
          return disallowedExternalLocal(c, it)

        let path = getStrLitNode(c, it) # the path or an error
        if path.isError:
          return path
        result = it
        let lib = getLib(c, libHeader, path)
        addToLib(lib, sym)
        sym.flags.incl sfImportc
        sym.extFlags.incl {exfHeader, exfNoDecl}
        # implies nodecl, because otherwise header would not make sense
        if sym.extname == "": sym.extname = sym.name.s
      of wNoSideEffect:
        result = noVal(c, it)
        incl(sym.flags, sfNoSideEffect)
        incl(sym.typ.flags, tfNoSideEffect)
      of wSideEffect:
        result = noVal(c, it)
        incl(sym.flags, sfSideEffect)
      of wNoreturn:
        result = noVal(c, it)
        incl(sym.flags, sfNoReturn)
        if sym.typ[0] != nil:
          # xxx: the info for this node used to be: sym.ast[paramsPos][0].info
          result = c.config.newError(it, PAstDiag(kind: adSemNoReturnHasReturn))
      of wNoDestroy:
        result = noVal(c, it)
        incl(sym.flags, sfGeneratedOp)
      of wNosinks:
        result = noVal(c, it)
        incl(sym.flags, sfWasForwarded)
      of wDynlib:
        if isLocal(sym):
          result = disallowedExternalLocal(c, it)
        else:
          result = processDynLib(c, it, sym)
      of wCompilerProc, wCore:
        result = noVal(c, it)           # compilerproc may not get a string!
        cppDefine(c.graph.config, sym.name.s)
        recordPragma(c, it, "cppdefine", sym.name.s)
        if sfFromGeneric notin sym.flags:
          markCompilerProc(c, sym)
      of wProcVar:
        result = noVal(c, it)
        incl(sym.flags, sfProcvar)
      of wExplain:
        result = it
        sym.flags.incl sfExplain
      of wDeprecated:
        result = it
        case sym.kind
        of routineKinds + {skType, skVar, skLet}:
          if it.kind in nkPragmaCallKinds:
            let e = getStrLitNode(c, it)
            if e.kind == nkError:
              result = e
          incl(sym.flags, sfDeprecated)
        else:
          # We don't support the extra annotation field
          if it.kind in nkPragmaCallKinds:
            result = c.config.newError(it, PAstDiag(kind: adSemMisplacedDeprecation))
          incl(sym.flags, sfDeprecated)
      of wVarargs:
        result = noVal(c, it)
        incl(sym.typ.flags, tfVarargs)
      of wBorrow:
        if sym.kind == skType:
          result = typeBorrow(c, sym, it)
        else:
          result = noVal(c, it)
          incl(sym.flags, sfBorrow)
      of wFinal:
        result = noVal(c, it)
        assert sym.typ != nil
        if tfInheritable in sym.typ.flags:
          # TODO: use a dedicated error diagnostic
          result = invalidPragma(c, it)
        else:
          incl(sym.typ.flags, tfFinal)
      of wInheritable:
        result = noVal(c, it)
        if tfFinal in sym.typ.flags:
          # TODO: this is an application error, not an "invalid pragma" one.
          #       Use a dedicated error diagnostic
          result = invalidPragma(c, it)
        else: incl(sym.typ.flags, tfInheritable)
      of wPackage:
        result = noVal(c, it)
        assert sym.typ != nil
        incl(sym.flags, sfForward)
      of wAcyclic:
        result = noVal(c, it)
        assert sym.typ != nil
        incl(sym.typ.flags, tfAcyclic)
      of wThread:
        result = noVal(c, it)
        sym.flags.incl {sfThread, sfProcvar}
        if sym.typ != nil:
          incl(sym.typ.flags, tfThread)
          if sym.typ.callConv == ccClosure: sym.typ.callConv = ccNimCall
      of wGcSafe:
        result = noVal(c, it)
        if sym.kind != skType:
          incl(sym.flags, sfThread)
        incl(sym.typ.flags, tfGcSafe)
      of wPacked:
        result = noVal(c, it)
        assert sym.typ != nil
        incl(sym.typ.flags, tfPacked)
      of wError:
        case sym.kind
        of skProcKinds, skType:
          # mark the symbol with error flag
          let s = getOptionalStrLit(c, it, "")
          case s.kind
          of nkError:
            result = s
          else:
            incl(sym.flags, sfError)
            excl(sym.flags, sfForward)
            result = it
        else:
          # report a user error
          let s = getStrLitNode(c, it)
          case s.kind
          of nkError:
            result = s
          else:
            recordPragma(c, it, "error", s.strVal)
            result = c.config.newError(
              it, PAstDiag(kind: adSemCustomUserError, errmsg: s.strVal))
      of wPragma:
        result = noVal(c, it)
        sym.flags.incl sfCustomPragma
      of wDiscardable:
        result = noVal(c, it)
        incl(sym.flags, sfDiscardable)
      of wNoInit:
        result = noVal(c, it)
        incl(sym.flags, sfNoInit)
      of wCodegenDecl:
        result = processCodegenDecl(c, it, sym)
      of wStackTrace, wLineTrace:
        result = processOption(c, it, sym.options)
      of FirstCallConv..LastCallConv:
        assert(sym != nil)
        assert sym.typ != nil
        result = it
        sym.typ.callConv = wordToCallConv(k)
        sym.typ.flags.incl tfExplicitCallConv
      of wIncompleteStruct:
        result = noVal(c, it)
        assert sym.typ != nil
        incl(sym.typ.flags, tfIncompleteStruct)
      of wCompleteStruct:
        result = noVal(c, it)
        assert sym.typ != nil
        incl(sym.typ.flags, tfCompleteStruct)
      of wUnion:
        if c.config.backend == backendJs:
          result = c.config.newError(it, PAstDiag(kind: adSemNoUnionForJs))
        else:
          result = noVal(c, it)
          incl(sym.typ.flags, tfUnion)
      of wRequiresInit:
        result = noVal(c, it)
        if sym.kind == skField:
          sym.flags.incl sfRequiresInit
        else:
          assert sym.typ != nil
          incl(sym.typ.flags, tfNeedsFullInit)
      of wByRef:
        result = noVal(c, it)
        assert sym != nil and sym.typ != nil
        incl(sym.typ.flags, tfByRef)
      of wByCopy:
        result = noVal(c, it)
        assert sym.kind == skType and sym.typ != nil
        incl(sym.typ.flags, tfByCopy)
      of wInject, wGensym:
        # We check for errors, but do nothing with these pragmas otherwise
        # as they are handled directly in 'evalTemplate'.
        result = noVal(c, it)
      of wRaises, wTags:
        result = pragmaRaisesOrTags(c, it)
      of wLocks:
          (sym.typ.lockLevel, result) = pragmaLocks(c, it)
          if result.isNil:
            result = it
      of wBitsize:
          assert sym.kind == skField
          (sym.bitsize, result) = intLitToIntOrErr(c, it)
          if result.isNil: result = it
          if sym.bitsize <= 0:
            result = c.config.newError(it,
                      PAstDiag(kind: adSemBitsizeRequiresPositive))
      of wGuard:
          assert sym.kind in {skVar, skLet, skField}
          sym.guard = pragmaGuard(c, it, sym.kind)
          if sym.guard != nil and sym.guard.kind == skError and sym.guard.ast != nil and sym.guard.ast.kind == nkError:
            result = sym.guard.ast
          else:
            result = it
      of wGoto:
        result = noVal(c, it)
        assert sym.kind in {skVar, skLet}
        sym.flags.incl sfGoto
      of wExportNims:
          # XXX: modifying the module graph during application of a symbol
          #      operator doesn't seem like a good idea...
          result = magicsys.registerNimScriptSymbol2(c.graph, sym)
          if result.kind != nkError:
            result = it
      of wBase:
        result = noVal(c, it)
        sym.flags.incl sfBase
      # TODO: support {.xxxdefine: "customName".}
      # Right now it just ignores the argument
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
        sym.flags.incl sfUsed
      of wCast:
        result = c.config.newError(it, PAstDiag(kind: adSemCastRequiresStatement))
      of wInvalid:
        # a non-builtin pragma reaching here must be a custom pragma
        result = it
      else:
        unreachable(k)

proc processBlockPragma(c: PContext, it: PNode, p: TSpecialWord): PNode =
  ## Pre-processes the pragma expression `it`, returning either the result or
  ## an error. `p` is the name of the built-in pragma.
  ##
  ## Application of the pragmas happens later.
  case p
  of wLine:
    pragmaLine(c, it)
  of wLocks:
    pragmaLockStmt(c, it)
  of wNoRewrite, wGcSafe, wNoSideEffect:
    noVal(c, it)
  of wCast:
    if whichPragma(it[1]) in {wTags, wRaises}:
      let e = pragmaRaisesOrTags(c, it[1])
      if e.kind == nkError:
        e
      else:
        it
    else:
      it
  of wInvalid:
    # must be a custom pragma -- they're allowed
    it
  of {low(TSpecialWord)..high(TSpecialWord)} - exprPragmas - {wCast, wInvalid}:
    invalidPragma(c, it)

proc applyStmtPragma(c: PContext, owner: PSym, it: PNode, k: TSpecialWord): PNode =
  ## Evaluates the single built-in pragma statement `it` for its side-effects.
  ## Depending on the pragma, this might mutate `owner`. If the statement
  ## is ill-formed (e.g. missing arguments) or the evaluation failed, an error
  ## is returned.
  case k
  of wUsed:
    result = noVal(c, it)
    # XXX: an escaping mutation if the the pragma is used inside a ``compiles``
    #      block
    owner.flags.incl sfUsed
  of wDeprecated:
    result = it
    # TODO: disallow when the owner is not a module
    if it.kind in nkPragmaCallKinds:
      let msg = getStrLitNode(c, it)
      if msg.kind == nkError:
        result = msg
      else:
        c.module.constraint = msg
        incl(c.module.flags, sfDeprecated)
    else:
      incl(c.module.flags, sfDeprecated)
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
      let s = getStrLitNode(c, it)
      case s.kind:
        of nkError:
          result = s # err
        else:
          recordPragma(c, it, "error", s.strVal)
          result = c.config.newError(
            it, PAstDiag(kind: adSemCustomUserError, errmsg: s.strVal))
  of wFatal:
    result = c.config.newError(it, PAstDiag(kind: adSemFatalError))
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
    assert owner.kind == skModule
    let (s, err) = strLitToStrOrErr(c, it)
    result =
      if err.isNil:
        extccomp.addLocalCompileOption(
          c.config, s, toFullPathConsiderDirty(c.config, owner.fileIdx))
        recordPragma(c, it, "localpassl", s)
        it
      else:
        err
  of wPush, wPop, wPragma:
    # these require more context and thus special handling by the callsite
    result = it
  of wChecks, wObjChecks, wFieldChecks, wRangeChecks, wBoundChecks,
      wOverflowChecks, wNilChecks, wAssertions, wWarnings, wHints,
      wLineDir, wOptimization, wStaticBoundchecks, wStyleChecks,
      wCallconv, wDebugger, wProfiler,
      wFloatChecks, wNanChecks, wInfChecks, wTrMacros,
      wStackTrace, wLineTrace:
    var tmp = c.config.options
    result = processOption(c, it, tmp)
    c.config.options = tmp
  of wEmit:
    result = pragmaEmit(c, it)
  of wComputedGoto:
    result = noVal(c, it)
  of wEffects:
    # is later processed in effect analysis:
    result = noVal(c, it)
  of wByRef:
    result = noVal(c, it)
    var tmp = c.config.options
    result = processOption(c, it, tmp)
    c.config.options = tmp
  of wExperimental:
    result =
      if isTopLevel(c):
        processExperimental(c, it)
      else:
        c.config.newError(it,
                          PAstDiag(kind: adSemExperimentalRequiresToplevel))
  of {low(TSpecialWord)..high(TSpecialWord)} - stmtPragmas:
    unreachable(k)

# XXX: ``prepareSinglePragma`` and ``semSinglePragmaInStmt``/
#      ``semIdentPragmaInStmt`` share a lot of code. The duplication coulde
#      be removed by also separating evaluation from pre-processing for pragma
#      statement. Doing so would slightly change the semantics, however.

proc prepareSinglePragma(c: PContext; it: PNode, result: var seq[PNode],
                         validPragmas: TSpecialWords, sym: PSym) =
  ## Pre-processes the single pragma `it`, but doesn't apply it yet. The pre-
  ## processed pragma (multiple if the input pragma is a user-pragma) is
  ## appended to `result`.
  ##
  ## `sym` (nil is allowed) is only provided for use by error diagnostics and
  ## isn't mutated.
  let key = it.key

  proc customPragma(c: PContext, n: PNode, s: PSym): PNode =
    if s == nil or s.kind in allowsCustomPragma:
      semCustomPragma(c, n)
    else:
      illegalCustomPragma(c, n, s)

  let r =
    case key.kind
    of nkBracketExpr:
      invalidPragma(c, it)
    of nkCast:
      # pass through the cast pragma. It's later going to be treated as a
      # ``wCast``
      it
    of nkIdentKinds:
      # uses normal processing
      nil
    else:
      # must be either a custom pragma or an error
      customPragma(c, it, sym)

  if r != nil:
    # already processed
    result.add r
    return

  let (ident, error) = considerQuotedIdent(c, key)
  if error != nil:
    return

  let userPragma = strTableGet(c.userPragmas, ident)
  if userPragma != nil and userPragma.kind != skError:
    if {optStyleHint, optStyleError} * c.config.globalOptions != {}:
      styleCheckUse(c.config, key.info, userPragma)

    # number of pragmas increase/decrease with user pragma expansion
    inc c.instCounter
    if c.instCounter > maxInstantiation:
      # too deep recursion
      result.add c.config.newError(
        it, PAstDiag(kind: adSemPragmaRecursiveDependency,
                     userPragma: userPragma))

    else:
      # expand the user pragma in-place:
      for it in userPragma.ast.items:
        prepareSinglePragma(c, it, result, validPragmas, sym)

    dec c.instCounter
  else:
    let k = whichKeyword(ident)
    if k in validPragmas:
      if {optStyleHint, optStyleError} * c.config.globalOptions != {}:
        checkPragmaUse(c.config, key.info, k, ident.s)

      result.add it
    else:
      # try to treat as a custom pragma, which will produce an error if it's
      # not a valid custom pragma
      result.add customPragma(c, it, sym)

proc semSinglePragmaInStmt(
    c: PContext; owner: PSym, it: PNode, validPragmas: TSpecialWords, r: var seq[PNode]): TSpecialWord

proc semIdentPragmaInStmt(c: PContext, owner: PSym, it: PNode, r: var seq[PNode],
                     valid: TSpecialWords): TSpecialWord =
  ## Semantically analyses a single pragma where the callee is an identifier,
  ## in the context of a pragma statement. This includes expanding (and
  ## recursively analyzing) user-pragmas.
  ##
  ## `valid` is the set of built-in pragmas that are valid in the context where
  ## the pragma is used.
  let
    key = it.key
    (ident, error) = considerQuotedIdent(c, key)

  result = wInvalid

  if error != nil:
    r.add error
    return

  let userPragma = strTableGet(c.userPragmas, ident)
  if userPragma != nil and userPragma.kind != skError:
    if {optStyleHint, optStyleError} * c.config.globalOptions != {}:
      styleCheckUse(c.config, key.info, userPragma)

    # number of pragmas increase/decrease with user pragma expansion
    inc c.instCounter
    if c.instCounter > maxInstantiation:
      # too deep recursion
      r.add c.config.newError(
        it, PAstDiag(kind: adSemPragmaRecursiveDependency,
                     userPragma: userPragma))

    else:
      # expand the user pragma in-place and analyse the result (which might
      # mean expanding further user-pragmas):
      for it in userPragma.ast.items:
        discard semSinglePragmaInStmt(c, owner, it, valid, r)

    dec c.instCounter
  else:
    result = whichKeyword(ident)
    if result in valid:
      if {optStyleHint, optStyleError} * c.config.globalOptions != {}:
        checkPragmaUse(c.config, key.info, result, ident.s)

      r.add applyStmtPragma(c, owner, it, result)
    else:
      # it might still be a custom pragma
      result = wInvalid
      r.add semCustomPragma(c, it)

proc semSinglePragmaInStmt(
    c: PContext; owner: PSym, it: PNode, validPragmas: TSpecialWords, r: var seq[PNode]): TSpecialWord =
  ## Processes the single pragma expression `it`. If the pragma is a user-
  ## pragma, it is expanded first and the result is processed recursively.
  ##
  ## The pragmas are evaluated for their side-effects directly. This is because
  ## some pragmas (e.g. note changing ones like ``warning[X]``) can alter state
  ## that changes how following pragmas are processed.
  var key = it.key

  # we start with "invalid", which signals to the caller that the pragma is
  # not a built-in one
  result = wInvalid

  case key.kind
  of nkBracketExpr:
    r.add processNote(c, it)
  of nkCast:
    r.add c.config.newError(it, PAstDiag(kind: adSemCastRequiresStatement))
  of nkIdentKinds:
    result = semIdentPragmaInStmt(c, owner, it, r, validPragmas)
  else:
    r.add semCustomPragma(c, it)

proc overwriteLineInfo(n: PNode; info: TLineInfo) =
  n.info = info
  for i in 0..<n.safeLen:
    overwriteLineInfo(n[i], info)

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

proc applySymbolPragmaList(c: PContext, sym: PSym, list: var openArray[PNode]): bool =
  ## Analyses the pre-processed pragma list `list`, applying each symbol operator
  ## in it to `sym`. Each item is overwritten with the respective analysis or
  ## application result (or an error, if one occurred).
  ##
  ## Returns whether an error occurred.
  result = false
  for it in list.mitems:
    if it.kind != nkError:
      let o = whichPragma(it)
      it = applySymbolPragma(c, sym, it)
      assert it != nil, $o

    if it.kind == nkError:
      result = true
      # don't process further
      break

proc pragmaDeclNoImplicit*(c: PContext, sym: PSym, n: PNode,
                           validPragmas: TSpecialWords): PNode =
  ## Analyses the pragma list `n`, making sure each pragma is semantically
  ## valid and applying the symbol operators to `sym`. `validPragmas` is the
  ## list of built-in pragmas that are allowed -- usage of one not present in
  ## the set is an error.
  ##
  ## No implicit pragmas (i.e. pushed ones) are added to the list nor applied.
  addInNimDebugUtils(c.config, "pragmaDeclNoImplicit", sym, n, result)
  assert n.kind == nkPragma
  result = copyNode(n)

  # first pass: pre-process each item and expand user-pragmas
  for i in 0..<n.len:
    prepareSinglePragma(c, n[i], result.sons, validPragmas, sym)

  # second pass: apply the symbol operators until there is an error
  if applySymbolPragmaList(c, sym, result.sons):
    result = c.config.wrapError(result)

proc implicitPragmas*(c: PContext, sym: PSym, n: PNode, validPragmas: TSpecialWords): PNode =
  ## Adds implicit pragmas to the list `n` and applies them to `sym`
  ## afterwards. Implicit pragmas that don't apply (because they're, for
  ## example, built-in pragmas not present in `validPragmas`) are ignored.
  case n.kind
  of nkError:
    return n
  of nkEmpty:
    result = newNodeI(nkPragma, n.info)
  of nkPragma:
    result = n
  else:
    unreachable(n.kind)

  let start = result.len

  # step 1: add the implicit pragmas to the list
  var tmp: seq[PNode]
  for it in c.optionStack.items:
    let o = it.otherPragmas
    if o != nil:
      for x in o.items:
        prepareSinglePragma(c, x, tmp, validPragmas, sym)

        # filter the nodes. If they're erroneous, it means that the pragma
        # doesn't apply to the symbol
        for y in tmp.items:
          if not y.isError:
            # we're mutating the pragma's AST, so a copy is required
            let pragma = copyTree(y)
            overwriteLineInfo(pragma, n.info)
            result.add(pragma)

        tmp.setLen(0)

  if result.len == 0:
    # there were no applicable pragmas; restore the original
    result = n
  else:
    # apply the merged pragmas
    if applySymbolPragmaList(c, sym,
                             result.sons.toOpenArray(start, result.len-1)):
      # application failed, wrap the error nodes with another error indicating
      # that the pragmas are implicit
      for it in result.sons.mitems:
        if it.kind == nkError:
          it = c.config.newError(it):
            PAstDiag(kind: adSemImplicitPragmaError, implicitPragma: sym)

      result = c.config.wrapError(result)

proc pragmaDecl*(c: PContext, sym: PSym, n: PNode, validPragmas: TSpecialWords): PNode =
  ## Similar to `pragmaDeclNoImplicit <#pragmaDeclNoImplicit>`_, but adds the
  ## implicit pragmas to the resulting list and applies.
  addInNimDebugUtils(c.config, "pragmaDecl", sym, n, result)
  result = pragmaDeclNoImplicit(c, sym, n, validPragmas)
  # only add the implicit pragmas if there was no error during application of
  # the explicit ones
  if result.kind != nkError:
    result = implicitPragmas(c, sym, result, validPragmas)

# NOTE: types previously also inherited a pushed dynlib
proc inheritDynlib*(c: PContext, sym: PSym) =
  ## Apply the dynlib pragma from the top of the option stack to `sym`, if
  ## applicable. The dynlib pragma can be applied if the symbol is marked as
  ## imported, but no header nor dynlib are specified.
  let lib = c.optionStack[^1].dynlib
  if not lib.isNil and sfImportc in sym.flags and
     {exfDynamicLib, exfHeader} * sym.extFlags == {}:
    incl(sym.extFlags, exfDynamicLib)
    addToLib(lib, sym)
    if sym.extname == "":
      # XXX: this looks like a unnecessary defensive check. If the symbol is
      #      marked as imported, it already has an external name set
      sym.extname = sym.name.s

proc containsError(n: PNode): bool =
  for it in n.items:
    if it.kind == nkError:
      return true

  result = false

proc pragmaExpr*(c: PContext, n: PNode): PNode =
  ## Pre-processes the pragma list `n` for the purpose of further processing
  ## by sem in the context of applying pragmas to an expression.
  assert n.kind == nkPragma
  result = copyNode(n)
  for it in n.items:
    prepareSinglePragma(c, it, result.sons, exprPragmas, nil)

  for it in result.sons.mitems:
    if it.kind != nkError:
      it = processBlockPragma(c, it, whichPragma(it))

    if it.kind == nkError:
      # wrap the resulting list in an error and abort
      result = c.config.wrapError(result)
      break

proc pragmaStmt*(c: PContext, n: PNode, owner: PSym): PNode =
  ## Analyses and evaluates for their side-effects the pragmas in list `n`.
  ## `owner` is provided for addition context and is sometimes mutated.
  assert n.kind == nkPragma
  result = copyNode(n)

  var hasError = false
  for i in 0..<n.len:
    let k = semSinglePragmaInStmt(c, owner, n[i], stmtPragmas, result.sons)

    case k
    of wPush:
      hasError =
        not processPush(c, n.info, n.sons.toOpenArray(i+1, n.len-1), result)
      break
    of wPop:
      result.add processPop(c, n[i])
      # XXX: maybe it'd be better to either report an error if there are items
      #      remaining
      break
    of wPragma:
      result.add processPragma(c, n.info, n.sons.toOpenArray(i, n.len-1))
      # cut off the rest -- they're part of the user-pragma now
      break
    else:
      discard "continue"

  assert not cyclicTree(result)

  if hasError or containsError(result):
    result = c.config.wrapError(result)
