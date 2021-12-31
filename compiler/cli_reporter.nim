import reports, ast, types, renderer, astmsgs, astalgo
import options as compiler_options
import std/[strutils, terminal, options, algorithm]


func wrap(str: string, color: ForegroundColor): string =
  result.add "\e["
  result.add $color.int
  result.add "m"
  result.add str
  result.add "\e[0m"

const
  reportTitles: array[ReportSeverity, string] = [
    "Debug: ", "Hint: ", "Warning: ", "Error: ", "Fatal: ", "Trace: "
  ]

  reportColors: array[ReportSeverity, ForegroundColor] = [
    fgDefault, fgGreen, fgYellow, fgRed, fgRed, fgCyan
  ]

proc writeContext*(conf: ConfigRef, ctx: seq[ReportContext]) =
  for ctx in items(ctx):
    case ctx.kind:
      of sckInstantiationOf:
        conf.writeln(
          "template/generic instantiation of `",
          ctx.entry.name.s,
          "` from here")

      of sckInstantiationFrom:
        conf.writeln("template/generic instantiation from here")

proc renderNotLValue(n: PNode): string =
  result = $n

  let n = if n.kind == nkHiddenDeref:
            n[0]
          else:
            n

  if n.kind == nkHiddenCallConv and n.len > 1:
    result = $n[0] & "(" & result & ")"

  elif n.kind in {nkHiddenStdConv, nkHiddenSubConv} and n.len == 2:
    result = typeToString(n.typ.skipTypes(abstractVar)) & "(" & result & ")"

proc addPragmaAndCallConvMismatch*(
    message: var string,
    formal, actual: PType,
    conf: ConfigRef,
  ) =
  assert formal.kind == tyProc and actual.kind == tyProc
  let (convMismatch, _) = getProcConvMismatch(conf, formal, actual)

  var
    gotPragmas = ""
    expectedPragmas = ""

  for reason in convMismatch:
    case reason:
      of pcmDifferentCallConv:
        message.add "\n  Calling convention mismatch: got '{.$1.}', but expected '{.$2.}'." % [
          $actual.callConv, $formal.callConv]

      of pcmNoSideEffect:
        expectedPragmas.add "noSideEffect, "
      of pcmNotGcSafe:
        expectedPragmas.add "gcsafe, "
      of pcmLockDifference:
        gotPragmas.add("locks: " & $actual.lockLevel & ", ")
        expectedPragmas.add("locks: " & $formal.lockLevel & ", ")
      of pcmNotIterator: discard

  if expectedPragmas.len > 0:
    gotPragmas.setLen(max(0, gotPragmas.len - 2)) # Remove ", "
    expectedPragmas.setLen(max(0, expectedPragmas.len - 2)) # Remove ", "
    message.add "\n  Pragma mismatch: got '{.$1.}', but expected '{.$2.}'." % [gotPragmas, expectedPragmas]


proc effectProblem(f, a: PType; result: var string) =
  if f.kind == tyProc and a.kind == tyProc:
    if tfThread in f.flags and tfThread notin a.flags:
      result.add "\n  This expression is not GC-safe. Annotate the " &
          "proc with {.gcsafe.} to get extended error information."
    elif tfNoSideEffect in f.flags and tfNoSideEffect notin a.flags:
      result.add "\n  This expression can have side effects. Annotate the " &
          "proc with {.noSideEffect.} to get extended error information."
    else:
      case compatibleEffects(f, a)
      of efCompat: discard
      of efRaisesDiffer:
        result.add "\n  The `.raises` requirements differ."
      of efRaisesUnknown:
        result.add "\n  The `.raises` requirements differ. Annotate the " &
            "proc with {.raises: [].} to get extended error information."
      of efTagsDiffer:
        result.add "\n  The `.tags` requirements differ."
      of efTagsUnknown:
        result.add "\n  The `.tags` requirements differ. Annotate the " &
            "proc with {.tags: [].} to get extended error information."
      of efLockLevelsDiffer:
        result.add "\n  The `.locks` requirements differ. Annotate the " &
            "proc with {.locks: 0.} to get extended error information."
      of efEffectsDelayed:
        result.add "\n  The `.effectsOf` annotations differ."

proc format(diag: SemCallDiagnostics): string =
  case diag.kind:
    of scalldDefaultParamIsIncompatible:
      return "The default parameter '" & diag.param.name.s &
        "' has incompatible type with the explicitly requested proc instantiation"



proc presentFailedCandidates(
    conf: ConfigRef,
    n: PNode,
    errors: seq[SemCallMismatch]
  ): (TPreferedDesc, string) =

  var prefer = preferName
  # to avoid confusing errors like:
  #   got (SslPtr, SocketHandle)
  #   but expected one of:
  #   openssl.SSL_set_fd(ssl: SslPtr, fd: SocketHandle): cint
  # we do a pre-analysis. If all types produce the same string, we will add
  # module information.
  let proto = "" # describeArgs(c, n, 1, preferName)
  for err in errors:
    var errProto = ""
    let n = err.target.typ.n
    for i in 1 ..< n.len:
      var p = n[i]
      if p.kind == nkSym:
        errProto.add(typeToString(p.sym.typ, preferName))
        if i != n.len - 1:
          errProto.add(", ")

    if errProto == proto:
      prefer = preferModuleInfo
      break

  var filterOnlyFirst = false
  if optShowAllMismatches notin conf.globalOptions:
    for err in errors:
      if err.arg > 1:
        filterOnlyFirst = true
        break

  var
    maybeWrongSpace = false
    candidatesAll: seq[string]
    candidates = ""
    skipped = 0

  for err in errors:
    candidates.setLen 0
    if filterOnlyFirst and err.arg == 1:
      inc skipped
      continue

    if err.target.kind in routineKinds and err.target.ast != nil:
      candidates.add(renderTree(
        err.target.ast, {renderNoBody, renderNoComments, renderNoPragmas}))

    else:
      candidates.add(getProcHeader(conf, err.target, prefer))

    candidates.addDeclaredLocMaybe(conf, err.target)
    candidates.add("\n")

    let nArg = if err.arg < n.len: n[err.arg] else: nil

    let nameParam = if err.targetArg != nil: err.targetArg.name.s else: ""
    if n.len > 1:
      candidates.add("  first type mismatch at position: " & $err.arg)
      # candidates.add "\n  reason: " & $err.firstMismatch.kind # for debugging
      case err.kind:
        of kUnknownNamedParam:
          if nArg == nil:
            candidates.add("\n  unknown named parameter")
          else:
            candidates.add("\n  unknown named parameter: " & $nArg[0])

        of kAlreadyGiven:
          candidates.add("\n  named param already provided: " & $nArg[0])

        of kPositionalAlreadyGiven:
          candidates.add("\n  positional param was already given as named param")

        of kExtraArg:
          candidates.add("\n  extra argument given")

        of kMissingParam:
          candidates.add("\n  missing parameter: " & nameParam)

        of kTypeMismatch, kVarNeeded:
          doAssert nArg != nil
          let wanted = err.targetArg.typ
          doAssert err.targetArg != nil

          candidates.add("\n  required type for " & nameParam &  ": ")
          candidates.addTypeDeclVerboseMaybe(conf, wanted)
          candidates.add "\n  but expression '"

          if err.kind == kVarNeeded:
            candidates.add renderNotLValue(nArg)
            candidates.add "' is immutable, not 'var'"

          else:
            candidates.add renderTree(nArg)
            candidates.add "' is of type: "

            let got = nArg.typ
            candidates.addTypeDeclVerboseMaybe(conf, got)
            doAssert wanted != nil

            if got != nil:
              if got.kind == tyProc and wanted.kind == tyProc:
                # These are proc mismatches so,
                # add the extra explict detail of the mismatch
                candidates.addPragmaAndCallConvMismatch(wanted, got, conf)
              effectProblem(wanted, got, candidates)

        of kUnknown:
          discard "do not break 'nim check'"

      candidates.add "\n"
      if err.arg == 1 and nArg.kind == nkTupleConstr and
          n.kind == nkCommand:
        maybeWrongSpace = true

    for diag in err.diagnostics:
      candidates.add(format(diag) & "\n")

    candidatesAll.add candidates

  candidatesAll.sort # fix #13538
  candidates = join(candidatesAll)

  if skipped > 0:
    candidates.add(
      $skipped &
        " other mismatching symbols have been " &
        "suppressed; compile with --showAllMismatches:on to see them\n")

  if maybeWrongSpace:
    candidates.add("maybe misplaced space between " & renderTree(n[0]) & " and '(' \n")

  result = (prefer, candidates)

proc argTypeToString(arg: PNode; prefer: TPreferedDesc): string =
  if arg.kind in nkSymChoices:
    result = typeToString(arg[0].typ, prefer)
    for i in 1 ..< arg.len:
      result.add(" | ")
      result.add typeToString(arg[i].typ, prefer)

  elif arg.typ == nil:
    result = "void"

  else:
    result = arg.typ.typeToString(prefer)


proc describeArgs(conf: ConfigRef, n: PNode, startIdx = 1; prefer = preferName): string =
  result = ""
  for i in startIdx ..< n.len:
    var arg = n[i]
    if n[i].kind == nkExprEqExpr:
      result.add renderTree(n[i][0])
      result.add ": "
      if arg.typ.isNil and arg.kind notin {nkStmtList, nkDo}:
        # XXX we really need to 'tryExpr' here!

        when false:
          # HACK original implementation of the `describeArgs` used
          # `semOperand` here, but until there is a clear understanding
          # /why/ is it necessary to additionall call sem on the arguments
          # I will leave this as it is now. This was introduced in commit
          # 5b0d8246f79730a473a869792f12938089ecced6 that "made some tests
          # green" (+98/-77)
          arg = c.semOperand(c, n[i][1])
          arg = n[i][1]
          n[i].typ = arg.typ
          n[i][1] = arg

        else:
          debug arg

    else:
      if arg.typ.isNil and arg.kind notin {
           nkStmtList, nkDo, nkElse, nkOfBranch, nkElifBranch, nkExceptBranch
         }:

        when false:
          # HACK same as comment above
          arg = c.semOperand(c, n[i])
          n[i] = arg

        else:
          debug arg


    if arg.typ != nil and arg.typ.kind == tyError:
      return

    result.add argTypeToString(arg, prefer)
    if i != n.len - 1:
      result.add ", "


proc toStr(conf: ConfigRef, r: SemReport): string =
  case SemReportKind(r.kind):
    of rsemCallTypeMismatch:
      let (prefer, candidates) = presentFailedCandidates(
        conf, r.ast, r.callMismatches)

      result.add "type mismatch: got <"
      result.add conf.describeArgs(r.ast, 1, prefer)
      result.add ">"
      if candidates != "":
        result.add "\nbut expected one of:\n" & candidates

    of rsemPragmaRecursiveDependency:
      result.add "recursive dependency: "
      result.add r.sym.name.s

    of rsemMisplacedDeprecation:
      result = "annotation to deprecated not supported here"

    of rsemNoUnionForJs:
      result = "`{.union.}` is not implemented for js backend."

    of rsemBitsizeRequiresPositive:
      result = "bitsize needs to be positive"

    of rsemExperimentalRequiresToplevel:
      result = "'experimental' pragma only valid as toplevel " &
        "statement or in a 'push' environment"

    of rsemDeprecated:
      result = r.str

    of rsemThisPragmaRequires01Args:
      # FIXME remove this report kind, reuse "wrong number of arguments"
      result = "'this' pragma is allowed to have zero or one arguments"

    of rsemTooManyRegistersRequired:
      result = "VM problem: too many registers required"

    of rsemVmCannotFindBreakTarget:
      result = "VM problem: cannot find 'break' target"

    of rsemVmNotUnused:
      result = "not unused"

    of rsemVmTooLargetOffset:
      result = "too large offset! cannot generate code for: " &
        r.sym.name.s

    of rsemVmCannotGenerateCode:
      result = "cannot generate code for: " &
        $r.ast

    of rsemVmCannotCast:
      let mis = r.typeMismatch[0]
      result = "VM does not support 'cast' from " &
        $mis.actualType.kind & " to " & $mis.wantedType.kind

    of rsemVmInvalidBindSym:
      result = "invalid bindSym usage"

    of rsemSymbolKindMismatch:
      result = "cannot use symbol of kind '$1' as a '$2'" %
        [$r.sym.kind, $r.expectedSymbolKind]

    of rsemTypeNotAllowed:
      let (t, typ, kind) = (
        r.allowedType.allowed, r.allowedType.actual, r.allowedType.kind)

      if t == typ:
        result = "invalid type: '$1' for $2" % [
          typeToString(typ), toHumanStr(kind)]

        if kind in {skVar, skLet, skConst} and
           taIsTemplateOrMacro in r.allowedType.allowedFlags:

          result &= ". Did you mean to call the $1 with '()'?" % [
            toHumanStr(typ.owner.kind)]

      else:
        result = "invalid type: '$1' in this context: '$2' for $3" % [
          typeToString(t), typeToString(typ), toHumanStr(kind)]

    of rsemCyclicTree:
      result = "the resulting AST is cyclic and cannot be processed further"

    of rsemConstExprExpected:
      result = "constant expression expected"

    of rsemTemplateInstantiationTooNested:
      result = "template instantiation too nested"

    of rsemExpressionHasNoType:
      result = "expression has no type: " & renderTree(r.ast, {renderNoComments})

    of rsemMissingGenericParamsForTemplate:
      result = "'$1' has unspecified generic parameters" % r.sym.name.s

    of rsemExpandMacro:
      result = r.expandedExpr.renderTree()

    of rsemUnusedImport:
      result = "imported and not used: '$1'" % r.sym.name.s

    of rsemCallNotAProcOrField:
      if r.explicitCall:
        if result.len == 0:
          result = "attempting to call undeclared routine: '$1'" % $r.str
        else:
          result = "attempting to call routine: '$1'$2" % [$r.str, $result]

      else:
        let sym = r.typ.typSym
        var typeHint = ""
        if sym == nil:
          # Perhaps we're in a `compiles(foo.bar)` expression, or
          # in a concept, e.g.:
          #   ExplainedConcept {.explain.} = concept x
          #     x.foo is int
          discard
        else:

          typeHint = " for type " & getProcHeader(conf, sym)

        let suffix = if result.len > 0: " " & result else: ""

        result = "undeclared field: '$1'" % r.str & typeHint & suffix

    of rsemUndeclaredField:
      result =  "undeclared field: '$1' for type $2" % [
        $r.ast.ident.s, $getProcHeader(conf, r.sym)]

    of rsemAmbiguous:
      var args = "("
      for i in 1 ..< r.ast.len:
        if i > 1:
          args.add(", ")
        args.add(typeToString(r.ast[i].typ))
      args.add(")")


      result = "ambiguous call; both $1 and $2 match for: $3" % [
        getProcHeader(conf, r.symbols[0]),
        getProcHeader(conf, r.symbols[1])
      ]

    else:
      return $r

proc toStr(conf: ConfigRef, loc: ReportLineInfo): string = $loc

proc report(conf: ConfigRef, r: SemReport)      =
  let sev = conf.severity(r)

  if r.kind == rsemProcessing and conf.hintProcessingDots:
    conf.write "."
    return

  if sev == rsevError:
    conf.writeContext(r.context)

  conf.writeln(
    # Optional report location
    if r.location.isSome():
      conf.toStr(r.location.get()) & " "
    else:
      "",

    # `Hint: `, `Error: ` etc.
    wrap(reportTitles[sev], reportColors[sev]),

    # Message body
    toStr(conf, r),

    # Trailing report message - `[GcMem]`
    if sev in {rsevHint, rsevWarning}:
      wrap(" [" & $r.kind & "]", fgCyan)

    else:
      ""
  )


proc report(conf: ConfigRef, r: ParserReport)   = echo r
proc report(conf: ConfigRef, r: LexerReport)    = echo r
proc report(conf: ConfigRef, r: InternalReport) = echo r
proc report(conf: ConfigRef, r: ExternalReport) = echo r
proc report(conf: ConfigRef, r: DebugReport)    = echo r
proc report(conf: ConfigRef, r: BackendReport)  = echo r
proc report(conf: ConfigRef, r: CmdReport)      = echo r

proc reportHook*(conf: ConfigRef, r: Report) =
  if not conf.isEnabled(r): return

  case r.category:
    of repLexer:    conf.report(r.lexReport)
    of repParser:   conf.report(r.parserReport)
    of repCmd:      conf.report(r.cmdReport)
    of repSem:      conf.report(r.semReport)
    of repDebug:    conf.report(r.debugReport)
    of repInternal: conf.report(r.internalReport)
    of repBackend:  conf.report(r.backendReport)
    of repExternal: conf.report(r.externalReport)
