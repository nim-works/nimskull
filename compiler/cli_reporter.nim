import reports, ast, types, renderer, astmsgs, astalgo, msgs
import options as compiler_options
import std/[strutils, terminal, options, algorithm, sequtils, strformat]

func add(target: var string, other: varargs[string, `$`]) =
  for item in other:
    target.add item

func wrap(
    str: string,
    color: ForegroundColor,
    style: set[Style] = {}
  ): string =

  result.add("\e[", color.int, "m")
  for s in style:
    result.add("\e[", s.int, "m")

  result.add str
  result.add "\e[0m"

func wrap*(
    conf: ConfigRef,
    str: string,
    color: ForegroundColor,
    style: set[Style] = {}
  ): string =

  if conf.useColor:
    result = wrap(str, color, style)

  else:
    result = str

const
  reportTitles: array[ReportSeverity, string] = [
    "Debug: ", "Hint: ", "Warning: ", "Error: ", "Fatal: ", "Trace: "
  ]

  reportColors: array[ReportSeverity, ForegroundColor] = [
    fgDefault, fgGreen, fgYellow, fgRed, fgRed, fgCyan
  ]

proc csvList(syms: seq[PSym]): string =
  syms.mapIt(it.name.s).join(", ")

proc getContext(conf: ConfigRef, ctx: seq[ReportContext]): string =
  for ctx in items(ctx):
    case ctx.kind:
      of sckInstantiationOf:
        result.add(
          "template/generic instantiation of `",
          ctx.entry.name.s,
          "` from here\n")

      of sckInstantiationFrom:
        result.add("template/generic instantiation from here\n")

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

proc presentSpellingCandidates*(
  conf: ConfigRef, candidates: seq[SemSpellCandidate]): string =

  result = "candidates (edit distance, scope distance); see '--spellSuggest':"
  for candidate in candidates:
    result.add "\n ($1, $2): '$3'" % [
      $candidate.dist,
      $candidate.depth,
      $candidate.sym.name.s
    ]

    result.addDeclaredLoc(conf, candidate.sym)

proc toStr(conf: ConfigRef, r: SemReport): string =
  proc render(n: PNode): string = renderTree(n, {renderNoComments})
  proc render(t: PType): string = typeToString(t)

  case SemReportKind(r.kind):
    of rsemCallTypeMismatch:
      let (prefer, candidates) = presentFailedCandidates(
        conf, r.ast, r.callMismatches)

      result.add "type mismatch: got <"
      result.add conf.describeArgs(r.ast, 1, prefer)
      result.add ">"
      if candidates != "":
        result.add "\nbut expected one of:\n" & candidates

      result.add "\nexpression: "
      result.add r.ast.render

    of rsemExpandArc:
      result.add(
        "--expandArc: ",
        r.symstr,
        "\n",
        r.expandedAst.render,
        "\n",
        "-- end of expandArc ------------------------"
      )

    of rsemCannotBorrow:
      result.add(
        "cannot borrow ",
        r.symstr,
        "; what it borrows from is potentially mutated"
      )

      if r.borrowPair.mutatedHere.isValid():
        result.add("\n", $r.borrowPair.mutatedHere, " the mutation is here")

      if r.borrowPair.connectedVia.isValid():
        result.add(
          "\n",
          $r.borrowPair.connectedVia,
          " is the statement that connected the mutation to the parameter")

    of rsemBorrowOutlivesSource:
      result.add(
        "'",
        r.symbols[0].name.s,
        "' borrows from location '",
        r.symbols[1].name.s,
        "' which does not live long enough"
      )

    of rsemImmutableBorrowMutation:
      result.add(
        "'",
        r.symbols[0].name.s,
        "' borrows from the immutable location '",
        r.symbols[1].name.s,
        "' and attempts to mutate it"
      )

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
      result = "VM does not support 'cast' from " &
        $r.actualType.kind & " to " & $r.formalType.kind

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
      result = "expression has no type: " & render(r.ast)

    of rsemMissingGenericParamsForTemplate:
      result = "'$1' has unspecified generic parameters" % r.sym.name.s

    of rsemExpandMacro:
      result = r.expandedAst.render()

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

    of rsemCopiesToSink:
      result = (
        "passing '$1' to a sink parameter introduces an implicit copy; " &
          "if possible, rearrange your program's control flow to prevent it") % [
          r.ast.render]


    of rsemAmbiguousIdent:
      result = "ambiguous identifier: '" & r.str & "' -- use one of the following:\n"
      var i = 0
      for sym in r.symbols:
        if 0 < i:
          result.add "\n"

        result.add "  " & sym.owner.name.s & "." & sym.name.s
        inc i


    of rsemStaticOutOfBounds:
      let (i, a, b) = r.indexSpec
      if b < a:
        result = "index out of bounds, the container is empty"
      else:
        result = "index " & $i & " not in " & $a & " .. " & $b

    of rsemStaticFieldNotFound:
      result = "field not found: " & r.sym.name.s

    of rsemInvalidIntdefine:
      result = "{.intdefine.} const was set to an invalid integer: '" & r.str & "'"

    of rsemInvalidBooldefine:
      result = "{.booldefine.} const was set to an invalid bool: '" & r.str & "'"

    of rsemSemfoldInvalidConversion:
      result = "conversion from $1 to $2 is invalid" % [
        typeToString(r.actualType()), typeToString(r.formalType())]

    of rsemIllformedAst:
      result = "ilformed ast: " & render(r.ast)

    of rsemCannotInstantiate:
      result = "cannot instantiate: '$1'" % r.sym.name.s

    of rsemTypeKindMismatch:
      result = r.str

    of rsemExprHasNoAddress:
      result = "expression has no address"
      if r.isUnsafeAddr:
        result.add "; maybe use 'unsafeAddr'"

    of rsemVmCannotEvaluateAtComptime:
      result = "typeof: cannot evaluate 'mode' parameter at compile-time"

    of rsemIntLiteralExpected:
      result = "integer literal expected"

    of rsemGenericTypeExpected:
      result = "expected generic type, got: type $2 of kind $1" % [
        r.actualType.kind.toHumanStr,
        typeToString(r.actualType)]

    of rsemUnknownTrait:
      result = "unknown trait: " & r.sym.name.s

    of rsemExpectedOrdinal:
      result = "ordinal type expected"

    of rsemStringLiteralExpected:
      result = "string literal expected"

    of rsemConditionAlwaysTrue:
      result = "condition is always true: '$1'" % render(r.ast)

    of rsemConditionAlwaysFalse:
      result = "condition is always false: '$1'" % render(r.ast)

    of rsemWrongNumberOfArguments:
      result = "wrong number of arguments"

    of rsemCannotBeOfSubtype:
      result = "'$1' cannot be of this subtype" % typeToString(r.actualType())

    of rsemQuantifierInRangeExpected:
      result = "<quantifier> 'in' <range> expected"

    of rsemOldTakesParameterName:
      result = "'old' takes a parameter name"

    of rsemOldDoesNotBelongTo:
      result = r.ast.sym.name.s & " does not belong to " & r.symstr

    of rsemCannotFindPlugin:
      result = "cannot find plugin " & r.symstr

    of rsemExpectedProcReferenceForFinalizer:
      result = "finalizer must be a direct reference to a proc"

    of rsemUnsafeSetLen:
      result = "setLen can potentially expand the sequence, " &
        "but the element type '$1' doesn't have a valid default value" %
        typeToString(r.typ)

    of rsemUnsafeDefault:
      result = "The '$1' type doesn't have a valid default value" %
        typeToString(r.typ)

    of rsemCannotIsolate:
      result = "expression cannot be isolated: " & render(r.ast)

    of rsemInnerCodeReordering:
      result = "Code reordering experimental pragma only valid at toplevel"

    of rsemUnknownExperimental:
      result = "unknown experimental feature"

    of rsemWrongIdent:
      result = joinAnyOf(r.expectedIdents, quote = true) & " expected"

    of rsemPragmaOptionExpected:
      result = "option expected"

    of rsemUnexpectedPushArgument:
      result = "'push' cannot have arguments"

    of rsemExcessiveCompilePragmaArgs:
      result = "'.compile' pragma takes up 2 arguments"

    of rsemEmptyAsm:
      result = "empty 'asm' statement"

    of rsemLinePragmaExpectsTuple:
      result = "tuple expected"

    of rsemRaisesPragmaExpectsObject:
      result = "invalid type for raises/tags list"

    of rsemLocksPragmaExpectsList:
      result = "locks pragma takes a list of expressions"

    of rsemLocksPragmaBadLevel:
      result = r.str

    of rsemBorrowPragmaNonDot:
      result = "a type can only borrow `.` for now"

    of rsemInvalidExtern:
      result = "invalid extern name: '" & r.externName & "'. (Forgot to escape '$'?)"

    of rsemBadDeprecatedArgs:
      result = r.str

    of rsemInvalidPragma:
      result = "invalid pragma"

    of rsemMisplacedEffectsOf:
      result = "parameter cannot be declared as .effectsOf"

    of rsemMissingPragmaArg:
      result = "parameter name expected"

    of rsemCannotPushCast:
      result = "a 'cast' pragma cannot be pushed"

    of rsemCastRequiresStatement:
      result = "'cast' pragma only allowed in statement context"

    of rsemImplicitObjConv:
      result = "Implicit conversion: Receiver '$2' will not receive fields of sub-type '$1'" % [
        typeToString(r.formalType),
        typeToString(r.actualType)
      ]

    of rsemExtendedContext:
      assert false, "This is a configuration hint"

    of rsemUserRaw:
      assert false, "Appears to be unused"

    of rsemNonMatchingCandidates:
      let (_, candidates) = presentFailedCandidates(conf, r.ast, r.callMismatches)
      result = "Non-matching candidates for " & render(r.ast) & "\n" &
              candidates

    of rsemEffectsListingHint:
      for tag in r.effectListing.exceptions & r.effectListing.tags:
        result.add typeToString(tag)
        result.add "\n"

    of rsemLockLevelMismatch:
      result = "expected lock level < " & $r.lockMismatch.expected &
        " but got lock level " & $r.lockMismatch.got

    of rsemCantPassProcvar:
      result = "'$1' cannot be passed to a procvar" % r.symstr

    of rsemCannotProveNotNil:
      result = "cannot prove '$1' is not nil" % render(r.ast)

    of rsemProvablyNil:
      result = "'$1' is provably nil" % render(r.ast)

    of rsemInvalidBindContext:
      result = "invalid context for 'bind' statement: " & render(r.ast)

    of rsemExpectedTypelessDeferBody:
      result = "'defer' takes a 'void' expression"

    of rsemUnexpectedToplevelDefer:
      result = "defer statement not supported at top level"

    of rsemExportRequiresToplevel:
      result = "export is only allowed at top level"

    of rsemImportRequiresToplevel:
      result = "import is only allowed at top level"

    of rsemBindDeprecated:
      result = "bind is deprecated"

    of rsemCannotMixTypesAndValuesInTuple:
      result = "Mixing types and values in tuples is not allowed."

    of rsemCannotExport:
      result = "cannot export: " & render(r.ast)
      if r.sym.kind == skEnumField:
        result.add "; enum field cannot be exported individually"

    of rsemExpectedModuleNameForImportExcept:
      result = "The export/except syntax expects a module name"

    of rsemDisallowedTypedescForTupleField:
      result = "typedesc not allowed as tuple field."

    of rsemFieldInitTwice:
      result = "field initialized twice: '$1'" % r.str

    of rsemNamedExprNotAllowed:
      result = "named expression not allowed here"

    of rsemNamedExprExpected:
      result = "named expression expected"

    of rsemExpectedExpressionForSpawn:
      result =  "'spawn' takes a call expression; got: " & render(r.ast)

    of rsemEnableExperimentalParallel:
      result = "use the {.experimental.} pragma to enable 'parallel'"

    of rsemExpectedTypeOrValue:
      result = "'$1' expects a type or value" % r.str

    of rsemSystemNeeds:
      result = "system needs: '$1'" % r.str

    of rsemCovariantUsedAsNonCovariant:
      result = "covariant param '" & r.symstr & "' used in a non-covariant position"

    of rsemContravariantUsedAsNonCovariant:
      result = "contravariant param '" & r.symstr & "' used in a non-contravariant position"

    of rsemExpectedInvariantParam:
      result = "non-invariant type param used in a proc type: " & $r.typ

    of rsemNonInvariantCannotBeUsedWith:
      result = "non-invariant type parameters cannot be used with types such '" & $r.typ & "'"

    of rsemNonInvariantCnnnotBeUsedInConcepts:
      result = "non-invariant type parameters are not supported in concepts"

    of rsemImplementationExpected:
      result = "implementation of '$1' expected" % r.symstr

    of rsemUnexpectedExportcInAlias:
      result = "{.exportc.} not allowed for type aliases"

    of rsemCannotCreateFlowVarOfType:
      result = "cannot create a flowVar of type: " & typeToString(r.typ)

    of rsemCannotSpawnMagicProc:
      result = "'spawn'ed function cannot have a 'typed' or 'untyped' parameter"

    of rsemCannotSpawnProcWithVar:
      result = "'spawn'ed function cannot have a 'var' parameter"

    of rsemCannotDiscardSpawn:
      result = "'spawn' must not be discarded"

    of rsemSpawnRequiresCall:
      result = "'spawn' takes a call expression; got: " & render(r.ast)

    of rsemSpawnRequiresGcSafe:
      result = "'spawn' takes a GC safe call expression"

    of rsemSpawnForbidsClosure:
      result = "closure in spawn environment is not allowed"

    of rsemSpawnForbidsIterator:
      result =  "iterator in spawn environment is not allowed"

    of rsemUnexpectedClosureOnToplevelProc:
      result = "'.closure' calling convention for top level routines is invalid"

    of rsemExpectedReturnTypeForIterator:
      result = "iterator needs a return type"

    of rsemUsageIsError:
      result = "$1usage of '$2' is an {.error.} defined at $3" %
      [r.str, r.symstr, toFileLineCol(conf, r.sym.ast.info)]

    of rsemCustomError, rsemCustomPrintMsgAndNodeError:
      assert false, $r.kind & " appears to be unused"

    of rsemTypeMismatch:
      let (actual, formal) = (r.actualType, r.formalType)
      let actualStr = typeToString(actual)
      let formalStr = typeToString(formal)
      let desc = typeToString(formal, preferDesc)

      let x = if formalStr == desc: formalStr else: formalStr & " = " & desc

      let verbose = actualStr == formalStr or optDeclaredLocs in conf.globalOptions
      result = "type mismatch:"
      if verbose:
        result.add "\n"

      if conf.isDefined("nimLegacyTypeMismatch"):
        result.add  " got <$1>" % actualStr

      else:
        result.add  " got '$1' for '$2'" % [actualStr, r.ast.renderTree]

      if verbose:
        result.addDeclaredLoc(conf, actual)
        result.add "\n"

      result.add " but expected '$1'" % x

      if verbose:
        result.addDeclaredLoc(conf, formal)

      if formal.kind == tyProc and actual.kind == tyProc:
        result.addPragmaAndCallConvMismatch(formal, actual, conf)
        case compatibleEffects(formal, actual):
          of efCompat:
            discard

          of efRaisesDiffer:
            result.add "\n.raise effects differ"

          of efRaisesUnknown:
            result.add "\n.raise effect is 'can raise any'"

          of efTagsDiffer:
            result.add "\n.tag effects differ"

          of efTagsUnknown:
            result.add "\n.tag effect is 'any tag allowed'"

          of efLockLevelsDiffer:
            result.add "\nlock levels differ"

          of efEffectsDelayed:
            result.add "\n.effectsOf annotations differ"

    of rsemConverterRequiresToplevel:
      result = "converter is only allowed at top level"

    of rsemUsingRequiresToplevel:
      result = "using is only allowed at top level"

    of rsemInvalidVisibility:
      result = "invalid visibility: '$1'" % r.ast.render

    of rsemUnknownPackageName:
      result = "unknown package name: " % r.str

    of rsemTypeCannotBeForwarded:
      result = r.symstr & " is not a type that can be forwarded"

    of rsemPackageRequiresToplevel:
      result = "only top level types in a package can be 'package'"

    of rsemDoubleCompletionOf:
      result = "cannot complete type '" &
        r.symbols[1].name.s &
        "' twice; " &
        "previous type completion was here: " &
        (conf $ r.symbols[0].info)

    of rsemInheritanceOnlyWorksWithAnEnum:
      result = "inheritance only works with an enum"

    of rsemWrongNumberOfVariables:
      result = "wrong number of variables"

    of rsemInvalidOrderInEnum:
      result = "invalid order in enum '$1'" & $r.symstr

    of rsemSetTooBig:
      result = "set is too large"

    of rsemTIsNotAConcreteType:
      result = "'$1' is not a concrete type" & r.typ.render()

    of rsemVarVarNotAllowed:
      result = "type 'var var' is not allowed"

    of rsemRangeIsEmpty:
      result = "range is empty"

    of rsemExpectedOrdinalOrFloat:
      result = "ordinal or float type expected"

    of rsemExpectedUnholyEnum:
      result = "enum '$1' has holes" % r.typ.render()

    of rsemRangeDoesNotSupportNan:
      result = "NaN is not a valid start or end for a range"

    of rsemRangeRequiresDotDot:
      result = "range types need to be constructed with '..', '..<' is not supported"

    of rsemExpectedRange:
      result = "expected range"

    of rsemArrayExpectsPositiveRange:
      result = "Array length can't be negative, but was " & $r.countMismatch.got

    of rsemDistinctDoesNotHaveDefaultValue:
      result = "The $1 distinct type doesn't have a default value." % r.typ.render

    of rsemObjectRequiresFieldInit:
      result = "The $1 type doesn't have a default value. The following fields must " &
      "be initialized: $2." % [r.typ.render, r.symbols.csvList()]

    of rsemExpectedObjectType:
      result = "object constructor needs an object type"

    of rsemAmbiguousCall:
      result = "overloaded '$1' leads to ambiguous calls" % r.symstr

    of rsemDeclarationVisibilityMismatch:
      result = (
        "public implementation '$1' has non-public forward declaration at $2"
      ) % [getProcHeader(conf, r.sym, getDeclarationPath = false), conf $ r.sym.info]

    of rsemVmInvalidObjectConstructor:
      result = "invalid object constructor"

    of rsemImplementationNotAllowed:
      result = "implementation of '$1' is not allowed" % r.symstr

    of rsemGenericLambdaNowAllowed:
      result = "A nested proc can have generic parameters only when " &
        "it is used as an operand to another routine and the types " &
        "of the generic paramers can be inferred from the expected signature."

    of rsemUnexpectedAutoInForwardDeclaration:
      result = "return type 'auto' cannot be used in forward declarations"

    of rsemInvalidControlFlow:
      result = "invalid control flow: $1" % [
        if r.sym.isNil: r.ast.render else: r.symstr
      ]

    of rsemContinueCannotHaveLabel:
      result = "'continue' cannot have a label"

    of rsemUseOrDiscard:
      result = "value of type '$1' has to be used (or discarded)" % r.typ.render

    of rsemCannotBeRaised:
      result = "only a 'ref object' can be raised"

    of rsemExceptionAlreadyHandled:
      result = "exception already handled"

    of rsemCannotExceptNativeAndImported:
      result = "Mix of imported and native exception types is not allowed in one except branch"

    of rsemExpectedSingleFinally:
      result = "Only one finally is allowed after all other branches"

    of rsemExpectedSingleGeneralExcept:
      result = "Only one general except clause is allowed after more specific exceptions"

    of rsemCannotConvertToRange:
      result = "cannot convert '$1' to '$2'" % [$r.ast.floatVal, typeToString(r.typ)]

    of rsemProveInit:
      result = "Cannot prove that '$1' is initialized. This will become a compile time error in the future." %
        r.symstr

    of rsemUsingRequiresType:
      result = "'using' section must have a type"

    of rsemUsingDisallowsAssign:
      result = "'using' sections cannot contain assignments"

    of rsemImplicitFieldConstructinoRequiresPartial:
      result = "implicit object field construction " &
        "requires a .partial object, but got " & r.typ.render

    of rsemDifferentTypeForReintroducedSymbol:
      result = "inconsistent typing for reintroduced symbol '" &
        r.symstr & "': previous type was: " & r.formalType.render() &
        "; new type is: " & r.actualType.render()


    of rsemCannotInferTypeOfLiteral:
      result = "cannot infer the type of the $1" % r.typ.render

    of rsemProcHasNoConcreteType:
      result = "'$1' doesn't have a concrete type, due to unspecified generic parameters." %
        r.ast.render

    of rsemEachIdentIsTuple:
      result = "each identifier is a tuple"

    of rsemResultShadowed:
      result = "Special variable 'result' is shadowed."

    of rsemThreadvarCannotInit:
      result = "a thread var cannot be initialized explicitly; this would only run for the main thread"

    of rsemLetNeedsInit:
      result = "'let' symbol requires an initialization"

    of rsemGlobalVar:
      result = "global variable declared here"

    of rsemForExpectsIterator:
      result = "iterator within for loop context expected"

    of rsemSelectorMustBeOfCertainTypes:
      result = "selector must be of an ordinal type, float, or string"

    of rsemUnreachableElse:
      result = "unreachable else, all cases are already covered"

    of rsemMissingCaseBranches:
      result = "not all cases are covered"
      if 0 < r.symbols.len:
        result.add "; missing: {$1}" % r.symbols.csvList()

    of rsemCannotRaiseNonException:
      result = "raised object of type $1 does not inherit from Exception" & r.typ.render

    of rsemUnexpectedEqInObjectConstructor:
      result = "object construction uses ':', not '='"

    of rsemConvFromXtoItselfNotNeeded:
      result = "conversion from $1 to itself is pointless" % r.typ.render

    of rsemIllegalConversion:
      result = "illegal conversion from '$1' to '$2'" % [
        r.actualType.render, r.formalType.render
      ]

    of rsemCannotBeConvertedTo:
      let value = if r.ast.kind in {nkCharLit..nkUInt64Lit}: $r.ast.getInt else: $r.ast.getFloat
      result = value & " can't be converted to " & r.typ.render

    of rsemCannotCastToNonConcrete:
      result = "cannot cast to a non concrete type: '$1'" % r.typ.render

    of rsemCannotCastTypes:
      let tar = $r.formalType
      let alt = typeToString(r.formalType, preferDesc)
      let msg = if tar != alt: tar & "=" & alt else: tar
      result = "expression cannot be cast to " & msg

    of rsemInvalidArgumentFor:
      result = "invalid argument for: " & r.str

    of rsemNoTupleTypeForConstructor:
      result = "no tuple type for constructor"

    of rsemInvalidTupleConstructor:
      result = "invalid tuple constructor"

    of rsemUnknownIdentifier:
      result = "unknown identifier: " & r.symstr

    of rsemIndexOutOfBounds:
      result = "size of array exceeds range of index type '$1' by $2 elements" % [
        typeToString(r.typ), $(r.countMismatch.got - r.countMismatch.expected)]

    of rsemVarForOutParamNeeded:
      result = "for a 'var' type a variable needs to be passed; but '$1' is immutable" %
        r.ast.render

    of rsemStackEscape:
      result = "address of '$1' may not escape its stack frame" % r.ast.render

    of rsemCannotInterpretNode:
      result = "cannot evaluate '$1'" % r.ast.render

    of rsemRecursiveDependencyIterator:
      result = "recursion is not supported in iterators: '$1'" % r.symstr

    of rsemDisallowedNilDeref:
      result = "nil dereference is not allowed"

    of rsemInvalidTupleSubscript:
      result = "invalid index value for tuple subscript"

    of rsemLocalEscapesStackFrame:
      result = "'$1' escapes its stack frame; context: '$2'" % [r.symstr, r.ast.render]

    of rsemImplicitAddrIsNotFirstParam:
      result = "'$1' is not the first parameter; context: '$2'" % [r.symstr, r.ast.render]

    of rsemExpectedOwnerReturn:
      result = "cannot return an owned pointer as an unowned pointer; " &
        "use 'owned(" & r.typ.render & ")' as the return type"

    of rsemExpectedUnownedRef:
      result = "assignment produces a dangling ref: the unowned ref lives longer than the owned ref"

    of rsemCannotAssignTo:
      result = "'$1' cannot be assigned to" % r.ast.render

    of rsemNoReturnTypeDeclared:
      result = "no return type declared"

    of rsemReturnNotAllowed:
      result = "'return' not allowed here"

    of rsemCannotInferReturnType:
      result = "cannot infer the return type of '$1'" % r.symstr

    of rsemUnexpectedYield:
      result = "'yield' only allowed in an iterator"

    of rsemCannotReturnTypeless:
      result = "current routine cannot return an expression"

    of rsemExpectedValueForYield:
      result = "yield statement must yield a value"

    of rsemExpectedIdentifier:
      result = "identifier expected, but got: " & r.ast.render

    of rsemExpectedMacroOrTemplate:
      result = "'$1' is not a macro or template" % (
        if r.sym.isNil: r.ast.render else: r.symstr)

    of rsemExpectedTemplateWithNArgs:
      result = "expected a template that takes " & $(r.countMismatch.expected) & " arguments"

    of rsemAmbiguousGetAst:
      result = "ambiguous symbol in 'getAst' context: " & r.ast.render

    of rsemExpectedCallForGetAst:
      result = "getAst takes a call, but got " & r.ast.render

    of rsemSuspiciousEnumConv:
      result = "suspicious code: enum to enum conversion"

    of rsemStringOrIdentNodeExpected:
      result = "string or ident node expected"

    of rsemExpectedObjectForOf:
      result = "'of' takes object types"

    of rsemSemfoldDivByZero:
      result = "over- or underflow"

    of rsemRuntimeDiscriminantRequiresElif:
      result = "branch initialization with a runtime discriminator only " &
        "supports ordinal types with 2^16 elements or less."

    of rsemRuntimeDiscriminantMustBeImmutable:
      result = "runtime discriminator must be immutable if branch fields are " &
        "initialized, a 'let' binding is required."

    of rsemObjectConstructorIncorrect:
      assert false, "TODO"

    of rsemVmBadExpandToAst:
      result = "expandToAst requires 1 argument"

    of rsemMissingImportcCompleteStruct:
      result = "'$1' requires '.importc' types to be '.completeStruct'" % r.str

    of rsemVmEnableFFIToImportc:
      result = "VM is not allowed to 'importc' without --experimental:compiletimeFFI"

    of rsemVmCannotImportc:
      result = "cannot 'importc' variable at compile time; " & r.symstr

    of rsemVmCannotCreateNullElement:
      result = "cannot create null element for: " & r.typ.render

    of rsemVmNoClosureIterators:
      result = "Closure iterators are not supported by VM!"

    of rsemVmCannotCallMethod:
      result = "cannot call method " & r.symstr & " at compile time"

    of rsemBorrowTargetNotFound:
      result = "no symbol to borrow from found"

    of rsemIncorrectResultProcSymbol:
      result = "incorrect result proc symbol"

    of rsemCannotInferTypeOfParameter:
      result = "cannot infer type of parameter: " & r.symstr

    of rsemRebidingImplicitDestructor:
      result = "cannot bind another '" & r.symstr & "' to: " & r.typ.render
      result.add "; previous declaration was constructed here implicitly: " & (conf $ r.sym.info)

    of rsemRebidingDestructor:
      result = "cannot bind another '" & r.symstr & "' to: " & r.typ.render
      result.add "; previous declaration was here: " & (conf $ r.sym.info)

    of rsemInseparableTypeBoundOp:
      result = "type bound operation `" & r.symstr &
        "` can be defined only in the same module with its type (" & r.typ.render & ")"

    of rsemUnexpectedTypeBoundOpSignature:
      result = "signature for '" & r.symstr & "' must be proc[T: object](x: var T)"

    of rsemRebidingDeepCopy:
      result = "cannot bind another 'deepCopy' to: " & r.typ.render

    of rsemExpectedDestroyOrDeepCopyForOverride:
      result = "'destroy' or 'deepCopy' expected for 'override'"

    of rsemGenericMethodsDeprecated:
      result = "generic methods are deprecated"

    of rsemExpectedObjectForMethod:
      result = "'method' needs a parameter that has an object type"

    of rsemUnexpectedPragmaInDefinitionOf:
      let proto = r.symbols[0]
      let s = r.symbols[1]
      result = "pragmas are only allowed in the header of a proc; redefinition of $1" %
        ("'" & proto.name.s & "' from " & conf $ proto.info &
        " '" & s.name.s & "' from " & conf $ s.info)

    of rsemParallelCannotProveDisjoint:
      result = r.str

    of rsemParallelInvalidControlFlow:
      result = "invalid control flow for 'parallel'"

    of rsemSpawnInvalidContext:
      result = "invalid context for 'spawn'"

    of rsemParallelWithoutSpawn:
      result = "'parallel' section without 'spawn'"

    of rsemDisjointFields:
      result = ("The fields '$1' and '$2' cannot be initialized together, " &
        "because they are from conflicting branches in the case object.") %
        [r.fieldMismatches.first.csvList(), r.fieldMismatches.second.csvList()]

    of rsemUnsafeRuntimeDiscriminantInit:
      result = ("cannot prove that it's safe to initialize $1 with " &
        "the runtime value for the discriminator '$2' ") %
        [r.fieldMismatches.first.csvList(), r.fieldMismatches.second.csvList()]

    of rsemConflictingDiscriminantInit:
      result = ("a case selecting discriminator '$1' with value '$2' " &
        "appears in the object construction, but the field(s) $3 " &
        "are in conflict with this value.") %
        [r.fieldMismatches.first.csvList(), r.ast.render, r.fieldMismatches.second.csvList()]

    of rsemConflictingDiscriminantValues:
      result = ("possible values " &
        "$2 are in conflict with discriminator values for " &
        "selected object branch $1.") % [r.ast.render, r.typ.render]

    of rsemRuntimeDiscriminantInitCap:
      result = "branch initialization with a runtime discriminator only " &
        "supports ordinal types with 2^16 elements or less."

    of rsemBitsizeRequires1248:
      result = "size may only be 1, 2, 4 or 8"

    of rsemAlignRequiresPowerOfTwo:
      result = "power of two expected"

    of rsemNoReturnHasReturn:
      result = "???"

    of rsemUserHint:
      result = r.str

    of rsemUserWarning:
      result = r.str

    of rsemUserError:
      result = r.str

    of rsemCustomUserError:
      result = r.str

    of rsemImplicitPragmaError:
      result = "???"

    of rsemInvalidModulePath:
      result = "invalid path: " & r.str

    of rsemDotForModuleImport:
      result = "using '.' instead of '/' in import paths is deprecated"

    of rsemInvalidModuleName:
      result = "invalid module name: '$1'" % r.ast.render

    of rsemInvalidMethodDeclarationOrder:
      result = "invalid declaration order; cannot attach '" & r.symbols[0].name.s &
        "' to method defined here: " & conf$r.symbols[1].info

    of rsemRecursiveInclude:
      result = "recursive dependency: '$1'" % r.str

    of rsemUnexpectedInfixInInclude:
      result = "Cannot use '" & r.str & "' in 'include'."

    of rsemInvalidPragmaBlock:
      result = "invalid pragma block: " & $r.ast.render

    of rsemConceptInferenceFailed:
      result = "cannot infer the concept parameter '%s', due to a type mismatch. " &
        "attempt to equate '%s' and '%s'." % [
          r.ast.render, r.actualType.render, r.formalType.render]

    of rsemConceptPredicateFailed:
      result = "concept predicate failed"

    of rsemUnreachableCode:
      result = "unreachable code after 'return' statement or '{.noReturn.}' proc"

    of rsemNoMagicEqualsForType:
      result = "can't find magic equals operator for type kind " & $r.typ.kind

    of rsemConflictingExportnims:
      result = "symbol conflicts with other .exportNims symbol at: " &
        conf $ r.symbols[1].info

    of rsemCantConvertLiteralToType:
      result =  "Cannot convert int literal to $1. The value is invalid." %
        r.typ.render

    of rsemNodeNotAllowed:
      result = "'$1' not allowed here" % r.ast.render

    of rsemCustomGlobalError:
      result = r.str

    of rsemCannotImportItself:
      result = "module '$1' cannot import itself" % r.symstr

    of rsemRecursiveImport:
      result = "recursive dependency: '$1'" % r.str

    of rsemCannotOpenFile:
      result = "cannot open '$1'" % r.str

    of rsemMethodRequiresToplevel:
      result = "'method' is only allowed at top level"

    of rsemExpectedReturnTypeForConverter:
      result = "converter needs a return type"

    of rsemExpectedOneArgumentForConverter:
      result = "a converter takes exactly one argument"

    of rsemSemfoldOverflow:
      result = "over- or underflow"

    of rsemCaseInUnion:
      result = "Illegal use of ``case`` in union type."

    of rsemOffsetInUnion:
      result = "union type may not have an object header"

    of rsemUnexpectedInNewConcept:
      result =  "unexpected construct in the new-styled concept: " & r.ast.render

    of rsemTooNestedConcept:
      result = r.ast.render & " too nested for type matching"

    of rsemIllegalRecursion:
      result = "illegal recursion in type '$1'" % r.typ.render

    of rsemCannotInferStaticValue:
      result = "cannot infer the value of the static param '" & (
        if r.sym.isNil: r.str else: r.symstr
      ) & "'"

    of rsemProcIsNotAConcreteType:
      result = ("'$1' is not a concrete type; " &
        "for a callback without parameters use 'proc()'") % r.typ.render

    of rsemCannotInstantiateWithParameter:
      result = "cannot instantiate "
      result.addTypeHeader(conf, r.typ)
      result.add "\ngot: <$1>\nbut expected: <$2>" % [
        describeArgs(conf, r.typ.n), describeArgs(conf, r.ast, 0)]

    of rsemCannotGenerateGenericDestructor:
      result = "cannot generate destructor for generic type: " & r.typ.render

    of rsemExpectedLow0Discriminant:
      result =  "low(" & r.symstr & ") must be 0 for discriminant"

    of rsemExpectedHighCappedDiscriminant:
      result = "len($1) must be less than 32768" % r.symstr

    of rsemCantConvertLiteralToRange:
      result = "cannot convert " & $r.str & " to " & r.typ.render

    of rsemCantComputeOffsetof:
      result = "can't compute offsetof on this ast"

    of rsemExpectObjectForBase:
      result = "cannot inherit from a type that is not an object type"

    of rsemExpectNonFinalForBase:
      result = "inheritance only works with non-final objects; " &
        "for " & r.typ.render & " to be inheritable it must be " &
        "'object of RootObj' instead of 'object'"

    of rsemTVoidNotAllowed:
      result = "type '$1 void' is not allowed" % r.typ.kind.toHumanStr

    of rsemExpectedObjectForRegion:
      result = "region needs to be an object type"

    of rsemPtrRegionIsDeprecated:
      result = "region for pointer types is deprecated"

    of rsemMacroBodyDependsOnGenericTypes:
      result = "the macro body cannot be compiled, " &
        "because the parameter '$1' has a generic type" % r.str

    of rsemUnexpectedVoidType:
      result = "'repr' doesn't support 'void' type"

    of rsemUnexpectedArrayAssignForCstring:
      result = "cstring doesn't support `[]=` operator"

    of rsemMalformedNotNilType:
      result = "Invalid syntax. When used with a type, 'not' can be followed only by 'nil'"

    of rsemEnableNotNilExperimental:
      result = "enable the 'not nil' annotation with {.experimental: \"notnil\".} or " &
        "  the `strict not nil` annotation with {.experimental: \"strictNotNil\".} " &
        "  the \"notnil\" one is going to be deprecated, so please use \"strictNotNil\""

    of rsemEnableDotOperatorsExperimental:
      result =  "the overloaded " & r.symstr &
        " operator has to be enabled with {.experimental: \"dotOperators\".}"

    of rsemEnableCallOperatorExperimental:
      result = "the overloaded " & r.symstr &
        " operator has to be enabled with {.experimental: \"callOperator\".}"

    of rsemExpectedImportedType:
      result = "the '$1' modifier can be used only with imported types" % r.ast.render

    of rsemExpectedDistinctForBorrow:
      result = "only a 'distinct' type can borrow `.`"

    of rsemRedefinitionOf:
      result = "attempt to redefine: '" & r.symstr & "'"

    of rsemDefaultParamIsIncompatible:
      assert false, "REMOVE"

    of rsemExpressionCannotBeCalled:
      result = "expression cannot be called"

    of rsemWrongNumberOfGenericParams:
      result = ""

    of rsemNoGenericParamsAllowed:
      result = "no generic parameters allowed for $1" % r.symstr

    of rsemCallingConventionMismatch:
      assert false, "REMOVE"

    of rsemParallelCounterAfterIncrement:
      result = "invalid usage of counter after increment"

    of rsemUndeclaredIdentifier:
      result = "undeclared identifier: '" & r.str & "'\n"
      result.add presentSpellingCandidates(
        conf, r.spellingCandidates)

    of rsemXDeclaredButNotUsed:
      result = "'$1' is declared but not used " % r.symstr

    else:
      result = $r

    # of rsemExpectedInvariantParam:
    #   result = "non-invariant type param used in a proc type: " & r.typ.render()
    # else:
    #   return $r

proc toStr(conf: ConfigRef, loc: ReportLineInfo): string =
  conf.wrap($loc, fgDefault, {styleBright})

proc toStr(conf: ConfigRef, loc: ReportLinePoint): string =
  conf.wrap($loc, fgDefault, {styleBright})

const repWithPrefix = repAllKinds - {rsemExpandArc}
const repWithSuffix = repWarningKinds + repHintKinds - {rsemExpandArc}

proc prefix(conf: ConfigRef, r: ReportTypes): string =
  let sev = conf.severity(r)
  if r.location.isSome():
    # Optional report location
    result.add conf.toStr(r.location.get()) & " "

  if r.kind in repWithPrefix:
    # `Hint: `, `Error: ` etc.
    result.add conf.wrap(reportTitles[sev], reportColors[sev])

proc suffix(
    conf: ConfigRef,
    r: ReportTypes
  ): string =
  if r.kind in repWithSuffix:
    result.add conf.wrap(" [" & $r.kind & "]", fgCyan)

  if conf.hasHint(rintMsgOrigin):
    result.add(
      "\n",
      conf.toStr(r.reportInst),
      " compiler msg instantiated here ",
      conf.wrap("[MsgOrigin]", fgCyan)
    )


proc report(conf: ConfigRef, r: SemReport): string =
  let sev = conf.severity(r)

  if r.kind == rsemProcessing and conf.hintProcessingDots:
    return "."

  if sev == rsevError:
    result.add conf.getContext(r.context)

  result.add(
    # `file(line, col) Error: ` prefix
    conf.prefix(r),
    # Message body
    toStr(conf, r),
    conf.suffix(r)
  )

proc toStr(conf: ConfigRef, r: ParserReport): string =
  case ParserReportKind(r.kind):
    of rparInvalidIndentation:
       result = r.msg

    of rparNestableRequiresIndentation:
       result = "nestable statement requires indentation"

    of rparIdentExpected:
      result = "identifier expected, but got '$1'"

    of rparIdentOrKwdExpected:
      result = "identifier expected, but got '$1'"

    of rparExprExpected:
      result = "expression expected, but found '$1'"

    of rparMissingToken:
      result = "expected " & r.expected[0]

    of rparUnexpectedToken:
      result = "expected: '" & $r.expected[0] & "', but got: '" & r.found & "'"

    of rparUnexpectedTokenKind:
      result = r.msg

    of rparFuncNotAllowed:
      result = "func keyword is not allowed in type descriptions, use proc with {.noSideEffect.} pragma instead"

    of rparTupleTypeWithPar:
      result = "the syntax for tuple types is 'tuple[...]', not 'tuple(...)'"

    of rparMisplacedParameterVar:
      result = "the syntax is 'parameter: var T', not 'var parameter: T'"

    of rparConceptNotinType:
      result = "the 'concept' keyword is only valid in 'type' sections"

    of rparRotineExpected:
      result = r.msg

    of rparPragmaAlreadyPresent:
      result = "pragma already present"

    of rparMisplacedExport:
      result = "invalid indentation; an export marker '*' follows the declared identifier"

    of rparTemplMissingEndClose:
      result = "?"

    of rparTemplInvalidExpression:
      result = "?"

    of rparInconsistentSpacing:
      result = "Number of spaces around '$#' is not consistent"

    of rparEnablePreviewDotOps:
      result = "?"

    of rparPragmaNotFollowingTypeName:
      result = "?"

    of rparPragmaBeforeGenericParameters:
      result = "?"

    of rparName:
      result = "?"

    of rparInvalidFilter:
      result = "?"



proc report(conf: ConfigRef, r: ParserReport): string =
  result = conf.prefix(r) & conf.toStr(r) & conf.suffix(r)

proc report(conf: ConfigRef, r: InternalReport): string =
  case r.kind:
    of rintStackTrace:
      for entry in r.trace:
        result.add(entry.filename, "(", entry.line, ") ", entry.procname, "\n")

    of rintSuccessX:
      var build = ""
      let par = r.buildParams
      if conf.cmd in cmdBackends:
        build.add "gc: $#; " % par.gc

        if par.threads:
          build.add "threads: on; "

        build.add "opt: "
        if par.optimize == "debug":
          build.add "none (DEBUG BUILD, `-d:release` generates faster code)"

        else:
          build.add par.optimize
          build.add "; "
          build.add par.buildMode

      let mem =
        if par.isMaxMem:
          formatSize(par.mem) & " peakmem"

        else:
          formatSize(par.mem) & " totmem"

      result = &"""
{conf.prefix(r)}{build}
{par.linesCompiled} lines; {par.sec:.3f}s; {mem}; proj: {par.project}; out: {par.output}"
"""

      result.add conf.suffix(r)

    of rintUsingLeanCompiler:
      result = r.msg

    of rintMissingStackTrace:
      result = """
No stack traceback available
To create a stacktrace, rerun compilation with './koch temp $1 <file>'
      """

    of rintAssert:
      result.add(
        conf.prefix(r),
        "Internal assert '",
        r.expression,
        "' failed in ",
        conf.toStr(r.reportInst)
      )

    of rintUnreachable:
      result.add(
        conf.prefix(r),
        "Internal unreachable code executed - ",
        conf.toStr(r.reportInst),
        "(", r.msg, ") should never be called."
      )

    else:
      result = $r

proc report(conf: ConfigRef, r: LexerReport): string    =
  result.add prefix(conf, r)
  case LexerReportKind(r.kind):
    of rlexMalformedTrailingUnderscre:
      result.add "invalid token: trailing underscore"

    of rlexMalformedUnderscores:
      result.add "only single underscores may occur in a token and token may not " &
        "end with an underscore: e.g. '1__1' and '1_' are invalid"

    of rlexInvalidToken:
      result.add r.msg

    of rlexNoTabs:
      result.add "tabs are not allowed, use spaces instead"

    of rlexInvalidIntegerPrefix:
      result.add r.msg

    of rlexInvalidIntegerSuffix:
      result.add r.msg

    of rlexNumberNotInRange:
      result.add r.msg

    of rlexExpectedHex:
      result.add r.msg

    of rlexInvalidIntegerLiteral:
      result.add r.msg

    of rlexInvalidCharLiteral:
      result.add r.msg

    of rlexMissingClosingApostrophe:
      result.add "missing closing ' for character literal"

    of rlexInvalidUnicodeCodepoint:
      result.add r.msg

    of rlexUnclosedTripleString:
      result.add "closing \"\"\" expected, but end of file reached"

    of rlexUnclosedSingleString:
      result.add "closing \" expected"

    of rlexExpectedToken:
      assert false

    of rlexCfgInvalidDirective:
      result.add "?"

    of rlexUnclosedComment:
      result.add "end of multiline comment expected"

    of rlexDeprecatedOctalPrefix:
      result.add r.msg

    of rlexLinterReport:
      result.add "?"

    of rlexLineTooLong:
      result.add "line too long"

    of rlexSyntaxesCode:
      result.add "?"



  result.add suffix(conf, r)


proc report(conf: ConfigRef, r: ExternalReport): string =
  case r.kind:
    of rextConf:
      result.add(
        conf.prefix(r),
        "used config file '$1'" % r.msg,
        conf.suffix(r)
      )

    of rextInvalidHint:
      result.add("Invalid hint - ", r.cmdlineProvided)

    of rextInvalidWarning:
      result.add("Invalid warning - ", r.cmdlineProvided)

    else:
      result = $r

proc report(conf: ConfigRef, r: DebugReport): string    = $r
proc report(conf: ConfigRef, r: BackendReport): string  =
  result = $r

proc report(conf: ConfigRef, r: CmdReport): string =
  case r.kind:
    of rcmdCompiling:
      result = "CC: " & r.msg

    of rcmdLinking:
      result = conf.prefix(r) & conf.suffix(r)

    else:
      result = $r

proc toStr*(conf: ConfigRef, r: Report): string =
  case r.category:
    of repLexer:    result = conf.report(r.lexReport)
    of repParser:   result = conf.report(r.parserReport)
    of repCmd:      result = conf.report(r.cmdReport)
    of repSem:      result = conf.report(r.semReport)
    of repDebug:    result = conf.report(r.debugReport)
    of repInternal: result = conf.report(r.internalReport)
    of repBackend:  result = conf.report(r.backendReport)
    of repExternal: result = conf.report(r.externalReport)

proc reportHook*(conf: ConfigRef, r: Report) =
  var lastDot {.global.}: bool

  if not conf.isEnabled(r):
    return

  elif r.kind == rsemProcessing and conf.hintProcessingDots:
    conf.write(".")
    lastDot = true

  else:
    if lastDot:
      conf.write("\n")
    lastDot = false
    conf.writeln(conf.toStr(r))
