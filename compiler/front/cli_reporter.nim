## This module provides default implementation of message reporting hook.
## Default compiler message formatting is done here.
##
## Implementation is split into several layers
## 1. `reportHook` - does the necessary message filtering
## 2. `reportFull` - convert given report to string. Main `Report` overload
##    dispatches into per-cateogory ones. Full report adds required prefix
##    and suffix (location information, severity, message kind for hints)
## 3. `reportBody` - generate main report body. Most of the error formatting
##    is done in this proc.

import
  std/[
    strutils,
    terminal,
    options,
    algorithm,
    sequtils,
    strformat,
    tables,
    intsets,
    json,
    strtabs,
    os
  ],
  experimental/[
    colortext
  ],
  compiler/ast/[
    lineinfos,
    astalgo,
    astmsgs,
    renderer,
    types,
    typesrenderer,
    ast,
    reports,
    reports_cmd,
    reports_lexer,
    reports_parser,
    reports_sem,
    reports_vm,
    reports_backend,
    reports_debug,
    reports_internal,
    reports_external,
  ],
  compiler/utils/[
    nversion,
    astrepr,
    idioms
  ],
  compiler/front/[
    msgs
  ],
  compiler/sem/[
    nilcheck_enums
  ],
  compiler/vm/[
    vm_enums
  ]

import compiler/front/options as compiler_options
from compiler/ast/reports_base_sem import ReportContext, ReportContextKind
from compiler/ast/ast_query import getStr

func assertKind(r: ReportTypes | Report) = assert r.kind != repNone

func add(target: var string, other: varargs[string, `$`]) =
  for item in other:
    target.add item

func wrap(
    str: string,
    color: ForegroundColor,
    style: set[Style] = {}
  ): string =
  ## Wrap text with ANSI color formatting codes

  if str.len == 0: # don't bother with empty strings
    return str

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
  ## Optionally wrap text in ansi color formatting, if `conf` has coloring
  ## enabled
  if conf.useColor:
    wrap(str, color, style)
  else:
    str

func wrap(conf: ConfigRef, text: ColText): string =
  toString(text, conf.useColor())

template tern(predicate: bool, tBranch: untyped, fBranch: untyped): untyped =
  ## Shorthand for inline if/else. Allows use of conditions in strformat,
  ## simplifies use in expressions. Less picky with formatting
  {.line: instantiationInfo(fullPaths = true).}:
    block:
      if predicate: tBranch else: fBranch

proc formatTrace*(conf: ConfigRef, trace: seq[StackTraceEntry]): string =
  ## Format stack trace entries for reporting
  var paths: seq[string]
  var width = 0
  for entry in trace:
    paths.add "$1($2)" % [
      formatPath(conf, $entry.filename), $entry.line]

    width = max(paths[^1].len, width)

  for idx, entry in trace:
    result.add(
      alignLeft(paths[idx], width + 1),
      entry.procname,
      tern(idx < trace.high, "\n", "")
    )

func dropExt(path: string, doDrop: bool): string =
  ## Optionally drop `.nim` file extension
  if doDrop and path.endsWith(".nim"): path[0 .. ^5] else: path

proc toStr(conf: ConfigRef, loc: TLineInfo, dropExt: bool = false): string =
  ## Convert location to printable string
  conf.wrap(
    "$1($2, $3)" % [
      conf.toMsgFilename(loc.fileIndex).dropExt(dropExt),
      $loc.line,
      $(loc.col + ColOffset)
    ],
    fgDefault,
    {styleBright})

proc toStr(conf: ConfigRef, loc: ReportLineInfo, dropExt: bool = false): string =
  ## Convert location to printable string
  conf.wrap(
    "$1($2, $3)" % [
      conf.formatPath(loc.file).dropExt(dropExt),
      $loc.line,
      $(loc.col + ColOffset)
    ],
    fgDefault,
    {styleBright})

const
  reportTitles: array[ReportSeverity, string] = [
    "Debug: ", "Hint: ", "Warning: ", "Error: ", "Fatal: ", "Trace: "
  ]

  reportColors: array[ReportSeverity, ForegroundColor] = [
    fgCyan, fgGreen, fgYellow, fgRed, fgRed, fgCyan
  ]

proc csvList(syms: seq[PSym]): string =
  ## Join symbol names in the `", "` list
  syms.mapIt(it.name.s).join(", ")

template csvListIt(syms: seq[PNode], expr: untyped): string =
  ## Join items in comma-separated list
  var res: string
  var idx: int = 0
  for it {.inject.} in items(syms):
    if 0 < idx: res.add ", "
    inc idx
    res.add expr

  res

proc getContext(conf: ConfigRef, ctx: seq[ReportContext]): string =
  ## Get active instantiation context from the configuration
  for ctx in items(ctx):
    result.add(conf.toStr(ctx.location))
    case ctx.kind:
      of sckInstantiationOf:
        result.add(
          " template/generic instantiation of `",
          ctx.entry.name.s,
          "` from here\n")

      of sckInstantiationFrom:
        result.add(" template/generic instantiation from here\n")

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
  ## Add pragma and calling convention mismatch for the `formal` (aka
  ## expected) and `actual` (aka provided) types.
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
  ## Add effect difference annotation for `f` (aka formal/expected) and `a`
  ## (aka actual/provided) types
  if f.kind == tyProc and a.kind == tyProc:
    if tfThread in f.flags and tfThread notin a.flags:
      result.add "\n  This expression is not GC-safe. Annotate the " &
          "proc with {.gcsafe.} to get extended error information."
    elif tfNoSideEffect in f.flags and tfNoSideEffect notin a.flags:
      result.add "\n  This expression can have side effects. Annotate the " &
          "proc with {.noSideEffect.} to get extended error information."
    else:
      case compatibleEffects(f, a):
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

proc argTypeToString(arg: PNode; prefer: TPreferedDesc): string =
  ## Convert argument node type to string
  if arg.kind in nkSymChoices:
    result = typeToString(arg[0].typ, prefer)
    for i in 1 ..< arg.len:
      result.add(" | ")
      result.add typeToString(arg[i].typ, prefer)

  elif arg.typ == nil:
    result = "void"

  else:
    result = arg.typ.typeToString(prefer)

proc describeArgs(conf: ConfigRef, args: seq[PNode]; prefer = preferName): string =
  ## Generate comma-separated list of arguments
  for idx, arg in args:
    if arg.kind == nkExprEqExpr:
      result.add renderTree(arg[0])
      result.add ": "
      if arg.typ.isNil and arg.kind notin {nkStmtList, nkDo}:
        assert false, (
          "call `semcall.maybeResemArgs` on report construciton site - " &
            "this is a temporary hack that is necessary to actually provide " &
            "proper types for error reports.")

    else:
      if arg.typ.isNil and arg.kind notin {
           nkStmtList, nkDo, nkElse, nkOfBranch, nkElifBranch, nkExceptBranch
      }:
        assert false, "call `semcall.maybeResemArgs` on report construction site"

    if arg.typ != nil and arg.typ.kind == tyError:
      return

    result.add argTypeToString(arg, prefer)
    if idx != args.len - 1:
      result.add ", "

proc describeArgs(
    conf: ConfigRef, n: PNode, startIdx = 1;
    prefer = preferName
  ): string =
  ## Geenrate comma-separated list of arguments, overload for sequence
  describeArgs(conf, toSeq(n.sons[startIdx .. ^1]), prefer)

proc renderAsType*(vals: IntSet, t: PType): string =
  ## Render integer values as type. For integer no representation
  ## conversion is done, for char convert to characters, for enums use enum
  ## symbols.
  result = "{"
  let t = t.skipTypes(abstractRange)
  var enumSymOffset = 0
  var i = 0
  for val in vals:
    if result.len > 1:
      result &= ", "
    case t.kind:
    of tyEnum, tyBool:
      while t.n[enumSymOffset].sym.position < val: inc(enumSymOffset)
      result &= t.n[enumSymOffset].sym.name.s
    of tyChar:
      result.addQuoted(char(val))
    else:
      if i == 64:
        result &= "omitted $1 values..." % $(vals.len - i)
        break
      else:
        result &= $val
    inc(i)
  result &= "}"

proc getProcHeader(
    conf: ConfigRef; sym: PSym; prefer: TPreferedDesc = preferName; getDeclarationPath = true): string =
  ## Formats procs and types
  ## Returns for procs `owner.name(argument: signature): return`
  ## Returns for types `owner.name`
  assert sym != nil
  # consider using `skipGenericOwner` to avoid fun2.fun2 when fun2 is generic
  result = sym.owner.name.s & '.' & sym.name.s
  if sym.kind in routineKinds:
    result.add '('
    var n = sym.typ.n
    for i in 1..<n.len:
      let p = n[i]
      if p.kind == nkSym:
        result.add(p.sym.name.s)
        result.add(": ")
        result.add(typeToString(p.sym.typ, prefer))
        if i != n.len-1: result.add(", ")
      else:
        result.add renderTree(p)
    result.add(')')
    if n[0].typ != nil:
      result.add(": " & typeToString(n[0].typ, prefer))
  if getDeclarationPath: result.addDeclaredLoc(conf, sym)

proc getSymRepr*(conf: ConfigRef; s: PSym, getDeclarationPath = true): string =
  case s.kind:
    of routineKinds, skType:
      result = getProcHeader(conf, s, getDeclarationPath = getDeclarationPath)
    else:
      result = "'$1'" % s.name.s
      if getDeclarationPath:
        result.addDeclaredLoc(conf, s)

proc addTypeDeclVerboseMaybe(result: var string, conf: ConfigRef; typ: PType) =
  ## Calls `typeToString` on given `typ`.
  ## If `--declaredlocs` is specified, it will also add the declared location.
  #
  # REFACTOR `addTypeDeclVerboseMaybe` can be implemented as a wrapper
  # around addTypeHeader, but simple change causes <test name> to fail with
  # 'got `seq[MyInt2{char}]` but expected `seq[MyInt2]`' - formatting
  # becomes buggy, so leaving as it is until all implicit code interactions
  # are figured out. commit:
  # https://github.com/nim-works/nimskull/pull/179/commits/55808ea55e90f01a3617d11d33a1699f3e7f2f12
  # failing test: https://github.com/nim-works/nimskull/runs/4951209502
  if optDeclaredLocs in conf.globalOptions:
    result.add typeToString(typ, preferMixed)
    result.addDeclaredLoc(conf, typ)
  else:
    result.add typeToString(typ)

proc presentFailedCandidates(
    conf: ConfigRef,
    n: PNode,
    errors: seq[SemCallMismatch]
  ): (TPreferedDesc, string)

proc presentSpellingCandidates*(
  conf: ConfigRef, candidates: seq[SemSpellCandidate]): string =
  ## Format potential spelling candidates

  result = "candidates (edit distance, scope distance); see '--spellSuggest':"
  for candidate in candidates:
    result.add "\n ($1, $2): '$3'" % [
      $candidate.dist,
      $candidate.depth,
      $candidate.sym.name.s
    ]

    result.addDeclaredLoc(conf, candidate.sym)

proc presentDiagnostics(
  conf: ConfigRef, d: SemDiagnostics, startWithNewLine: bool): string

const defaultRenderFlags: set[TRenderFlag] = {
    renderNoComments,
    renderWithoutErrorPrefix
  }


proc reportBody*(conf: ConfigRef, r: SemReport): string =
  proc render(n: PNode, rf = defaultRenderFlags): string = renderTree(n, rf)
  proc render(t: PType): string = typeToString(t)

  case SemReportKind(r.kind):
    of rsemTypelessParam:
      result = "typeless param"

    of rsemOwnedTypeDeprecated:
      result = "the `owned` type-operator is deprecated and treated as a no-op"

    of rsemLinterReport:
      result.addf("'$1' should be: '$2'", r.linterFail.got, r.linterFail.wanted)

    of rsemLinterReportUse:
      result.addf("'$1' should be: '$2'", r.linterFail.got, r.linterFail.wanted)
      result.addDeclaredLoc(conf, r.sym)

    of rsemWrappedError:
      assert false, (
        "Cannot report wrapped sem error - use `walkErrors` in " &
          "order to write out all accumulated reports")

    of rsemCannotConvertTypes:
      result = "cannot convert $1 to $2" % [
        r.actualType.render, r.formalType.render]

    of rsemProveField:
      result = "cannot prove that field '$1' is accessible" % r.ast.render

    of rsemUninit:
      result = "use explicit initialization of '$1' for clarity" % r.symstr

    of rsemDuplicateCaseLabel:
      result = "duplicate case label"

    of rsemIllegalMemoryCapture:
      let s = r.sym
      result = (
        "'$1' is of type <$2> which cannot be captured as it would violate memory" &
          " safety, declared here: $3; using '-d:nimNoLentIterators' helps in some cases"
      ) % [s.name.s, typeToString(s.typ), conf $ s.info]
    of rsemIllegalCompTimeCapture:
      result = "'$1' is inaccesible because run-time values cannot be" &
               " captured in a compile-time context"
      result = result % [r.sym.name.s]
    of rsemUnavailableTypeBound:
      result.add(
        "'",
        r.str,
        "' is not available for type <",
        r.typ.render,
        ">"
      )

      if r.str in ["=", "=copy"]:
        result.add(
          "; requires a copy because it's not the last read of '",
          r.ast.render,
          "'"
        )

        if r.missingTypeBoundElaboration.anotherRead.isSome():
          result.add(
            "; another read is done here: ",
            conf.toStr(r.missingTypeBoundElaboration.anotherRead.get()))

        elif r.missingTypeBoundElaboration.tryMakeSinkParam:
          result.add("; try to make", r.ast.render, "a 'sink' parameter")

      result.add("; routine: ", r.symstr)

    of rsemIllegalCallconvCapture:
      let s = r.symbols[0]
      let owner = r.symbols[1]
      result = "illegal capture '$1' because '$2' has the calling convention: <$3>" % [
        s.name.s, owner.name.s, $owner.typ.callConv]

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

    of rsemCallIndirectTypeMismatch:
      result.addf(
        "type mismatch: got <$1>\nbut expected one of:\n$2",
        conf.describeArgs(r.ast, 1),
        r.typ.render)

      if r.typ.sym != nil and
         sfAnon notin r.typ.sym.flags and
         r.typ.kind == tyProc:
        result.add(" = ", typeToString(r.typ, preferDesc))

    of rsemExpandArc:
      result.add(
        "--expandArc: ",
        r.symstr,
        "\n",
        r.expandedAst.renderTree({renderIr, renderNoComments}),
        "\n",
        "-- end of expandArc ------------------------"
      )

    of rsemCannotBorrow:
      result.add(
        "cannot borrow ",
        r.symstr,
        "; what it borrows from is potentially mutated"
      )

      if r.borrowPair.mutatedHere.isKnown():
        result.add("\n", conf.toStr(r.borrowPair.mutatedHere), " the mutation is here")

      if r.borrowPair.connectedVia.isKnown():
        result.add(
          "\n",
          conf.toStr(r.borrowPair.connectedVia),
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
      if r.symbols.len == 2:
        # symbols and it's alternative
        result.add(
          "use ",
          r.symbols[1].name.s,
          " instead; ",
          r.symbols[0].name.s,
          " is deprecated"
        )
      else:
        result = r.str
        if not r.sym.isNil:
          let s = r.sym
          if 0 < r.str.len:
            result.add("; ")

          # Depreaction was added for a whole enum, not a specific field
          if s.kind == skEnumField and sfDeprecated notin s.flags:
            result.addf(
              "enum '$1' which contains field '$2' is deprecated",
              s.owner.name.s,
              s.name.s,
            )
          elif s.kind == skModule and not s.constraint.isNil():
            result.addf("$1; $2 is deprecated", s.constraint.strVal, s.name.s)
          else:
            result.add(s.name.s, " is deprecated")

    of rsemThisPragmaRequires01Args:
      # FIXME remove this report kind, reuse "wrong number of arguments"
      result = "'this' pragma is allowed to have zero or one arguments"

    of rsemSymbolKindMismatch:
      var ask: string
      if len(r.expectedSymbolKind) == 1:
         for n in r.expectedSymbolKind:
           ask = n.toHumanStr
      else:
        ask = $r.expectedSymbolKind

      result = "cannot use symbol of kind '$1' as a '$2'" %
        [$r.sym.kind.toHumanStr, ask]

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
      result = "expanded macro:\n" & r.expandedAst.render()

    of rsemUnusedImport:
      result = "imported and not used: '$1'" % r.sym.name.s

    of rsemCallNotAProcOrField:
      for sym in r.unexpectedCandidate:
        result.addf("\n  found $1", getSymRepr(conf, sym))

      result =
        if r.explicitCall:
          if result.len == 0:
            "attempting to call undeclared routine: '$1'" % $r.notProcOrField
          else:
            "attempting to call routine: '$1'$2" %
              [$r.notProcOrField, $result]
        else:
          let
            sym = r.typ.typSym
            suffix = if result.len > 0: " " & result else: ""
            typeHint =
              if sym == nil:
                # Perhaps we're in a `compiles(foo.bar)` expression, or
                # in a concept, e.g.:
                #   ExplainedConcept {.explain.} = concept x
                #     x.foo is int
                ""
              else:
                " for type " & getProcHeader(conf, sym)

          "undeclared field: '$1$2$3'" % [$r.notProcOrField, typeHint, suffix]

    of rsemUndeclaredField:
      result =  "undeclared field: '$1' for type $2" % [
        $r.ast.ident.s, $getProcHeader(conf, r.sym)]

    of rsemCannotCodegenCompiletimeProc:
      result = "request to generate code for .compileTime proc: " & r.symstr

    of rsemFieldAssignmentInvalid:
      result = "Invalid field assignment '$1'" % r.ast.render
    
    of rsemFieldAssignmentInvalidNeedSpace:
      result = "Invalid field assignment '$1'; use a space after the colon" %
                  r.ast.render

    of rsemAmbiguous:
      var args = "("
      for i in 1 ..< r.ast.len:
        if i > 1:
          args.add(", ")
        args.add(typeToString(r.ast[i].typ))
      args.add(")")

      result = "ambiguous call; both $1 and $2 match for: $3" % [
        getProcHeader(conf, r.symbols[0]),
        getProcHeader(conf, r.symbols[1]),
        args
      ]

    of rsemCopiesToSink:
      result = (
        "passing '$1' to a sink parameter introduces an implicit copy; " &
          "if possible, rearrange your program's control flow to prevent it") % [
          r.ast.render]

    of rsemAmbiguousIdent, rsemAmbiguousIdentWithCandidates:
      let (sym, candidates) =
        case SemReportKind(r.kind)
        of rsemAmbiguousIdent:
          (r.ast[0].sym, r.ast.sons.map(proc(child: PNode): PSym = child.sym))
        of rsemAmbiguousIdentWithCandidates:
          (r.sym, r.symbols)
        else:
          unreachable()

      result = "ambiguous identifier: '$1' -- use one of the following:\n" %
                sym.name.s

      for i, child in candidates.pairs:
        result.add "$1  $2.$3: $4" % [if i > 0: "\n" else: "",
                                      child.owner.name.s,
                                      child.name.s,
                                      child.typ.render()]

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
      result = "illformed AST: " & render(r.ast)

    of rsemTypeExpected:
      if r.sym.isNil:
        result = "type expected, but expression has no type"
      elif r.sym.typ.isNil:
        result = "type expected, but symbol '$1' has no type." % r.symstr
      else:
        result = "type expected, but got symbol '$1' of kind '$2'" %
          [r.sym.name.s, r.sym.kind.toHumanStr]

    of rsemCyclicDependency:
      result = "recursive dependency: '$1'" % r.symstr

    of rsemCannotInstantiate:
      if r.typ.isNil:
        if r.sym.isNil:
          result = "cannot instantiate: '$1'" % r.ast.render
        else:
          result = "cannot instantiate: '$1'" % r.symstr
      elif r.ownerSym.isNil:
        result.addf(
          "cannot instantiate: '$1'; Maybe generic arguments are missing?",
          typeToString(r.typ, preferDesc)
        )
      else:
        result.addf(
          "cannot instantiate '$1' inside of type definition: '$2'; " &
            "Maybe generic arguments are missing?",
          typeToString(r.typ, preferDesc),
          r.ownerSym.name.s
        )

    of rsemTypeKindMismatch:
      result = r.str
      result.add  " got '$1'" % typeToString(r.actualType)
      result.add " but expected "
      var first = true
      for typKind in r.formalTypeKind:
        if not first:
          result.add " or "
        result.add "'$1'" % typeToStr[typKind]
        first = false

    of rsemExprHasNoAddress:
      result = "expression has no address"
      if r.isUnsafeAddr:
        result.add "; maybe use 'unsafeAddr'"

    of rsemIntLiteralExpected:
      result = "integer literal expected"

    of rsemGenericTypeExpected:
      result = "expected generic type, got: type $2 of kind $1" % [
        r.actualType.kind.toHumanStr,
        typeToString(r.actualType)]

    of rsemUnknownTrait:
      result = "unknown trait: " & r.sym.name.s

    of rsemExpectedOrdinal:
      if r.ast != nil and r.ast.kind == nkExprColonExpr:
        result.add "expected ordinal value for array index, got '$1'" % r.wrongNode.render
      else:
        result = "ordinal type expected"

    of rsemStringLiteralExpected:
      result = "string literal expected"

    of rsemConditionAlwaysTrue:
      result = "condition is always true: '$1'" % render(r.ast)

    of rsemConditionAlwaysFalse:
      result = "condition is always false: '$1'" % render(r.ast)

    of rsemWrongNumberOfArguments:
      result = "wrong number of arguments"
    
    of rsemIsOperatorTakes2Args:
      result = "'is' operator takes 2 arguments"

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

    of rsemUnsafeSetLen:
      result = "setLen can potentially expand the sequence, " &
        "but the element type '$1' doesn't have a valid default value" %
        typeToString(r.typ)

    of rsemUnsafeDefault:
      result = "The '$1' type doesn't have a valid default value" %
        typeToString(r.typ)

    of rsemCannotIsolate:
      result = "expression cannot be isolated: " & render(r.ast)

    of rsemUnknownExperimental:
      result = "unknown experimental feature"

    of rsemWrongIdent:
      result = joinAnyOf(r.expectedIdents, quote = true) & " expected"

    of rsemPragmaDisallowedForTupleUnpacking:
      result = "pragmas are disallowed during tuple unpacking assignment, " &
        "this is a know design issue; as a work around split pragmas and " &
        "unpacking into two steps."

    of rsemIllegalCompileTime:
      result = "the '.compileTime' pragma must only be used with locals inside" &
               " compile-time-only procedures or with globals."

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

    of rsemInvalidPragma:
      result = "invalid pragma: " & r.ast.render

    of rsemMisplacedEffectsOf:
      result = "parameter cannot be declared as .effectsOf"

    of rsemMissingPragmaArg:
      result = "parameter name expected"

    of rsemCannotPushCast:
      result = "a 'cast' pragma cannot be pushed"

    of rsemCastRequiresStatement:
      result = "'cast' pragma only allowed in statement context"

    of rsemImplicitObjConv:
      result = "Implicit conversion: Receiver '$1' will not receive fields of sub-type '$2'" % [
        typeToString(r.formalType),
        typeToString(r.actualType)
      ]

    of rsemExtendedContext:
      assert false, "This is a configuration hint"

    of rsemNonMatchingCandidates:
      let (_, candidates) = presentFailedCandidates(conf, r.ast, r.callMismatches)
      result = "Non-matching candidates for " & render(r.ast) & "\n" &
              candidates

    of rsemEffectsListingHint:
      for tag in r.effectListing.exceptions:
        result.add typeToString(tag)
        result.add "\n"

      for tag in r.effectListing.tags:
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
      if r.sym != nil and r.sym.kind == skEnumField:
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
      result = "invalid order in enum '$1'" % $r.symstr

    of rsemSetTooBig:
      result = "set is too large"

    of rsemTIsNotAConcreteType:
      result = "'$1' is not a concrete type" % r.typ.render()

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
      result = "The $1 type requires the following fields to be initialized: $2." % [
        r.typ.render, r.symbols.csvList()]

    of rsemObjectRequiresFieldInitNoDefault:
      result = ("The $1 type doesn't have a default value. The following fields must " &
        "be initialized: $2.") % [r.typ.render, r.symbols.csvList()]

    of rsemExpectedObjectType:
      result = "object constructor needs an object type"

    of rsemAmbiguousCall:
      result = "overloaded '$1' leads to ambiguous calls" % r.symstr

    of rsemDeclarationVisibilityMismatch:
      result = (
        "public implementation '$1' has non-public forward declaration at $2"
      ) % [getProcHeader(conf, r.sym, getDeclarationPath = false), conf $ r.sym.info]

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

    of rsemDiscardingProc:
      result = "illegal discard"

    of rsemUseOrDiscardExpr:
      let n = r.wrongNode

      result.add(
        "expression '",
        n.render,
        "' is of type '",
        n.typ.skipTypes({tyVar}).render,
        "' and has to be used (or discarded)"
      )

      if r.ast.info.line != n.info.line or
         r.ast.info.fileIndex != n.info.fileIndex:

        result.add "; start of expression here: " & conf$r.ast.info

      if r.ast.typ.kind == tyProc:
        result.add "; for a function call use ()"

    of rsemHasSideEffects:
      if r.sideEffectTrace[0].trace == ssefParameterMutation:
        let part = r.sideEffectTrace[0]
        result.addf(
          "'$1' can have side effects.\nan object reachable " &
            "from '$2' is potentially mutated",
          part.isUnsafe.name.s,
          part.unsafeVia.name.s
        )

        if part.location != unknownLineInfo:
          result.addf("\n$1 the mutation is here", conf.toStr(part.location))

        if r.sideEffectMutateConnection != unknownLineInfo:
          result.addf(
            "\n$1 is the statement that connected the mutation to the parameter",
            conf.toStr(r.sideEffectMutateConnection))

      else:
        result = "'$1' can have side effects\n" % r.symstr
        template addHint(
            msg: string, lineInfo: TLineInfo, sym: string, level: int) =
          result.addf(
            "$# $# $#'$#' $#\n",
            repeat(">", level),
            conf.toStr(lineInfo),
            conf.wrap(reportTitles[rsevHint], reportColors[rsevHint]),
            sym,
            msg
          )

        for part in r.sideEffectTrace:
          let s = part.isUnsafe
          let u = part.unsafeVia
          let useLineInfo = part.location

          case part.trace:
            of ssefUsesGlobalState:
              addHint(
                "accesses global state '$#'" % u.name.s,
                useLineInfo, s.name.s, part.level)

              addHint(
                "accessed by '$#'" % s.name.s, u.info,
                u.name.s, part.level + 1)

            of ssefCallsSideEffect:
              addHint(
                "calls `.sideEffect` '$#'" % u.name.s,
                useLineInfo, s.name.s, part.level)

              addHint(
                "called by '$#'" % s.name.s,
                u.info, u.name.s, part.level + 1)

            of ssefCallsViaHiddenIndirection:
              addHint(
                "calls routine via hidden pointer indirection",
                useLineInfo, s.name.s, part.level)

            of ssefCallsViaIndirection:
              addHint(
                "calls routine via pointer indirection",
                useLineInfo, s.name.s, part.level)

            of ssefParameterMutation:
              assert false, "Must be handled as a standalone effect"

      if result[^1] == '\n':
        result.setLen(result.high)

    of rsemCannotBeRaised:
      result = "only a 'ref object' can be raised"

    of rsemXCannotRaiseY:
      result = "'$1' cannot raise '$2'" % [r.ast.render, r.raisesList.render]

    of rsemUnlistedRaises, rsemWarnUnlistedRaises:
      result.add("$1 can raise an unlisted exception: " % r.ast.render,
                 r.typ.render)

    of rsemUnlistedEffects:
      result.add(r.ast.render, "can have an unlisted effect: ", r.typ.render)

    of rsemWarnGcUnsafe:
      result = "not GC-safe: '$1'" % r.ast.render

    of rsemExceptionAlreadyHandled:
      result = "exception already handled"

    of rsemCannotExceptNativeAndImported:
      result = "Mix of imported and native exception types is not allowed in one except branch"

    of rsemExpectedSingleFinally:
      result = "Only one finally is allowed after all other branches"

    of rsemExpectedSingleGeneralExcept:
      result = "Only one general except clause is allowed after more specific exceptions"

    of rsemCannotConvertToRange:
      result = "cannot convert '$1' to '$2'" % [$r.str, typeToString(r.typ)]

    of rsemProveInit:
      result = "Cannot prove that '$1' is initialized. This will become a compile time error in the future." %
        (if r.sym != nil: r.symstr else: r.ast.render())
        # presently this can be either a sym or an ast node

    of rsemUsingRequiresType:
      result = "'using' section must have a type"

    of rsemUsingDisallowsAssign:
      result = "'using' sections cannot contain assignments"

    of rsemDifferentTypeForReintroducedSymbol:
      result = "inconsistent typing for reintroduced symbol '" &
        r.symstr & "': previous type was: " & r.formalType.render() &
        "; new type is: " & r.actualType.render()

    of rsemMisplacedRunnableExample:
      result = "runnableExamples must appear before the first non-comment statement"

    of rsemCannotInferTypeOfLiteral:
      result = "cannot infer the type of the $1" % r.typ.kind.toHumanStr

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
      if 0 < r.nodes.len:
        result.add "; missing: {$1}" % r.nodes.csvListIt(it.render)

    of rsemCannotRaiseNonException:
      result = "raised object of type $1 does not inherit from Exception" & r.typ.render

    of rsemUnexpectedEqInObjectConstructor:
      result = "object construction uses ':', not '='"

    of rsemConvFromXtoItselfNotNeeded:
      result = "conversion from $1 to itself is pointless" % r.typ.render
    of rsemClosureWithoutEnv:
      result = "procedure explicitly uses the 'closure' calling convention," &
               " but it doesn't capture anything"
    of rsemIllegalConversion:
      result = "illegal conversion from '$1' to '$2'" % [
        r.actualType.render, r.formalType.render
      ]

    of rsemCannotBeConvertedTo:
      let value =
        case r.ast.kind
        of nkIntLiterals:   $r.ast.getInt
        of nkFloatLiterals: $r.ast.getFloat
        else:               unreachable()
      result = "$1 can't be converted to $2" % [value, r.typ.render]

    of rsemCannotCastToNonConcrete:
      result = "cannot cast to a non concrete type: '$1'" % r.typ.render

    of rsemCannotCastTypes:
      let
        tar = $r.formalType
        alt = typeToString(r.formalType, preferDesc)
        msg = if tar != alt: tar & "=" & alt else: tar
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

    of rsemYieldExpectedTupleConstr:
      # TODO: make the error message more helpful by also showing which
      #       elements are of view type
      result = "literal tuple constructor expected for 'yield', as the " &
               "iterator returns a tuple directly containing a view. Got: " &
               r.ast.render

    of rsemCannotReturnTypeless:
      result = "current routine cannot return an expression"

    of rsemExpectedValueForYield:
      result = "yield statement must yield a value"

    of rsemModuleAliasMustBeIdentifier:
      result = "module alias must be an identifier, got: " & r.ast.render

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

    of rsemRuntimeDiscriminantRequiresElif:
      result = "branch initialization with a runtime discriminator " &
        "is not supported inside of an `elif` branch."

    of rsemRuntimeDiscriminantMustBeImmutable:
      result = "runtime discriminator must be immutable if branch fields are " &
        "initialized, a 'let' binding is required."

    of rsemObjectConstructorIncorrect:
      result = "Invalid object constructor: '$1'" % r.ast.render

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

    of rsemDisjointFields:
      result = ("The fields '$1' and '$2' cannot be initialized together, " &
        "because they are from conflicting branches in the case object.") %
        [r.fieldMismatches.first.csvList(), r.fieldMismatches.second.csvList()]

    of rsemUnsafeRuntimeDiscriminantInit:
      result = ("cannot prove that it's safe to initialize '$1' with " &
        "the runtime value for the discriminator '$2' ") %
        [r.fieldMismatches.first.csvList(), r.fieldMismatches.second.csvList()]

    of rsemConflictingDiscriminantInit:
      result = ("a case selecting discriminator '$1' with value '$2' " &
        "appears in the object construction, but the field(s) $3 " &
        "are in conflict with this value.") %
        [r.fieldMismatches.first.csvList(), r.ast.render, r.fieldMismatches.second.csvList()]

    of rsemConflictingDiscriminantValues:
      result = ("possible values " &
        "{$1} are in conflict with discriminator values for " &
        "selected object branch $2") % [
          r.nodes.csvListIt(render(it)), r.str]

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
      result = "application of implicit pragma failed"

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

    of rsemIncompatibleDefaultExpr:
      result = (
        "The default parameter '$1' has incompatible type " &
        "with the explicitly requested proc instantiation"
      ) % r.symstr

    of rsemSemfoldOverflow:
      result = "over- or underflow"

    of rsemSemfoldDivByZero:
      result = "division by zero"

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
        describeArgs(conf, r.arguments.got),
        describeArgs(conf, r.arguments.expected)
      ]

    of rsemCannotGenerateGenericDestructor:
      result = "cannot generate destructor for generic type: " & r.typ.render

    of rsemExpectedLow0Discriminant:
      result =  "low(" & r.symstr & ") must be 0 for discriminant"

    of rsemExpectedHighCappedDiscriminant:
      result = "len($1) must be less than 32768" % r.symstr

    of rsemCantConvertLiteralToRange:
      result = "cannot convert $# to $#" % [r.wrongNode.render, $r.ast.typ.render]

    of rsemCantComputeOffsetof:
      result = "can't compute offsetof on this ast"

    of rsemExpectObjectForBase:
      result = "cannot inherit from a type that is not an object type"

    of rsemExpectNonFinalForBase:
      result = "inheritance only works with non-final objects; " &
        "for " & r.typ.render & " to be inheritable it must be " &
        "'object of RootObj' instead of 'object'"

    of rsemTVoidNotAllowed:
      result = "type '$1 void' is not allowed" % r.str

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

    of rsemUnexpectedPattern:
      result = "patterns can only be specified on macros and templates"

    of rsemExpectedImportedType:
      result = "the '$1' modifier can be used only with imported types" % r.ast.render

    of rsemExpectedDistinctForBorrow:
      result = "only a 'distinct' type can borrow `.`"

    of rsemRedefinitionOf:
      if r.sym.isNil:
        result.addf(
          "redefinition of '$1'; previous declaration here: $2",
          r.symbols[0].name.s,
          conf.toStr(r.symbols[1].info)
        )
      else:
        result = "attempt to redefine: '" & r.symstr & "'"

    of rsemDefaultParamIsIncompatible:
      assert false, "REMOVE"

    of rsemExpressionCannotBeCalled:
      result = "expression cannot be called"

    of rsemWrongNumberOfGenericParams:
      result.addf(
        "cannot instantiate: '$1'; got $2 typeof(s) but expected $3",
        r.ast.render,
        $r.countMismatch.got,
        $r.countMismatch.expected
      )

    of rsemCalleeHasAnError:
      result = "cannot call '$1'; its definition has an error [defined at '$2']" %
               [r.symstr, conf.toFileLineCol(r.sym.info)]

    of rsemNoGenericParamsAllowed:
      result = "no generic parameters allowed for $1" % r.symstr

    of rsemIllegalCustomPragma:
      result = "cannot attach a custom pragma to '$1'" % r.symstr

    of rsemCallingConventionMismatch:
      assert false, "REMOVE"

    of rsemUndeclaredIdentifier:
      result = "undeclared identifier: '$1'" % r.str

      if r.recursiveDeps.len > 0:
        result.add "\nThis might be caused by recursive module dependencies:"
        for i, dep in r.recursiveDeps.pairs:
          result.add "\n(importer: $1, importee: $2)" %
            [conf.toFullPath(dep.importer), conf.toFullPath(dep.importee)]

      if r.spellingCandidates.len > 0:
        result.add "\n"
        result.add presentSpellingCandidates(
          conf, r.spellingCandidates)

    of rsemOnlyDeclaredIdentifierFoundIsError:
      result = "identifier '$1' found but it has errors, see: $2" %
                [r.str, conf.toStr(r.wrongNode.info)]

    of rsemXDeclaredButNotUsed:
      result = "'$1' is declared but not used" % r.symstr

    of rsemCompilesReport, rsemCompilesHasSideEffects:
      assert false, "Temporary report for `compiles()` speedup, cannot be printed"

    of rsemCannotMakeSink:
      result = "could not turn '$1' to a sink parameter" % r.symstr

    of rsemExprAlwaysX:
      result = "expression always evaluates to constant value"

    of rsemProcessingStmt:
      result = "processing stmt"

    of rsemProcessing:
      let path = toFilenameOption(
        conf, r.processing.fileIdx, conf.filenameOption)
      let indent = repeat(">", r.processing.importStackLen)
      let fromModule = r.sym
      let fromModule2 = if fromModule != nil: $fromModule.name.s else: "(toplevel)"
      let mode = if r.processing.isNimscript: "(nims) " else: ""
      result = "$#$# $#: $#: $#" % [mode, indent, fromModule2, r.processing.moduleStatus, path]

    of rsemConvToBaseNotNeeded:
      result = "??"

    of rsemDuplicateModuleImport:
      result = "duplicate import of '$1'; previous import here: $2" %
        [r.symstr, conf.toStr(r.previous.info)]

    of rsemHintLibDependency:
      result = r.str

    of rsemCaseTransition:
      result = "Potential object case transition, instantiate new object instead"

    of rsemObservableStores:
      result = "observable stores to '$1'" % r.ast.render

    of rsemResultUsed:
      result = "used 'result' variable"

    of rsemTypedReturnDeprecated:
      result = "`typed` will change its meaning in future versions of Nim. " &
        "`void` or no return type declaration at all has the same " &
        "meaning as the current meaning of `typed` as return type " &
        "declaration."

    of rsemInheritFromException:
      result = "inherit from a more precise exception type like ValueError, " &
        "IOError or OSError. If these don't suit, inherit from CatchableError or Defect."

    of rsemUseBase:
      result = "use {.base.} for base methods; baseless methods are deprecated"

    of rsemMethodLockMismatch:
      result = "method has lock level $1, but another method has $2" %
        [r.lockMismatch[0], r.lockMismatch[1]]

    of rsemUnknownMagic:
      result = "unknown magic '$1' might crash the compiler" % r.str

    of rsemErrGcUnsafe:
      result = r.ast.render & " is not GC safe"

    of rsemStaticIndexLeqUnprovable:
      let (a, b) = r.rangeExpression
      result = "cannot prove: " & $a & " <= " & $b

    of rsemStaticIndexGeProvable:
      let (a, b) = r.rangeExpression
      result = "can prove: " & $a & " > " & $b

    of rsemInvalidGuardField:
      result = "invalid guard field: " & r.symstr

    of rsemUnguardedAccess:
      result = "unguarded access: " & r.ast.render

    of rsemInvalidNestedLocking:
      result = "invalid nested locking"

    of rsemMultilockRequiresSameLevel:
      result = "multi-lock requires the same static lock level for every operand"

    of rsemLocksRequiresArgs:
      result = "locks pragma without argument"

    of rsemMismatchedPopPush:
      result = "{.pop.} without a corresponding {.push.}"

    of rsemImportjsRequiresJs:
      result = "`importjs` pragma requires the JavaScript target"

    of rsemDynlibRequiresExportc:
      assert false, "UNUSED?"

    of rsemTypeInvalid:
      result = "invalid type"

    of rsemInitHereNotAllowed:
      result = "initialization not allowed here"

    of rsemPragmaDynlibRequiresExportc:
      result = ".dynlib requires .exportc"

    of rsemPropositionExpected:
      result = "proposition expected"

    of rsemUnexpectedPragma:
      result = "unexpected pragma"

    of rsemDisallowedReprForNewruntime:
      result = "'repr' is not available for --gc:arc|orc"

    of rsemDisallowedOfForPureObjects:
      result = "no 'of' operator available for pure objects"

    of rsemRequiresDeepCopyEnabled:
      result = "for --gc:arc|orc 'deepcopy' support has to be enabled with --deepcopy:on"

    of rsemExpectedLiteralForGoto:
      result = "'goto' target must be a literal value"

    of rsemExpectedParameterForJsPattern:
      result =  "wrong importjs pattern; expected parameter at position " &
        $r.countMismatch.expected & " but got only: " & $r.countMismatch.got

    of rsemDisallowedRangeForComputedGoto:
      result = "range notation not available for computed goto"

    of rsemExpectedCaseForComputedGoto:
      result = "no case statement found for computed goto"

    of rsemExpectedLow0ForComputedGoto:
      result = "case statement has to start at 0 for computed goto"

    of rsemTooManyEntriesForComputedGoto:
      result = "case statement has too many cases for computed goto"

    of rsemExpectedUnholyEnumForComputedGoto:
      result = "case statement cannot work on enums with holes for computed goto"

    of rsemExpectedExhaustiveCaseForComputedGoto:
      result = "case statement must be exhaustive for computed goto"

    of rsemExpectedNimcallProc:
      result = r.symstr & " needs to have the 'nimcall' calling convention"

    of rsemRttiRequestForIncompleteObject:
      result = "request for RTTI generation for incomplete object: " & r.typ.render

    of rsemCannotDetermineBorrowTarget:
      result = "cannot determine the target of the borrow"

    of rsemResultMustBorrowFirst:
      result = "'result' must borrow from the first parameter"

    of rsemExpressionIsNotAPath:
      result = "cannot borrow from " & r.ast.render & ", it is not a path expression"

    of rsemCallconvExpected:
      result = "calling convention expected"

    of rsemOnOrOffExpected:
      result = "'on' or 'off' expected"

    of rsemUnresolvedGenericParameter:
      result = "unresolved generic parameter"

    of rsemRawTypeMismatch:
      result = "type mismatch"

    of rsemCannotAssignToDiscriminantWithCustomDestructor:
      result = "Assignment to discriminant for objects with user " &
        "defined destructor is not supported, object must have default " &
        "destructor.\nIt is best to factor out piece of object that needs " &
        "custom destructor into separate object or not use discriminator assignment"

    of rsemCannotCreateImplicitOpenarray:
      result = "cannot create an implicit openArray copy to be passed to a sink parameter"

    of rsemWrongNumberOfQuoteArguments:
      assert false, "UNUSED"

    of rsemIllegalNimvmContext:
      result = "illegal context for 'nimvm' magic"

    of rsemInvalidOrderInArrayConstructor:
      result = "invalid order in array constructor"

    of rsemTypeConversionArgumentMismatch:
      result = "a type conversion takes exactly one argument"

    of rsemConstantOfTypeHasNoValue:
      result = "constant of type '" & r.typ.render & "' has no value"

    of rsemNoObjectOrTupleType:
      result = "no object or tuple type"

    of rsemParallelFieldsDisallowsCase:
      result = "parallel 'fields' iterator does not work for 'case' objects"

    of rsemFieldsIteratorCannotContinue:
      result = "'continue' not supported in a 'fields' loop"

    of rsemConstExpressionExpected:
      result = "constant expression expected"

    of rsemDiscardingVoid:
      result = "statement returns no value that can be discarded"

    of rsemParameterNotPointerToPartial:
      result = "parameter '$1' is not a pointer to a partial object" % r.ast.render

    of rsemGenericInstantiationTooNested:
      result = "generic instantiation too nested"

    of rsemMacroInstantiationTooNested:
      result = "macro instantiation too nested"

    of rsemExpectedNonemptyPattern:
      result = "a pattern cannot be empty"

    of rsemInvalidExpression:
      result = "invalid expression: " & render(r.ast)

    of rsemParameterRedefinition:
      result = "attempt to redefine: '" & r.symstr & "'"

    of rsemParameterRequiresAType:
      result = "parameter '$1' requires a type" % r.symstr

    of rsemCannotInferParameterType:
      result = "cannot infer the type of parameter '" & r.ast.render & "'"

    of rsemMisplacedMagicType:
      result = "return type '" & r.typ.render &
        "' is only valid for macros and templates"

    of rsemIgnoreInvalidForLoop:
      result = "ignored invalid for loop"

    of rsemNotABaseMethod:
      result = "method is not a base"

    of rsemMissingMethodDispatcher:
      result = "'" & r.ast.render & "' lacks a dispatcher"

    of rsemWarnUnsafeCode:
      result = "not GC-safe: '$1'" % r.ast.render

    of rsemImplicitCstringConvert:
      result = "implicit conversion to 'cstring' from a non-const location: " &
        ("$1; this will become a compile time error in the future" % r.ast.render)

    of rsemHoleEnumConvert:
      result = "conversion to enum with holes is unsafe: $1" % r.ast.render

    of rsemAnyEnumConvert:
      result = "enum conversion: $1" % r.ast.render

    of rsemUseOfGc:
      result = "'$1' uses GC'ed memory" % r.ast.render

    of rsemPattern:
      result = r.ast.render

    of rsemFatalError:
      result = r.str

    of rsemSugNoSymbolAtPosition:
      result = "found no symbol at position"

    of rsemOverrideSafetyMismatch:
      result = "base method is GC-safe, but '$1' is not" % r.symbols[1].name.s

    of rsemOverrideLockMismatch:
      result = "base method has lock level $1, but dispatcher has $2" % [
        $r.symbols[1].typ.lockLevel,
        $r.symbols[0].typ.lockLevel
      ]

    of rsemExpectedIdentifier:
      result = "identifier expected, but found: " & r.ast.render

    of rsemExpectedIdentifierInExpr:
      # `ast` the expression (where the error is), the identifier `wrongNode`
      # clarifies the message.
      result = "in expression '$1': identifier expected, but found '$2'" %
                [r.ast.render(), r.wrongNode.render()]

    of rsemExpectedIdentifierQuoteLimit:
      result =
        "identifier expected, found too large identifier construction '$1'" %
          [r.ast.render()]

    of rsemExpectedIdentifierWithExprContext:
      # `ast` the identifier, the expression `wrongNode` (where the error is)
      # clarifies the message.
      result = "identifier expected, but found: '$1'; within expression: '$2'" %
                [r.ast.render(), r.wrongNode.render()]

    of rsemFieldNotAccessible:
      result = "the field '$1' is not accessible." % r.symstr

    of rsemFieldOkButAssignedValueInvalid:
      result = "Invalid field assignment '$1'$2" % [
        r.wrongNode.render,
        tern(r.ast.isNil, "", "; " & r.ast.render)
      ]

    of rsemStrictNotNilResult:
      case r.nilIssue:
      of Nil:
        result = "return value is nil"
      of MaybeNil:
        result = "return value might be nil"
      of Unreachable:
        result = "return value is unreachable"
      of Safe, Parent:
        discard

    of rsemStrictNotNilExpr:
      result.add(
        "can't deref ",
        r.ast.render,
        ", ",
        case r.nilIssue:
          of Nil: "it is nil"
          of MaybeNil: "it might be nil"
          of Unreachable: "it is unreachable"
          else: ""
      )

      if r.nilHistory.len > 0:
        result.add("\n")

      for step in r.nilHistory:
        result.addf("  $1 on line "):
          case step.kind:
            of NilTransition.TArg: "param with nilable type"
            of NilTransition.TNil: "it returns true for isNil"
            of NilTransition.TAssign: "assigns a value which might be nil"
            of NilTransition.TVarArg: "passes it as a var arg which might change to nil"
            of NilTransition.TResult: "it is nil by default"
            of NilTransition.TType: "it has ref type"
            of NilTransition.TSafe: "it is safe here as it returns false for isNil"
            of NilTransition.TPotentialAlias: "it might be changed directly or through an alias"
            of NilTransition.TDependant: "it might be changed because its base might be changed"

        result.addf("$1:$2", $step.info.line, $step.info.col)

    of rsemWarnGcUnsafeListing, rsemErrGcUnsafeListing:
      let
        trace = r.gcUnsafeTrace
        (s, u) = (trace.isUnsafe.name.s, trace.unsafeVia.name.s)
      case trace.unsafeRelation:
      of sgcuCallsUnsafe:
        result.addf("'$#' is not GC-safe as it calls '$#'", s, u)
      of sgcuAccessesGcGlobal:
        result.addf(
          "'$#' is not GC-safe as it accesses '$#' which is a global using GC'ed memory",
          s, u)
      of sgcuIndirectCallVia:
        result.addf(
          "'$#' is not GC-safe as it performs an indirect call via '$#'", s, u)
      of sgcuIndirectCallHere:
        result.addf(
          "'$#' is not GC-safe as it performs an indirect call here", s)

    of rsemDiagnostics:
      result.add presentDiagnostics(conf, r.diag, startWithNewLine = false)

    of rsemCompilerOptionInvalid:
      result = "Invalid compiler option - " % r.str

    of rsemDeprecatedCompilerOpt:
      result = "'$#' is deprecated, now a noop" % r.str

    of rsemCompilerOptionArgInvalid:
      result = "Unexpected value for option '$#'. Expected one of $#, but got '$#'" %
                [r.str, r.allowedOptArgs.join(", "), r.badCompilerOptArg]

    of rsemDeprecatedCompilerOptArg:
      result = "'$#' is deprecated for option '$#', now a noop" %
                [r.str, r.compilerOptArg]


const standalone = {
  rsemExpandArc, # Original compiler did not consider it as a hint
  rvmStackTrace, # Always associated with extra report
  rsemDiagnostics, # Wraps other reports
}

const repWithPrefix = repAllKinds - standalone
const repWithSuffix = repWarningKinds + repHintKinds - standalone
const repWithLocation = repAllKinds - standalone

func prefixShort(conf: ConfigRef, r: ReportTypes): string {.inline.} =
  if r.kind in repWithPrefix:
    # `Hint: `, `Error: ` etc.
    reportTitles[conf.severity(r)]
  else:
    ""

proc prefix(conf: ConfigRef, r: ReportTypes): string =
  let sev = conf.severity(r)
  if r.location.isSome() and r.kind in repWithLocation:
    # Optional report location
    result.add conf.toStr(r.location.get()) & " "

  if r.kind in repWithPrefix:
    # `Hint: `, `Error: ` etc.
    result.add conf.wrap(prefixShort(conf, r), reportColors[sev])

func suffixShort(conf: ConfigRef, r: ReportTypes): string {.inline.} =
  if r.kind in repWithSuffix or conf.hasHint(rintErrKind):
    " [" & $r.kind & "]"
  else:
    ""

proc suffix(conf: ConfigRef, r: ReportTypes): string =
  if r.kind in repWithSuffix or conf.hasHint(rintErrKind):
    result.add conf.wrap(suffixShort(conf, r), fgCyan)

  if conf.hasHint(rintMsgOrigin):
    result.add(
      "\n",
      conf.toStr(r.reportInst),
      " compiler msg instantiated here ",
      conf.wrap("[MsgOrigin]", fgCyan)
    )

    if r.reportInst != r.reportFrom:
      result.add(
        "\n",
        conf.toStr(r.reportFrom),
        " compiler report submitted here ",
        conf.wrap("[MsgOrigin]", fgCyan)
      )

proc reportFull*(conf: ConfigRef, r: SemReport): string =
  assertKind r

  if r.kind == rsemProcessing and conf.hintProcessingDots:
    return "."

  result.add(
    conf.getContext(r.context),
    conf.prefix(r),
    reportBody(conf, r),
    conf.suffix(r)
  )

proc reportShort*(conf: ConfigRef, r: SemReport): string =
  # mostly created for nimsuggest
  assertKind r
  if r.kind == rsemProcessing and conf.hintProcessingDots:
    "."
  else:
    reportBody(conf, r) & suffixShort(conf, r)

proc reportBody*(conf: ConfigRef, r: ParserReport): string =
  assertKind r
  case ParserReportKind(r.kind)
  of rparInvalidIndentation:
    result = "invalid indentation"

  of rparInvalidIndentationWithForgotEqualSignHint:
    result = "invalid indentation, maybe missing '=' at $1?" % conf$r.eqInfo

  of rparNestableRequiresIndentation:
    result = "nestable statement requires indentation"

  of rparIdentExpected:
    result = "identifier expected, but found '$1'" % r.found

  of rparIdentExpectedEmptyAccQuote:
    result = "identifier expected, but got empty accent quotes"

  of rparExprExpected:
    result = "expression expected, but found '$1'" % r.found

  of rparMissingToken:
    result =
      case r.expected.len
      of 1:
        "expected " & r.expected[0]
      of 2:
        "expected $1 or $2" % [r.expected[0], r.expected[1]]
      else:
        unreachable("only 1-2 expected tokesn allowed, update message to fix")

  of rparUnexpectedToken:
    result = "expected: '$1', but got: '$2'" % [r.expectedKind, r.found]

  of rparAsmStmtExpectsStrLit:
    result = "the 'asm' statement takes a string literal, got: '$1'" % r.found

  of rparFuncNotAllowed:
    result = "func keyword is not allowed in type descriptions, " &
      "use proc with {.noSideEffect.} pragma instead"

  of rparTupleTypeWithPar:
    result = "the syntax for tuple types is 'tuple[...]', not 'tuple(...)'"

  of rparMisplacedParameterVar:
    result = "the syntax is 'parameter: var T', not 'var parameter: T'"

  of rparConceptNotinType:
    result = "the 'concept' keyword is only valid in 'type' sections"

  of rparMisplacedExport:
    result = "invalid indentation; an export marker '*' " &
      "follows the declared identifier"

  of rparTemplMissingEndClose:
    result = "'end' does not close a control flow construct"

  of rparTemplInvalidExpression:
    result = "invalid expression"

  of rparInconsistentSpacing:
    result = "Number of spaces around '$1' is not consistent" % r.found

  of rparEnablePreviewDotOps:
    result = "dot-like operators will be parsed differently " &
      "with `-d:nimPreviewDotLikeOps`"

  of rparPragmaNotFollowingTypeName:
    result = "type pragmas follow the type name; this form of " &
      "writing pragmas is deprecated"

  of rparPragmaBeforeGenericParameters:
    result = "pragma must come after any generic parameter list"

  of rparInvalidFilter:
    result = "invalid filter: $1" % r.node.renderTree

proc reportFull*(conf: ConfigRef, r: ParserReport): string =
  assertKind r
  result = conf.prefix(r) & conf.reportBody(r) & conf.suffix(r)

proc reportShort*(conf: ConfigRef, r: ParserReport): string =
  # mostly created for nimsuggest
  assertKind r
  reportBody(conf, r) & suffixShort(conf, r)

proc presentDiagnostics(conf: ConfigRef, d: SemDiagnostics, startWithNewLine: bool): string =
  assert d.diags.len == 0, "empty until SemReport is removed, see sigmatch"
  if d.tempDiagFailCount > 0:
    if startWithNewLine:
      result.add '\n'
    result.add "number of diags failed: " & $d.tempDiagFailCount
  
  when false:
    # TODO: restore a reworked version of this once Report/SemReport are gone
    var newLine = startWithNewLine
    for diag in d.diags:
      let report =
        block:
          let r = astDiagToLegacyReport(diag)
          case r.category
          of repSem:
            r.semReport
          else:
            continue
      if newLine:
        result.add '\n'
      else:
        newLine = true
      result.add conf.getContext(report.context)
      if report.location.isSome() and report.kind in repWithLocation:
        # Optional report location
        result.add conf.toStr(report.location.get()) & " "

      if report.kind in repWithPrefix:
        # Instead of "Error:", take the concept sym that the diagnostics concern
        let sev = conf.severity(report)
        result.add conf.wrap(
          if sev == rsevError:
            d.diagnosticsTarget.name.s & ": "
          else:
            prefixShort(conf, report)
          , reportColors[sev])

      result.add reportBody(conf, report)
      result.add conf.suffix(report)

proc presentFailedCandidates(
    conf: ConfigRef,
    n: PNode,
    errors: seq[SemCallMismatch]
  ): (TPreferedDesc, string) =
  ## Format failed candidates for call overload resolution failure

  var prefer = preferName
  # to avoid confusing errors like:
  #   got (SslPtr, SocketHandle)
  #   but expected one of:
  #   openssl.SSL_set_fd(ssl: SslPtr, fd: SocketHandle): cint
  # we do a pre-analysis. If all types produce the same string, we will add
  # module information.
  let proto = describeArgs(conf, n, 1, preferName)
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
      if err.firstMismatch.pos > 1:
        filterOnlyFirst = true
        break

  var
    maybeWrongSpace = false
    candidatesAll: seq[string]
    candidates = ""
    skipped = 0

  for err in errors:
    candidates.setLen 0
    if filterOnlyFirst and err.firstMismatch.pos == 1:
      inc skipped
      continue

    if err.target.kind in routineKinds and err.target.ast != nil:
      candidates.add(renderTree(
        err.target.ast, {renderNoBody, renderNoComments, renderNoPragmas}))
    else:
      candidates.add(getProcHeader(conf, err.target, prefer))

    candidates.addDeclaredLocMaybe(conf, err.target)
    candidates.add("\n")

    let
      nArg = err.firstMismatch.arg
      nameParam =
        if err.firstMismatch.formal != nil: err.firstMismatch.formal.name.s
        else:                               ""
    
    if n.len > 1:
      candidates.add("  first type mismatch at position: " & $err.firstMismatch.pos)
      # candidates.add "\n  reason: " & $err.firstMismatch.kind # for debugging
      case err.firstMismatch.kind:
        of kNotGeneric:
          candidates.add("\n  routine is not generic")
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

        of kTypeMismatch, kVarNeeded, kGenericTypeMismatch:
          doAssert nArg != nil
          doAssert err.firstMismatch.formal != nil
          let wanted = err.firstMismatch.formal.typ

          candidates.add("\n  required type for " & nameParam &  ": ")
          candidates.addTypeDeclVerboseMaybe(conf, wanted)
          candidates.add "\n  but expression '"

          if err.firstMismatch.kind == kVarNeeded:
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

          candidates.add presentDiagnostics(conf, err.diag, startWithNewLine = true)

        of kUnknown:
          discard "do not break 'nim check'"

      candidates.add '\n'
      if err.firstMismatch.pos == 1 and nArg.kind == nkTupleConstr and
          n.kind == nkCommand:
        maybeWrongSpace = true

    candidatesAll.add candidates

  candidatesAll.sort # fix #13538
  candidates = join(candidatesAll)

  if skipped > 0:
    candidates.add(
      $skipped &
        " other mismatching symbols have been " &
        "suppressed; compile with --showAllMismatches:on to see them\n")

  if maybeWrongSpace:
    candidates.add(
      "maybe misplaced space between " & renderTree(n[0]) & " and '(' \n")

  result = (prefer, candidates)

proc reportBody*(conf: ConfigRef, r: InternalReport): string =
  assertKind r
  case InternalReportKind(r.kind):
    of rintStackTrace:
      result = conf.formatTrace(r.trace)

    of rintListWarnings:
      result = "Warnings:"
      for kind in repWarningKinds:
        result.addf("\n  [$1] $2", tern(
          kind in r.enabledOptions, "X", " "), $kind)

    of rintListHints:
      result = "Hints:"
      for kind in repHintKinds:
        result.addf("\n  [$1] $2", tern(
          kind in r.enabledOptions, "X", " "), $kind)

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
          build.add " "

      let mem =
        if par.isMaxMem:
          formatSize(par.mem) & " peakmem"

        else:
          formatSize(par.mem) & " totmem"

      result = &"{conf.prefix(r)}{build}{par.linesCompiled} lines; {par.sec:.3f}s; {mem}; proj: {par.project}; out: {par.output}{conf.suffix(r)}"

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
        " (", r.msg, ") should never be called."
      )

    of rintEchoMessage:
      if conf.cmd == cmdInteractive:
        result = ">>> " & r.msg

      else:
        result = r.msg

    of rintCannotOpenFile, rintWarnCannotOpenFile:
      result = "cannot open file: $1" % r.file

    of rintUnknown:
      result = "unknown"

    of rintFatal:
      result = "fatal"

    of rintIce:
      result = r.msg

    of rintNotUsingNimcore:
      result = "Nim tooling must be built using -d:nimcore"

    of rintNotImplemented:
      result = r.msg

    of rintUnexpected:
      result = "unexpected"

    of rintWarnFileChanged:
      result = "file changed: $1" % r.file

    of rintSource:
      assert false, "is a configuration hint, should not be reported manually"

    of rintQuitCalled:
      result = "quit() called"

    of rintMsgOrigin, rintErrKind:
      assert false, "is a configuration hint, should not be reported manually"

    of rintNimconfWrite:
      result = ""

    of rintDumpState:
      if getConfigVar(conf, "dump.format") == "json":
        let s = r.stateDump
        var definedSymbols = newJArray()
        for s in s.definedSymbols:
          definedSymbols.elems.add(%s)

        var libpaths = newJArray()
        var lazyPaths = newJArray()
        for dir in conf.searchPaths:
          libpaths.elems.add(%dir.string)

        for dir in conf.lazyPaths:
          lazyPaths.elems.add(%dir.string)

        var hints = newJObject()
        for (a, state) in s.hints:
          hints[$a] = %(state)

        var warnings = newJObject()
        for (a, state) in s.warnings:
          warnings[$a] = %(state)

        result = $(%[
          (key: "version",         val: %s.version),
          (key: "nimExe",          val: %s.nimExe),
          (key: "prefixdir",       val: %s.prefixdir),
          (key: "libpath",         val: %s.libpath),
          (key: "project_path",    val: %s.projectPath),
          (key: "defined_symbols", val: definedSymbols),
          (key: "lib_paths",       val: libpaths),
          (key: "lazyPaths",       val: lazyPaths),
          (key: "outdir",          val: %s.outdir),
          (key: "out",             val: %s.out),
          (key: "nimcache",        val: %s.nimcache),
          (key: "hints",           val: hints),
          (key: "warnings",        val: warnings),
        ])

      else:
        result.add "-- list of currently defined symbols --\n"
        let s = r.stateDump
        for s in s.definedSymbols:
          result.add(s, "\n")

        result.add "-- end of list --\n"

        for it in s.libPaths:
          result.add it, "\n"

proc reportFull*(conf: ConfigRef, r: InternalReport): string =
  assertKind r
  case r.kind:
  of rintCannotOpenFile, rintWarnCannotOpenFile:
    result.add(conf.prefix(r), conf.reportBody(r), conf.suffix(r))
  else:
    result = reportBody(conf, r)

proc reportShort*(conf: ConfigRef, r: InternalReport): string =
  # mostly created for nimsuggest
  assertKind r
  result = reportBody(conf, r)
  if r.kind in {rintCannotOpenFile, rintWarnCannotOpenFile}:
    result.add conf.suffixShort(r)


proc reportBody*(conf: ConfigRef, r: LexerReport): string =
  assertKind r
  case LexerReportKind(r.kind):
    of rlexMalformedTrailingUnderscre:
      result.add "invalid token: trailing underscore"

    of rlexMalformedUnderscores:
      result.add "only single underscores may occur in a token and token may not " &
        "end with an underscore: e.g. '1__1' and '1_' are invalid"

    of rlexInvalidToken:
      result.add "invalid token: $1 (\\$2)" % [r.msg, $ord(r.msg[0])]

    of rlexInvalidTokenSpaceBetweenNumAndIdent:
      result.add "invalid token: no whitespace between number and identifier"

    of rlexNoTabs:
      result.add "tabs are not allowed, use spaces instead"

    of rlexInvalidIntegerLiteralOctalPrefix:
      result.addf(
        "$1 is an invalid int literal; For octal literals use the '0o' prefix",
        r.msg)

    of rlexInvalidIntegerSuffix:
      result.addf("invalid number suffix: '$1'", r.msg)

    of rlexNumberNotInRange:
      result.addf("number out of range: '$1'", r.msg)

    of rlexExpectedHex:
      result.addf("expected a hex digit, but found: '$1'; maybe prefix with 0",
                  r.msg)

    of rlexInvalidNumericLiteral, rlexInvalidIntegerLiteral:
      result.addf("invalid number: '$1'", r.msg)

    of rlexInvalidCharLiteral:
      result.add "invalid character literal"

    of rlexInvalidCharLiteralConstant:
      result.add "invalid character constant"

    of rlexInvalidCharLiteralPlatformNewline:
      result.add "\\p not allowed in character literal"

    of rlexInvalidCharLiteralUnicodeCodepoint:
      result.add "\\u not allowed in character literal"

    of rlexMissingClosingApostrophe:
      result.add "missing closing ' for character literal"

    of rlexInvalidUnicodeCodepointEmpty:
      result.add "Unicode codepoint cannot be empty"

    of rlexInvalidUnicodeCodepointGreaterThan0x10FFFF:
      result.addf("Unicode codepoint must be 0x10FFFF or lower, but was: $1",
                  r.msg)

    of rlexUnclosedTripleString:
      result.add "closing \"\"\" expected, but end of file reached"

    of rlexUnclosedSingleString:
      result.add "closing \" expected"

    of rlexExpectedToken:
      assert false

    of rlexCfgInvalidDirective:
      result.addf("invalid directive: '$1'", r.msg)

    of rlexUnclosedComment:
      result.add "end of multiline comment expected"

    of rlexDeprecatedOctalPrefix:
      result.add "octal escape sequences do not exist; leading zero is ignored"

    of rlexLinterReport:
      result.addf("'$1' should be: '$2'", r.got, r.wanted)

    of rlexLineTooLong:
      result.add "line too long"

    of rlexSourceCodeFilterOutput:
      result.add "generated code listing:"
      result.add r.msg
      result.add "end of listing"

proc reportFull*(conf: ConfigRef, r: LexerReport): string    =
  assertKind r
  result.add(prefix(conf, r), reportBody(conf, r), suffix(conf, r))

proc reportShort*(conf: ConfigRef, r: LexerReport): string =
  # mostly created for nimsuggest
  assertKind r
  prefixShort(conf, r) & reportBody(conf, r) & suffixShort(conf, r)


proc reportBody*(conf: ConfigRef, r: ExternalReport): string =
  assertKind r
  case ExternalReportKind(r.kind):
    of rextConf:
      result.add(
        conf.prefix(r),
        "used config file '$1'" % r.msg,
        conf.suffix(r)
      )

    of rextCommandMissing:
      result.add("Command missing")

    of rextInvalidHint:
      result.add("Invalid hint - ", r.cmdlineProvided)

    of rextInvalidWarning:
      result.add("Invalid warning - ", r.cmdlineProvided)

    of rextInvalidCommandLineOption:
      result.add("Invalid command line option - ", r.cmdlineProvided)

    of rextUnknownCCompiler:
      result = "unknown C compiler: '$1'. Available options are: $2" %
                [r.passedCompiler, r.knownCompilers.join(", ")]

    of rextOnlyAllOffSupported:
      result = "only 'all:off' is supported"

    of rextExpectedOnOrOff:
      result = "option '$1' expected 'on' or 'off', but '$2' found" %
                [r.cmdlineSwitch, r.cmdlineProvided]

    of rextExpectedTinyCForRun:
      result = "'run' command not available; rebuild with -d:tinyc"

    of rextExpectedCbackendForRun:
      result = "'run' requires c backend, got: '$1'" % $conf.backend

    of rextInvalidPath:
      result = "invalid path: " & r.cmdlineProvided

    of rextInvalidPackageName:
      result = "invalid package name: " & r.packageName

    of rextDeprecated:
      result = r.msg

    of rextPath:
      result = "added path: '$1'" % r.packagePath

proc reportFull*(conf: ConfigRef, r: ExternalReport): string =
  assertKind r
  reportBody(conf, r)

proc reportShort*(conf: ConfigRef, r: ExternalReport): string {.inline.} =
  # mostly created for nimsuggest
  reportBody(conf, r)

const
  dropTraceExt = off
  reportCaller = on

proc reportBody*(conf: ConfigRef, r: TraceSemReport): string =
  assertKind r

  case DbgTraceReportKind(r.kind):
  of rdbgTraceStep:
    let
      s = r.semstep
      indent = s.level * 2 + (
        2 #[ Item indentation ]# +
        5 #[ Global entry indentation ]#
      )

    proc render(node: PNode): string =
      conf.wrap(conf.treeRepr(node,
                              indent = indent + 2,
                              rconf = implicitCompilerTraceReprConf))

    proc render(typ: PType): string =
      conf.wrap(conf.treeRepr(typ,
                              indent = indent + 2,
                              rconf = implicitCompilerTraceReprConf))

    proc render(sym: PSym): string =
      conf.wrap(conf.treeRepr(sym,
                              indent = indent + 2,
                              rconf = implicitCompilerTraceReprConf))

    proc render(sym: PIdent): string =
      conf.wrap(conf.treeRepr(sym,
                              indent = indent + 2,
                              rconf = implicitCompilerTraceReprConf))


    result.addf("$1]", align($s.level, 2, '#'))
    result.add(
      repeat("  ", s.level),
      tern(s.direction == semstepEnter, "> ", "< "),
      conf.wrap(s.name, tern(s.direction == semstepEnter, fgGreen, fgRed)),
      " @ ",
      conf.wrap(conf.toStr(r.reportInst, dropTraceExt), fgCyan),
      tern(
        reportCaller and s.steppedFrom.isValid(),
        " from " & conf.toStr(s.steppedFrom, dropTraceExt),
        ""))

    var res = addr result
    proc field(name: string, value: string = "\n") =
      res[].add "\n"
      res[].add repeat(" ", indent)
      res[].add name
      res[].add ":"
      res[].add value

    let enter = s.direction == semstepEnter
    if conf.hack.semTraceData:
      # field("kind", $s.kind) # if you're new to reading traces, uncomment
      
      case s.kind:
        of stepNodeToNode:
          if enter:
            field("from node")
          else:
            field("to node")

          result.add render(s.node)

        of stepSymNodeToNode:
          if enter:
            field("from sym")
            result.add render(s.sym)
            field("from node")
            result.add render(s.node)
          else:
            field("to node")
            result.add render(s.node)

        of stepNodeTypeToNode:
          if enter:
            field("from node")
            result.add render(s.node)
            field("from type")
            result.add render(s.typ)
          else:
            field("to node")
            result.add render(s.node)

        of stepNodeFlagsToNode:
          if enter:
            field("from flags",  " " & conf.wrap($s.flags + fgCyan))
            result.add
            field("from node")
            result.add render(s.node)
          else:
            field("to node")
            result.add render(s.node)

        of stepTrack:
          discard #[ 'track' has no extra data fields ]#

        of stepError, stepWrongNode:
          field("node")
          result.add render(s.node)

        of stepNodeToSym:
          if enter:
            field("from node")
            result.add render(s.node)
          else:
            field("to sym")
            result.add render(s.sym)

        of stepIdentToSym:
          if enter:
            field("from ident")
            result.add render(s.ident)
          else:
            field("to sym")
            result.add render(s.sym)

        of stepTypeTypeToType:
          if enter:
            field("from type")
            result.add render(s.typ)
            field("from type1")
            result.add render(s.typ1)
          else:
            field("to type")
            result.add render(s.typ)
        
        of stepNodeSigMatch, stepResolveOverload:
          if enter:
            field("from node")
            result.add render(s.node)
          else:
            field("match status", s.candidate.state)

            if s.candidate.call.isNil:
              field("mismatch kind", $s.candidate.error.firstMismatch.kind)
            else:
              if s.candidate.calleeSym.isNil:
                field("callee")
                result.add render(s.candidate.callee)
              else:
                field("calleeSym")
                result.add render(s.candidate.calleeSym)
              field("call")
              result.add render(s.candidate.call)
  of rdbgTraceLine:
    let ind = repeat("  ", r.ctraceData.level)
    var
      paths: seq[string]
      width = 0
    for entry in r.ctraceData.entries:
      paths.add "$1($2)" % [
        formatPath(conf, $entry.filename), $entry.line]

      width = max(paths[^1].len, width)

    for idx, entry in r.ctraceData.entries:
      result.add(
        "  ]",
        ind, " | ",
        alignLeft(paths[idx], width + 1),
        conf.wrap($entry.procname, fgGreen),
        tern(idx < r.ctraceData.entries.high, "\n", "")
      )

  of rdbgTraceStart:
    result = ">>] trace start"

  of rdbgTraceEnd:
    result = "<<] trace end"

  of rdbgTraceDefined:
    result = ">>] debug trace defined at " & toStr(conf, r.location.get())

  of rdbgTraceUndefined:
    result = "<<] debug trace undefined " & toStr(conf, r.location.get())

proc reportFull*(conf: ConfigRef, r: TraceSemReport): string =
  assertKind r
  result = reportBody(conf, r)

proc reportShort*(conf: ConfigRef, r: TraceSemReport): string =
  # mostly created for nimsuggest
  assertKind r
  result = reportBody(conf, r)

proc reportBody*(conf: ConfigRef, r: DebugReport): string =
  assertKind r
  func toStr(opc: TOpcode): string = substr($opc, 3)

  case DebugReportKind(r.kind)
  of rdbgVmExecTraceMinimal:
    result.addf(
      "$# [$#] $#",
      conf.toStr(r.vmgenExecMinimal.info),
      r.vmgenExecMinimal.opc.toStr,
      conf.sourceLine(r.vmgenExecMinimal.info),
    )

  of rdbgOptionsPush:
    result = "PUSH: " & $r.optionsNow

  of rdbgOptionsPop:
    result = "POP: " & $r.optionsNow

  of rdbgVmExecTraceFull:
    let exec = r.vmgenExecFull
    result.addf(
      "PC:$1 $2 $3 $4 $5",
      $exec.pc,
      $exec.opc.toStr,
      tern(exec.ra == rkNone, "", $exec.ra),
      tern(exec.rb == rkNone, "", $exec.rb),
      tern(exec.rc == rkNone, "", $exec.rc)
    )

  of rdbgFinishedConfRead:
    result = "finished configuration file read $1" % r.filename

  of rdbgStartingConfRead:
    result = "starting configuration file read $1" % r.filename

  of rdbgCfgTrace:
    result = "cfg trace '" & r.str & "'"

  of rdbgVmCodeListing:
    result.add "Code listing"
    let l = r.vmgenListing

    if not l.sym.isNil():
      result.addf(
        " for the '$#' $#\n\n",
        l.sym.name.s,
        conf.toStr(l.sym.info))
    else:
      result.add "\n\n"

    for e in r.vmgenListing.entries:
      if e.isTarget:
        result.add("L:", e.pc, "\n")

      func `$<`[T](arg: T): string = alignLeft($arg, 5)
      func `$<`(opc: TOpcode): string = alignLeft(opc.toStr, 12)

      var line: string

      case e.opc
      of {opcIndCall, opcIndCallAsgn}:
        line.addf("  $# r$# r$# #$#", $<e.opc, $<e.ra, $<e.rb, $<e.rc)
      of {opcConv, opcCast}:
        line.addf(
          "  $# r$# r$# $# $#",
          $<e.opc,
          $<e.ra,
          $<e.rb,
          $<e.types[0].typeToString(),
          $<e.types[1].typeToString())
      elif e.opc < firstABxInstr:
        line.addf("  $# r$# r$# r$#", $<e.opc, $<e.ra, $<e.rb, $<e.rc)
      elif e.opc in relativeJumps + {opcTry}:
        line.addf("  $# r$# L$#", $<e.opc, $<e.ra, $<e.idx)
      elif e.opc in {opcExcept}:
        line.addf("  $# $# $#", $<e.opc, $<e.ra, $<e.idx)
      elif e.opc in {opcLdConst, opcAsgnConst}:
        line.addf(
          "  $# r$# $# $#",
          $<e.opc, $<e.ra, $<e.ast.renderTree(), $<e.idx)
      else:
        line.addf("  $# r$# $#", $<e.opc, $<e.ra, $<e.idx)

      result.add(
        line,
        tern(line.len <= 48, repeat(" ", 48 - line.len), ""),
        toStr(conf, e.info),
        "\n")

proc reportFull*(conf: ConfigRef, r: DebugReport): string =
  assertKind r
  case r.kind
  of rdbgCfgTrace:
    result.add(prefix(conf, r), reportBody(conf, r), suffix(conf, r))
  else:
    result = reportBody(conf, r)

proc reportShort*(conf: ConfigRef, r: DebugReport): string =
  # mostly created for nimsuggest
  assertKind r
  result = reportBody(conf, r)
  if r.kind == rdbgCfgTrace:
    result.add suffixShort(conf, r)

proc reportBody*(conf: ConfigRef, r: BackendReport): string  =
  assertKind r
  case BackendReportKind(r.kind):
  of rbackJsTooCaseTooLarge:
    "Your case statement contains too many branches, consider using if/else instead!"

  of rbackCannotWriteScript, rbackCannotWriteMappingFile:
    "could not write to file: " & r.filename

  of rbackTargetNotSupported:
    "Compiler '$1' doesn't support the requested target" % r.usedCompiler

  of rbackJsonScriptMismatch:
    (
      "jsonscript command outputFile '$1' must " &
        "match '$2' which was specified during --compileOnly, see \"outputFile\" entry in '$3' "
    ) % [
      r.jsonScriptParams[0],
      r.jsonScriptParams[1],
      r.jsonScriptParams[2],
    ]

  of rbackVmFileWriteFailed:
    "writing to output file '$1' failed with error: '$2'" %
              [r.outFilename, r.failureMsg]

  of rbackRstCannotOpenFile:
    "cannot open '$1'" % r.msg

  of rbackRstExpected:
    "'$1' expected" % r.msg

  of rbackRstGridTableNotImplemented:
    "grid table is not implemented"

  of rbackRstMarkdownIllformedTable:
    "illformed delimiter row of a Markdown table"

  of rbackRstNewSectionExpected:
    "new section expected $1" % r.msg

  of rbackRstGeneralParseError:
    "general parse error" % r.msg

  of rbackRstInvalidDirective:
    "invalid directive: '$1'" % r.msg

  of rbackRstInvalidField:
    "invalid field: $1" % r.msg

  of rbackRstFootnoteMismatch:
    "mismatch in number of footnotes and their refs: $1" % r.msg

  of rbackCannotProduceAssembly:
    "Couldn't produce assembler listing " &
      "for the selected C compiler: " & r.usedCompiler

  of rbackRstTestUnsupported:
    "the ':test:' attribute is not supported by this backend"

  of rbackRstRedefinitionOfLabel:
    "redefinition of label '$1'" % r.msg

  of rbackRstUnknownSubstitution:
    "unknown substitution '$1'" % r.msg

  of rbackRstBrokenLink:
    "unknown substitution '$1'" % r.msg

  of rbackRstUnsupportedLanguage:
    "language '$1' not supported" % r.msg

  of rbackRstUnsupportedField:
    "field '$1' not supported" % r.msg

  of rbackRstRstStyle:
    "RST style: $1" % r.msg

  of rbackProducedAssembly:
    "Produced assembler here: " & r.filename

  of rbackLinking:
    ""

  of rbackCompiling:
    ""

proc reportFull*(conf: ConfigRef, r: BackendReport): string =
  assertKind r
  result.add(
    conf.prefix(r),
    conf.reportBody(r),
    conf.suffix(r))

proc reportShort*(conf: ConfigRef, r: BackendReport): string =
  # mostly created for nimsuggest
  assertKind r
  result = reportBody(conf, r)
  if BackendReportKind(r.kind) == rbackJsTooCaseTooLarge:
    result.add conf.suffixShort(r)


proc reportBody*(conf: ConfigRef, r: VMReport): string =
  proc render(n: PNode, rf = defaultRenderFlags): string = renderTree(n, rf)
  proc render(t: PType): string = typeToString(t)

  case VMReportKind(r.kind)
  of rvmUserError:
    result = r.str

  of rvmMissingImportcCompleteStruct:
    result = "'$1' requires '.importc' types to be '.completeStruct'" % r.str

  of rvmNotAFieldSymbol:
    result = "no field symbol"

  of rvmBadExpandToAst:
    result = "expandToAst requires 1 argument"

  of rvmCannotImportc:
    result = "cannot 'importc' variable/proc at compile time: " & r.symstr

  of rvmInvalidObjectConstructor:
    result = "invalid object constructor"

  of rvmStackTrace:
    result = "stack trace: (most recent call last)\n"
    for idx in countdown(r.stacktrace.high, 0):
      let (sym, loc) = r.stacktrace[idx]
      result.add(
        conf.toStr(loc),
        " ",
        if sym != nil: sym.name.s else: "???"
      )
      if r.skipped > 0 and idx == r.stacktrace.high:
        # The entry point is always the last element in the list
        result.add(
          "\nSkipped ", r.skipped,
          " entries, calls that led up to printing")

      if idx > 0:
        result.add("\n")

  of rvmUnhandledException:
    result.addf(
      "unhandled exception: $1 [$2]",
      r.ast[3].skipColon.strVal,
      r.ast[2].skipColon.strVal
    )

  of rvmNodeNotASymbol:
    result = "node is not a symbol"

  of rvmNodeNotAProcSymbol:
    result = "node is not a proc symbol"

  of rvmNilAccess:
    result = "attempt to access a nil address"

  of rvmAccessOutOfBounds:
    result = "trying to access a location outside of the VM's memory"

  of rvmAccessTypeMismatch:
      # TODO: provide both dynamic and static type in the message
    result = "trying to access a location with a handle of incompatible type"

  of rvmAccessNoLocation:
    result = "handle doesn't reference a valid location"

  of rvmOverOrUnderflow:
    result = "over- or underflow"

  of rvmDivisionByConstZero:
    result = "division by zero"

  of rvmTooManyIterations:
    result = "interpretation requires too many iterations; " &
      "if you are sure this is not a bug in your code, compile " &
      "with `--maxLoopIterationsVM:number` (current value: $1)" %
      $conf.maxLoopIterationsVM

  of rvmCannotModifyTypechecked:
    result = "typechecked nodes may not be modified"

  of rvmNoType:
    result = "node has no type"

  of rvmIllegalConv:
    result = r.str

  of rvmIllegalConvFromXToY:
    result = "illegal conversion from '$1' to '$2'" %
             [$r.actualType, $r.formalType]

  of rvmFieldNotFound:
    result = "node lacks field: " & r.str

  of rvmCannotSetChild:
    result = "cannot set child of node kind: n" & $r.ast.kind

  of rvmCannotAddChild:
    result = "cannot add to node kind: n" & $r.ast.kind

  of rvmCannotGetChild:
    result = "cannot get child of node kind: n" & $r.ast.kind

  of rvmMissingCacheKey:
    result = "key does not exist: " & r.str

  of rvmCacheKeyAlreadyExists:
    result = "key already exists: " & r.str

  of rvmFieldInavailable:
    result = r.str

  of rvmUnsupportedNonNil:
    case r.typ.kind
    of tyRef:
      result = "static expressions where the result contains non-nil 'ref'" &
              " values are not supported"
    of tyPtr, tyPointer:
      result = "static expressions where the result contains non-nil pointer" &
              " values are not supported"
    else:
      assert false

  of rvmCannotFindBreakTarget:
    result = "VM problem: cannot find 'break' target"

  of rvmNotUnused:
    result = "not unused"

  of rvmTooLargetOffset:
    result = "too large offset! cannot generate code for: " &
      r.sym.name.s

  of rvmCannotGenerateCode:
    result = "cannot generate code for: " &
      $r.ast

  of rvmCannotCast:
    result = "VM does not support 'cast' from " &
      $r.actualType.kind & " to " & $r.formalType.kind

  of rvmCannotEvaluateAtComptime:
    result = "cannot evaluate at compile time: " & r.ast.render

  of rvmTooManyRegistersRequired:
    result = "VM problem: too many registers required"

  of rvmCannotCallMethod:
    result = "cannot call method " & r.symstr & " at compile time"

  of rvmNotAField:
    result = "symbol is not a field (nskField)"

  of rvmOutOfRange:
    result = "unhandled exception: value out of range"

  of rvmErrInternal:
    result = r.str

  of rvmCallingNonRoutine:
    result = "NimScript: attempt to call non-routine: " & r.symstr

  of rvmGlobalError:
    result = r.str

  of rvmOpcParseExpectedExpression:
    result = "expected expression, but got multiple statements"

  of rvmIndexError:
    let (i, a, b) = r.indexSpec
    if b < a:
      result = "index out of bounds, the container is empty"
    else:
      result = "index " & $i & " not in " & $a & " .. " & $b

  of rvmQuit:
    result = "`quit` called with exit-code: " & $r.exitCode


proc reportFull*(conf: ConfigRef, r: VMReport): string =
  assertKind r
  result.add(
    conf.getContext(r.context),
    conf.prefix(r),
    reportBody(conf, r),
    conf.suffix(r)
  )

proc reportShort*(conf: ConfigRef, r: VMReport): string =
  # mostly created for nimsuggest
  assertKind r
  reportBody(conf, r) & suffixShort(conf, r)


proc reportBody*(conf: ConfigRef, r: CmdReport): string =
  assertKind r
  case CmdReportKind(r.kind)
  of rcmdCompiling:
    result = "CC: " & r.msg

  of rcmdLinking:
    result = conf.prefix(r) & conf.suffix(r)

  of rcmdFailedExecution:
    result = "execution of an external program '$1' failed with exit code '$2'" % [
      r.cmd, $r.code
    ]

  of rcmdExecuting:
    result = r.cmd

  of rcmdRunnableExamplesSuccess:
    result = "runnableExamples: " & r.msg

proc reportFull*(conf: ConfigRef, r: CmdReport): string =
  assertKind r
  reportBody(conf, r)

proc reportShort*(conf: ConfigRef, r: CmdReport): string =
  # mostly created for nimsuggest
  reportBody(conf, r)


proc reportBody*(conf: ConfigRef, r: Report): string =
  ## Generate main body of the report. Dispatches into implementations for
  ## specific report categories.
  assertKind r
  case r.category:
  of repLexer:    conf.reportBody(r.lexReport)
  of repParser:   conf.reportBody(r.parserReport)
  of repCmd:      conf.reportBody(r.cmdReport)
  of repSem:      conf.reportBody(r.semReport)
  of repDbgTrace: conf.reportBody(r.dbgTraceReport)
  of repDebug:    conf.reportBody(r.debugReport)
  of repInternal: conf.reportBody(r.internalReport)
  of repBackend:  conf.reportBody(r.backendReport)
  of repExternal: conf.reportBody(r.externalReport)
  of repVM:       conf.reportBody(r.vmReport)

proc reportFull*(conf: ConfigRef, r: Report): string =
  ## Generate full version of the report (location, severity, body,
  ## optional suffix)
  assertKind r
  case r.category:
  of repLexer:    conf.reportFull(r.lexReport)
  of repParser:   conf.reportFull(r.parserReport)
  of repCmd:      conf.reportFull(r.cmdReport)
  of repSem:      conf.reportFull(r.semReport)
  of repDbgTrace: conf.reportFull(r.dbgTraceReport)
  of repDebug:    conf.reportFull(r.debugReport)
  of repInternal: conf.reportFull(r.internalReport)
  of repBackend:  conf.reportFull(r.backendReport)
  of repExternal: conf.reportFull(r.externalReport)
  of repVM:       conf.reportFull(r.vmReport)

proc reportShort*(conf: ConfigRef, r: Report): string =
  ## Generate short report version of the report
  assertKind r
  case r.category:
  of repLexer:    conf.reportShort(r.lexReport)
  of repParser:   conf.reportShort(r.parserReport)
  of repCmd:      conf.reportShort(r.cmdReport)
  of repSem:      conf.reportShort(r.semReport)
  of repDbgTrace: conf.reportShort(r.dbgTraceReport)
  of repDebug:    conf.reportShort(r.debugReport)
  of repInternal: conf.reportShort(r.internalReport)
  of repBackend:  conf.reportShort(r.backendReport)
  of repExternal: conf.reportShort(r.externalReport)
  of repVM:       conf.reportShort(r.vmReport)

const
  rdbgTracerKinds* = {rdbgTraceDefined .. rdbgTraceEnd}
  traceDir = "nimCompilerDebugTraceDir"

var
  lastDot: bool = false
  traceFile: File
  fileIndex: int = 0

var counter = 0

proc rotatedTrace(conf: ConfigRef, r: Report) =
  ## Write out debug traces into separate files in directory defined by
  ## `nimCompilerDebugTraceDir`
  # Dispatch each `{.define(nimCompilerDebug).}` section into separate file
  assert r.kind in rdbgTracerKinds, $r.kind
  
  case r.kind
  of rdbgTraceStart, rdbgTraceEnd:
    # Rotated trace is constrolled by the define-undefine pair, not by
    # singular call to the nested recursion handling.
    discard
  of rdbgTraceDefined:
    if not dirExists(conf.getDefined(traceDir)):
      createDir conf.getDefined(traceDir)

    counter = 0
    let loc = r.location.get()
    let path = conf.getDefined(traceDir) / "compiler_trace_$1_$2.nim" % [
      conf.toFilename(loc).multiReplace({
        "/": "_",
        ".nim": ""
      }),
      $fileIndex
    ]

    echo "$1($2, $3): opening $4 trace" % [
      conf.toFilename(loc), $loc.line, $loc.col, path]

    traceFile = open(path, fmWrite)
    inc fileIndex
  of rdbgTraceUndefined:
    let loc = r.location.get()
    echo "$1($2, $3): closing trace, wrote $4 records" % [
      conf.toFilename(loc), $loc.line, $loc.col, $counter]

    close(traceFile)
  else:
    inc counter
    conf.excl optUseColors
    traceFile.write(conf.reportFull(r))
    traceFile.write("\n")
    traceFile.flushFile() # Forcefully flush the file in case of abrupt
    # exit by the compiler.
    conf.incl optUseColors


func astDiagToLegacyReport(conf: ConfigRef, diag: PAstDiag): Report {.inline.} =
  ## legacy because it converts a diag to a Sem/VM report wrapped in a Report.
  ## Why, you might ask? Well that's because reports are a mess and it'll take
  ## many messy intermediate steps to get out of this quagmire. So at present
  ## we do this conversion such that we can make use of `cli_reporter`...
  ## another things that needs to be broken down.

  template magicToStr(m: TMagic): string =
    # TODO: figure out what consistent approach to take for creating human
    #       readable strings out of magics
    case m
    of mLow: "low"
    of mHigh: "high"
    of mSizeOf: "sizeof"
    else: unreachable()

  let kind = astDiagToLegacyReportKind(diag)
      
  var
    semRep: SemReport
    vmRep: VMReport

  case diag.kind
  of adWrappedError:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemWrappedError,
        ast: diag.wrongNode)
  of adSemTypeMismatch:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemTypeMismatch,
        typeMismatch: diag.typeMismatch,
        ast: diag.wrongNode)
  of adSemIllegalConversion:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemIllegalConversion,
        typeMismatch: diag.typeMismatch,
        ast: diag.wrongNode)
  of adSemConflictingExportnims:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemConflictingExportnims,
        symbols: @[diag.wrongNode.sym, diag.conflict],
        ast: diag.wrongNode)
  of adSemAmbiguousIdentWithCandidates:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemAmbiguousIdentWithCandidates,
        sym: diag.candidateSyms[0],
        symbols: diag.candidateSyms,
        ast: diag.wrongNode)
  of adSemTypeNotAllowed:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemTypeNotAllowed,
        allowedType: diag.allowedType,
        ast: diag.wrongNode)
  of adSemAmbiguousIdent,
      adSemExpectedIdentifier,
      adSemModuleAliasMustBeIdentifier,
      adSemInvalidPragma,
      adSemStringLiteralExpected,
      adSemIntLiteralExpected,
      adSemOnOrOffExpected,
      adSemCallconvExpected,
      adSemUnknownExperimental,
      adSemPragmaOptionExpected,
      adSemUnexpectedPushArgument,
      adSemMismatchedPopPush,
      adSemExcessiveCompilePragmaArgs,
      adSemEmptyAsm,
      adSemLinePragmaExpectsTuple,
      adSemLocksPragmaExpectsList,
      adSemBorrowPragmaNonDot,
      adSemMisplacedEffectsOf,
      adSemMissingPragmaArg,
      adSemCannotPushCast,
      adSemCastRequiresStatement,
      adSemImportjsRequiresJs,
      adSemBitsizeRequires1248,
      adSemAlignRequiresPowerOfTwo,
      adSemNoReturnHasReturn,
      adSemMisplacedDeprecation,
      adSemFatalError,
      adSemNoUnionForJs,
      adSemBitsizeRequiresPositive,
      adSemExperimentalRequiresToplevel,
      adSemPragmaDynlibRequiresExportc,
      adSemWrongNumberOfArguments,
      adVmDerefNilAccess,
      adVmDerefAccessOutOfBounds,
      adVmDerefAccessTypeMismatch,
      adSemRawTypeMismatch,
      adSemExpressionCannotBeCalled,
      adSemCallInCompilesContextNotAProcOrField,
      adSemExpressionHasNoType,
      adSemTypeExpected,
      adSemIllformedAst,
      adSemInvalidExpression,
      adSemExpectedNonemptyPattern,
      adSemPragmaDisallowedForTupleUnpacking,
      adSemIllegalCompileTime,
      adSemThreadvarCannotInit,
      adSemLetNeedsInit,
      adSemConstExpressionExpected,
      adSemSelectorMustBeOfCertainTypes,
      adSemInvalidPragmaBlock,
      adSemConceptPredicateFailed,
      adSemDotOperatorsNotEnabled,
      adSemCallOperatorsNotEnabled,
      adSemUnexpectedPattern,
      adSemIsOperatorTakes2Args,
      adSemNoTupleTypeForConstructor,
      adSemInvalidOrderInArrayConstructor, # xxx: used to capture, but not report, count mismatch
      adSemStackEscape,
      adSemVarForOutParamNeeded,
      adSemExprHasNoAddress,
      adSemConstExprExpected,
      adSemDisallowedNilDeref,
      adSemCannotReturnTypeless,
      adSemExpectedValueForYield,
      adSemNamedExprExpected,
      adSemDisallowedTypedescForTupleField,
      adSemNamedExprNotAllowed,
      adSemNoReturnTypeDeclared,
      adSemReturnNotAllowed,
      adSemFieldAssignmentInvalidNeedSpace,
      adSemFieldAssignmentInvalid,
      adSemObjectConstructorIncorrect,
      adSemExpectedObjectType,
      adSemFoldOverflow,
      adSemFoldDivByZero,
      adSemFoldCannotComputeOffset,
      adSemExpectedIdentifierQuoteLimit,
      adSemExpectedRangeType:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: kind,
        ast: diag.wrongNode)
  of adSemInvalidRangeConversion:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemSemfoldInvalidConversion,
        ast: diag.wrongNode,
        typeMismatch: @[typeMismatch(diag.wrongNode[0].typ,
                                     diag.wrongNode.typ)])
  of adSemUseOrDiscardExpr:
    semRep = SemReport(
        location: some diag.undiscarded.info, # xxx: location override
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemUseOrDiscardExpr,
        ast: diag.wrongNode,
        wrongNode: diag.undiscarded)
  of adSemUnexpectedEqInObjectConstructor:
    semRep = SemReport(
        location: some diag.eqInfo,   # xxx: location override
        reportInst: diag.instLoc.toReportLineInfo,
        kind: kind,
        ast: diag.wrongNode)
  of adSemExpectedIdentifierInExpr:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemExpectedIdentifierInExpr,
        wrongNode: diag.notIdent,
        ast: diag.wrongNode)
  of adSemExpectedIdentifierWithExprContext:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemExpectedIdentifierWithExprContext,
        wrongNode: diag.expr,
        ast: diag.wrongNode)
  of adSemUndeclaredIdentifier:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemUndeclaredIdentifier,
        str: diag.name,
        spellingCandidates: diag.spellingCandidates,
        recursiveDeps: diag.recursiveDeps,
        ast: diag.wrongNode)
  of adSemOnlyDeclaredIdentifierFoundIsError:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemOnlyDeclaredIdentifierFoundIsError,
        str: diag.identName,
        ast: diag.wrongNode,
        wrongNode: diag.err)
  of adSemCannotImportItself:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemCannotImportItself,
        sym: diag.selfModule,
        ast: diag.wrongNode)
  of adSemIllegalCustomPragma:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemIllegalCustomPragma,
        sym: diag.customPragma,
        ast: diag.wrongNode)
  of adSemWrongIdent:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemWrongIdent,
        expectedIdents: diag.allowedIdents,
        ast: diag.wrongNode)
  of adSemAsmEmitExpectsStringLiteral:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemIllformedAst,
        str: "Expected string literal for asm/emit statement [1] but AST" &
              "contains '$1'" % $diag.unexpectedKind,
        ast: diag.wrongNode)
  of adSemRaisesPragmaExpectsObject,
      adSemCannotInferTypeOfLiteral,
      adSemProcHasNoConcreteType,
      adSemCannotAssignTo:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: kind,
        typ: diag.wrongType,
        ast: diag.wrongNode)
  of adSemLocksPragmaBadLevelRange:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemLocksPragmaBadLevel,
        str: "integer must be within 0.." & $MaxLockLevel,
        ast: diag.wrongNode)
  of adSemLocksPragmaBadLevelString:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemLocksPragmaBadLevel,
        str: "invalid string literal for locks pragma (only allowed string is \"unknown\")",
        ast: diag.wrongNode)
  of adSemInvalidExtern:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemInvalidExtern,
        sym: diag.compProcToBe,
        externName: diag.externName,
        ast: diag.wrongNode)
  of adSemPragmaRecursiveDependency:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemPragmaRecursiveDependency,
        sym: diag.userPragma,
        ast: diag.wrongNode)
  of adSemCustomUserError:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemCustomUserError,
        str: diag.errmsg,
        ast: diag.wrongNode)
  of adSemImplicitPragmaError:
    semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemImplicitPragmaError,
        sym: diag.implicitPragma,
        ast: diag.wrongNode)
  of adSemIncompatibleDefaultExpr:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemIncompatibleDefaultExpr,
      sym: diag.formal,
      ast: diag.wrongNode)
  of adVmUnsupportedNonNil:
    vmRep = VMReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rvmUnsupportedNonNil,
      typ: diag.unsupported,
      ast: diag.wrongNode)
  of adCyclicTree:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rvmUnsupportedNonNil,
      ast: diag.cyclic)
  of adSemCallTypeMismatch:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCallTypeMismatch,
      ast: diag.wrongNode,
      spellingCandidates: diag.callSpellingCandidates,
      callMismatches: diag.callMismatches)
  of adSemCallNotAProcOrField:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCallNotAProcOrField,
      ast: diag.wrongNode,
      explicitCall: true,
      notProcOrField: diag.notProcOrField,
      unexpectedCandidate: diag.unexpectedCandidates,
      spellingCandidates: diag.spellingAlts)
  of adSemImplicitDotCallNotAProcOrField:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCallNotAProcOrField,
      ast: diag.wrongNode,
      explicitCall: false,
      notProcOrField: diag.notProcOrField,
      typ: diag.wrongNode[1].typ,
      unexpectedCandidate: diag.unexpectedCandidates,
      spellingCandidates: diag.spellingAlts)
  of adSemUndeclaredField:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemUndeclaredField,
      ast: diag.wrongNode,
      sym: diag.givenSym,
      typ: diag.symTyp)
  of adSemCannotInstantiate:
    semRep = SemReport(
      location: some diag.callLineInfo,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCannotInstantiate,
      ast: diag.wrongNode)
  of adSemWrongNumberOfGenericParams:
    semRep = SemReport(
      location: some diag.gnrcCallLineInfo,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemWrongNumberOfGenericParams,
      ast: diag.wrongNode,
      countMismatch: diag.countMismatch)
  of adSemCalleeHasAnError:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCalleeHasAnError,
      ast: diag.wrongNode,
      sym: diag.callee)
  of adSemIllformedAstExpectedPragmaOrIdent:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemIllformedAst,
      ast: diag.wrongNode,
      str: "Expected postfix, pragma or indent, but found " &
              $diag.wrongNode.kind)
  of adSemIllformedAstExpectedOneOf:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemIllformedAst,
      ast: diag.wrongNode,
      str: createSemIllformedAstMsg(diag.wrongNode, diag.expectedKinds))
  of adSemImplementationExpected:
    semRep = SemReport(
      location: some diag.routineDefStartPos,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.routineSym)
  of adSemImplementationNotAllowed:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.symWithImpl)
  of adSemCannotConvertToRange:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      typ: diag.convTyp,
      str: $diag.floatVal)
  of adSemProveInit:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.unproven)
  of adSemDifferentTypeForReintroducedSymbol:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemDifferentTypeForReintroducedSymbol,
      ast: diag.wrongNode,
      sym: diag.reintrod,
      typeMismatch: @[SemTypeMismatch(actualType: diag.foundTyp,
                                      formalType: diag.reintrod.typ)])
  of adSemTypeKindMismatch:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemTypeKindMismatch,
      ast: diag.wrongNode,
      typeMismatch: @[SemTypeMismatch(actualType: diag.givenTyp,
                                      formalTypeKind: diag.expectedTypKinds)])
  of adSemCannotCastTypes:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCannotCastTypes,
      ast: diag.wrongNode,
      typeMismatch: diag.typeMismatch)
  of adSemWrongNumberOfVariables:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemWrongNumberOfVariables,
      ast: diag.wrongNode,
      countMismatch: (diag.varsExpected, diag.varsGiven))
  of adSemConstantOfTypeHasNoValue:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.constSym)
  of adSemTypeConversionArgumentMismatch:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemTypeConversionArgumentMismatch,
      ast: diag.wrongNode,
      countMismatch: (1, diag.convArgsRecvd))
  of adSemCannotBeConvertedTo:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCannotBeConvertedTo,
      ast: diag.inputVal,              # xxx: ignores `diag.wrongNode`
      typ: diag.targetTyp)
  of adSemCannotCastToNonConcrete:
    semRep = SemReport(
      location: some diag.wrongNode[0].info, # xxx: location override
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCannotCastToNonConcrete,
      ast: diag.wrongNode,
      typ: diag.wrongType)
  of adSemFoldRangeCheckForLiteralConversionFailed:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCantConvertLiteralToRange,
      ast: diag.wrongNode,              # xxx: ignores `diag.wrongNode`
      wrongNode: diag.inputLit)
  of adSemIndexOutOfBoundsStatic:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemStaticOutOfBounds,
      ast: diag.wrongNode,
      indexSpec: (usedIdx: getInt(diag.staticIndex),
                  minIdx: if diag.staticCollection.kind == nkBracket:
                            firstOrd(conf, diag.staticCollection.typ)
                          else:
                            toInt128(0),
                  maxIdx: toInt128(diag.staticCollection.len - 1)))
  of adSemMagicExpectTypeOrValue:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      str: magicToStr(diag.magic))
  of adSemLowHighInvalidArgument:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      typ: diag.invalidTyp,
      str: magicToStr(diag.highLow))
  of adSemUnknownIdentifier,
      adSemStaticFieldNotFound:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.unknownSym)
  of adSemInvalidTupleConstructorKey:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.invalidKey) # xxx: wrongNode/ast override
  of adSemExpectedOrdinalArrayIdx:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemExpectedOrdinal,
      ast: diag.indexExpr,
      wrongNode: diag.nonOrdInput, # xxx: wrongNode/ast override
      typ: diag.nonOrdInput.typ)
  of adSemIndexOutOfBounds:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemIndexOutOfBounds,
      ast: diag.wrongNode,
      typ: diag.ordRange,
      countMismatch: (toInt(lastOrd(conf, diag.ordRange)),
                      diag.outOfBoundsIdx))
  of adSemExpectedOrdinal:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemExpectedOrdinal,
      ast: diag.wrongNode,
      typ: diag.nonOrdTyp)
  of adSemRecursiveDependencyIterator:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.recurrCallee)
  of adSemCallIndirectTypeMismatch:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      typ: diag.indirCallTyp)
  of adSemSystemNeeds:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      str: diag.sysIdent)
  of adSemLocalEscapesStackFrame:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.escCtx)
  of adSemImplicitAddrIsNotFirstParam:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.exprRoot)
  of adSemYieldExpectedTupleConstr:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      typ: diag.tupleTyp)
  of adSemFieldInitTwice:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      str: diag.dupFld.s)
  of adSemCannotMixTypesAndValuesInTuple:
    semRep = SemReport(
      location: some diag.wrongFldInfo, # xxx: location override
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode)
  of adSemFieldNotAccessible:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      sym: diag.inaccessible)
  of adSemFieldOkButAssignedValueInvalid:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.initVal,
      sym: diag.targetField)
  of adSemObjectRequiresFieldInitNoDefault:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemObjectRequiresFieldInitNoDefault,
      ast: diag.wrongNode,
      symbols: diag.missing,
      typ: diag.objTyp)
  of adSemDistinctDoesNotHaveDefaultValue:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      typ: diag.distinctTyp)
  of adSemExpectedObjectOfType:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      typ: diag.expectedObjTyp)
  of adSemInvalidIntDefine,
      adSemInvalidBoolDefine:
    semRep = SemReport(
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      str: diag.invalidDef)
  of adSemDefNameSym:
    case diag.defNameSymData.kind
    of adSemDefNameSymExpectedKindMismatch:
      semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemSymbolKindMismatch,
        sym: diag.wrongNode.sym,
        expectedSymbolKind: {diag.defNameSymData.expectedKind},
        ast: diag.wrongNode)
    of adSemDefNameSymIllformedAst:
      semRep = SemReport(
        location: some diag.location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rsemExpectedIdentifier,
        ast: diag.wrongNode)
    of adSemDefNameSymIdentGenFailed:
      return astDiagToLegacyReport(conf, diag.defNameSymData.identGenErr.diag)
    of adSemDefNameSymExistingError:
      return astDiagToLegacyReport(conf, diag.wrongNode.diag)
  of adSemCompilerOptionInvalid,
      adSemDeprecatedCompilerOpt:
    semRep = SemReport(
      location: std_options.some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: kind,
      ast: diag.wrongNode,
      str: diag.badCompilerOpt.getStr)
  of adSemCompilerOptionArgInvalid:
    semRep = SemReport(
      location: std_options.some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemCompilerOptionArgInvalid,
      ast: diag.wrongNode,
      str: diag.forCompilerOpt.getStr,
      badCompilerOptArg: diag.badCompilerOptArg.getStr,
      allowedOptArgs: diag.possibleValidArgs)
  of adSemDeprecatedCompilerOptArg:
    semRep = SemReport(
      location: std_options.some diag.location,
      reportInst: diag.instLoc.toReportLineInfo,
      kind: rsemDeprecatedCompilerOptArg,
      ast: diag.wrongNode,
      str: diag.compilerOpt.getStr,
      compilerOptArg: diag.compilerOptArg.getStr)
  of adVmError:
    let
      kind = diag.vmErr.kind.astDiagVmToLegacyReportKind()
      location = diag.location

    case kind
    of rvmCannotCast, rvmIllegalConvFromXToY:
      vmRep = VMReport(
        kind: kind,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo,
        actualType: diag.vmErr.actualType,
        formalType: diag.vmErr.formalType)
    of rvmIndexError:
      vmRep = VMReport(
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: kind,
        indexSpec: diag.vmErr.indexSpec)
    of rvmCannotSetChild, rvmCannotAddChild, rvmCannotGetChild,
        rvmUnhandledException, rvmNoType, rvmNodeNotASymbol:
      case diag.vmErr.kind
      of adVmArgNodeNotASymbol:
        vmRep = VMReport(
          location: some diag.vmErr.argAst.info,
          reportInst: diag.instLoc.toReportLineInfo,
          kind: kind,
          str: diag.vmErr.callName & "()",
          ast: diag.vmErr.argAst)
      else:
        vmRep = VMReport(
          location: some location,
          reportInst: diag.instLoc.toReportLineInfo,
          kind: kind,
          ast: diag.vmErr.ast)
    of rvmUserError:
      vmRep = VMReport(
        kind: kind,
        str: diag.vmErr.errMsg,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo)
    of rvmErrInternal, rvmNilAccess, rvmIllegalConv, rvmFieldInavailable,
        rvmFieldNotFound, rvmCacheKeyAlreadyExists, rvmMissingCacheKey:
      vmRep = VMReport(
        kind: kind,
        str: diag.vmErr.msg,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo)
    of rvmNotAField:
      vmRep = VMReport(
        kind: kind,
        sym: diag.vmErr.sym,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo)
    else:
      vmRep = VMReport(
        kind: kind,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo)
  of adVmGenError:
    template magicToString(m: TMagic): string =
      # TODO: duplicated above, consolidate
      case m
      of mSizeOf:   "sizeOf"
      of mAlignOf:  "align"
      of mOffsetOf: "offset"
      else:         $m

    let
      kind = diag.vmGenErr.kind.astDiagVmGenToLegacyReportKind()
      location = diag.location

    case diag.vmGenErr.kind
    of adVmGenCannotCast:
      vmRep = VMReport(
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: rvmCannotCast,
        actualType: diag.vmGenErr.actualType,
        formalType: diag.vmGenErr.formalType)
    of adVmGenCodeGenUnhandledMagic,
        adVmGenMissingImportcCompleteStruct:
      vmRep = VMReport(
        str: magicToString(diag.vmGenErr.magic),
        kind: kind,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo)
    of adVmGenCodeGenGenericInNonMacro,
        adVmGenCodeGenUnexpectedSym,
        adVmGenCannotImportc,
        adVmGenCannotCallMethod,
        adVmGenTooLargeOffset:
      vmRep = VMReport(
        str: case diag.vmGenErr.kind
              of adVmGenCodeGenGenericInNonMacro:
                "Attempt to generate VM code for generic parameter in non-macro proc"
              of adVmGenCodeGenUnexpectedSym:
                "Unexpected symbol for VM code - " & $diag.vmGenErr.sym.kind
              else:
                "",
        sym: diag.vmGenErr.sym,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo,
        kind: kind)
    of adVmGenBadExpandToAstArgRequired:
      vmRep = VMReport(
        str: "expandToAst requires 1 argument",
        kind: kind,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo)
    of adVmGenNotUnused,
        adVmGenNotAFieldSymbol,
        adVmGenCannotGenerateCode,
        adVmGenCannotEvaluateAtComptime,
        adVmGenInvalidObjectConstructor:
      vmRep = VMReport(
        ast: diag.vmGenErr.ast,
        kind: kind,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo)
    else:
      vmRep = VMReport(
        kind: kind,
        location: some location,
        reportInst: diag.instLoc.toReportLineInfo)
  of adVmQuit:
    vmRep = VMReport(
      kind: rvmQuit,
      exitCode: diag.vmExitCode,
      location: some diag.location,
      reportInst: diag.instLoc.toReportLineInfo)

  result =
    case kind
    of repVMKinds:
      if diag.kind == adVmGenError and diag.duringJit or
          diag.kind == adVmError:
        let st =
          case diag.kind
          of adVmGenError: diag.vmGenTrace
          of adVmError:    diag.vmTrace
          else:            unreachable()

        {.cast(noSideEffect).}: # side-effect analysis fails
          vmRep.trace =
            (ref VMReport)(kind: rvmStackTrace,
                      currentExceptionA: st.currentExceptionA,
                      currentExceptionB: st.currentExceptionB,
                      stacktrace: st.stacktrace,
                      skipped: st.skipped,
                      location: some st.location,
                      reportInst: toReportLineInfo(st.instLoc))

      Report(category: repVM, vmReport: vmRep)
    of repSemKinds:
      Report(category: repSem, semReport: semRep)
    else:
      unreachable()

proc legacyReportBridge*(conf: ConfigRef, diag: PAstDiag): Report =
  ## Converts AST Diagnostics to legacy reports; meant to be assigned to a
  ## `ConfigRef` as a procedural value and act as a callback.
  # Not the ideal place for this proc, but I'd rather keep the reports mess in
  # as few files as possible
  # TODO: will use conf soon
  astDiagToLegacyReport(conf, diag)

proc reportHook*(conf: ConfigRef, r: Report): TErrorHandling =
  ## Default implementation of the report hook. Dispatches into
  ## `reportBody` for report, which then calls respective (for each report
  ## category) `reportBody` overloads defined above
  assertKind r
  let wkind = conf.writabilityKind(r)
  # debug reports can be both enabled and force enabled, and sem tracer
  # first needs to be checked for the trace group rotation. So adding a
  # case here is not really useful, since report writability kind does not
  # necessarily dictate how it is written, just whether it can/must/cannot
  # be written.
  if wkind == writeDisabled:
    return
  else:
    if r.kind in rdbgTracerKinds and conf.isDefined(traceDir):
      rotatedTrace(conf, r)
    elif wkind == writeForceEnabled:
      echo conf.reportFull(r)
    elif r.kind == rsemProcessing and conf.hintProcessingDots:
      # xxx: the report hook is handling processing dots output, why? this whole
      #      infrastructure is overwrought. seriously, they're not hints, they're
      #      progress indicators.
      conf.write(".")
      lastDot = true
    else:
      var msg: seq[string]
      if lastDot:
        msg.add("")
        lastDot = false

      if conf.hack.reportInTrace:
        var indent {.global.}: int

        case r.kind:
        of rdbgTracerKinds:
          if r.kind == rdbgTraceStep:
            indent = r.dbgTraceReport.semstep.level
          msg.add(conf.reportFull(r))
        of repSemKinds:
          if indent > 0:
            for line in conf.reportFull(r).splitLines():
              msg.add("  ]" & repeat("  ", indent) & " ! " & line)
          else:
            msg.add(conf.reportFull(r))
        else:
          msg.add(conf.reportFull(r))
      else:
        msg.add(conf.reportFull(r))

      if conf.hack.bypassWriteHookForTrace:
        for item in msg:
          echo item
      else:
        for item in msg:
          conf.writeln(item)
  if r.category in {repInternal, repExternal, repBackend} and
      conf.severity(r) in {rsevFatal, rsevError}:
    result = doAbort
