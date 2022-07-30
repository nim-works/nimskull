## New-style error message reporter

import
  experimental/[
    diff,
    colordiff,
    colortext
  ],
  std/[
    algorithm,
    sequtils,
    strformat,
    strutils,
    enumerate
  ],
  compiler/front/[
    options
  ],
  compiler/ast/[
    ast_types,
    ast,
    reports,
    astalgo,
    renderer,
    typesrenderer
  ],
  compiler/utils/[
    astrepr
  ]

import cli_reporter as old

type
  StringMismatchCandidate* = object
    ## Description of a single string edit operation
    distance*: int ## Edit distance between target (provided) and input string (not provided)
    edits*: seq[SeqEdit] ## Sequence of edit operations to convert input string into target
    target*: string ## Target string

proc stringMismatchCandidates*(
    input: string,
    expected: openArray[string]
  ): seq[StringMismatchCandidate] =

  var results: seq[tuple[
    edits: tuple[distance: int, operations: seq[SeqEdit]],
    target: string
  ]]

  for str in expected:
    if str == input:
      return @[]

    else:
      let (distance, edits) = levenshteinDistance(input.toSeq(), str.toSeq())
      result.add StringMismatchCandidate(
        distance: distance,
        edits: edits,
        target: str
      )

type ItDiff = tuple[it: StringMismatchCandidate, idx: int]
proc mismatchCandidates*(
      input: string,
      expected: openArray[string]): seq[ItDiff] =

   let expected = deduplicate(expected)
   for idx, it in enumerate(stringMismatchCandidates(input, expected)):
     result.add((it, idx))

   return result.sortedByIt(it.it.distance)

proc countEdits(best: StringMismatchCandidate): int =
  for edit in best.edits:
    if edit.kind != sekKeep:
      inc result

proc inlineFormatter(): DiffFormatConf =
  var fmt = diffFormatter(false)
  let default = fmt.formatChunk
  fmt.lineSplit = proc(s: string): seq[string] = mapIt(s, $it)
  fmt.formatChunk = proc(
    text: string, mode, secondary: SeqEditKind, inline: bool): ColText =
    if mode == sekReplace and inline:
      if secondary == sekDelete:
        text + fgRed

      else:
        text + fgGreen

    else:
      default(text, mode, secondary, inline)

  return fmt


proc formatEdit*(input: string, best: StringMismatchCandidate): ColText =
  var fmt = inlineFormatter()
  coloredResult()
  if countEdits(best) < min(3, input.len div 2):
    add formatInlineDiff(input, best.target, fmt)

  else:
    add input + fgRed
    add " -> "
    add best.target + fgGreen

proc formatAlternatives(input: string, results: seq[StringMismatchCandidate]): ColText =
  coloredResult()
  var fmt = inlineFormatter()
  for idx, alt in results:
    if idx > 0:
      add " "

    if countEdits(alt) < min(3, input.len div 2):
      add formatInlineDiff(input, alt.target, fmt)

    else:
      add alt.target + fgGreen

    add "?"

proc didYouMean(input: string, candidates: seq[ItDiff]): ColText =
  coloredResult()
  add "did you mean to use "
  add formatEdit(input, candidates[0].it)
  if 1 < candidates.len:
    add " ("
    add formatAlternatives(input, candidates[1 ..^ 1].mapIt(it.it))
    add ")"

  add "?"


proc stringMismatchMessage*(
    input: string,
    expected: openArray[string],
    fixSuggestion: bool = true,
    showAll: bool = true,
  ): ColText =

  coloredResult()

  let results = mismatchCandidates(input, expected)
  if expected.len == 0:
    add "No matching alternatives"
    return

  let best = results[0].it

  if best.distance > int(input.len.float * 0.8):
    add "no close matches to "
    add input + fgRed
    add ", possible alternative(s): "
    var first = true
    for it in results[0 .. min(results.high, 3)]:
      if not first: add " or "
      first = false
      add it.it.target + fgYellow

  else:
    add "Did you mean to use '"
    add best.target + fgYellow
    add "'?'"

    if fixSuggestion:
      add formatEdit(input, best)

    if showAll and expected.len > 1:
      add "\n  ("
      add formatAlternatives(input, results[1..^1].mapIt(it.it))
      add ")"

## Large rank/cost value means the error was severe (error in the first
## argument, completely different types). Smaller cost means the error was
## pretty minor (typo in the named argument, mismatch in the 8th position)

const cost = (
  genericLayer: 1 shl 10, # Base value of the first level of the type
                          # mismatch. When recursing into generics cost
                          # decreases exponentially, so `int-float` mismatch
                          # is ranked higher than `seq[int]-seq[float]`.

  literalMultiplier: 0.3  # Multiplier applied when provided expression has
                          # type mismatched /and/ it was a literal value.
                          # For example function expects `uint8` and `int`
                          # literal was provided.
)

type
  Arg = PNode
  ArgList = seq[Arg] ## List of the procedure arguments - either passed,
                       ## or expected

  RankedCallMismatch = object
    sem: SemCallMismatch ## Original call mismatch
    rank: int ## Final rank value
    mismatches: seq[ArgCompare]

  ArgCompare = object
    ## Structural diff between two types
    wanted, found: PType
    rank: int
    nested: seq[ArgCompare]

func rankMismatch(wanted, found: Arg): ArgCompare =
  assert not isNil(wanted.typ)
  assert not isNil(found.typ)
  ArgCompare(wanted: wanted.typ, found: found.typ)

func typoCost(used: string, expected: seq[string]): int =
  # Specific details of the typo correction costs can be checked for later,
  # since candidate that has /only/ typo in the name would have a low cost
  # anyway (all types match, the only error is in the named parameter).
  12

func argSyms(procType: PType): seq[PNode] =
  procType.n.sons[1 ..^ 1]

proc updateRank*(mis: var RankedCallMismatch, args: ArgList) =
  let sem = mis.sem
  let argSyms = sem.target.typ.argSyms()
  let matchingLen = min(argSyms.len(), args.len())
  for idx in 0 ..< matchingLen:
    var mismatch = rankMismatch(argSyms[idx], args[idx])
    mis.mismatches.add mismatch
    if idx <= sem.firstMismatch.pos:
      # Cost is declreased from the first mismatch position - error in the
      # first argument is more likely
      mis.rank += mismatch.rank * ((matchingLen - idx) - sem.firstMismatch.pos)

  case sem.firstMismatch.kind:
    of kTypeMismatch:
      # Type mismatch arguments are handled uniformly and decreasing
      # ranking based on the first mismatch position
      discard

    of kUnknownNamedParam:
      mis.rank += typoCost(
        $sem.firstMismatch.arg,
        sem.target.typ.argSyms().mapIt(it.sym.name.s))

    else:
      discard


proc toRanked(
    mis: seq[SemCallMismatch],
    args: ArgList
  ): seq[RankedCallMismatch] =
  for m in mis:
    result.add RankedCallMismatch(sem: m)
    result[^1].updateRank(args)

proc typeHeadName(t: PType, withModule: bool = false): string =
  case t.kind:
    of tyGenericBody:
      result = t.lastSon.typeToString()

    of tyInt: result = "int"

    else:
      result = t.typeToString()

proc format(target: PType, other: PType = nil): ColText =
  ## Custom type format implementation, used *specifically* for error
  ## reporting - it might fall back to general type rendering logic from
  ## time to time, but otherwise is specifically geared towards
  ## human-readable, colored representation.
  coloredResult()
  const
    ctarget = fgGreen
    cother = fgRed

  proc aux(target, other: PType) =
    if other.isNil:
      add typeToString(target)
      return

    else:
      var tname = target.typeHeadName()
      var oname = other.typeHeadName()
      if tname == oname:
        add tname

      elif (tname, oname) in [
        ("float", "float64"),
        ("float", "float32"),
      ]:
        add tname

      elif target.kind == tyTuple and other.kind == tyTuple:
        let
          tnamed = target.isNamedTuple()
          onamed = other.isNamedTuple()

        add tern(tnamed, "tuple[", "(")

        for idx in 0 ..< min(len(target), len(other)):
          if 0 < idx: add ", "
          let
            tfield = tern(tnamed, target.n[idx].getIdentStr(), "")
            ofield = tern(onamed, other.n[idx].getIdentStr(), "")

          if tnamed:
            if tfield != ofield and onamed:
              add tfield + ctarget
              add " != "
              add ofield + cother

            else:
              add tfield

            add ": "

          aux(target[idx], other[idx])

        if len(target) < len(other):
          for sub in len(target) ..< len(other):
            if 0 < sub: add ", "
            if onamed: add (other.n[sub].getIdentStr() & ": ") + ctarget
            add format(other[sub]) + ctarget

        if len(other) < len(target):
          for sub in len(other) ..< len(target):
            if 0 < sub: add ", "
            if tnamed: add (target.n[sub].getIdentStr() & ": ") + cother
            add format(target[sub]) + cother

        add tern(tnamed, "]", ")")

      else:
        add &"{tname}" + cother
        add " != "
        add &"{oname}" + ctarget

    case target.kind:
      of tyGenericBody, tyBuiltInTypeClass:
        add "["
        for idx in 0 ..< len(target):
          if idx > 0: add ", "
          aux(target[idx], other[idx])

        add "]"

      else:
        discard

  aux(target, other)

  endResult()

proc formatProc(p: PSym): ColText =
  coloredResult()

  add "proc "
  add p.name.s + fgGreen
  add "("
  for idx, arg in pairs(p.typ.n.sons[1 ..^ 1]):
    if idx > 0: add ", "
    add arg.getIdentStr() + fgCyan
    add ": "
    add arg.typ.format()

  add ")"

  endResult()



proc format(arg: ArgCompare): ColText =
  assert not isNil(arg.wanted)
  assert not isNil(arg.found)
  result.add format(arg.wanted, arg.found)

proc format(mis: RankedCallMismatch): ColText =
  ## Format single call mismatch instance - procedure with incorrect
  ## argument types, unknown named parameter.
  coloredResult()

  let sem = mis.sem
  case sem.firstMismatch.kind:
    of kUnknownNamedParam:
      # This part deals with typos in the arguments - current
      # implementation provides rather strange-looking elements
      add "unknown named argument - "
      add sem.target.formatProc()
      add ":\n"
      add stringMismatchMessage(
        $sem.firstMismatch.arg,
        sem.target.typ.argSyms().mapIt(it.getIdentStr())).
        indent(2)

    of kTypeMismatch:
      # Argment type mismatch message
      add "("
      var first = true
      let syms = sem.target.typ.argSyms()
      for idx, argMis in mis.mismatches:
        if not first: add ", "
        first = false
        add $syms[idx] + fgCyan
        add ": "
        add argMis.format()

      add ")"

    else:
      discard

  endResult()


func groupByIdx(mis: sink seq[SemCallMismatch]): tuple[
    byType: seq[seq[SemCallMismatch]],
    other: seq[SemCallMismatch]
  ] =
  ## Split overload candidates into ones that failed because of the
  ## argument mismatch, and everything else.

  var mis = mis
  mis.sort(proc(a, b: SemCallMismatch): int =
             cmp($a.firstMismatch.arg, $b.firstMismatch.arg))

  var prev = -1
  for mis in mis:
    if mis.firstMismatch.kind != kTypeMismatch:
      result.other.add mis

    elif mis.firstMismatch.pos != prev:
      prev = mis.firstMismatch.pos
      result.byType.add @[mis]

    else:
      result.byType[^1].add mis

proc reportCallMismatch(conf: ConfigRef, r: SemReport): ColText =
  let args = mapIt(r.ast, it)[1 .. ^1]
  # First group call candidates by argument index and mismatch kind. This
  # allows to show 'but expression is of type' and other diagnostics only
  # once per argument position.
  let (byType, other) = r.callMismatches.groupByIdx()

  let name =
    if 0 < byType.len:
      byType[0][0].target.getIdentStr()

    else:
      other[0].target.getIdentStr()

  coloredResult()
  add "Cannot call "
  add name + fgRed
  add " due to type mismatch failures:"
  for group in byType:
    # Convert each group's items into ranked nodes and sort candidates
    # within the byType using more advanced heuristics.
    let first = group[0].firstMismatch
    let expr = first.arg
    add "\n  Mismatch for argument #"
    add $first.pos
    add "\n  Expression '"
    add $first.arg
    add "' is of type "
    add first.arg.typ.format() + fgRed
    add ", but expected any of\n"
    for mis in group.toRanked(args).sortedByIt(-it.rank):
      add "\n    "
      add mis.format()

  block:
    let other = other.toRanked(args).sortedByIt(-it.rank)
    if 0 < len(other):
      add "\n\nor other mismatches:"
      for mis in other:
        add "\n  "
        add mis.format()


proc objFields(obj: PNode): seq[PNode] =
  ## Collect list of fields from the object.
  # I failed to understand how field lookup is perofmed, it is all mixed in
  # with the general overload resolution, so I have almost no chances on
  # decoding this abomination right now
  proc aux(n: PNode, r: var seq[PNode]) =
    case n.kind:
      of nkTypeDef:
        for sub in n[2][2]:
          aux(sub, r)

      of nkRecList:
        for sub in n:
          aux(n, r)


      of nkIdentDefs:
        for it in n.sons[0..^3]:
          r.add it

      else:
        debug obj
        assert false, $n.kind

  aux(obj, result)

proc reportBody*(conf: ConfigRef, r: SemReport): ColText =
  coloredResult()
  echo "Semantic report body ", r.kind
  case r.kind:
    of rsemCallTypeMismatch:
      add reportCallMismatch(conf, r)

    of rsemUndeclaredIdentifier:
      add "undeclared identifier: '" & r.str & "' - "
      let candidates = mismatchCandidates(
        r.str, mapIt(r.spellingCandidates, it.sym.getIdentStr()))
      if 0 < candidates.len:
        add didYouMean(r.str, candidates)
      else:
        add "no matching alternatives"

    of rsemInvalidOrderInEnum:
      addf(
        "invalid order for enum field '$#': got $#, but expected $# or more",
        r.sym.getIdentStr() + fgGreen,
        $r.countMismatch.got + fgCyan,
        $r.countMismatch.expected + fgCyan
      )

    of rsemExpectedOrdinal:
      add "ordinal type expected; given "
      add format(r.typ)

    of rsemHasSideEffects:
      if r.sideEffectTrace[0].trace == ssefParameterMutation:
        result.add old.reportBody(conf, r)

      else:
        result.addf("'$1' can have side effects", r.symstr)
        for part in r.sideEffectTrace:
          let s = part.isUnsafe
          let u = part.unsafeVia
          add "\n"
          addf(repeat(">", part.level))
          add " "
          add conf.toStr(part.location)
          addf(" '$#' ", s.getIdentStr() + fgGreen)

          case part.trace:
            of ssefUsesGlobalState:
              addf(
                "$# '$#' ($# in $#)",
                "accesses external" + fgYellow,
                u.getIdentStr() + fgGreen,
                toHumanStr(u.kind),
                conf.toStr(u.info))

            of ssefCallsSideEffect:
              addf(
                "calls '$#' ($# in $#)",
                u.getIdentStr() + fgGreen,
                toHumanStr(u.kind),
                conf.toStr(u.info))

            of ssefCallsViaHiddenIndirection:
              addf(
                "calls routine via $#",
                "hidden pointer indirection" + fgYellow)

            of ssefCallsViaIndirection:
              addf(
                "calls routine via $#",
                "pointer indirection" + fgYellow)

            of ssefParameterMutation:
              assert false, "Must be handled as a standalone effect"

    of rsemDuplicateCaseLabel:
      addf(
        "duplicate case label: $# on line $# overlaps with $# on line $#",
        $r.overlappingGroup + fgRed,
        $r.overlappingGroup.info.line,
        $r.ast + fgGreen,
        $r.ast.info.line
      )

    of rsemUndeclaredField:
      # TODO check if field had been exported or not
      let flds = objFields(r.sym.ast).mapIt(it.getIdentStr())
      let candidates = mismatchCandidates(r.str, flds)
      addf(
        "undeclared field '$#' for type $# - $#",
        $r.ast,
        $r.sym.typ,
        tern(
          candidates.len == 0,
          "object has no fields" + fgDefault,
          didYouMean($r.ast, candidates)
        )
      )


      # debug r.ast
      # debug r.sym.typ

    else:
      add old.reportBody(conf, r)

proc getContext(conf: ConfigRef, ctx: seq[ReportContext]): ColText =
  ## Format report context message
  coloredResult()
  for ctx in items(ctx):
    # Instantiation reports have their own context message information
    add(old.toStr(conf, ctx.location))
    case ctx.kind:
      of sckInstantiationOf:
        add " instantiation of "
        add ctx.entry.getIdentStr()
        if 0 < ctx.params.data.len:
          add "["
          var first = true
          for pair in ctx.params.data:
            # Instantiation context has random items mixed in - not
            # everything is a type or a symbol, so we need to filter out
            # unwanted elements here.
            if pair.key of PType:
              if not first:
                add ", "
              first = false
              add pair.key.PType().format() + fgYellow
              add " = "

            if pair.val of PType:
              add pair.val.PType().format() + fgGreen

          add "]"
        add " from here\n"

      of sckInstantiationFrom:
        add(" template/generic instantiation from here\n")

proc reportFull*(conf: ConfigRef, r: SemReport): ColText =
  if r.kind == rsemProcessing and conf.hintProcessingDots:
    result.add "."
    return

  result.add conf.getContext(r.context)
  result.add reportBody(conf, r)
  result.add "\n"
  result.add conf.suffix(r)

proc reportFull*(conf: ConfigRef, r: LexerReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: CmdReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: ParserReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: DebugReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: InternalReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: BackendReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: ExternalReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: Report): ColText =
  ## Generate full version of the report (location, severity, body,
  ## optional suffix)
  case r.category:
    of repLexer:    result = conf.reportFull(r.lexReport)
    of repParser:   result = conf.reportFull(r.parserReport)
    of repCmd:      result = conf.reportFull(r.cmdReport)
    of repSem:      result = conf.reportFull(r.semReport)
    of repDebug:    result = conf.reportFull(r.debugReport)
    of repInternal: result = conf.reportFull(r.internalReport)
    of repBackend:  result = conf.reportFull(r.backendReport)
    of repExternal: result = conf.reportFull(r.externalReport)

proc reportHook*(conf: ConfigRef, r: Report): TErrorHandling =
  conf.incl(cnCurrent, rintMsgOrigin)
  conf.incl(cnCurrent, rintErrKind)

  let wkind = conf.writabilityKind(r)
  if wkind == writeDisabled:
    return

  elif wkind in { writeForceEnabled, writeEnabled }:
    echo conf.reportFull(r)
    if r.kind notin rdbgTracerKinds:
      echo ""

  else:
    echo "?"
