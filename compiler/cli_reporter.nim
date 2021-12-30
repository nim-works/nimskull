import reports, ast, types
import ./options as compiler_options
import std/[strformat, strutils, options,
            parseutils, algorithm, sequtils]

import nimsuggest/sexp

## Implementation of the default command-line error hook. All the
## pretty-printed messages are constructed in this module.

proc report(r: SemReport)      = echo r
proc report(r: ParserReport)   = echo r
proc report(r: LexerReport)    = echo r
proc report(r: InternalReport) = echo r
proc report(r: ExternalReport) = echo r
proc report(r: DebugReport)    = echo r
proc report(r: BackendReport)  = echo r
proc report(r: CmdReport)      = echo r


proc addFields[T](s: var SexpNode, r: T, ignore: seq[string] = @[])

proc sexp[T: object | tuple](obj: T): SexpNode =
  result = newSList()
  addFields(result, obj)

proc sexp[T: object | tuple](obj: ref T): SexpNode =
  result = newSList()
  addFields(result, obj[])

proc sexp*[E: enum](e: E): SexpNode = newSSymbol($e)

proc sexpItems*[T](s: T): SexpNode =
  result = newSList()
  for item in items(s):
    result.add sexp(item)


proc sexp*[T](s: seq[T]): SexpNode = sexpItems(s)
proc sexp*[R, T](s: array[R, T]): SexpNode = sexpItems(s)
proc sexp*[I](s: set[I]): SexpNode = sexpItems(s)
proc sexp*(s: cstring): SexpNode = sexp($s)

proc sexp*(v: SomeInteger): SexpNode = newSInt(BiggestInt(v))
proc sexp*(id: FileIndex): SexpNode = newSInt(int(id))

iterator sexpFields[T](obj: T, ignore: seq[string] = @[]): tuple[key, val: SexpNode] =
  for name, value in fieldPairs(obj):
    if name notin ignore:
      yield (newSSymbol(":" & name), sexp(value))

func add*(other: var SexpNode, str: string, expr: SexpNode) =
  other.add newSSymbol(":" & str)
  other.add expr

proc sexp*[T](o: Option[T]): SexpNode =
  if o.isNone: newSNil() else: sexp(o.get())

proc addFields[T](s: var SexpNode, r: T, ignore: seq[string] = @[]) =
  for key, val in sexpFields(r, ignore):
    s.add key
    s.add val

proc sexp*(typ: PType): SexpNode =
  if typ.isNil: return newSNil()
  result = newSList()
  result.add newSSymbol(($typ.kind)[2 ..^ 1])
  if typ.sons.len > 0:
    result.add("sons", sexp(typ.sons))

proc sexp*(node: PNode): SexpNode =
  if node.isNil: return newSNil()

  result = newSList()
  result.add newSSymbol(($node.kind)[2 ..^ 1])
  case node.kind:
    of nkCharLit..nkUInt64Lit:    result.add sexp(node.intVal)
    of nkFloatLit..nkFloat128Lit: result.add sexp(node.floatVal)
    of nkStrLit..nkTripleStrLit:  result.add sexp(node.strVal)
    of nkSym:                     result.add newSSymbol(node.sym.name.s)
    of nkIdent:                   result.add newSSymbol(node.ident.s)
    else:
      for node in node.sons:
        result.add sexp(node)

proc sexp*(t: PSym): SexpNode = newSSymbol("<sym>")

import
  hmisc/algo/clformat,
  hmisc/types/colorstring,
  hnimast/hast_common


#----------------  ranked type mismatch errors for calls  ----------------#

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

import astalgo

func typoCost(used: string, expected: seq[string]): int =
  # Specific details of the typo correction costs can be checked for later,
  # since candidate that has /only/ typo in the name would have a low cost
  # anyway (all types match, the only error is in the named parameter).
  12

func argSyms(procType: PType): seq[PNode] =
  procType.n[1 ..^ 1]

proc updateRank*(mis: var RankedCallMismatch, args: ArgList) =
  let sem = mis.sem
  let matchingLen = min(sem.target.typ.n.len, args.len)
  for idx in 0 ..< matchingLen:
    var mismatch = rankMismatch(sem.target.typ.argSyms()[idx], args[idx])
    mis.mismatches.add mismatch
    if idx <= sem.arg:
      # Cost is declreased from the first mismatch position - error in the
      # first argument is more likely
      mis.rank += mismatch.rank * ((matchingLen - idx) - sem.arg)

  case sem.kind:
    of kTypeMismatch:
      # Type mismatch arguments are handled uniformly and decreasing
      # ranking based on the first mismatch position
      discard

    of kUnknownNamedParam:
      mis.rank += typoCost(
        sem.nameParam,
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

    else:
      result = $t.kind

proc format(target: PType, other: PType = nil): ColoredText =
  coloredResult()

  proc aux(target, other: PType) =
    # if target.isNil:
    #   add typeToString(target)
    #   return

    if other.isNil:
      add typeToString(target)
      return

    else:
      var tname = target.typeHeadName()
      var oname = other.typeHeadName()
      if tname == oname:
        add tname

      else:
        add tname + fgGreen
        add " != "
        add oname + fgRed

    case target.kind:
      of tyGenericBody, tyBuiltinTypeClass:
        add "["
        for idx in 0 ..< len(target):
          if idx > 0: add ", "
          aux(target[idx], other[idx])

        add "]"

      else:
        discard

  aux(target, other)

  endResult()

proc formatProc(p: PSym): ColoredText =
  coloredResult()

  add "proc "
  add p.name.s + fgGreen
  add "("
  for idx, arg in pairs(p.typ.n[1 ..^ 1]):
    if idx > 0: add ", "
    add arg.sym.name.s + fgCyan
    add ": "
    add arg.typ.format()

  add ")"

  endResult()



proc format(arg: ArgCompare): ColoredText =
  assert not isNil(arg.wanted)
  assert not isNil(arg.found)
  result.add format(arg.wanted, arg.found)

proc format(mis: RankedCallMismatch): ColoredText =
  coloredResult()

  let sem = mis.sem
  case sem.kind:
    of kUnknownNamedParam:
      add sem.target.formatProc()
      add "\n  "
      add stringMismatchMessage(
        sem.nameParam, sem.target.typ.argSyms().mapIt(it.sym.name.s))

    of kTypeMismatch:
      for argMis in mis.mismatches:
        add "\n"
        add argMis.format()

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
  mis.sort(proc(a, b: SemCallMismatch): int = cmp(a.arg, b.arg))
  var prev = -1
  for mis in mis:
    if mis.kind != kTypeMismatch:
      result.other.add mis

    elif mis.arg != prev:
      prev = mis.arg
      result.byType.add @[mis]

    else:
      result.byType[^1].add mis

proc reportCallMismatch(conf: ConfigRef, r: SemReport) =
  let args = mapIt(r.expression, it)[1 .. ^1]
  # First group call candidates by argument index and mismatch kind. This
  # allows to show 'but expression is of type' and other diagnostics only
  # once per argument position.
  let (byType, other) = r.callMismatches.groupByIdx()

  for group in byType:
    # Convert each group's items into ranked nodes and sort candidates
    # within the byType using more advanced heuristics.
    let expr = group[0].expression
    echo "Expression for argument is of type ", expr.typ.format()
    for mis in group.toRanked(args).sortedByIt(-it.rank):
      echo mis.format()

  for mis in other.toRanked(args).sortedByIt(-it.rank):
    echo mis.format()

proc format(s: SexpNode): ColoredText =
  coloredResult()

  proc aux(s: SexpNode, level: int) =
    if s.isNil: return
    case s.kind:
      of SInt: add hshow(s.getNum())
      of SString: add toYellow("\"" & s.getStr() & "\"")
      of SFloat: add hshow(s.getFNum())
      of SNil: add "nil" + fgCyan
      of SSymbol: add s.getSymbol() + fgBlue
      of SCons:
        add "("
        let (car, cdr) = s.getCons()
        aux(car, level + 1)
        add "."
        aux(cdr, level + 1)
        add ")"
      of SList:
        add "("
        var afterKwd = false
        var idx = 0
        for item in items(s):
          if not afterKwd and 0 < idx:
            add "\n"
            addIndent(level + 1)

          if item.kind == SSymbol and
             item.getSymbol().len > 0 and
             item.getSymbol()[0] == ':':
            add item.getSymbol() + fgMagenta
            add " "
            afterKwd = true

          else:
            aux(item, level + 1)
            afterKwd = false

          inc idx

        add ")"

  aux(s, 0)

  endResult()

proc reportHook*(conf: ConfigRef, r: Report) =
  if not conf.isEnabled(r):
    if r.kind notin {rsemProcessingStmt, rsemXDeclaredButNotUsed}:
      echo r.kind, " is disabled"

    return

  if r.kind in {rintStackTrace}:
    for it in r.internalReport.trace:
      echo it

    return

  if r.kind == rsemCallTypeMismatch:
    reportCallMismatch(conf, r.semReport)
    return

  let k = $r.kind
  var s = newSList()
  s.add newSSymbol($r.category & "-" & k)
  s.add newSSymbol(":severity")
  s.add sexp(conf.severity(r))
  case r.category:
    of repLexer:    s.addFields(r.lexReport)
    of repParser:   s.addFields(r.parserReport)
    of repCmd:      s.addFields(r.cmdReport)
    of repSem:
      if r.kind == rsemProcessingStmt:
        s.addFields(r.semReport, @["expression"])

      else:
        s.addFields(r.semReport)

    of repDebug:    s.addFields(r.debugReport)
    of repInternal: s.addFields(r.internalReport)
    of repBackend:  s.addFields(r.backendReport)
    of repExternal: s.addFields(r.externalReport)

  echo s.format()
