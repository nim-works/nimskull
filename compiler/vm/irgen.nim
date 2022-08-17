import
  std/[
    tables,
    strutils
  ],
  compiler/ast/[
    renderer,
    types,
    trees,
    ast,
    astalgo,
    reports,
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
  compiler/vm/[
    irtypes,
    vmir
  ],
  experimental/[
    results
  ]

from compiler/vm/vmaux import findRecCase, findMatchingBranch
from compiler/vm/vmdef import unreachable

# XXX: temporary import; needed for ``PassEnv``
import compiler/vm/irpasses

type TBlock = object
  label: PSym
  start: JoinPoint

type LocalId = int
type ScopeId = uint32

type PProc* = object
  sym*: PSym

  excHandlers: seq[JoinPoint]
  blocks: seq[TBlock]
  variables: seq[LocalId] ## each non-temporary local in the order of their definition

  # each local has an owning scope (the smallest enclosing one). When the
  # control-flow leaves a scope, all locals it owns need be destroyed
  scopes: seq[(bool, ScopeId, IRIndex)]

  finalizers: seq[(Slice[IRIndex], JoinPoint)] #

  numLocals: seq[uint32] # the number of locals for each scope

  scopeStack: seq[ScopeId]
  nextScopeId: ScopeId

  locals: Table[int, int]

type TCtx* = object

  irs*: IrStore3

  prc*: PProc
  handlers: seq[PNode] # the defered exception handlers

  graph*: ModuleGraph # only needed for testing if a proc has a body
  idgen*: IdGenerator # needed for creating magics on-demand

  passEnv*: PassEnv

  module*: PSym

  config*: ConfigRef

  options*: set[TOption]

  symEnv*: SymbolEnv
  procs*: ProcedureEnv
  types*: DeferredTypeGen


type IrGenResult* = Result[IrStore3, SemReport]

when defined(nimCompilerStacktraceHints):
  import std/stackframes

type
  VmGenError = object of CatchableError
    report: SemReport

const
  NormalExit = 0
  ExceptionalExit = 1

func raiseVmGenError(
  report: sink SemReport,
  loc:    TLineInfo,
  inst:   InstantiationInfo
  ) {.noinline, noreturn.} =
  report.location = some(loc)
  report.reportInst = toReportLineInfo(inst)
  raise (ref VmGenError)(report: report)

func fail(
  info: TLineInfo,
  kind: ReportKind,
  ast:  PNode = nil,
  sym:  PSym = nil,
  str:  string = "",
  loc:  InstantiationInfo = instLoc()
  ) {.noinline, noreturn.} =
  raiseVmGenError(
    SemReport(kind: kind, ast: ast, sym: sym, str: str),
    info,
    loc)

func irSym(c: var TCtx, sym: PSym): IRIndex =
  let id = c.symEnv.requestSym(sym)
  c.irs.irSym(id)

func irParam(c: var TCtx, sym: PSym): IRIndex =
  c.irSym(sym)

func irGlobal(c: var TCtx, sym: PSym): IRIndex =
  c.irSym(sym)

func irConst(c: var TCtx, sym: PSym): IRIndex =
  c.irSym(sym)

func irLit(c: var TCtx, n: PNode): IRIndex =
  let typ =
    if n.typ != nil:
      c.types.requestType(n.typ)
    else:
      NoneType

  c.irs.irLit((n, typ))

proc irImm(c: var TCtx, val: SomeInteger): IRIndex =
  # XXX: getSysType has side-effects
  c.irLit newIntTypeNode(BiggestInt(val), c.graph.getSysType(unknownLineInfo, tyInt))

template tryOrReturn(code): untyped =
  try:
    code
  except VmGenError as e:
    return IrGenResult.err(move e.report)


proc getTemp(cc: var TCtx; tt: PType): IRIndex

func openScope(c: var TCtx) =
  let id = c.prc.nextScopeId
  inc c.prc.nextScopeId

  c.prc.numLocals.add(0)
  c.prc.scopes.add((false, id, c.irs.len))
  c.prc.scopeStack.add(id)

func closeScope(c: var TCtx) =
  let id = c.prc.scopeStack.pop()
  c.prc.scopes.add((true, id, c.irs.len))

proc genProcSym(c: var TCtx, s: PSym): IRIndex =
  c.irs.irProc(c.procs.requestProc(s))

proc irCall(c: var TCtx, name: string, args: varargs[IRIndex]): IRIndex =
  # TODO: compiler procs should be cached here in `TCtx`
  let prc = c.passEnv.getCompilerProc(name)
  c.irs.irCall(c.irs.irProc(prc), args)

func irCall(c: var TCtx, name: string, m: TMagic, args: varargs[IRIndex]): IRIndex {.inline.} =
  # TODO: maybe store all used magics directly in ``TCtx``?
  c.irs.irCall(c.irs.irProc(c.passEnv.magics[m]), args)

func genLocal(c: var TCtx, kind: LocalKind, t: PType): IRIndex =
  let
    tid = c.types.requestType(t)

  c.irs.genLocal(kind, tid)

func genLocal(c: var TCtx, kind: LocalKind, s: PSym): IRIndex =
  let
    sid = c.symEnv.requestSym(s)
    tid = c.types.requestType(s.typ)

  c.irs.genLocal(kind, tid, sid)

proc getTemp(cc: var TCtx; tt: PType): IRIndex =
  let id = cc.genLocal(lkTemp, tt)
  cc.irs.irLocal(id)

func irNull(c: var TCtx, t: PType): IRIndex =
  # XXX: maybe `irNull` should be a dedicated IR node?
  let id = c.genLocal(lkTemp, t)
  c.irs.irLocal(id)

proc popBlock(c: var TCtx; oldLen: int) =
  #for f in c.prc.blocks[oldLen].fixups:
  #  c.patch(f)
  c.prc.blocks.setLen(oldLen)

template withBlock(labl: PSym; next: JoinPoint; body: untyped) {.dirty.} =
  var oldLen {.gensym.} = c.prc.blocks.len
  c.prc.blocks.add TBlock(label: labl, start: next)
  body
  popBlock(c, oldLen)

proc gen(c: var TCtx; n: PNode; dest: var IRIndex)

proc gen(c: var TCtx; n: PNode) =
  doAssert n.typ.isEmptyType
  var tmp: IRIndex
  gen(c, n, tmp)

proc genx(c: var TCtx; n: PNode): IRIndex =
  #var tmp: TDest = -1
  #gen(c, n, tmp)
  #internalAssert c.config, tmp >= 0 # 'nim check' does not like this internalAssert.
  c.gen(n, result)
  c.config.internalAssert(result != InvalidIndex, n.info, $n.kind)

proc gen2(c: var TCtx, n: PNode): tuple[r: IRIndex, exits: bool] =
  c.gen(n, result.r)

  # if the statement ends with a goto, it's not a normal exit
  # XXX: it's probably a better idea to look at the `n` instead
  result.exits = not c.irs.isLastAGoto()

proc isNotOpr(n: PNode): bool =
  n.kind in nkCallKinds and n[0].kind == nkSym and
    n[0].sym.magic == mNot

proc isTrue(n: PNode): bool =
  n.kind == nkSym and n.sym.kind == skEnumField and n.sym.position != 0 or
    n.kind == nkIntLit and n.intVal != 0

proc genStmt2(c: var TCtx, n: PNode): bool =
  gen(c, n)
  result = true # XXX: calculate the correct value

proc genWhile(c: var TCtx; n: PNode, next: JoinPoint) =
  # lab1:
  #   cond, tmp
  #   fjmp tmp, lab2
  #   body
  #   jmp lab1
  # lab2:
  var entrances: seq[IRIndex]
  withBlock(nil, next):

    # the scope needs to be opened _before_ emitting the join, or else a loop
    # iteration would be treated as leaving the scope
    c.openScope()
    let loop = c.irs.irLoopJoin()
    if isTrue(n[0]):
      # don't emit a branch if the condition is always true
      discard
    else:
      # TODO: omit the while loop if cond == false?
      var tmp = c.genx(n[0])
      let lab2 = c.irs.irBranch(tmp, next)
      #c.prc.blocks[^1].endings.add(lab2)

    let exits = c.genStmt2(n[1])
    if exits:
      discard c.irs.irGoto(loop)
      #entrances.add(c.irs.irGetCf())

    c.closeScope()

  #c.irs.irPatchStart(start, entrances)

  #result = c.irs.irJoin(c.prc.blocks[^1].endings)

proc genBlock(c: var TCtx; n: PNode, next: JoinPoint): IRIndex =
  withBlock(n[0].sym, next):
    c.gen(n[1], result)

func irGotoRaise(c: var IrStore, i: IRIndex): IRIndex =
  missingImpl()

func irGotoLink(c: var IrStore, i: IRIndex): IRIndex =
  missingImpl()

iterator rmitems[T](x: var openArray[T]): var T =
  var i = x.high
  while i >= 0:
    yield x[i]
    dec i

proc genBreak(c: var TCtx; n: PNode): IRIndex =
  var i = c.prc.blocks.high
  if n[0].kind == nkSym:
    #echo cast[int](n[0].sym)
    while i >= 0 and c.prc.blocks[i].label != n[0].sym:
      dec i

    if i < 0:
      # XXX: this isn't a user error
      fail(n.info, rsemVmCannotFindBreakTarget)

  c.irs.irGoto(c.prc.blocks[i].start)

func irFwd(c: var TCtx): IRIndex =
  missingImpl()

func irEnd(c: var TCtx, target, value: IRIndex): IRIndex =
  missingImpl()


proc genIf(c: var TCtx, n: PNode, next: JoinPoint): IRIndex =
  #  if (!expr1) goto lab1;
  #    thenPart
  #    goto LEnd
  #  lab1:
  #  if (!expr2) goto lab2;
  #    thenPart2
  #    goto LEnd
  #  lab2:
  #    elsePart
  #  Lend:

  let hasValue = not isEmptyType(n.typ)

  var value: IRIndex
  if hasValue:
    value = c.getTemp(n.typ)

  var prev = next
  for i in 0..<n.len:
    var it = n[i]
    var then: PNode
    if it.len == 2:
      if prev != next:
        # join state from previous condition code
        # XXX: maybe not necessary?
        c.irs.irJoin(prev)

      if i < n.len - 1:
        prev = c.irs.irJoinFwd()
      else:
        prev = next

      let tmp = c.genx(it[0]) # condition
      discard c.irs.irBranch(tmp, prev)

      then = it[1]
    else:
      assert prev != next
      c.irs.irJoin(prev) # an else branch needs a join
      then = it[0]

    let r = c.gen2(then)

    if r.exits:
      #result = true # if one 'then' block exits, the `if` exits

      if hasValue and r.r != InvalidIndex:
        # if `then` ends in a void `noreturn` call, `r.r` is unset
        c.irs.irAsgn(askInit, value, r.r)

      c.irs.irGoto(next)

  result = value


proc genAndOr(c: var TCtx; n: PNode; isAnd: bool, next: JoinPoint): IRIndex =
  #   asgn dest, a
  #   branch (not) dest -> next
  #   asgn dest, b

  let tmp = c.getTemp(n.typ)
  let a = c.genx(n[1])
  c.irs.irAsgn(askInit, tmp, a)

  let cond =
    if isAnd: a
    else: c.irCall("not", mNot, a)

  let p = c.irs.irBranch(cond, next)

  let b = c.genx(n[2])
  c.irs.irAsgn(askInit, tmp, b)

  result = tmp

proc genCase(c: var TCtx; n: PNode, next: JoinPoint): IRIndex =
  #  if (!expr1) goto lab1;
  #    thenPart
  #    goto LEnd
  #  lab1:
  #  if (!expr2) goto lab2;
  #    thenPart2
  #    goto LEnd
  #  lab2:
  #    elsePart
  #  Lend:
  let hasValue = not isEmptyType(n.typ)

  let dest =
    if hasValue: c.getTemp(n.typ)
    else: InvalidIndex

  let selType = n[0].typ.skipTypes(abstractVarRange)
  var b = next
  block:
    let tmp = c.genx(n[0])
    # branch tmp, codeIdx
    # fjmp   elseLabel

    # iterate of/else branches
    for i in 1..<n.len:
      let branch = n[i]

      var r: (IRIndex, bool)

      if i > 1:
        c.irs.irJoin(b)

      b =
        if i < n.len - 1: c.irs.irJoinFwd()
        else: next

      if branch.len == 1:
        # else stmt:
        if branch[0].kind != nkNilLit or branch[0].typ != nil:
          # TODO: re-document the intention behind the if
          r = c.gen2(branch[0])

      else:
        # elif branches were eliminated during transformation
        doAssert branch.kind == nkOfBranch

        let cond = c.irs.irCall(bcOf, NoneType, c.irLit(branch))

        c.irs.irBranch(cond, b)
        r = c.gen2(branch.lastSon)

      if r[1]:
        if hasValue and r[0] != InvalidIndex:
          # `r[0]` is unset if `branch` ends in a void `noreturn` call, so we
          # have to guard against that case
          c.irs.irAsgn(askInit, dest, r[0])

        c.irs.irGoto(next)

  result = dest

func genExceptCond(c: var TCtx, val: IRIndex, n: PNode, next: JoinPoint) =
  ## Lowers exception matching into an if
  # XXX: maybe too early for this kind of lowering
  for i in 0..<n.len-1:
    let cond = c.irCall("of", mOf, val)
    c.irs.irBranch(cond, next)

func nextHandler(c: PProc): JoinPoint =
  if c.excHandlers.len > 0:
    c.excHandlers[^1]
  else:
    ExceptionalExit


proc genTry(c: var TCtx; n: PNode, next: JoinPoint): IRIndex =

  let
    hasFinally = n.lastSon.kind == nkFinally
    hasExcept = n[1].kind == nkExceptBranch

  if hasFinally:
    # the finally also applies for the ``except`` blocks
    discard #c.prc.finalizers.add(c.irs.irJoinFwd())

  if hasExcept:
    c.prc.excHandlers.add(c.irs.irJoinFwd())

  let dest =
    if not isEmptyType(n.typ): c.getTemp(n.typ)
    else: InvalidIndex

  let r = c.gen2(n[0])

  if r.exits:
    if dest != InvalidIndex:
      # TODO: assert that gen2 doesn't return a value
      c.irs.irAsgn(askInit, dest, r.r)
    #[let t =
      if hasFinally: c.prc.finalizers[^1]
      else: next]#

    c.irs.irGoto(next)

  let len =
    if hasFinally: n.len-1
    else: n.len

  if hasExcept:
    let eVal = c.irCall("getCurrentException")
    let handler = c.prc.excHandlers.pop() # pop the handler we registered at
                                          # the start
    var currNext = handler
    for i in 1..<len:
      let it = n[i]

      c.irs.irJoin(currNext)
      currNext =
        if i < len-1: c.irs.irJoinFwd()
        else:         c.prc.nextHandler()

      c.genExceptCond(eVal, it, currNext)

      let r = c.gen2(it.lastSon)
      if r.exits:
        if r.r != InvalidIndex:
          # XXX: the guard below is wrong, but sem currently doesn't report an
          #      error in this case
          if dest != InvalidIndex:
            c.irs.irAsgn(askInit, dest, r.r)
        c.irs.irGoto(next)

  if hasFinally:
    # TODO: join is missing
    let r = c.gen2(lastSon(n)[0])
    # a finally block never has a result
    if r.exits:
      # where execution resumes after the finally depends on
      # run-time control-flow
      c.irs.irContinue()

  result = dest

proc genRaise(c: var TCtx; n: PNode) =
  if n[0].kind != nkEmpty:
    let
      dest = c.genx(n[0])
      typ = skipTypes(n[0].typ, abstractPtrs)

    # get the exception name
    let name = newStrNode(nkStrLit, typ.sym.name.s)#c.genLit(n[0], c.toStringCnst(typ.sym.name.s))

    discard c.irs.irCall(bcRaise, NoneType, dest, c.irLit name)
  else:
    # reraise
    discard c.irs.irCall(bcRaise, NoneType)

  # XXX: if the exception's type is statically known, we could do the
  #      exception branch matching at compile-time (i.e. here)
  c.irs.irGoto(c.prc.nextHandler())

func resultVar(p: PProc): IRIndex =
  doAssert false

proc genReturn(c: var TCtx; n: PNode): IRIndex =
  if n[0].kind != nkEmpty:
    discard genStmt2(c, n[0])

  c.irs.irGoto(NormalExit)

proc genLit(c: var TCtx; n: PNode): IRIndex =
  c.irLit(n)


proc genProcLit(c: var TCtx, n: PNode, s: PSym): IRIndex =
  c.irs.irProc(c.procs.requestProc(s))

#[
func doesAlias(c: TCtx, a, b: IRIndex): bool =
  ## Tests if the locations refered to by `a` and `b` overlap. If either `a`
  ## or `b` is no location but a value, returns false, since values can't
  ## overlap in memory
  missingImpl()
]#

func isMove(n: PNode): bool {.inline.} =
  getMagic(n) == mMove

proc raiseExit(c: var TCtx) =
  # TODO: document

  # if isError: goto surrounding handler
  let cond = c.irs.irCall(bcTestError, NoneType) # XXX: should pass tyBool
  c.irs.irBranch(cond, c.prc.nextHandler())

func isVarParam(t: PType): bool =
  # XXX: checking if a parameter type is mutable with the logic below is a
  #      bit brittle. Testing for `nkHiddenAddr` won't work however, since
  #      it's elided in the case that a var parameter is passed as a var
  #      argument
  t.skipTypes({tyAlias, tyGenericInst}).kind == tyVar

proc genArg(c: var TCtx, formal: PType, useTemp: bool, n: PNode): IRIndex =
  # TODO: add a test to make sure that a ``move x`` passed to a var parameter
  #       doesn't reach here
  let
    isMove = isMove(n)
    arg = c.genx(if isMove: n[1] else: n)

  if formal == nil or formal.isVarParam():
    # no special handling for c-style varargs or ``var`` params
    assert not isMove
    result = arg
  elif useTemp:
    # turn each immutable argument into a `let tmp = arg; tmp`. The
    # subsequent passes take care of turning the `let` into an
    # alias (if safe). Introducing a temporary makes sure that the "input must
    # not alias with output" rule is not violated. The alternative would be
    # to report a warning or error
    let tmp = c.getTemp(n.typ)
    c.irs.irAsgn(askInit, tmp, arg)
    result = c.irs.irUse(tmp)
  else:
    result = arg

proc genCall(c: var TCtx; n: PNode): IRIndex =
  let fntyp = skipTypes(n[0].typ, abstractInst)

  # Important: call expressions (as all other expression and statments) are
  # evaluated strictly left-to-right

  let shouldGenCT = false ## whether static and typeDesc parameters should be
                          ## code-gen'ed

  let callee =
    if n[0].kind == nkSym and n[0].sym.kind in routineKinds:
      genProcSym(c, n[0].sym)
    else:
      genx(c, n[0])

  # XXX: maybe the parameter aliasing rules should be enforced via
  #      compile-time errors or warnings, instead of silently introducing
  #      temporaries (for what it's worth, the C back-end currently does the
  #      same)

  # TODO: sink handling is missing

  #[
  var useTemp = true # whether to introduce temporaries for all
                      # non-var/sink parameters
  var hasIndirectParam = false
  if tfNoSideEffect in n.typ:
    for it in 1..<n.typ.len:
      let t = it.skipTypes(abstractInst)
      # TODO: `noSideEffect` (without `strictFuncs` enabled) means that we
      #       also need to consider `ref` and `ptr` types inside other types
      if t.kind in {tyVar}
      ]#

  var args = newSeq[IRIndex](n.len)
  var L = 0
  for i in 1..<n.len:
    {.warning: "The case where nkHiddenAddr was elided isn't handled here".}
    # TODO: elision of compile-time parameters (i.e. static, typeDesc) needs
    #       some further thought
    if not n[i].typ.isCompileTimeOnly or shouldGenCT:
      let t =
        if i < fntyp.len: fntyp[i]
        else: nil

      c.config.internalAssert(t != nil or tfVarargs in fntyp.flags, n[i].info):
        "too many arguments"

      args[L] = genArg(c, t, true, n[i])
      inc L

  # resize to the real amount
  args.setLen(L)

  result = c.irs.irCall(callee, args)
  if canRaiseConservative(n[0]):
    raiseExit(c)

  if n.typ.isEmptyType():
    result = InvalidIndex

template isGlobal(s: PSym): bool = sfGlobal in s.flags and s.kind != skForVar
proc isGlobal(n: PNode): bool = n.kind == nkSym and isGlobal(n.sym)

func local(prc: PProc, sym: PSym): int {.inline.} =
  ## Returns the register associated with the local variable `sym` (or -1 if
  ## `sym` is not a local)
  let
    s = sym
    r = prc.locals.getOrDefault(s.id, -1)
  # Problem: in macro bodies, copies of the parameters' symbols with differnt
  # IDs are used (see `addParamOrResult`), meaning that these symbols have no
  # mapping in the table after `genParams`. Which register a parameter with
  # position X maps to is deteministic, so a simple fallback can be used.
  if r >= 0: r
  else:
    unreachable(s.name.s)

proc genField(c: TCtx; n: PNode): int =
  if n.kind != nkSym or n.sym.kind != skField:
    fail(n.info, rsemNotAFieldSymbol, ast = n)

  let s = n.sym
  if s.position > high(typeof(result)):
    fail(n.info, rsemVmTooLargetOffset, sym = s)

  result = s.position

func irLit(c: var TCtx, i: SomeInteger): IRIndex =
  c.irLit newIntNode(nkIntLit, BiggestInt(i))

proc genIndex(c: var TCtx; n: PNode; arr: PType): IRIndex =
  if arr.skipTypes(abstractInst).kind == tyArray and (let x = firstOrd(c.config, arr);
      x != Zero):
    let tmp = c.genx(n)

    result = c.irCall("-", mSubI, tmp, c.irLit(toInt(x)))
  else:
    result = c.genx(n)

proc genCheckedObjAccessAux(c: var TCtx; n: PNode; dest: var IRIndex)

template sizeOfLikeMsg(name): string =
  "'$1' requires '.importc' types to be '.completeStruct'" % [name]

func genTypeLit(c: var TCtx, t: PType): IRIndex

proc isInt8Lit(n: PNode): bool =
  if n.kind in {nkCharLit..nkUInt64Lit}:
    result = n.intVal >= low(int8) and n.intVal <= high(int8)

proc isInt16Lit(n: PNode): bool =
  if n.kind in {nkCharLit..nkUInt64Lit}:
    result = n.intVal >= low(int16) and n.intVal <= high(int16)

func wrapIf(c: var TCtx, wrapper: BuiltinCall, typ: TypeId, expr: IRIndex, cond: bool): IRIndex {.inline.} =
  if cond: c.irs.irCall(wrapper, typ, expr)
  else:    expr

proc genMagic(c: var TCtx; n: PNode; m: TMagic): IRIndex =
  result = InvalidIndex
  case m
  of mAnd, mOr:
    let fwd = c.irs.irJoinFwd()
    result = c.genAndOr(n, isAnd = (m == mAnd), fwd)
    c.irs.irJoin(fwd)
  of mAddI:
    # idea: also insert builtin calls to the various check functions here.
    #       Makes it easier to get uniformity across the back-ends.
    result = c.genCall(n)
    result = c.wrapIf(bcOverflowCheck, c.types.requestType(n.typ), result, optOverflowCheck notin c.options)
    if optOverflowCheck in c.options:
      # idea: defects (or error in general) could be encoded as part of the values. I.e. a
      #       `bcOverflowCheck` call would return a result-like value (only on
      #       the IR level, not in the resulting generate code)
      # TODO: unfinished
      c.raiseExit()
  of mSwap:
    let
      tmp = c.getTemp(n[1].typ)
      a = c.genx(n[1])
      b = c.genx(n[2])
    # TODO: maybe don't lower this early?
    # XXX: a swap could be treated as a rename...
    c.irs.irAsgn(askShallow, tmp, a)
    c.irs.irAsgn(askShallow, a, b)
    c.irs.irAsgn(askShallow, b, tmp)
  of mReset:
    var d = c.genx(n[1])
    unreachable("missing")
  of mGetTypeInfo:
    # transform the `getTypeInfo(x)` into a `getTypeInfo(typeof(x))`
    result = c.irCall("getTypeInfo", mGetTypeInfo, genTypeLit(c, n[1].typ))

  of mDefault:
    result = c.irNull(n.typ)
  of mRunnableExamples:
    discard "just ignore any call to runnableExamples"
  of mDestroy, mTrace:
    # these should not exist yet
    unreachable(n.kind)
  of mMove:
    unreachable("not handled here")
  of mConStrStr:
    # the `mConStrStr` magic is very special. Nested calls to it are flattened
    # into a single call in ``transf``
    var args = newSeq[IRIndex](n.len - 1)
    for i in 1..<n.len:
      # we need no temporaries here since all arguments are read-only
      args[i-1] = c.genx(n[i])

    result = c.irCall("&", mConStrStr, args)
    # the proc doesn't raise so no ``raiseExit`` is needed here
  of mNew:
    # problem: ``lambdalifting`` inserts calls to ``internalNew`` by just
    #          using the symbol of the generic proc. This is a problem for
    #          type translation following after the IR-gen step, since it
    #          can't handle `tyGenericParam` types.
    if n[0].sym.name.s == "internalNew":
      let t = c.types.requestType(n[1].typ)
      discard c.irs.irCall(bcNew, t, genx(c, n[1]))
    else:
      # a normal new. Don't do any special transformation
      result = genCall(c, n)
  else:
    # TODO: return a bool instead and let the callsite call `genCall` in case
    #       the magic doesn't use special logic here
    # no special transformation for the other magics:
    result = genCall(c, n)

proc canElimAddr(n: PNode): PNode =
  if n[0].typ.skipTypes(abstractInst).kind in {tyObject, tyTuple, tyArray}:
    return nil
  case n[0].kind
  of nkObjUpConv, nkObjDownConv, nkChckRange, nkChckRangeF, nkChckRange64:
    var m = n[0][0]
    if m.kind in {nkDerefExpr, nkHiddenDeref}:
      # addr ( nkConv ( deref ( x ) ) ) --> nkConv(x)
      result = copyNode(n[0])
      result.add m[0]
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    var m = n[0][1]
    if m.kind in {nkDerefExpr, nkHiddenDeref}:
      # addr ( nkConv ( deref ( x ) ) ) --> nkConv(x)
      result = copyNode(n[0])
      result.add m[0]
  of nkError: result = nil
  else:
    if n[0].kind in {nkDerefExpr, nkHiddenDeref}:
      # addr ( deref ( x )) --> x
      result = n[0][0]

proc genAddr(c: var TCtx, n: PNode): IRIndex =
  if (let m = canElimAddr(n); m != nil):
    return genx(c, m)

  let tmp = c.genx(n[0])
  result = c.irs.irAddr(tmp)

proc genDeref(c: var TCtx, n: PNode): IRIndex =
  let dest = genx(c, n[0])
  c.irs.irDeref(dest)

proc genAsgn(c: var TCtx; dest: IRIndex; ri: PNode; requiresCopy: bool) =
  if isMove(ri):
    # a moving assign
    # TODO: a `move(move(a))` would wreak havoc
    let tmp = c.genx(ri[1])
    c.irs.irAsgn(askMove, dest, tmp)
  else:
    let tmp = c.genx(ri)
    c.irs.irAsgn(askCopy, dest, tmp)

func cannotEval(c: TCtx; n: PNode) {.noinline, noreturn.} =
  raiseVmGenError(
    reportAst(rsemVmCannotEvaluateAtComptime, n),
    n.info,
    instLoc())

func isOwnedBy(a, b: PSym): bool =
  var a = a.owner
  while a != nil and a.kind != skModule:
    if a == b: return true
    a = a.owner

func getOwner(c: TCtx): PSym =
  result = c.prc.sym
  if result.isNil: result = c.module


proc genDiscrVal(c: var TCtx, discr: PSym, n: PNode, oty: PType): (IRIndex, IRIndex) =
  ## Generate the code for preparing and loading the discriminator value
  ## as expected by the execution engine

  let oty = oty.skipTypes(abstractPtrs)
  assert oty.kind == tyObject

  let discrTyp = lookupInRecord(oty.n, discr.name).typ

  let recCase = findRecCase(oty, discr)
  assert recCase != nil

  if n.kind in nkCharLit..nkUInt64Lit:
    # Discriminator value is known at compile-time

    let b = findMatchingBranch(recCase, n)
    assert b != -1 # no matching branch; should have been caught already

    result[0] = c.genLit(n) # discr value
    result[1] = c.irLit(b) # branch index
  else:
    let tmp = c.genx(n)
    result[0] = tmp
    result[1] = c.irs.irCall(bcGetBranchIndex, NoneType, tmp, c.genTypeLit(oty), c.irSym(discr))

func isCursor(n: PNode): bool

proc genFieldAsgn(c: var TCtx, obj: IRIndex; le, ri: PNode) =
  c.config.internalAssert(le.kind == nkDotExpr)

  let idx = c.genField(le[1])
  let s = le[1].sym

  var tmp: IRIndex

  let p = c.irs.irPathObj(obj, idx)

  if sfDiscriminant notin s.flags:
    genAsgn(c, p, ri, requiresCopy = not isCursor(le))
  else:
    # Can't use `s.owner.typ` since it may be a `tyGenericBody`
    #tmp = c.genDiscrVal(le[1], ri, le[0].typ)
    #c.irs.irAsgn(askDiscr, p, tmp)
    let (dVal, bVal) = c.genDiscrVal(s, ri, le[0].typ)
    discard c.irs.irCall(bcSwitch, NoneType, p, dVal, bVal)

func isCursor(n: PNode): bool =
  case n.kind
  of nkSym:
    sfCursor in n.sym.flags
  of nkDotExpr:
    isCursor(n[0]) or isCursor(n[1])
  of nkCheckedFieldExpr, nkBracketExpr:
    isCursor(n[0])
  else:
    false


proc genRdVar(c: var TCtx; n: PNode;): IRIndex =
  let s = n.sym
  if sfGlobal in s.flags:
    c.irGlobal(s)
  elif s.kind == skParam:
    c.irParam(s)
  elif s.kind == skResult: c.irs.irLocal(0) # TODO: don't hardcode
  else: c.irs.irLocal(c.prc.local(s))

proc genAsgn(c: var TCtx; le, ri: PNode; requiresCopy: bool) =
  # TODO: move and cursor handling is missing
  case le.kind
  of nkError:
    # XXX: do a better job with error generation
    fail(le.info, rsemVmCannotGenerateCode, le)

  of nkBracketExpr:
    let typ = le[0].typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind
    let dest = c.genx(le[0])

    let x =
      if typ == tyTuple:
        c.irs.irPathObj(dest, le[1].intVal.int)
      else:
        c.irs.irPathArr(dest, c.genIndex(le[1], le[0].typ))

    genAsgn(c, x, ri, requiresCopy = not isCursor(le))

  of nkCheckedFieldExpr:
    var objR: IRIndex
    genCheckedObjAccessAux(c, le, objR)
    c.genFieldAsgn(objR, le[0], ri)
  of nkDotExpr:
    let dest = c.genx(le[0])
    c.genFieldAsgn(dest, le, ri)
  of nkSym:
    let dest = genRdVar(c, le)
    genAsgn(c, dest, ri, requiresCopy = not isCursor(le))
  of nkDerefExpr, nkHiddenDeref:
    let dest = c.genx(le[0])
    genAsgn(c, c.irs.irDeref(dest), ri, requiresCopy = true) # XXX: is `requiresCopy = true` correct?
  else:
    unreachable(le.kind)

proc genArrAccessOpcode(c: var TCtx; n: PNode): tuple[arr, idx: IRIndex] =
  result.arr = c.genx(n[0])
  result.idx = c.genIndex(n[1], n[0].typ)

proc genObjAccess(c: var TCtx; n: PNode): IRIndex =
  let a = c.genx(n[0])
  let b = genField(c, n[1])
  c.irs.irPathObj(a, b)

proc genCheckedObjAccessAux(c: var TCtx; n: PNode; dest: var IRIndex) =
  internalAssert(
    c.config,
    n.kind == nkCheckedFieldExpr,
    "genCheckedObjAccessAux requires checked field node")

  # nkDotExpr to access the requested field
  let accessExpr = n[0]
  # nkCall to check if the discriminant is valid
  var checkExpr = n[1]

  let negCheck = checkExpr[0].sym.magic == mNot
  if negCheck:
    checkExpr = checkExpr[^1]

  # Discriminant symbol
  let disc = checkExpr[2]
  internalAssert(
    c.config, disc.sym.kind == skField, "Discriminant symbol must be a field")

  # Load the object in `dest`
  dest = c.genx(accessExpr[0])

  if optFieldCheck in c.options:
    let discVal = c.irs.irUse(c.irs.irPathObj(dest, genField(c, disc)))
    var cond = c.irCall("contains", mInSet, c.irLit(checkExpr[1]), discVal)
    if negCheck:
      cond = c.irCall("not", mNot, cond)

    let lab1 = c.irs.irJoinFwd()
    c.irs.irBranch(cond, lab1)
    discard c.irs.irCall(bcRaiseFieldErr, NoneType, discVal)
    c.raiseExit()
    c.irs.irJoin(lab1)

proc genCheckedObjAccess(c: var TCtx; n: PNode): IRIndex =
  var objR: IRIndex
  genCheckedObjAccessAux(c, n, objR)

  let accessExpr = n[0]
  # Field symbol
  var field = accessExpr[1]
  internalAssert(
    c.config,
    field.sym.kind == skField,
    "Access expression must be a field, but found " & $field.sym.kind)

  # Load the content now
  let fieldPos = genField(c, field)
  c.irs.irPathObj(objR, fieldPos)

func genTypeLit(c: var TCtx, t: PType): IRIndex =
  c.irs.irLit((nil, c.types.requestType(t)))

proc genArrAccess(c: var TCtx; n: PNode): IRIndex =
  let arrayType = n[0].typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind
  case arrayType
  of tyTypeDesc:
    c.genTypeLit(n.typ)
  of tyTuple:
    let a = c.genx(n[0])
    let b = n[1].intVal
    c.irs.irUse(c.irs.irPathObj(a, b.int))
  of tyArray, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray, tyString, tyCstring:
    let acc = genArrAccessOpcode(c, n)
    c.irs.irUse(c.irs.irPathArr(acc.arr, acc.idx))
  else: unreachable(arrayType)

func addVariable(c: var TCtx, kind: LocalKind, s: PSym): IRIndex =
  assert kind != lkTemp

  let id = c.genLocal(kind, s)
  c.prc.locals[s.id] = id
  c.prc.variables.add(id)
  inc c.prc.numLocals[^1]

  c.irs.irLocal(id)

proc genVarTuple(c: var TCtx, kind: LocalKind, n: PNode) =
  ## Generates the code for a ``let/var (a, b) = c`` statement
  assert n.kind == nkVarTuple
  var lhs = newSeq[IRIndex](n.len - 2)

  # first, generate the IR for the left side (left-to-right evaluation)
  for i in 0..<n.len-2:
    let e =
      if n[i].kind == nkSym:
        let s = n[i].sym

        if s.isGlobal: c.irGlobal(s)
        else: c.addVariable(kind, s)
      else:
        c.genx(n[i])

    lhs[i] = e

  # then, generate the initialization
  let initExpr = n[^1]
  case initExpr.kind
  of nkPar, nkTupleConstr:
    # skip constructing a temporary and directly assign the fields
    c.config.internalAssert(lhs.len == initExpr.len, n.info)
    for i, left in lhs.pairs:
      let val = c.genx(initExpr[i].skipColon())
      c.irs.irAsgn(askInit, left, val)

  of nkEmpty:
    discard "do nothing for empty tuple initializers"
  else:
    let val = c.genx(initExpr)

    for i, left in lhs.pairs:
      let p = c.irs.irPathObj(val, i)
      c.irs.irAsgn(askInit, left, p)

proc genLocalInit(c: var TCtx, kind: LocalKind, a: PNode) =
      ## Generate code for a ``let/var a = b`` statement
      assert a.kind == nkIdentDefs
      let s = a[0].sym
      if s.isGlobal:
        # a function-level global

        if a[2].kind != nkEmpty:
          let dest = c.irGlobal(s)
          # we don't know if the global was initialized already so we
          # always copy
          genAsgn(c, dest, a[2], requiresCopy=true)
      else:
        let local = c.addVariable(kind, s)
        let val =
          if a[2].kind == nkEmpty: c.irNull(s.typ)
          else: genx(c, a[2])

        # TODO: assign kind handling needs to be rethought, an assign can be both an init _and_ a move (or shallow)
        c.irs.irAsgn(askInit, local, val)

proc genVarSection(c: var TCtx; n: PNode) =
  let lk =
    if n.kind == nkLetSection: lkLet
    else: lkVar

  for a in n:
    case a.kind
    of nkCommentStmt: continue
    of nkVarTuple:
      genVarTuple(c, lk, a)
    of nkIdentDefs:
      if a[0].kind == nkSym:
        genLocalInit(c, lk, a)
      else:
        # initialization of a local that was lifted into a closure's env
        # TODO: tell `genAsgn` that we want initialization
        if a[2].kind != nkEmpty:
          genAsgn(c, a[0], a[2], true)
    else:
      unreachable(a.kind)

proc genArrayConstr(c: var TCtx, n: PNode): IRIndex =
  result = c.getTemp(n.typ)

  if n.len > 0:
    for i, x in n.pairs:
      let
        a = c.genx(x)
        idx = c.irImm(i)

      # XXX: the loss of information due to the lowering might be a problem
      #      for the code-generators
      c.irs.irAsgn(askInit, c.irs.irPathArr(result, idx), a)

  result = c.irs.irUse(result)

proc genSetElem(c: var TCtx, n: PNode, first: int): IRIndex =
  result = c.getTemp(n.typ)

  if first != 0:
    if n.kind in nkIntKinds:
      # a literal value
      result = c.irImm(int(n.intVal - first))
    else:
      result = genx(c, n)
      if first > 0:
        result = c.irCall("-", mSubI, result, first)
      else:
        result = c.irCall("+", mAddI, result, -first)

  else:
    result = genx(c, n)

proc genSetConstr(c: var TCtx, n: PNode): IRIndex =
  result = c.getTemp(n.typ)
  # XXX: since `first` stays the same across the loop, we could invert
  #      the loop around `genSetElem`'s logic...
  let first = firstOrd(c.config, n.typ.skipTypes(abstractInst)).toInt()
  for x in n:
    if x.kind == nkRange:
      let a = c.genSetElem(x[0], first)
      let b = c.genSetElem(x[1], first)
      discard c.irs.irCall(bcInclRange, NoneType, result, a, b)
    else:
      let a = c.genSetElem(x, first)
      discard c.irCall("incl", mIncl, result, a)

func irConv(c: var TCtx, typ: PType, val: IRIndex): IRIndex =
  result = c.irs.irCall(bcConv, c.types.requestType(typ), val)

func irCast(c: var TCtx, typ: PType, val: IRIndex): IRIndex =
  result = c.irs.irCall(bcCast, c.types.requestType(typ), val)


proc genObjConstr(c: var TCtx, n: PNode): IRIndex =
  result = c.getTemp(n.typ)
  let t = n.typ.skipTypes(abstractRange+{tyOwned}-{tyTypeDesc})
  var obj: IRIndex
  if t.kind == tyRef:
    discard c.irs.irCall(bcNew, NoneType, result)
    obj = c.irs.irDeref(result)
  else:
    obj = result

  for i in 1..<n.len:
    let it = n[i]
    c.config.internalAssert(it.kind == nkExprColonExpr and it[0].kind == nkSym, it.info)
    let idx = genField(c, it[0])
    var tmp: IRIndex
    if sfDiscriminant notin it[0].sym.flags:
      tmp = c.genx(it[1])
      let
        le = it[0].sym.typ
        ri = it[1].typ
      if le.kind == tyOpenArray and not sameType(le, ri):
        # XXX: this is a hack to make `tests/vm/tconst_views` work for now.
        #      `transf` removes `nkHiddenStdConv` for array/seq to openArray
        #      conversions, which we could have otherwise relied on
        tmp = c.irConv(le, tmp)

      c.irs.irAsgn(askInit, c.irs.irPathObj(obj, idx), tmp)
    else:
      let (dVal, bVal) = c.genDiscrVal(it[0].sym, it[1], n.typ)
      # TODO: askDiscr should be replaced with a magic call (e.g. bcInitDiscr)
      c.irs.irAsgn(askDiscr, c.irs.irPathObj(obj, idx), dVal)

  result = c.irs.irUse(result)

proc genTupleConstr(c: var TCtx, n: PNode): IRIndex =
  result = c.getTemp(n.typ)
  # a ``nkTupleConstr`` might also represent a tuple type. Don't perform
  # code-gen for those
  if n.typ.kind != tyTypeDesc:
    for i in 0..<n.len:
      let it = n[i]
      let (idx, src) =
        if it.kind == nkExprColonExpr:
          (genField(c, it[0]), it[1])
        else:
          (i, it)

      let tmp = c.genx(src)
      c.irs.irAsgn(askInit, c.irs.irPathObj(result, idx), tmp)

proc genClosureConstr(c: var TCtx, n: PNode): IRIndex =
  let tmp = c.genx(n[0])
  let env =
    if n[1].kind == nkNilLit: c.irNull(c.graph.getSysType(n.info, tyNil))
    else: c.genx(n[1])

  c.irs.irCall(bcNewClosure, c.types.requestType(n.typ), tmp, env)

template wrapCf(code) =
  let next {.inject.} = c.irs.irJoinFwd()
  code
  c.irs.irJoin(next)

proc gen(c: var TCtx; n: PNode; dest: var IRIndex) =
  when defined(nimCompilerStacktraceHints):
    setFrameMsg c.config$n.info & " " & $n.kind

  template nodeType(): TypeId =
    c.types.requestType(n.typ)

  dest = InvalidIndex
  case n.kind
  of nkError:
    # XXX: do a better job with error generation
    fail(n.info, rsemVmCannotGenerateCode, n)

  of nkSym:
    let s = n.sym
    case s.kind
    of skVar, skForVar, skTemp, skLet, skParam, skResult:
      dest = genRdVar(c, n)

    of skProc, skFunc, skConverter, skMacro, skMethod, skIterator:
      dest = genProcLit(c, n, s)
    of skConst:
      # ``transf`` should've inlined all simple constants already
      dest = c.irConst(s)

    of skEnumField:
      unreachable("skEnumField not folded")
    of skType:
      dest = genTypeLit(c, s.typ)
    of skGenericParam:
      if c.prc.sym != nil and c.prc.sym.kind == skMacro:
        dest = genRdVar(c, n)
      else:
        fail(n.info,
          rsemVmCannotGenerateCode,
          sym = s,
          str = "Attempt to generate VM code for generic parameter in non-macro proc"
        )

    else:
      # TODO: shouldn't be a user-error
      fail(n.info,
        rsemVmCannotGenerateCode,
        sym = s,
        str = "Unexpected symbol for VM code - " & $s.kind
      )
  of nkCallKinds:
    if n[0].kind == nkSym and (let s = n[0].sym; s.magic != mNone):
      dest = genMagic(c, n, s.magic)
    else:
      dest = genCall(c, n)
  of nkCharLit..nkInt64Lit:
    dest = c.irLit(n)
  of nkUIntLit..pred(nkNilLit): dest = genLit(c, n)
  of nkNilLit:
    if not n.typ.isEmptyType:
      let t = n.typ.skipTypes(abstractInst)
      internalAssert(c.config,
        t.kind in {tyPtr, tyRef, tyPointer, tyNil, tyProc, tyCstring},
        n.info, $t.kind)
      dest = c.irNull(t)
    else: doAssert false, "why is this needed again?"#unused(c, n)
  of nkAsgn, nkFastAsgn:
    genAsgn(c, n[0], n[1], n.kind == nkAsgn)
  of nkDotExpr: dest = genObjAccess(c, n)
  of nkCheckedFieldExpr: dest = genCheckedObjAccess(c, n)
  of nkBracketExpr: dest = genArrAccess(c, n)
  of nkDerefExpr, nkHiddenDeref: dest = genDeref(c, n)
  of nkAddr, nkHiddenAddr: dest = genAddr(c, n)
  of nkIfStmt, nkIfExpr:
    let fwd = c.irs.irJoinFwd()
    dest = genIf(c, n, fwd)
    c.irs.irJoin(fwd)

  of nkWhenStmt:
    # TODO: wrong
    # This is "when nimvm" node. Choose the second branch.
    gen(c, n[1][0], dest)
  of nkCaseStmt:
    wrapCf: dest = genCase(c, n, next)
  of nkWhileStmt:
    wrapCf:
      genWhile(c, n, next)
  of nkBlockExpr, nkBlockStmt:
    wrapCf:
      dest = genBlock(c, n, next)
  of nkReturnStmt:
    dest = genReturn(c, n)
  of nkRaiseStmt:
    genRaise(c, n)
  of nkBreakStmt:
    dest = genBreak(c, n)
  of nkTryStmt, nkHiddenTryStmt:
    wrapCf:
      dest = genTry(c, n, next)
  of nkStmtList:
    for x in n: gen(c, x)
  of nkStmtListExpr:
    for i in 0..<n.len-1: gen(c, n[i])
    gen(c, n[^1], dest)
  of nkPragmaBlock:
    gen(c, n.lastSon, dest)
  of nkDiscardStmt:
    if n[0].kind != nkEmpty:
      discard genx(c, n[0])
    # TODO: something like `irVoid` might make sense...
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    dest = c.irConv(n.typ, c.genx(n[1]))
  of nkObjDownConv:
    dest = c.irConv(n.typ, c.genx(n[0]))
  of nkObjUpConv:
    dest = c.irConv(n.typ, c.genx(n[0]))
  of nkVarSection, nkLetSection:
    genVarSection(c, n)
  of nkLambdaKinds:
    #let s = n[namePos].sym
    #discard genProc(c, s)
    let s = n[namePos].sym
    dest = genProcLit(c, n, s)
  of nkChckRangeF, nkChckRange64, nkChckRange:
    let
      tmp0 = c.genx(n[0])
      tmp1 = c.genx(n[1])
      tmp2 = c.genx(n[2])

    let destTyp = skipTypes(n.typ, abstractVar)
    if optRangeCheck notin c.options or (destTyp.kind in {tyUInt..tyUInt64} and
       checkUnsignedConversions notin c.config.legacyFeatures):
      # skip the range-check if range-checks are disabled or not applicable
      dest = c.irConv(n.typ, tmp0)
    else:
      dest = c.irs.irCall(bcRangeCheck, nodeType(), tmp0, tmp1, tmp2)
      raiseExit(c)

  of routineDefs:
    dest = InvalidIndex
  of nkEmpty, nkCommentStmt, nkTypeSection, nkConstSection, nkPragma,
     nkIncludeStmt, nkImportStmt, nkFromStmt, nkExportStmt,
     nkMixinStmt, nkBindStmt:
    dest = InvalidIndex
  of nkStringToCString, nkCStringToString:
    gen(c, n[0], dest)
  of nkBracket:
    if isDeepConstExpr(n):
      dest = c.irLit(n)
    elif skipTypes(n.typ, abstractVarRange).kind == tySequence:
      # XXX: why is this even possible? It is, yes
      #c.irLit()
      doAssert false
    else:
      dest = genArrayConstr(c, n)
  of nkCurly:
    if isDeepConstExpr(n):
      dest = c.irLit(n)
    else:
      dest = genSetConstr(c, n)
  of nkObjConstr: dest = genObjConstr(c, n)
  of nkPar, nkTupleConstr: dest = genTupleConstr(c, n)
  of nkClosure: dest = genClosureConstr(c, n)
  of nkCast:
    dest = c.irCast(n.typ, c.genx(n[1]))
  of nkTypeOfExpr:
    dest = genTypeLit(c, n.typ)
  else:
    if n.typ != nil and n.typ.isCompileTimeOnly:
      doAssert false, "why is this needed?"
      dest = genTypeLit(c, n.typ)
    else:
      fail(n.info, rsemVmCannotGenerateCode, n)

iterator iterate(ir: IrStore3, cr: var IrCursor): (int, lent IrNode3) =
  # XXX: `cr` makes more sense as the first parameter, but then we can't use
  #      `lent IrNode3` (since we'd be borrowing from the second parameter)
  var i = 0
  let L = ir.len
  while i < L:
    cr.setPos(i)
    yield (i, ir.at(i))
    inc i

# not working yet
#[
func injectFinalizers(ir: var IrStore3, prc: PProc) =
  ## Adjust ``ntkGoto``s to consider finalizers and also inject scope
  ## finalizers for scopes that have locals defined. A scope finalizer is
  ## basically a 'finally' with a `ntkLocEnd` for each local in the order
  ## their definitions appear in the code
  if prc.finalizers.len == 0 and prc.variables.len == 0:
    # neither finalizers nor locals are used so we can exit early
    return

  var cr: IrCursor
  cr.setup(ir)

  var s = 1

  var localIndex = 0

  for i, n in iterate(ir, cr):
    # TODO: invert the loop; have the outer loop iterate over the scopes and the inner moving the cursor
    if i == prc.scopes[s]:
      i == 1

    case n.kind
    of ntkGoto:
      let p = ir.position(n.target)
      if p notin

      n = ir.position(n.target)

    if isScopeExit:
      let next = localIndex + numLocals[s]
      if next > localIndex:
        let f = cr.newJoinPoint()
        cr.insertJoin(f)
        for v in localIndex..<next:
          cr.insertLocEnd()

        cr.insertContinue()


  ir.update(cr)
]#

func appendCode(c: var vmdef.TCtx, f: CodeFragment) =
  var tmp: CodeFragment
  swap(tmp.code, c.code)
  swap(tmp.debug, c.debug)

  tmp.append(f)

  swap(tmp.code, c.code)
  swap(tmp.debug, c.debug)


proc genStmt*(c: var TCtx; n: PNode): IrGenResult =
  try:
    let r = c.gen2(n)
  except VmGenError as e:
    return typeof(result).err(move e.report)

  result = IrGenResult.ok:
    c.irs

proc genExpr*(c: var TCtx; n: PNode): Result[IRIndex, SemReport] =
  # TODO: function needs a refactoring
  assert not n.typ.isEmptyType()
  result =
    try:
      typeof(result).ok: c.genx(n)
    except VmGenError as e:
      typeof(result).err(move e.report)

proc startProc*(c: var TCtx) =
  discard c.irs.irJoinFwd() # the target for return
  discard c.irs.irJoinFwd() # the target for raise

  c.openScope()

func endProc*(c: var TCtx) =
  # important: the prodecure's scope needs to be closed _before_ the final
  #            joins
  c.closeScope()

  c.irs.irJoin(ExceptionalExit)
  c.irs.irJoin(NormalExit)

proc genProcBody(c: var TCtx; s: PSym, body: PNode) =
    var p = PProc(blocks: @[], sym: s)
    let oldPrc = c.prc
    c.prc = p

    if tfCapturesEnv in s.typ.flags:
      #let env = s.ast[paramsPos].lastSon.sym
      #assert env.position == 2
      discard#c.prc.regInfo.add RegInfo(refCount: 1, kind: slotFixedLet)

    startProc(c)

    # TODO: what's the sfPure flag check needed for?
    if not s.typ[0].isEmptyType() and sfPure notin s.flags:
      # important: the 'result' variable is not tracked in ``prc.variables``
      discard c.genLocal(lkVar, s.ast[resultPos].sym)

    gen(c, body)

    endProc(c)

proc genProc*(c: var TCtx; s: PSym, body: PNode): IrGenResult =
  tryOrReturn:
    c.genProcBody(s, body)

  result = IrGenResult.ok:
    c.irs