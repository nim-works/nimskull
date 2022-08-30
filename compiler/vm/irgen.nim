import
  std/[
    packedsets,
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

from compiler/vm/vmaux import findRecCase, findMatchingBranch, getEnvParam
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

  paramRemap: seq[int] # maps the parameter position given by ``TSym.position``
    ## to the index used for the IR. This is required in the case that
    ## parameters are removed during the translation step (e.g. ``static T``
    ## parameters). The list is empty if the positions can be used directly

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

  # XXX: if constants would use their own ID namespace, `seensConst` could be
  #      a ``Slice[ConstId]``
  seenConsts: PackedSet[SymId] ## used for keeping track of which constants
                               ## still require scanning
  collectedConsts*: seq[PSym]

  defSyms*: DeferredSymbols
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
  let id = c.defSyms.requestSym(sym)
  c.irs.irSym(id)

func irParam(c: var TCtx, sym: PSym): IRIndex =
  c.irs.irParam(sym.position.uint32)

func irGlobal(c: var TCtx, sym: PSym): IRIndex =
  c.irSym(sym)

func irConst(c: var TCtx, sym: PSym): IRIndex =
  assert sym.kind == skConst
  let id = c.defSyms.requestSym(sym)

  if not c.seenConsts.containsOrIncl(id):
    # XXX: collecting constants should *not* be the responsibility of
    #      ``irgen``. But with constants still sharing their ID namespace with
    #      globals, it's the easiest solution for now
    c.collectedConsts.add sym

  c.irs.irSym(id)

func irLit(c: var TCtx, n: PNode): IRIndex =
  let typ =
    if n.typ != nil:
      c.types.requestType(n.typ)
    else:
      NoneType

  c.irs.irLit((n, typ))

func irImm(c: var TCtx, val: SomeInteger): IRIndex =
  c.irs.irLit (newIntNode(nkIntLit, BiggestInt(val)), c.passEnv.sysTypes[tyInt])

template tryOrReturn(code): untyped =
  try:
    code
  except VmGenError as e:
    return IrGenResult.err(move e.report)

func requestType(c: var TCtx, k: TTypeKind): TypeId =
  {.cast(noSideEffect).}:
    # XXX: ``getSysType`` has error reporting related side-effects
    c.types.requestType(c.graph.getSysType(unknownLineInfo, k))

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

func irCall*(ir: var IrStore3, callee: IRIndex, args: varargs[IRIndex]): IRIndex =
  ## A shortcut for procedures taking only immutable arguments
  for arg in args.items:
    discard ir.irUse(arg)
  ir.irCall(callee, args.len.uint32)

func irCall(ir: var IrStore3, bc: BuiltinCall, typ: TypeId, args: varargs[IRIndex]): IRIndex =
  ## A shortcut for procedures taking only immutable arguments
  for arg in args.items:
    discard ir.irUse(arg)
  ir.irCall(bc, typ, args.len.uint32)

func irCall(ir: var IrStore3, m: TMagic, typ: TypeId, args: varargs[IRIndex]): IRIndex =
  ## A shortcut for procedures taking only immutable arguments
  for arg in args.items:
    discard ir.irUse(arg)
  ir.irCall(m, typ, args.len.uint32)

proc irCall(c: var TCtx, name: string, args: varargs[IRIndex]): IRIndex =
  # TODO: compiler procs should be cached here in `TCtx`
  let prc = c.passEnv.getCompilerProc(name)
  c.irs.irCall(c.irs.irProc(prc), args)

func genLocal(c: var TCtx, kind: LocalKind, t: PType): IRIndex =
  let
    tid = c.types.requestType(t)

  c.irs.addLocal Local(kind: kind, typ: tid)

func genLocal(c: var TCtx, kind: LocalKind, s: PSym): IRIndex =
  # TODO: move `LocFlags` somewhere else
  const LocFlags = {sfRegister, sfVolatile} ## flags that are relevant for locations

  let alignment =
    case s.kind
    of skVar, skLet, skForVar: s.alignment
    else: 0

  let local = Local(kind: kind,
                    typ: c.types.requestType(s.typ),
                    decl: c.defSyms.requestDecl(s),
                    loc: LocDesc(flags: s.flags * LocFlags, alignment: alignment.uint32))

  c.irs.addLocal(local)

proc getTemp(cc: var TCtx; tt: PType): IRIndex =
  let id = cc.genLocal(lkTemp, tt)
  cc.irs.irLocal(id)

func irNull(c: var TCtx, t: TypeId): IRIndex =
  # XXX: maybe `irNull` should be a dedicated IR node?
  c.irs.irCall(mDefault, t, c.irs.irLit((nil, t)))

func irNull(c: var TCtx, t: PType): IRIndex =
  # XXX: maybe `irNull` should be a dedicated IR node?
  let t = c.types.requestType(t)
  c.irs.irCall(mDefault, t, c.irs.irLit((nil, t)))

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
      let lab2 = c.irs.irBranch(c.irs.irCall(mNot, c.requestType(tyBool), tmp), next)
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
      # 'branch' currently means "branch if condition" so we have to wrap it
      # in a 'not'
      # TODO: instead of having to use a magic call, the 'branch' should have
      #       a flag to indicate whether or not the condition should be
      #       inverted
      discard c.irs.irBranch(c.irs.irCall(mNot, c.requestType(tyBool), tmp), prev)

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
    if isAnd: c.irs.irCall(mNot, c.requestType(tyBool), a)
    else: a

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

        let cond = c.irs.irCall(bcMatch, NoneType, tmp, c.irLit(branch))

        c.irs.irBranch(cond, b)
        r = c.gen2(branch.lastSon)

      if r[1]:
        if hasValue and r[0] != InvalidIndex:
          # `r[0]` is unset if `branch` ends in a void `noreturn` call, so we
          # have to guard against that case
          c.irs.irAsgn(askInit, dest, r[0])

        c.irs.irGoto(next)

  result = dest

func genTypeLit(c: var TCtx, t: PType): IRIndex

func genExceptCond(c: var TCtx, val: IRIndex, n: PNode, next: JoinPoint) =
  ## Lowers exception matching into an if
  # XXX: maybe too early for this kind of lowering
  for i in 0..<n.len-1:
    assert n[i].kind == nkType
    let cond = c.irs.irCall(mOf, c.requestType(tyBool), val, genTypeLit(c, n[i].typ))
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
    let handler = c.prc.excHandlers.pop() # pop the handler we registered at
                                          # the start

    c.irs.irJoin(handler)
    let eVal = c.irCall("getCurrentException")

    var currNext = handler
    for i in 1..<len:
      let it = n[i]

      if currNext != handler:
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
  let
    boolTy = c.requestType(tyBool)
    cond = c.irs.irCall(mNot, boolTy, c.irs.irCall(bcTestError, boolTy))
  let fwd = c.irs.irJoinFwd()
  c.irs.irBranch(cond, fwd)
  c.irs.irGoto(c.prc.nextHandler())

  c.irs.irJoin(fwd)

func isVarParam(t: PType): bool =
  # XXX: checking if a parameter type is mutable with the logic below is a
  #      bit brittle. Testing for `nkHiddenAddr` won't work however, since
  #      it's elided in the case that a var parameter is passed as a var
  #      argument
  t.skipTypes({tyAlias, tyGenericInst}).kind == tyVar

func irConv(c: var TCtx, typ: PType, val: IRIndex): IRIndex

proc genArg(c: var TCtx, formal: PType, useTemp: bool, n: PNode): IRIndex =
  # TODO: add a test to make sure that a ``move x`` passed to a var parameter
  #       doesn't reach here
  let destTyp =
    if formal == nil: tyVoid
    else:             formal.skipTypes(abstractVarRange-{tySink}).kind

  const OpenArrayLike = {tyOpenArray, tyVarargs}

  case destTyp
  of OpenArrayLike:
    # remove the hidden addr operator that gets introduced when passing
    # something to a ``var openArray``
    let arg = if n.kind == nkHiddenAddr: n[0] else: n
    result = c.genx(arg)
    # restore the conversion that was eliminated by ``transf``
    if arg.typ.skipTypes(abstractVarRange).kind notin OpenArrayLike:
      result = c.irConv(formal.skipTypes(abstractVar), result)

  of tySink:
    # TODO: there needs to be some way to mark an `ntkConsume` as forced (i.e.
    #       no copy must be introduced by the move analyzer)
    result = c.genx(if isMove(n): n[1] else: n)
  else:
    if isMove(n):
      # TODO: the temporary must be destroyed immediately after the call (even
      #       before the raise handling)
      let tmp = c.getTemp(n.typ)
      # TODO: the assignment is also an 'init'...
      c.irs.irAsgn(askMove, tmp, c.genx(n[1]))
      result = tmp
    else:
      result = c.genx(n)


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
    let t =
      if i < fntyp.len: fntyp[i]
      else: nil

    # if they're to be omitted, guard against arguments to compile-time-only
    # parameters
    if t == nil or not t.isCompileTimeOnly or shouldGenCT:
      c.config.internalAssert(t != nil or tfVarargs in fntyp.flags, n[i].info):
        "too many arguments"

      args[L] = genArg(c, t, true, n[i])
      inc L

  # emit the arguments
  for i in 0..<L:
    # TODO: use ntkModify and ntkConsume where applicable
    discard c.irs.irUse(args[i])

  result = c.irs.irCall(callee, L.uint32)
  if canRaiseConservative(n[0]):
    raiseExit(c)

  if n.typ.isEmptyType():
    result = InvalidIndex

template isGlobal(s: PSym): bool = sfGlobal in s.flags
proc isGlobal(n: PNode): bool = n.kind == nkSym and n.sym.isGlobal

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
  let arr = arr.skipTypes(abstractInst)
  if arr.kind == tyArray and (let x = firstOrd(c.config, arr); x != Zero):
    let tmp = c.genx(n)
    result = c.irs.irCall(mSubI, c.types.requestType(arr[0]), tmp, c.irLit(toInt(x)))
  else:
    result = c.genx(n)

proc genCheckedObjAccessAux(c: var TCtx; n: PNode; dest: var IRIndex)

template sizeOfLikeMsg(name): string =
  "'$1' requires '.importc' types to be '.completeStruct'" % [name]

proc isInt8Lit(n: PNode): bool =
  if n.kind in {nkCharLit..nkUInt64Lit}:
    result = n.intVal >= low(int8) and n.intVal <= high(int8)

proc isInt16Lit(n: PNode): bool =
  if n.kind in {nkCharLit..nkUInt64Lit}:
    result = n.intVal >= low(int16) and n.intVal <= high(int16)

func wrapIf(c: var TCtx, wrapper: BuiltinCall, typ: TypeId, expr: IRIndex, cond: bool): IRIndex {.inline.} =
  if cond: c.irs.irCall(wrapper, typ, expr)
  else:    expr

proc genOffset(c: var TCtx, n: PNode, off: Int128): IRIndex =
  let off = off.toInt()
  if n.kind in nkLiterals:
    c.irLit(n.intVal - off)
  else:
    let tmp = genx(c, n)
    # TODO: use mSubU for unsigned integers?
    c.irs.irCall(mSubI, c.types.requestType(n.typ), tmp, c.irLit(off))

proc genMagic(c: var TCtx; n: PNode; m: TMagic): IRIndex =
  result = InvalidIndex
  case m
  of mAnd, mOr:
    let fwd = c.irs.irJoinFwd()
    result = c.genAndOr(n, isAnd = (m == mAnd), fwd)
    c.irs.irJoin(fwd)
  of mAddI..mModI:
    # TODO: mPred and mSucc also need to be checked for overflow - but only if
    #       the operand is signed
    # idea: also insert builtin calls to the various check functions here.
    #       Makes it easier to get uniformity across the back-ends.
    result = c.genCall(n)
    result = c.wrapIf(bcOverflowCheck, c.types.requestType(n.typ), result, optOverflowCheck in c.options)
    if optOverflowCheck in c.options:
      # idea: defects (or error in general) could be encoded as part of the values. I.e. a
      #       `bcOverflowCheck` call would return a result-like value (only on
      #       the IR level, not in the resulting generate code)
      # TODO: unfinished
      c.raiseExit()

  of mInc, mDec:
    # already do the lowering here since it's the same across all targets
    let
      typ = n[1].typ.skipTypes({tyVar})
      typId = c.types.requestType(typ)
      isUnsigned = isUnsigned(typ)
      dest = genx(c, n[1])
      val = genx(c, n[2])

    const Magic = [mInc: [false: mAddI, true: mAddU],
                   mDec: [false: mSubI, true: mSubU]]

    var r = c.irs.irCall(Magic[m][isUnsigned], typId, dest, val)

    let testOverflow = optOverflowCheck in c.options or not isUnsigned
    r = c.wrapIf(bcOverflowCheck, typId, r, testOverflow)

    if testOverflow:
      c.raiseExit()

    result = c.irs.irAsgn(askCopy, dest, r)

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
    # transform ``mGetTypeInfo`` calls into the format the IR expects
    #
    # .. code-block:: nim
    #   getTypeInfo(x)
    #   # -->
    #   addr getTypeInfo(typeof(x))
    #
    let typ = c.types.requestType(c.graph.getCompilerProc("TNimType").typ)
    result = c.irs.irAddr(c.irs.irCall(mGetTypeInfo, typ, genTypeLit(c, n[1].typ)))

  of mDefault:
    assert n[1].typ.kind == tyTypeDesc
    result = c.irs.irCall(mDefault, c.types.requestType(n.typ), genTypeLit(c, n[1].typ[0]))
  of mRunnableExamples:
    discard "just ignore any call to runnableExamples"
  of mDestroy, mTrace:
    # these should not exist yet
    unreachable(n.kind)
  of mMove:
    unreachable("not handled here")
  of mSlice:
    # special-cased in order to handle array parameters that don't start at
    # index '0'
    let
      typId = c.types.requestType(n.typ)
      arg = genx(c, n[1])

    let typ = n.typ.skipTypes(abstractVar)
    if typ.kind == tyArray:
      let first = c.config.firstOrd(typ)
      if first != Zero:
        # the IR transformations don't have access to this information, so we
        # perform the 'first' and 'last' argument adjustment here
        let
          # the arguments need to be emitted in the right order (left-to-right)
          lo = genOffset(c, n[2], first)
          hi = genOffset(c, n[3], first)

        return c.irs.irCall(mSlice, typId, arg, lo, hi)

    # no adjustments needed
    result = c.irs.irCall(mSlice, typId, arg, genx(c, n[2]), genx(c, n[3]))

  of mLengthOpenArray:
    # Consider:
    #
    # .. code-block:: nim
    #
    #   template tmpl(x: openArray[int]) = x.len
    #   var s: seq[int]
    #   tmpl(s)
    #
    # ``mLengthOpenArray`` is used for the ``len`` call here, even though
    # ``x`` is of type ``seq[int]``!

    # XXX: correcting this here is better than allowing it to reach the IR,
    #      but it's still very late. It's better fixed at the root

    let fixup =
      case n[1].typ.skipTypes(abstractVar).kind:
      of tySequence:  mLengthSeq
      of tyString:    mLengthStr
      of tyArray:     mLengthArray
      of tyOpenArray, tyVarargs: mLengthOpenArray
      else: unreachable(n[1].typ.skipTypes(abstractVar).kind)

    result =
      if fixup != mLengthOpenArray:
        c.irs.irCall(fixup, c.types.requestType(n.typ), genx(c, n[1]))
      else:
        # emit the original
        c.irs.irCall(m, c.types.requestType(n.typ), genx(c, n[1]))

  of mConStrStr:
    # the `mConStrStr` magic is very special. Nested calls to it are flattened
    # into a single call in ``transf``. It can't be passed on to ``genCall``
    # since the number of arguments doesn't match with the number of parameters
    var args = newSeq[IRIndex](n.len - 1)
    for i in 1..<n.len:
      # we need no temporaries here since all arguments are read-only
      args[i-1] = c.genx(n[i])

    result = c.irs.irCall(mConStrStr, c.types.requestType(n.typ), args)
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

  of mEnumToStr:
    # XXX: this works, but it also means that targets can't use dedicated
    #      logic for how they handle enum-to-str conversion. A better approach
    #      would be to not transform the magic here and let the targets use
    #      some form of lifting pass to replace the magic with whatever they
    #      want/need
    # XXX: rtti-based memory management strategies used ``reprEnum`` for
    #      this
    let
      t = n[1].typ.skipTypes(abstractRange)
      s = c.genProcSym c.graph.getToStringProc(t)
    result = c.irs.irCall(s, c.genx(n[1]))

  of mEcho:
    # unpack the array expression here, so that the target-specific handling
    # for ``echo`` is simpler
    let arr = n[1]
    var args = newSeq[IRIndex](arr.len)

    for i in 0..<arr.len:
      args[i] = c.genx(arr[i])

    for it in args.items:
      discard c.irs.irUse(it)

    discard c.irs.irCall(mEcho, c.requestType(tyVoid), args.len.uint32)

    # depending on how the target implements echo, the corrsponding IR pass
    # might needs the array type of the original arguments. We make sure the
    # type exists here so that the pass doesn't have to create it first.
    assert n[1].typ.kind == tyArray
    assert n[1].typ.elemType.kind == tyString
    discard c.types.requestType(n[1].typ)

  of mSizeOf:
    # ``sizeof`` is a generic procedure that doesn't get instantiated during
    # sem - the generic routine symbol is instead kept. For compiler-known
    # sizes, the expression is folded into a literal, but for types with
    # unknown size (e.g. `.incompleteStruct`), the raw call expression is left
    # as is. Passing a non-instantiated routine to `requestProc` would cause
    # an error during type translation because of the ``tyGenericParam``.
    # Instead, we replace the original expression with a direct magic call.
    result = c.irs.irCall(mSizeOf, c.types.requestType(n.typ), c.genx(n[1]))

  of mHigh:
    # --->
    #   len(x) - 1
    let lenCall =
      case n[1].typ.skipTypes(abstractVar).kind:
      of tySequence:  mLengthSeq
      of tyString:    mLengthStr
      of tyArray:     mLengthArray
      of tyOpenArray, tyVarargs: mLengthOpenArray
      else: unreachable()
    let typ = c.types.requestType(n.typ)

    result = c.irs.irCall(mSubI, typ, c.irs.irCall(lenCall, typ, c.genx(n[1])), c.irLit(1))

  of mOf:
    # ``mOf`` uses a ``typedesc`` parameter which would be omitted by
    # ``genCall``, so we manually translate it
    assert n[2].typ.kind == tyTypeDesc
    result = c.irs.irCall(mOf, c.types.requestType(n.typ), c.genx(n[1]), c.genTypeLit(n[2].typ[0]))

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
    result[1] = c.irs.irCall(bcGetBranchIndex, NoneType, tmp, c.genTypeLit(oty), c.irLit(discr.position))

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
    if s.position < c.prc.sym.typ.len - 1:
      let p =
        if c.prc.paramRemap.len == 0:
          s.position
        else:
          c.prc.paramRemap[s.position]

      c.irs.irParam(p.uint32)
    else:
      assert tfCapturesEnv in c.prc.sym.typ.flags
      # the parameter is the hidden environment parameter
      let envT = c.types.requestType(getEnvParam(c.prc.sym).typ)
      c.irs.irCall(bcAccessEnv, envT)

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
    var cond = c.irs.irCall(mInSet, c.requestType(tyBool), c.irLit(checkExpr[1]), discVal)
    if negCheck:
      cond = c.irs.irCall(mNot, c.requestType(tyBool), cond)

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
          if a[2].kind == nkEmpty:
            if sfNoInit in s.flags: return # don't initialize with empty
            else:                   c.irNull(s.typ)
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
      # XXX: the correct type would be ``typeof(ord(val) - ord(low(T)))``
      let t = c.requestType(tyInt)

      result = genx(c, n)
      if first > 0:
        result = c.irs.irCall(mSubI, t, result, c.irLit(first))
      else:
        result = c.irs.irCall(mAddI, t, result, c.irLit(-first))

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
      discard c.irs.irCall(mIncl, c.types.requestType(nil), result, a)

func irConv(c: var TCtx, typ: PType, val: IRIndex): IRIndex =
  result = c.irs.irConv(c.types.requestType(typ), val)

func irCast(c: var TCtx, typ: PType, val: IRIndex): IRIndex =
  result = c.irs.irCast(c.types.requestType(typ), val)


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
    if n[1].kind == nkNilLit: c.irNull(c.passEnv.sysTypes[tyPointer])
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
      dest = c.irNull(n.typ)
    else: doAssert false, "why is this needed again?"#unused(c, n)
  of nkAsgn, nkFastAsgn:
    genAsgn(c, n[0], n[1], n.kind == nkAsgn)
  of nkDotExpr: dest = genObjAccess(c, n)
  of nkCheckedFieldExpr: dest = genCheckedObjAccess(c, n)
  of nkBracketExpr: dest = genArrAccess(c, n)
  of nkDerefExpr: dest = genDeref(c, n)
  of nkHiddenDeref:
    if n[0].typ.skipTypes({tyVar, tyLent}).kind == tyOpenArray:
      # don't generate a 'deref' for openArray
      # XXX: sem shouldn't introduce a ``nkHiddenDeref`` for openArrays in the
      #      first place
      dest = genx(c, n[0])
    else:
      dest = genDeref(c, n)
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

    # TODO: move this elsewhere
    block:
      var needsRemap = false

      # figure out the number of parameters that we want to skip
      for i in 1..<s.typ.len:
        if s.typ[i].isCompileTimeOnly():
          needsRemap = true
          break

      if needsRemap:
        c.prc.paramRemap.newSeq(s.typ.len - 1) # -1 for the return type
        var j = 0
        for i in 1..<s.typ.len:
          c.prc.paramRemap[i - 1] =
            if s.typ[i].isCompileTimeOnly():
              -1
            else:
              let tmp = j; inc j; tmp

    if tfCapturesEnv in s.typ.flags:
      #let env = s.ast[paramsPos].lastSon.sym
      #assert env.position == 2
      discard#c.prc.regInfo.add RegInfo(refCount: 1, kind: slotFixedLet)

    startProc(c)

    # TODO: what's the sfPure flag check needed for?
    if not s.typ[0].isEmptyType() and sfPure notin s.flags:
      # TODO: respect ``sfNoInit``
      # important: the 'result' variable is not tracked in ``prc.variables``
      discard c.genLocal(lkVar, s.ast[resultPos].sym)

    gen(c, body)

    endProc(c)

proc genProc*(c: var TCtx; s: PSym, body: PNode): IrGenResult =
  tryOrReturn:
    c.genProcBody(s, body)

  result = IrGenResult.ok:
    c.irs