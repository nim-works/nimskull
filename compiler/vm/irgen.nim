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
    irliterals,
    irtypes,
    vmir
  ],
  experimental/[
    results
  ]

from compiler/vm/vmaux import findRecCase, findMatchingBranch, getEnvParam
from compiler/vm/vmdef import unreachable

from compiler/utils/ropes import `$`

# XXX: temporary import; needed for ``PassEnv``
import compiler/vm/irpasses

type
  LocalId = int
  ScopeId = int

  Scope = object
    firstLocal: int ## an index into ``PProc.activeLocals``

    # TODO: using -1 to represent unset for ``JoinPoint`` is problematic for a
    #       multitude of reasons. An ID-like distinct type should be used for
    #       it too!
    finalizer: JoinPoint
    handler: JoinPoint

  TBlock = object
    label: PSym
    start: JoinPoint

    scope: ScopeId ## the scope attached to this block

type PProc* = object
  sym*: PSym

  blocks: seq[TBlock]
  variables: seq[LocalId] ## each non-temporary local in the order of their definition

  scopes: seq[Scope]

  activeLocals: seq[LocalId]
  locals: Table[int, int]

  paramRemap: seq[int] # maps the parameter position given by ``TSym.position``
    ## to the index used for the IR. This is required in the case that
    ## parameters are removed during the translation step (e.g. ``static T``
    ## parameters). The list is empty if the positions can be used directly

type TCtx* = object

  irs*: IrStore3

  prc*: PProc

  graph*: ModuleGraph # only needed for testing if a proc has a body
  idgen*: IdGenerator # needed for creating magics on-demand

  magicPredicate*: proc (m: TMagic): bool {.noSideEffect, nimcall.} ##
    ## Called to decided if a procedure with the given magic is to be treated
    ## as a real procedure. Use 'true' to indicate yes - 'false' otherwise
  # XXX: a proc is used instead of a ``set[TMagic]``, due to the latter being
  #      33 bytes in size

  passEnv*: PassEnv

  module*: PSym

  config*: ConfigRef

  options*: set[TOption]

  # XXX: if constants would use their own ID namespace, `seensConst` could be
  #      a ``Slice[ConstId]``
  seenConsts: PackedSet[SymId] ## used for keeping track of which constants
                               ## still require scanning
  collectedConsts*: seq[PSym]

  constData*: Table[SymId, PNode]

  defSyms*: DeferredSymbols
  procs*: ProcedureEnv
  types*: DeferredTypeGen
  data*: LiteralData


type IrGenResult* = Result[IrStore3, SemReport]

when defined(nimCompilerStacktraceHints):
  import std/stackframes

type
  VmGenError = object of CatchableError
    report: SemReport

const
  NormalExit = 0

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
    c.constData[id] = astdef(sym)

  c.irs.irSym(id)

func irLit(c: var TCtx, n: PNode): IRIndex =
  let typ =
    if n.typ != nil:
      c.types.requestType(n.typ)
    elif n.kind in nkStrLit..nkTripleStrLit:
      # XXX: without type information, we cannot know if the string literal
      #      is supposed to be a Nim-string or cstring, and so fall back to
      #      unconditionally treating it as a Nim-string. Not doing so
      #      would result in the code-generator treating it as a cstring
      # TODO: instead of this workaround, each occurence in the compiler where
      #       a string literal without type information is inserted needs to
      #       corrected instead. One such occurence is ``genEnumToStrProc``.
      {.noSideEffect.}:
        c.types.requestType(c.graph.getSysType(unknownLineInfo, tyString))
    else:
      NoneType

  c.irs.irLit (c.data.add(n), typ)

func irImm(c: var TCtx, val: SomeInteger): IRIndex =
  # TODO: ``ntkImm(ediate)`` needs to be used here
  c.irs.irLit (c.data.newLit(val), c.passEnv.sysTypes[tyInt])

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

func pushScope(p: var PProc; finalizer, handler = JoinPoint(-1)) =
  p.scopes.add Scope(firstLocal: p.activeLocals.len,
                     finalizer: finalizer, handler: handler)

func popScope(p: var PProc) =
  # TODO: take ``PProc`` instead of ``TCtx``
  let scope = p.scopes.pop()
  p.activeLocals.setLen(scope.firstLocal)

# XXX: the emission of ``ntkLocEnd`` instructions is disabled for now. When
#      building the compiler with ``enableLocEnd = true``, the total code IR
#      memory usage increases by 70 MB (!) and the time taken to execute for
#      all code passes increases by ~50%. There are two approaches on how to
#      continue here:
#      1. reduce the amount of emitted ``ntkLocEnd`` instructions. Reuse
#        duplicated cleanup sections (of which there are likely a lot) by
#        leveraging the linked section.
#        Reducing the amount of raise exits by taking ``sfNeverRaises`` and
#        etc. into account will also reduce the amount of cleanup code.
#        ``ntkLocEnd`` is a very small instruction and will only take up
#        8-byte instead of the current 32-byte after the packed code
#        representation is used. Specifying a range of locals instead of only
#        a single one would also help with reducing the amount of instructions.
#      2. use a different approach for transporting lifetime information to
#        the analysis passes. For example, an out-of-band approach where the
#        slice of locals for which the lifetime ends is attached to each
#        '(goto|goto-link|continue)' instruction. That will be a problem if the
#        code is changed between ``irgen`` and the analysis passes however.
const enableLocEnd = false

proc handleExit(code: var IrStore3, prc: PProc, numScopes: int = 1) =
  ## Emits the cleanup code for exiting the provided number of scopes
  ## (`numScopes`). ``handleExit`` should be used before emitting control-flow
  ## for leaving a scope in the non-exceptional case
  var localsEnd = prc.activeLocals.len
  for i in countdown(prc.scopes.high, prc.scopes.len-numScopes):
    let s = prc.scopes[i]
    # first destroy the locals in the scope, and only then execute the
    # finalizer
    when enableLocEnd:
      for j in countdown(localsEnd - 1, s.firstLocal):
        code.irLocEnd(prc.activeLocals[j])

      localsEnd = s.firstLocal

    if s.finalizer != -1:
      # the scope has a finalizer attached - visit it
      code.irGotoLink(s.finalizer)

proc cleanupScope(code: var IrStore3, prc: PProc) =
  ## Emits a ``ntkLocEnd`` for each local in the current scope
  let s = prc.scopes[^1]
  when enableLocEnd:
    for j in countdown(prc.activeLocals.high, s.firstLocal):
      code.irLocEnd(prc.activeLocals[j])

proc handleRaise(code: var IrStore3, prc: PProc) =
  ## Searches for the nearest scope that has an exception handler attached and
  ## emits the cleanup logic for all scopes leading up to and including the
  ## one with the found handler. Emits a 'goto' to the exception handler at the
  ## end
  var localsEnd = prc.activeLocals.len
  for i in countdown(prc.scopes.high, 0):
    let s = prc.scopes[i]
    # first destroy the locals in the block
    when enableLocEnd:
      for j in countdown(localsEnd - 1, s.firstLocal):
        code.irLocEnd(j)

      localsEnd = s.firstLocal

    if s.handler != -1:
      # do not invoke the finalizer - that's the responsiblity of the
      # exception handler
      code.irGoto(s.handler)
      return

    if s.finalizer != -1:
      code.irGotoLink(s.finalizer)

  unreachable("no handler was found")

proc genProcSym(c: var TCtx, s: PSym): IRIndex =
  # prevent accidentally registering magics:
  assert s.magic == mNone or c.magicPredicate(s.magic)
  c.irs.irProc():
    if lfImportCompilerProc notin s.loc.flags:
      # common case
      c.procs.requestProc(s)
    else:
      # the procedure is an importc'ed ``.compilerproc`` --> use the
      # referenced ``.compilerproc`` directly
      c.passEnv.getCompilerProc($s.loc.r)

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

func irNull(c: var TCtx, t: PType): IRIndex

proc getTemp(cc: var TCtx; tt: PType): IRIndex =
  let id = cc.genLocal(lkTemp, tt)
  # XXX: zero-initializing the temporaries is better than the previous
  #      default-initialization, but there are still a few issues:
  #      - if the temporary is fully initialized (all of it's bits are written
  #        to prior to the first usage) it's unnecessary
  #      - the temporary should be implicitly zero-initialized. That is, a later
  #        pass that maps the IR's semantics to the target's semantics should
  #        be responsible for zero-initializing if required. With something
  #        like a ``ntkLocStart``, that's not going to be possible however.
  discard cc.irs.irCall(bcInitLoc, cc.requestType(tyVoid), cc.irs.irLocal(id))
  cc.irs.irLocal(id)

func irNull(c: var TCtx, t: TypeId): IRIndex =
  # XXX: maybe `irNull` should be a dedicated IR node?
  c.irs.irCall(mDefault, t, c.irs.irLit((NoneLit, t)))

func irNull(c: var TCtx, t: PType): IRIndex =
  # XXX: maybe `irNull` should be a dedicated IR node?
  let t = c.types.requestType(t)
  c.irs.irCall(mDefault, t, c.irs.irLit((NoneLit, t)))

proc popBlock(c: var TCtx; oldLen: int) =
  #for f in c.prc.blocks[oldLen].fixups:
  #  c.patch(f)
  c.prc.blocks.setLen(oldLen)

template withScope(body: untyped) {.dirty.} =
  pushScope(c.prc)
  body
  popScope(c.prc)

template withBlock(labl: PSym; next: JoinPoint; body: untyped) {.dirty.} =
  var oldLen {.gensym.} = c.prc.blocks.len
  withScope:
    c.prc.blocks.add TBlock(label: labl, start: next, scope: c.prc.scopes.high)
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
  # XXX: the ``withBlock`` usage here is probably wrong now
  withBlock(nil, next):

    c.prc.pushScope()
    let loop = c.irs.irLoopJoin()
    if isTrue(n[0]):
      # don't emit a branch if the condition is always true
      discard
    else:
      # TODO: omit the while loop if cond == false?
      let
        tmp = c.genx(n[0])
        fwd = c.irs.irJoinFwd()

      # XXX: the following pattern is quite common. Maybe ``ntkBranch`` should
      #      get an extra flag for indicating that it's an "unstructured branch"
      #      (or "conditional goto") so that the pattern can be replaced with
      #      just a branch? A ``ntkBranch`` is currently required to never
      #      jump out of the current logical scope
      c.irs.irBranch(tmp, fwd)
      # TODO: the condition expression might introduce locals. Are they part
      #       of the while's scope or of it's enclosing one?
      c.irs.irGoto(next)
      c.irs.irJoin(fwd)

    let exits = c.genStmt2(n[1])
    if exits:
      # destroy locals defined inside the loop once an iteration finishes
      handleExit(c.irs, c.prc)
      discard c.irs.irGoto(loop)
      #entrances.add(c.irs.irGetCf())

    c.prc.popScope()

  #c.irs.irPatchStart(start, entrances)

  #result = c.irs.irJoin(c.prc.blocks[^1].endings)

proc genBlock(c: var TCtx; n: PNode, next: JoinPoint): IRIndex =
  withBlock(n[0].sym, next):
    c.gen(n[1], result)

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

  # exit all scopes including the one of the block we're breaking out of
  handleExit(c.irs, c.prc, c.prc.scopes.len - c.prc.blocks[i].scope)
  c.irs.irGoto(c.prc.blocks[i].start)

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

      # destroy locals inside the if's scope *after* the result variable
      # has been initialized
      handleExit(c.irs, c.prc)
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

proc ofBranchToLit(d: var LiteralData, n: PNode, kind: TTypeKind): LiteralId =
  ## Generates a literal from the given ``nkOfBranch`` node. If the branch has
  ## only a single value, an atomic literal (int, string, float) is
  ## generated - a slice-list otherwise
  assert n.kind == nkOfBranch
  const
    IntTypes = {tyBool, tyChar, tyInt..tyInt64, tyUInt..tyUInt64, tyEnum}
    FloatTypes = {tyFloat..tyFloat128}

  if n.len == 2 and n[0].kind != nkRange:
    # a single-valued of-branch
    result =
      case kind
      of IntTypes:   d.newLit n[0].intVal
      of FloatTypes: d.newLit n[0].floatVal
      # TODO: also support cstrings
      of tyString:   d.newLit n[0].strVal
      else: unreachable(kind)

  else:
    # the branch uses a slice-list. They're stored as an array of pairs
    var arr = d.startArray((n.len - 1) * 2)

    template addAll(field: untyped, kinds: set[TNodeKind]) =
      for i in 0..<n.len-1:
        let it = n[i]
        case it.kind
        of nkRange:
          d.addLit(arr): d.newLit(it[0].field)
          d.addLit(arr): d.newLit(it[1].field)
        of kinds:
          let lit = d.newLit(it.field)
          d.addLit(arr): lit
          d.addLit(arr): lit
        else:
          unreachable(it.kind)

    case kind
    of IntTypes:   addAll(intVal, nkIntKinds)
    of FloatTypes: addAll(floatVal, nkFloatLiterals)
    of tyString:   addAll(strVal, nkStrKinds)
    else:          unreachable(kind)

    result = d.finish(arr)


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

        let lit = ofBranchToLit(c.data, branch, selType.kind)
        let cond = c.irs.irCall(bcMatch, NoneType, tmp, c.irs.irLit((lit, NoneType)))

        c.irs.irBranch(c.irs.irCall(mNot, c.requestType(tyBool), cond), b)
        r = c.gen2(branch.lastSon)

      if r[1]:
        if hasValue and r[0] != InvalidIndex:
          # `r[0]` is unset if `branch` ends in a void `noreturn` call, so we
          # have to guard against that case
          c.irs.irAsgn(askInit, dest, r[0])

        handleExit(c.irs, c.prc)
        c.irs.irGoto(next)

  result = dest

func genTypeLit(c: var TCtx, t: PType): IRIndex

func genExceptCond(c: var TCtx, val: IRIndex, n: PNode, next: JoinPoint) =
  ## Lowers exception matching into an if
  # XXX: maybe too early for this kind of lowering
  # XXX: how the exception matching is implemented should be left to a later
  #      phase. That is, something like a ``bcMatchException`` should be
  #      emitted here instead
  for i in 0..<n.len-1:
    assert n[i].kind == nkType
    let cond = c.irs.irCall(mOf, c.requestType(tyBool), val, genTypeLit(c, n[i].typ))
    c.irs.irBranch(c.irs.irCall(mNot, c.requestType(tyBool), cond), next)

proc genTry(c: var TCtx; n: PNode, next: JoinPoint): IRIndex =
  let
    hasFinally = n.lastSon.kind == nkFinally
    hasExcept = n[1].kind == nkExceptBranch

    finalizer =
      if hasFinally: c.irs.irJoinFwd()
      else:          -1

    handler =
      if hasExcept: c.irs.irJoinFwd()
      else:         -1

  # the finally also applies for the ``except`` blocks
  pushScope(c.prc, finalizer=finalizer) # the scope for the exception handler

  # TODO: move the translation of the ``try``'s body into a separate
  #       procedure
  pushScope(c.prc, handler=handler) # scope for the try

  let dest =
    if not isEmptyType(n.typ): c.getTemp(n.typ)
    else: InvalidIndex

  let r = c.gen2(n[0])

  # XXX: instead of handling the omission of dead code before emitting the
  #      code (e.g. here), it's maybe a better idea to use a separate pass
  #      for that
  if r.exits:
    if dest != InvalidIndex:
      # TODO: assert that gen2 doesn't return a value
      c.irs.irAsgn(askInit, dest, r.r)
    #[let t =
      if hasFinally: c.prc.finalizers[^1]
      else: next]#

    handleExit(c.irs, c.prc, 2) # invokes the finally block if one exists
    c.irs.irGoto(next)

  popScope(c.prc)

  let len =
    if hasFinally: n.len-1
    else: n.len

  if hasExcept:
    c.irs.irJoin(handler)
    let
      # TODO: don't use ``getCurrentException`` here; introduce and emit a new
      #       builtin instead
      # XXX: ``cgen`` uses ``nimBorrowCurrentException``
      eVal = c.irCall("getCurrentException")
      exit = c.irs.irJoinFwd() # where to exit to in case of a sucessfully
                               # processed exception

    # if the last exception clause has only one sub-node, it's a general
    # catch-all handler in which case we don't need to generate one ourselves
    let needsUnhandled = n[len - 1].len > 1

    var currNext = JoinPoint(-1)
    for i in 1..<len:
      let it = n[i]

      if currNext != JoinPoint(-1):
        c.irs.irJoin(currNext)

      currNext =
        if i < len - 1 or needsUnhandled:
          # the next matcher (or the "didn't match" branch):
          c.irs.irJoinFwd()
        else:
          exit

      c.genExceptCond(eVal, it, currNext)
      discard c.irs.irCall(bcEnterExcHandler, c.requestType(tyVoid))

      let r = c.gen2(it.lastSon)
      if r.exits:
        if r.r != InvalidIndex:
          # XXX: the guard below is wrong, but sem currently doesn't report an
          #      error in this case
          if dest != InvalidIndex:
            c.irs.irAsgn(askInit, dest, r.r)

        c.irs.irGoto(exit)

    # XXX: there's an optimization possible where if ``handleRaise`` is nested
    #      inside an 'except' section, it jumps to the 'unhandled' handler of
    #      the exception handler it's nested inside. This would reduce the
    #      amount of emitted cleanup code and also simplify control-flow a bit.
    #      ``cgen`` implements this optimization
    if needsUnhandled:
      # the "unhandled" branch. The exception didn't match any of the provided types,
      # so we continue raising the exception
      c.irs.irJoin(currNext)
      handleRaise(c.irs, c.prc)

    # the "exception was handled" path
    block:
      c.irs.irJoin(exit)
      discard c.irs.irCall(bcExitRaise, c.requestType(tyVoid))

      handleExit(c.irs, c.prc)
      c.irs.irGoto(next)

  # pop the scope before generating the finalizer, as the finalizer would
  # otherwise apply to itself
  popScope(c.prc)

  if hasFinally:
    # TODO: error mode needs to be disabled for the duration of the 'finally'
    #       section. ``cgen`` omits the error mode disabling if no statement
    #       in the body of the 'finally' raises
    pushScope(c.prc)
    c.irs.irJoin(finalizer)

    let r = c.gen2(lastSon(n)[0])
    # a 'finally' section never has a result
    if r.exits:
      cleanupScope(c.irs, c.prc)
      # where execution resumes after the 'finally' depends on
      # run-time control-flow
      c.irs.irContinue()

    popScope(c.prc)

  result = dest

proc genRaise(c: var TCtx; n: PNode) =
  if n[0].kind != nkEmpty:
    let
      dest = c.genx(n[0])
      typ = skipTypes(n[0].typ, abstractPtrs)

    # create a string literal with the exception's name
    let nameLit = (c.data.newLit(typ.sym.name.s),
                   c.passEnv.sysTypes[tyCstring])

    discard c.irs.irCall(bcRaise, NoneType, dest, c.irs.irLit nameLit)
  else:
    # reraise
    discard c.irs.irCall(bcRaise, NoneType)

  # XXX: if the exception's type is statically known, we could do the
  #      exception branch matching at compile-time (i.e. here)
  handleRaise(c.irs, c.prc)

func resultVar(p: PProc): IRIndex =
  doAssert false

proc genReturn(c: var TCtx; n: PNode): IRIndex =
  if n[0].kind != nkEmpty:
    discard genStmt2(c, n[0])

  # exit all scopes
  handleExit(c.irs, c.prc, numScopes = c.prc.scopes.len)
  c.irs.irGoto(NormalExit)

proc genLit(c: var TCtx; n: PNode): IRIndex =
  c.irLit(n)


proc genProcLit(c: var TCtx, n: PNode, s: PSym): IRIndex =
  genProcSym(c, s)

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
    fwd = c.irs.irJoinFwd()

  c.irs.irBranch(cond, fwd)
  handleRaise(c.irs, c.prc)

  c.irs.irJoin(fwd)

func isVarParam(t: PType): bool =
  # XXX: checking if a parameter type is mutable with the logic below is a
  #      bit brittle. Testing for `nkHiddenAddr` won't work however, since
  #      it's elided in the case that a var parameter is passed as a var
  #      argument
  t.skipTypes({tyAlias, tyGenericInst}).kind == tyVar

func irConv(c: var TCtx, typ: PType, val: IRIndex): IRIndex

func genMove(c: var TCtx, dest, src: IRIndex) =
  # TODO: inserting ``mWasMoved`` here is too early! The move analyser should
  #       be responsible for this. In order for that to work, we'll probably
  #       have to leave the ``mMove`` magic as is during ``irgen`` so that
  #       the move analyser can correctly handle it (``askMove`` and ``mMove``
  #       do *not* have the same meaning)
  c.irs.irAsgn(askMove, dest, src)
  discard c.irs.irCall(mWasMoved, c.requestType(tyVoid), src)

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
      genMove(c, tmp, c.genx(n[1]))
      result = tmp
    else:
      result = c.genx(n)

proc genCallee(c: var TCtx, n: PNode): tuple[loc: IRIndex, m: TMagic] =
  ## Generates the code for the callee `n`, but only if `n` is not the symbol
  ## of a non-real magic procedure. A real procedure is one that requires
  ## a call in the final generated code
  if n.kind == nkSym and n.sym.kind in routineKinds:
    # a direct call
    let s = n.sym
    if s.magic == mNone or c.magicPredicate(s.magic):
      # a call to a "real" procedure
      (genProcSym(c, s), mNone)
    else:
      # a magic call
      (InvalidIndex, s.magic)

  else:
    # an indirect call
    (genx(c, n), mNone)

proc genCall(c: var TCtx; n: PNode): IRIndex =
  let fntyp = skipTypes(n[0].typ, abstractInst)

  # Important: call expressions (as all other expression and statments) are
  # evaluated strictly left-to-right

  let shouldGenCT = false ## whether static and typeDesc parameters should be
                          ## code-gen'ed

  let callee = genCallee(c, n[0])

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

  result =
    if callee.loc == InvalidIndex:
      # don't trust ``n.typ`` and instead use the return type of the procedure
      c.irs.irCall(callee.m,
                   c.types.requestType(n[0].sym.getReturnType()),
                   L.uint32)
    else:
      c.irs.irCall(callee.loc, L.uint32)

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

proc fewCmps(conf: ConfigRef; s: PNode): bool =
  # XXX: copied from ``ccgexprs.nim``

  # this function estimates whether it is better to emit code
  # for constructing the set or generating a bunch of comparisons directly
  if s.kind != nkCurly:
    result = false
  elif (getSize(conf, s.typ) <= conf.target.intSize) and
       (nfAllConst in s.flags):
    result = false
  elif elemType(s.typ).kind in {tyInt, tyInt16..tyInt64}:
    result = true
  else:
    result = s.len <= 8

proc irLit(c: var TCtx, b: bool): IRIndex =
  c.irs.irLit (c.data.newLit(ord(b)), c.requestType(tyBool))

# TODO: implement this as an IR pass instead
proc genSetCmp(c: var TCtx, setExpr, elem: PNode): IRIndex =
  ## Generates an if-then-else chain for testing if `elem` is present in the
  ## ``set`` that `setExpr` evaluates to
  assert setExpr.kind == nkCurly
  # note: do not use ``genSetElem`` here
  # XXX: emitting the `elem` expression first is an evaluation-order violation
  #      (``cgen`` has the same problem). Turning this fixup into an IR pass
  #      will allow us to fix the order
  let e = genx(c, elem)

  if setExpr.len > 0:
    # --->
    #   result = true
    #    a := ...
    #    b := ...
    #    cond := elem < a
    #    branch cond, next_1
    #    cond := b < elem
    #    branch cond, next_1
    #    goto exit # a match
    #   next_1:
    #    a := ...
    #    cond := elem == a
    #    branch cond, next_2
    #    ...
    #    result = false # no match
    #   exit:

    let (lt, eq) =
      case elem.typ.skipTypes(abstractVarRange).kind
      of tyInt, tyInt8..tyInt64:    (mLtI, mEqI)
      of tyUInt, tyUInt8..tyUInt64: (mLtU, mEqI)
      of tyChar: (mLtCh,   mLtCh)
      of tyBool: (mLtB,    mEqB)
      of tyEnum: (mLtEnum, mEqEnum)
      else:      unreachable()

    let
      tmp = c.getTemp(c.graph.getSysType(unknownLineInfo, tyBool))
      boolTy = c.requestType(tyBool)
      exit = c.irs.irJoinFwd()

    c.irs.irAsgn(askInit, tmp, c.irLit(true))

    for n in setExpr.items:
      if n.kind == nkRange:
        let
          a = genx(c, n[0])
          b = genx(c, n[1])
          next = c.irs.irJoinFwd()

        c.irs.irBranch(c.irs.irCall(lt, boolTy, e, a), next)
        c.irs.irBranch(c.irs.irCall(lt, boolTy, b, e), next)
        c.irs.irGoto(exit) # a match is found
        c.irs.irJoin(next)

      else:
        # if the element matches, we're finished
        c.irs.irBranch(c.irs.irCall(eq, boolTy, e, genx(c, n)), exit)

    c.irs.irAsgn(askCopy, tmp, c.irLit(false)) # no match was found
    c.irs.irJoin(exit)

    result = tmp

  else:
    # handle the case of an empty set
    result = c.irLit(false) # always false

proc genSetElem(c: var TCtx, n: PNode, typ: PType): IRIndex {.inline.}

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

    let testOverflow = optOverflowCheck in c.options and not isUnsigned
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
    # XXX: data-flow-analysis should know about the swap, so lowering it here
    #      is a problem
    c.irs.irAsgn(askBlit, tmp, a)
    c.irs.irAsgn(askMove, a, b)
    c.irs.irAsgn(askMove, b, tmp)
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
    # create a temporary and move into it
    # TODO: the lifetime of said temporary needs to be figured out
    result = getTemp(c, n.typ)
    genMove(c, result, genx(c, n[1]))
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
      discard c.irs.irCall(mNew, c.requestType(tyVoid), genx(c, n[1]))
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

  of mOffsetOf:
    # ``offsetOf`` is folded into a literal if the offset is known during
    # sem - if it's not, the magic reaches here. It's currently also inserted
    # by ``injectdestructors``. The expression gets transformed into:
    #
    # .. code-block::nim
    #
    #   # `offsetOf(a.b)`
    #   offset
    let dotExpr =
      case n[1].kind
      of nkDotExpr:          n[1]
      of nkCheckedFieldExpr: n[1][0]
      else: unreachable()

    let objTyp = dotExpr[0].typ.skipTypes(abstractVarRange)

    # the type of the path expression on the left *must* be the record type
    # the member is located in - base types won't work
    assert lookupInRecord(objTyp.n, dotExpr[1].sym.name) != nil

    result = c.irs.irCall(mOffsetOf, c.types.requestType(n.typ), c.genTypeLit(dotExpr[0].typ), c.irImm(c.genField(dotExpr[1])))

  of mInSet:
    if fewCmps(c.config, n[1]):
      # WARNING: the range-check is omitted for all signed-integer-based sets,
      #          allowing code that would otherwise fail at run-time to pass!
      #          For example:
      #
      #          .. code-block::nim
      #
      #            let i = -1
      #            assert i notin {1, 2}
      #
      #          would raise a ``RangeDefect`` without this special case
      let elem =
        if n[2].kind in {nkChckRange, nkChckRange64}:
          n[2][0]
        else:
          n[2]

      if isDeepConstExpr(n[1]):
        # note: don't inline `a` and `b` - the evaluation order would be
        #       violated
        let
          a = genx(c, n[1])
          b = genSetElem(c, elem, n[1].typ)

        result = c.irs.irCall(bcMatch, c.requestType(tyBool), b, a)
      else:
        result = genSetCmp(c, n[1], elem)

    else:
      # the offset to zero needs to be accounted for for set-element operands
      result = c.irs.irCall(mInSet, c.types.requestType(n.typ),
                            genx(c, n[1]), genSetElem(c, n[2], n[1].typ))

  of mIncl, mExcl:
    # TODO: the first paramter is mutable and thus requires an ``ntkModify``
    #       instead of ``ntkUse``
    result = c.irs.irCall(m, c.types.requestType(n.typ),
                          genx(c, n[1]), genSetElem(c, n[2], n[1].typ))

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
    # a moving assign. The special handling here is only an
    # optimization, meant to reduce the amount of work the move analyser has
    # to do
    genMove(c, dest, genx(c, ri[1]))
  else:
    # TODO: make sure that ``nkFastAsgn(dst, move src)`` does the right thing,
    #       i.e. moving `src` into a temporary and then performing a shallow
    #       copy of the temporary into `dst`
    let kind =
      if requiresCopy: askCopy else: askMove

    let tmp = c.genx(ri)
    c.irs.irAsgn(kind, dest, tmp)

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

proc genFieldAsgn(c: var TCtx, obj: IRIndex; le, ri: PNode, requiresCopy: bool) =
  c.config.internalAssert(le.kind == nkDotExpr)

  let idx = c.genField(le[1])
  let s = le[1].sym

  var tmp: IRIndex

  let p = c.irs.irPathObj(obj, idx)

  if sfDiscriminant notin s.flags:
    genAsgn(c, p, ri, requiresCopy)
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

# XXX: it's not clear yet whether or not the removal of ``askInit`` will turn
#      out to be a mistake
# TODO: all assignments need to go through ``genAsgn`` (or a similar but more
#       low-level facility) so that all assignment-related processing logic
#       is located in one place
proc genAsgn(c: var TCtx; le, ri: PNode; requiresCopy: bool) =
  # XXX: assignment handling is still unfinished
  # TODO: cursor handling is missing
  let typ = skipTypes(le.typ, abstractVarRange)
  # only consider ``tfShallow`` for array and record types
  # XXX: sem should prevent the ``tfShallow`` flag on all other types
  let requiresCopy =
    if typ.kind in {tyArray, tyTuple, tyObject} and tfShallow in typ.flags:
      false
    else:
      requiresCopy

  case le.kind
  of nkBracketExpr:
    let typ = le[0].typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind
    let dest = c.genx(le[0])

    let x =
      if typ == tyTuple:
        c.irs.irPathObj(dest, le[1].intVal.int)
      else:
        c.irs.irPathArr(dest, c.genIndex(le[1], le[0].typ))

    genAsgn(c, x, ri, requiresCopy)

  of nkCheckedFieldExpr:
    var objR: IRIndex
    genCheckedObjAccessAux(c, le, objR)
    c.genFieldAsgn(objR, le[0], ri, requiresCopy)
  of nkDotExpr:
    let dest = c.genx(le[0])
    c.genFieldAsgn(dest, le, ri, requiresCopy)
  of nkSym:
    let dest = genRdVar(c, le)
    genAsgn(c, dest, ri, requiresCopy)
  of nkDerefExpr, nkHiddenDeref:
    let dest = c.genx(le[0])
    genAsgn(c, c.irs.irDeref(dest), ri, requiresCopy)
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
    let discVal = c.irs.irPathObj(dest, genField(c, disc))
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
  c.irs.irLit((NoneLit, c.types.requestType(t)))

proc genArrAccess(c: var TCtx; n: PNode): IRIndex =
  let arrayType = n[0].typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind
  case arrayType
  of tyTypeDesc:
    c.genTypeLit(n.typ)
  of tyTuple:
    let a = c.genx(n[0])
    let b = n[1].intVal
    c.irs.irPathObj(a, b.int)
  of tyArray, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray, tyString, tyCstring:
    let acc = genArrAccessOpcode(c, n)
    c.irs.irPathArr(acc.arr, acc.idx)
  else: unreachable(arrayType)

func addVariable(c: var TCtx, kind: LocalKind, s: PSym): IRIndex =
  assert kind != lkTemp

  let id = c.genLocal(kind, s)
  c.prc.locals[s.id] = id
  c.prc.variables.add(id)
  c.prc.activeLocals.add(id)

  c.irs.irLocal(id)

func prepareForInit(c: var TCtx, dest: IRIndex, typ: PType) =
  ## Generates the code for preparing `dest` for the following initializing
  ## assignment
  const Scalar = {tyBool, tyChar, tyInt..tyInt64, tyUInt..tyUInt64,
                  tyFloat..tyFloat128, tyPtr, tyPointer}

  # for simple scalar types, the assignment itself acts as the complete
  # initialization, so there's no need to first call ``bcInitLoc`` for them.
  # Globals don't need to be prepared via ``bcInitLoc`` too, as that happens
  # implicitly at program startup
  # XXX: this part needs some further iteration. What we're actually interested
  #      in here is whether or not the following assignment:
  #      - completely initializes the location (i.e. each bit that's part of
  #        the location is written to)
  #      - requires a zero'ed location (``genericAssign`` does for example)
  if c.irs[dest].kind != ntkSym and
     typ.skipTypes(abstractRange).kind notin Scalar:
    discard c.irs.irCall(bcInitLoc, c.requestType(tyVoid), dest)

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
        # XXX: what is this case needed for?
        c.genx(n[i])

    prepareForInit(c, e, n[i].typ)
    lhs[i] = e

  # then, generate the initialization
  let initExpr = n[^1]
  case initExpr.kind
  of nkPar, nkTupleConstr:
    # skip constructing a temporary and directly assign the fields
    c.config.internalAssert(lhs.len == initExpr.len, n.info)
    for i, left in lhs.pairs:
      let val = c.genx(initExpr[i].skipColon())
      c.irs.irAsgn(askCopy, left, val)

  of nkEmpty:
    # TODO: is this even valid?
    discard "do nothing for empty tuple initializers"
  else:
    let val = c.genx(initExpr)

    for i, left in lhs.pairs:
      let p = c.irs.irPathObj(val, i)
      c.irs.irAsgn(askCopy, left, p)

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
        if a[2].kind == nkEmpty:
          if sfNoInit notin s.flags:
            let voidTy = c.requestType(tyVoid)
            discard c.irs.irCall(bcInitLoc, voidTy, local)
            # an empty value is constructed here, so ``bcFinishConstr`` needs
            # to be emitted
            discard c.irs.irCall(bcFinishConstr, voidTy, local)

        else:
          prepareForInit(c, local, s.typ)
          genAsgn(c, local, a[2], requiresCopy = true)

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
        if a[2].kind != nkEmpty:
          # the setup of the environment already zero-initialized the
          # location - there's no need to use ``prepareForInit`` here
          genAsgn(c, a[0], a[2], true)
    else:
      unreachable(a.kind)

proc genArrayConstr(c: var TCtx, n: PNode): IRIndex =
  result = c.getTemp(n.typ)

  if n.len > 0:
    for i, x in n.pairs:
      let idx = c.irImm(i)

      # XXX: the loss of information due to the lowering might be a problem
      #      for the code-generators
      genAsgn(c, c.irs.irPathArr(result, idx), x, requiresCopy = true)

  # XXX: the array construction is considered to be finished if the
  #      construction of all it's sub-locations is finished.
  #      ``genTupleConstr`` and ``genObjConstr`` already take care of that,
  #      so using ``bcFinishConstr`` here would be redundant. For efficiency,
  #      it might be a good idea to prevent the emission of ``bcFinishConstr``
  #      instructions for arrays, tuples, and objects if they're constructed
  #      in the context of another construction, and then only invoke
  #      ``bcFinishConstr`` once for the outermost construction
  #discard c.irs.irCall(bcFinishConstr, c.requestType(tyVoid), result)

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

proc genSetElem(c: var TCtx, n: PNode, typ: PType): IRIndex {.inline.} =
  ## `typ` is the type to derive the lower bound from
  let t = typ.skipTypes(abstractInst)
  assert t.kind == tySet

  # `first` can't be reliably derived from `n.typ` since the type may not
  # match the set element type. This happens with the set in a
  # `nkCheckedFieldExpr` for example
  let first = toInt(c.config.firstOrd(t))
  genSetElem(c, n, first)

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
    discard c.irs.irCall(mNew, c.requestType(tyVoid), result)
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

      # TODO: ``genAsgn`` should be used here instead
      c.irs.irAsgn(askCopy, c.irs.irPathObj(obj, idx), tmp)
    else:
      let (dVal, bVal) = c.genDiscrVal(it[0].sym, it[1], n.typ)
      # TODO: askCopy should be replaced with a magic call here (e.g. bcInitDiscr)
      c.irs.irAsgn(askCopy, c.irs.irPathObj(obj, idx), dVal)

  let loc =
    if t.kind == tyRef: c.irs.irDeref(result)
    else:               result

  # TODO: for non-refs, ``bcFinishConstr`` only needs to be used on the
  #       outermost locations for which the value is constructed
  discard c.irs.irCall(bcFinishConstr, c.requestType(tyVoid), loc)

proc genTupleConstr(c: var TCtx, n: PNode): IRIndex =
  # TODO: don't create a temporary for typedescs
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

      genAsgn(c, c.irs.irPathObj(result, idx), src, requiresCopy = true)

    discard c.irs.irCall(bcFinishConstr, c.requestType(tyVoid), result)

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
  of nkStringToCString:
    dest = c.irs.irCall(bcStrToCStr, nodeType(), genx(c, n[0]))
    raiseExit(c)
  of nkCStringToString:
    dest = c.irs.irCall(bcCStrToStr, nodeType(), genx(c, n[0]))
    raiseExit(c)
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
    unreachable(n.kind)


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
  let errorExit = c.irs.irJoinFwd() # the target for raise

  pushScope(c.prc, handler = errorExit)

func endProc*(c: var TCtx) =
  assert c.prc.scopes.len == 1
  let scope = c.prc.scopes.pop()

  c.irs.irJoin(scope.handler)
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
      # important: the 'result' variable is not tracked in ``prc.variables``
      let r = c.genLocal(lkVar, s.ast[resultPos].sym)
      # initialize 'result'
      if sfNoInit notin s.flags:
        # TODO: ``genAsgn`` should be used here instead
        c.irs.irAsgn(askCopy, c.irs.irLocal(r), c.irNull(s.typ[0]))

    gen(c, body)

    endProc(c)

proc genProc*(c: var TCtx; s: PSym, body: PNode): IrGenResult =
  tryOrReturn:
    c.genProcBody(s, body)

  result = IrGenResult.ok:
    c.irs