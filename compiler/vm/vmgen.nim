#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the code generator for the VM.

# Important things to remember:
# - The VM does not distinguish between definitions ('var x = y') and
#   assignments ('x = y'). For simple data types that fit into a register
#   this doesn't matter. However it matters for strings and other complex
#   types that use the 'node' field; the reason is that slots are
#   re-used in a register based VM. Example:
#
#.. code-block:: nim
#   let s = a & b  # no matter what, create fresh node
#   s = a & b  # no matter what, keep the node
#
# Also *stores* into non-temporary memory need to perform deep copies:
# a.b = x.y
# We used to generate opcAsgn for the *load* of 'x.y' but this is clearly
# wrong! We need to produce opcAsgn (the copy) for the *store*. This also
# solves the opcLdConst vs opcAsgnConst issue. Of course whether we need
# this copy depends on the involved types.

import
  std/[
    tables,
    strutils
  ],
  compiler/ast/[
    renderer,
    types,
    ast,
    lineinfos,
    astmsgs,
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    lowerings
  ],
  compiler/utils/[
    idioms
  ],
  compiler/backend/cgir,
  compiler/vm/[
    vmaux,
    vmdef,
    vmobjects,
    vmtypegen,
    vmtypes,
  ],
  experimental/[
    results
  ]

import compiler/mir/mirtrees

import std/options as std_options

from std/bitops import bitor

when defined(nimCompilerStacktraceHints):
  import std/stackframes


type
  VmGenResult* = Result[CodeInfo, VmGenDiag] ## Result of a vmgen invocation

  VmGenError = object of CatchableError
    diag: VmGenDiag

func raiseVmGenError(diag: sink VmGenDiag) {.noinline, noreturn.} =
  raise (ref VmGenError)(diag: diag)

func raiseVmGenError(
  diag: VmGenDiagKindAstRelated,
  n: PNode,
  instLoc = instLoc()
  ) =
  let d = VmGenDiag(kind: diag, location: n.info, ast: n, instLoc: instLoc)
  raise (ref VmGenError)(diag: d)

func raiseVmGenError(
  diag: sink VmGenDiag,
  loc:  TLineInfo,
  inst: InstantiationInfo
  ) {.noinline, noreturn.} =
  diag.location = loc
  diag.instLoc = inst
  raise (ref VmGenError)(diag: diag)

func fail(
  info: TLineInfo,
  kind: VmGenDiagKind,
  loc:  InstantiationInfo = instLoc()
  ) {.noinline, noreturn.} =
  raiseVmGenError(
    VmGenDiag(kind: kind),
    info,
    loc)

func fail(
  info: TLineInfo,
  kind: VmGenDiagKindAstRelated,
  ast:  PNode,
  loc:  InstantiationInfo = instLoc()
  ) {.noinline, noreturn.} =
  raiseVmGenError(
    VmGenDiag(kind: kind, ast: ast),
    info,
    loc)

func fail(
  info: TLineInfo,
  kind: VmGenDiagKindSymRelated,
  sym:  PSym,
  loc:  InstantiationInfo = instLoc()
  ) {.noinline, noreturn.} =
  raiseVmGenError(
    VmGenDiag(kind: kind, sym: sym),
    info,
    loc)

func fail(
  info:  TLineInfo,
  kind:  VmGenDiagKindMagicRelated,
  magic: TMagic,
  loc:   InstantiationInfo = instLoc()
  ) {.noinline, noreturn.} =
  assert kind in {vmGenDiagMissingImportcCompleteStruct,
                  vmGenDiagCodeGenUnhandledMagic}, "Diag needs magic field"
  raiseVmGenError(
    VmGenDiag(kind: kind, magic: magic),
    info,
    loc)

template tryOrReturn(code): untyped =
  try:
    code
  except VmGenError as e:
    return VmGenResult.err(move e.diag)

type
  # TODO: remove ``TGenFlag``. Dispatch to the right procedure in a similar
  #       way to how ``genAddr`` does it
  TGenFlag = enum
    gfNode ## affects how locations are loaded - always loads as ``rkHandle``
  TGenFlags = set[TGenFlag]

# forward declarations
proc genLit(c: var TCtx; n: PNode; lit: int; dest: var TDest)

template isUnset(x: TDest): bool = x < 0

proc debugInfo(c: TCtx; info: TLineInfo): string =
  result = toFileLineCol(c.config, info)

func registerLinkItem(tbl: var Table[int, LinkIndex], list: var seq[PSym],
                      sym: PSym, next: var LinkIndex): int =
  let linkIdx = tbl.mgetOrPut(sym.id, next)
  if linkIdx == next:
    # a not seen before symbol:
    list.add(sym)
    inc next

  result = linkIdx.int


func registerProc(c: var TCtx, sym: PSym): int {.inline.} =
  registerLinkItem(c.symToIndexTbl, c.codegenInOut.newProcs, sym,
                   c.codegenInOut.nextProc)

func registerGlobal(c: var TCtx, sym: PSym): int {.inline.} =
  registerLinkItem(c.symToIndexTbl, c.codegenInOut.newGlobals, sym,
                   c.codegenInOut.nextGlobal)

func registerConst(c: var TCtx, sym: PSym): int {.inline.} =
  registerLinkItem(c.symToIndexTbl, c.codegenInOut.newConsts, sym,
                   c.codegenInOut.nextConst)


func gABC*(ctx: var TCtx; n: CgNode; opc: TOpcode; a, b, c: TRegister = 0) =
  ## Takes the registers `b` and `c`, applies the operation `opc` to them, and
  ## stores the result into register `a`
  ## The node is needed for debug information
  assert opc.ord < 255
  let ins = (opc.TInstrType or (a.TInstrType shl regAShift) or
                           (b.TInstrType shl regBShift) or
                           (c.TInstrType shl regCShift)).TInstr
  #[
  when false:
    if ctx.code.len == 43:
      writeStackTrace()
      echo "generating ", opc
  ]#
  ctx.code.add(ins)
  ctx.debug.add(n.info)

proc gABI(c: var TCtx; n: CgNode; opc: TOpcode; a, b: TRegister; imm: BiggestInt) =
  # Takes the `b` register and the immediate `imm`, applies the operation `opc`,
  # and stores the output value into `a`.
  # `imm` is signed and must be within [-128, 127]
  c.config.internalAssert(imm in -128..127 , n.info,
    "VM: immediate value does not fit into an int8")

  let ins = (opc.TInstrType or (a.TInstrType shl regAShift) or
                           (b.TInstrType shl regBShift) or
                           (imm+byteExcess).TInstrType shl regCShift).TInstr
  c.code.add(ins)
  c.debug.add(n.info)

proc gABx*(c: var TCtx; n: CgNode; opc: TOpcode; a: TRegister = 0; bx: int) =
  # Applies `opc` to `bx` and stores it into register `a`
  # `bx` must be signed and in the range [regBxMin, regBxMax]

  #[
  when false:
    if c.code.len == 43:
      writeStackTrace()
      echo "generating ", opc
      ]#

  c.config.internalAssert(bx in regBxMin-1..regBxMax, n.info,
    "VM: immediate value does not fit into regBx")

  let ins = (opc.TInstrType or a.TInstrType shl regAShift or
            (bx+wordExcess).TInstrType shl regBxShift).TInstr
  c.code.add(ins)
  c.debug.add(n.info)

proc xjmp(c: var TCtx; n: CgNode; opc: TOpcode; a: TRegister = 0): TPosition =
  #assert opc in {opcJmp, opcFJmp, opcTJmp}
  result = TPosition(c.code.len)
  gABx(c, n, opc, a, 0)

func genLabel(c: TCtx): TPosition =
  result = TPosition(c.code.len)
  #c.jumpTargets.incl(c.code.len)

proc jmpBack(c: var TCtx, n: CgNode, p = TPosition(0)) =
  let dist = p.int - c.code.len
  internalAssert(c.config, regBxMin < dist and dist < regBxMax)
  gABx(c, n, opcJmpBack, 0, dist)

proc patch(c: var TCtx, p: TPosition) =
  # patch with current index
  let p = p.int
  let diff = c.code.len - p
  #c.jumpTargets.incl(c.code.len)
  internalAssert(c.config, regBxMin < diff and diff < regBxMax)
  let oldInstr = c.code[p]
  # opcode and regA stay the same:
  c.code[p] = ((oldInstr.TInstrType and regBxMask).TInstrType or
               TInstrType(diff+wordExcess) shl regBxShift).TInstr

proc getSlotKind(t: PType): TSlotKind =
  case t.skipTypes(abstractRange-{tyTypeDesc}).kind
  of tyBool, tyChar, tyEnum, tyOrdinal, tyInt..tyInt64, tyUInt..tyUInt64:
    slotTempInt
  of tyString, tyCstring:
    slotTempStr
  of tyFloat..tyFloat128:
    slotTempFloat
  else:
    slotTempComplex

const
  HighRegisterPressure = 40

func bestEffort(c: TCtx): TLineInfo =
  if c.prc != nil and c.prc.sym != nil:
    c.prc.sym.info
  else:
    c.module.info

template inUse(x: RegInfo): bool =
  x.refCount > 0

proc getFreeRegister(cc: var TCtx; k: TSlotKind; start: int): TRegister =
  let c = cc.prc
  # we prefer the same slot kind here for efficiency. Unfortunately for
  # discardable return types we may not know the desired type. This can happen
  # for e.g. mNAdd[Multiple]:
  for i in start..c.regInfo.len-1:
    if c.regInfo[i].kind == k and not c.regInfo[i].inUse:
      c.regInfo[i].refCount = 1
      return TRegister(i)

  # if register pressure is high, we re-use more aggressively:
  if c.regInfo.len >= high(TRegister):
    for i in start..c.regInfo.len-1:
      if not c.regInfo[i].inUse:
        c.regInfo[i] = RegInfo(refCount: 1, kind: k)
        return TRegister(i)
  if c.regInfo.len >= high(TRegister):
    fail(cc.bestEffort, vmGenDiagTooManyRegistersRequired)

  result = TRegister(max(c.regInfo.len, start))
  c.regInfo.setLen int(result)+1
  c.regInfo[result] = RegInfo(refCount: 1, kind: k)

proc getTemp(cc: var TCtx; tt: PType): TRegister =
  let typ = tt.skipTypesOrNil({tyStatic})
  # we prefer the same slot kind here for efficiency. Unfortunately for
  # discardable return types we may not know the desired type. This can happen
  # for e.g. mNAdd[Multiple]:
  let k = if typ.isNil: slotTempComplex else: typ.getSlotKind
  result = getFreeRegister(cc, k, start = 0)

  when false:
    # enable this to find "register" leaks:
    if result == 4:
      echo "begin ---------------"
      writeStackTrace()
      echo "end ----------------"

proc makeHandleReg(cc: var TCtx, r, loc: TRegister) =
  ## Mark register `r` as a register storing a handle into the location
  ## stored in register `loc`

  # XXX: vmgen depends on locations still existing after freeing the
  #      corresponding register, which was made possible due to the VM using
  #      `ref` types internally (PNode). With the new semantics, locations
  #      are freed immediately once their corresponding register transitions
  #      to another state, thus leading to issues with temporaries. Consider:
  #      .. code-block::nim
  #        newSeq[SomeObject](1)[0].field
  #
  #      Once the `[0]` expression is evaluated, the register holding the
  #      `newSeq` result is released. If it's reused for holding `field`, the
  #      location gets cleaned up, leading to the handle returned by `[0]`
  #      turning invalid.
  #
  #      As a temporary fix, `vmgen` now uses ref counting for registers, so
  #      that handle registers keep their location registers from being reused
  #      during the lifetime of the handle. Once vmgen gets a IR, register
  #      lifetime management can be properly implemented
  let c = cc.prc
  let fk = c.regInfo[loc].kind
  internalAssert cc.config, fk != slotEmpty, ""

  if c.regInfo[r].kind < slotTempUnknown:
    # XXX: a hack inside a hack. `makeHandleReg` is sometimes called on
    #      registers that store variables. Instead of finding out why and
    #      properly fixing the issue, we simply don't apply the adjustment.
    return

  # If the src is a handle-into-temp itself, don't ref it, but use it's target
  # as the source instead
  let loc =
    if fk == slotTempHandle: c.regInfo[loc].locReg
    else: uint16(loc)

  internalAssert cc.config, c.regInfo[r].refCount == 1
  internalAssert cc.config, c.regInfo[loc].kind != slotTempHandle

  c.regInfo[r].kind = slotTempHandle
  c.regInfo[r].locReg = loc
  inc c.regInfo[loc].refCount

func freeTemp(c: var TCtx; r: TRegister) =
  let p = c.prc
  case p.regInfo[r].kind
  of {slotSomeTemp..slotTempComplex}:
    # this seems to cause https://github.com/nim-lang/Nim/issues/10647
    dec p.regInfo[r].refCount
  of slotTempHandle:
    let rI = addr p.regInfo[r]
    assert rI.refCount == 1
    rI.refCount = 0
    freeTemp(c, TRegister(rI.locReg))
  else:
    discard # do nothing


proc getTempRange(cc: var TCtx; n: int; kind: TSlotKind): TRegister =
  # if register pressure is high, we re-use more aggressively:
  let c = cc.prc
  # we could also customize via the following (with proper caching in ConfigRef):
  # let highRegisterPressure = cc.config.getConfigVar("vm.highRegisterPressure", "40").parseInt
  if c.regInfo.len >= HighRegisterPressure or c.regInfo.len+n >= high(TRegister):
    for i in 0..c.regInfo.len-n:
      if not c.regInfo[i].inUse:
        block search:
          for j in i+1..i+n-1:
            if c.regInfo[j].inUse: break search
          result = TRegister(i)
          for k in result..result+n-1: c.regInfo[k] = RegInfo(refCount: 1, kind: kind)
          return
  if c.regInfo.len+n >= high(TRegister):
    fail(cc.bestEffort, vmGenDiagTooManyRegistersRequired)

  result = TRegister(c.regInfo.len)
  setLen c.regInfo, c.regInfo.len+n
  for k in result .. result + n - 1:
    c.regInfo[k] = RegInfo(refCount: 1, kind: kind)

proc freeTempRange(c: var TCtx; start: TRegister, n: int) =
  for i in start .. start + n - 1:
    c.freeTemp(TRegister(i))

func prepare(c: var TCtx, dest: var TDest, typ: PType) =
  ## Initializes `dest` to a temporary register if it's not already set. `typ`
  ## is passed to register allocation logic to improve allocation behaviour.
  ## Do note that this is only a hint, no invalid code will be generated if
  ## `typ` doesn't matches what's stored in the register.
  if dest.isUnset:
    dest = c.getTemp(typ)

template withTemp(tmp, typ, body: untyped) {.dirty.} =
  var tmp = getTemp(c, typ)
  body
  c.freeTemp(tmp)

proc popBlock(c: var TCtx; oldLen: int) =
  for f in c.prc.blocks[oldLen].fixups:
    c.patch(f)
  c.prc.blocks.setLen(oldLen)

template withBlock(labl: LabelId; body: untyped) {.dirty.} =
  var oldLen {.gensym.} = c.prc.blocks.len
  c.prc.blocks.add TBlock(label: uint32(labl), fixups: @[])
  body
  popBlock(c, oldLen)

proc gen(c: var TCtx; n: CgNode; dest: var TDest; flags: TGenFlags = {})
proc gen(c: var TCtx; n: CgNode; dest: TRegister; flags: TGenFlags = {}) =
  var d: TDest = dest
  gen(c, n, d, flags)
  #internalAssert c.config, d == dest # issue #7407

proc gen(c: var TCtx; n: CgNode)

proc genx(c: var TCtx; n: CgNode; flags: TGenFlags = {}): TRegister =
  var tmp: TDest = -1
  gen(c, n, tmp, flags)
  #internalAssert c.config, tmp >= 0 # 'nim check' does not like this internalAssert.
  if tmp >= 0:
    result = TRegister(tmp)

proc clearDest(c: var TCtx; n: PNode; dest: var TDest) {.inline.} =
  # stmt is different from 'void' in meta programming contexts.
  # So we only set dest to -1 if 'void':
  if dest >= 0 and (n.typ.isNil or n.typ.kind == tyVoid):
    c.freeTemp(dest)
    dest = -1

proc isNotOpr(n: CgNode): bool =
  n.kind == cgnkCall and n.callee.kind == cgnkMagic and
    n.callee.magic == mNot

proc genRepeat(c: var TCtx; n: CgNode) =
  # lab1:
  #   body
  #   jmp lab1
  let lab1 = c.genLabel()
  c.gen(n.body)
  c.jmpBack(n, lab1)

proc genScope(c: var TCtx; n: CgNode) =
  assert n.kind == cgnkScope
  let oldRegisterCount = c.prc.regInfo.len

  c.gen(n.body)

  for i in oldRegisterCount..<c.prc.regInfo.len:
    #if c.prc.regInfo[i].kind in {slotFixedVar, slotFixedLet}:
    when not defined(release):
      if c.prc.regInfo[i].inUse and c.prc.regInfo[i].kind in {slotTempUnknown,
                                slotTempInt,
                                slotTempFloat,
                                slotTempStr,
                                slotTempComplex,
                                slotTempHandle}:
        doAssert false, "leaking temporary " & $i & " " & $c.prc.regInfo[i].kind
    c.prc.regInfo[i] = RegInfo(kind: slotEmpty)

proc genBlock(c: var TCtx; n: CgNode) =
  withBlock(n.label):
    c.gen(n.body)

proc genBreak(c: var TCtx; n: CgNode) =
  let lab1 = c.xjmp(n, opcJmp)
  if n.label != NoLabel:
    #echo cast[int](n[0].sym)
    for i in countdown(c.prc.blocks.len-1, 0):
      if c.prc.blocks[i].label == n.label.LabelId:
        c.prc.blocks[i].fixups.add lab1
        return

    fail(n.info, vmGenDiagCannotFindBreakTarget)
    # TODO: the above was originally used to detect situation such as:
    #
    #         while x:
    #           static: break
    #
    #       with the introduction of the MIR and ``mirgen``, however, the
    #       above code now crashes the compiler, meaning that the ``fail``
    #       here is never reached anymore. It is kept as a reminder to
    #       the reject the code during semantic analysis already, preventing
    #       it from ever reaching ``mirgen``.
  else:
    c.prc.blocks[c.prc.blocks.high].fixups.add lab1

proc genIf(c: var TCtx, n: CgNode) =
  #  if (!cond) goto lab1;
  #    body
  #  lab1:
  # optimize the emitted bytecode a bit by fusing the `not` predicate (if one
  # exists) with the conditional jump instruction
  let (opr, opc) =
    if isNotOpr(n.condition): (n.condition.arg(0), opcTJmp) # if true
    else:                     (n.condition,        opcFJmp) # if false

  let
    tmp = c.genx(opr)
    start = c.xjmp(opr, opc, tmp) # if true
  c.freeTemp(tmp)

  c.gen(n.body)
  c.patch(start)

func isTemp(c: TCtx; dest: TDest): bool =
  result = dest >= 0 and c.prc.regInfo[dest].kind >= slotTempUnknown

# XXX `rawGenLiteral` should be a func, but can't due to `internalAssert`
proc rawGenLiteral(c: var TCtx, val: sink VmConstant): int =
  result = c.constants.len
  c.constants.add val
  internalAssert c.config, result < regBxMax, "Too many constants used"

template cmpFloatRep(a, b: BiggestFloat): bool =
  ## Compares the bit-representation of floats `a` and `b`
  # Special handling for floats, so that floats that have the same
  # value but different bit representations are treated as different constants
  cast[uint64](a) == cast[uint64](b)
  # refs bug #16469
  # if we wanted to only distinguish 0.0 vs -0.0:
  # if a.floatVal == 0.0: result = cast[uint64](a.floatVal) == cast[uint64](b.floatVal)
  # else: result = a.floatVal == b.floatVal

func cmpNodeCnst(a, b: PNode): bool {.inline.} =
  ## Compares two trees for structural equality, also taking the type of
  ## ``nkType`` nodes into account. This procedure is used to prevent the same
  ## AST from being added as a node constant more than once
  if a == b:
    return true
  elif a.kind == b.kind:
    case a.kind
    of nkSym:           result = a.sym == b.sym
    of nkIdent:         result = a.ident.id == b.ident.id
    of nkEmpty:         result = true
    of nkType:          result = a.typ == b.typ
    of nkStrKinds:      result = a.strVal == b.strVal
    of nkIntKinds:      result = a.intVal == b.intVal
    of nkFloatLiterals: result = cmpFloatRep(a.floatVal, b.floatVal)
    else:
      if a.len == b.len:
        for i in 0..<a.len:
          if not cmpNodeCnst(a[i], b[i]): return
        result = true

template makeCnstFunc(name, vType, aKind, valName, cmp) {.dirty.} =
  proc name(c: var TCtx, val: vType): int =
    for (i, cnst) in c.constants.pairs():
      if cnst.kind == aKind and cmp(cnst.valName, val):
        return i

    c.rawGenLiteral: VmConstant(kind: aKind, valName: val)


makeCnstFunc(toNodeCnst, PNode, cnstNode, node, cmpNodeCnst)

makeCnstFunc(toIntCnst, BiggestInt, cnstInt, intVal, `==`)

makeCnstFunc(toFloatCnst, BiggestFloat, cnstFloat, floatVal, cmpFloatRep)

makeCnstFunc(toStringCnst, string, cnstString, strVal, `==`)

proc genLiteral(c: var TCtx, n: PNode): int =
  ## Create a constant, add it to the `c.constants` list and return
  ## the index of where it's located there
  case n.kind
  of nkIdent, nkType, nkEmpty: toNodeCnst(c, n)
  of nkCharLit..nkUInt64Lit: toIntCnst(c, n.intVal)
  of nkFloatLit, nkFloat64Lit: toFloatCnst(c, n.floatVal)
  of nkFloat32Lit: toFloatCnst(c, n.floatVal.float32.float64)
  of nkStrLit..nkTripleStrLit: toStringCnst(c, n.strVal)
  else:
    # While we could treat `n` as a PNode constant in this, we don't, forcing
    # explicit usage of `toNodeCnst` in order to prevent bugs
    c.config.internalError(n.info, $n.kind)
    0

template fillSliceList[T](sl: var seq[Slice[T]], nodes: openArray[CgNode],
                          get: untyped) =
  sl.newSeq(nodes.len)

  template getIt(n): untyped =
    block:
      let it {.cursor, inject.} = n
      get

  for (i, n) in nodes.pairs:
    sl[i] =
      if n.kind == nkRange:
        getIt(n[0]) .. getIt(n[1])
      else:
        let e = getIt(n)
        e .. e

proc genBranchLit(c: var TCtx, n: CgNode, t: PType): int =
  ## Turns the slice-list or single literal of the given `nkOfBranch` into
  ## a constant and returns it's index in `c.constant`.
  ##
  ## slice-lists are always added as a new constant while single literals
  ## are reused

  # XXX: slice-list constants (maybe `VmConstant`s in general) should be
  #      stored in a `BiTable` so that it can be easily detected if they
  #      already exist
  assert t.kind in IntegralTypes+{tyString}

  if n.len == 2 and n[0].kind in cgnkLiteralData:
    # It's an 'of' branch with a single value
    result = c.genLiteral(n.data)
  else:
    # It's an 'of' branch with multiple and/or range values
    var cnst: VmConstant

    template values: untyped =
      n.sons.toOpenArray(0, n.sons.high - 1) # -1 for the branch body

    case t.kind
    of IntegralTypes-{tyFloat..tyFloat128}:
      cnst = VmConstant(kind: cnstSliceListInt)
      cnst.intSlices.fillSliceList(values):
        it.intVal

    of tyFloat..tyFloat128:
      cnst = VmConstant(kind: cnstSliceListFloat)
      cnst.floatSlices.fillSliceList(values):
        it.floatVal

    of tyString:
      cnst = VmConstant(kind: cnstSliceListStr)
      cnst.strSlices.fillSliceList(values):
        c.toStringCnst(it.strVal)

    else:
      unreachable(t.kind)

    result = c.rawGenLiteral(cnst)


proc unused(c: TCtx; n: CgNode; x: TDest) {.inline.} =
  if x >= 0:
    fail(n.info, vmGenDiagNotUnused, n)

proc genCase(c: var TCtx; n: CgNode) =
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
  let selType = n.condition.typ.skipTypes(abstractVarRange)
  var endings: seq[TPosition] = @[]
  withTemp(tmp, n.condition.typ):
    c.gen(n.condition, tmp)
    # branch tmp, codeIdx
    # fjmp   elseLabel

    # iterate of/else branches
    for i, branch in branches(n):
      if branch.len == 1:
        # the catch-all branch
        c.gen(branch.body)
        # must be the last branch, so no jump is required
      else:
        let b = genBranchLit(c, branch)

        c.gABx(branch, opcBranch, tmp, b)
        let elsePos = c.xjmp(branch, opcFJmp, tmp)
        c.gen(branch.body)
        # generate a jump to the end of the block
        if i < numBranches(n):
          endings.add(c.xjmp(branch.body, opcJmp, 0))
        c.patch(elsePos)

  for endPos in endings: c.patch(endPos)

proc genType(c: var TCtx; typ: PType): int =
  ## Returns the ID of `typ`'s corresponding `VmType` as an `int`. The
  ## `VmType` is created first if it doesn't exist already
  let t = c.getOrCreate(typ)
  # XXX: `getOrCreate` doesn't return the id directly yet. Once it does, the
  #      linear search below can be removed
  result = c.types.find(t)
  assert result != -1

  internalAssert(c.config, result <= regBxMax, "")

proc genTypeInfo(c: var TCtx, typ: PType): int =
  ## Returns the stable index into `PCtx.rtti` where `typ`'s corresponding
  ## `VmTypeInfo` is located. If it doesn't exist yet, it is created first
  for i, t in c.rtti.pairs:
    if sameType(t.nimType, typ): return i

  result = c.rtti.len
  c.rtti.add:
    VmTypeInfo(nimType: typ, internal: c.getOrCreate(typ))

  internalAssert(c.config, result <= regBxMax, "")

proc genTry(c: var TCtx; n: PNode) =
  var endings: seq[TPosition] = @[]
  let ehPos = c.xjmp(n, opcTry, 0)
  c.gen(n.body)
  # Add a jump past the exception handling code
  let jumpToFinally = c.xjmp(n, opcJmp, 0)
  # This signals where the body ends and where the exception handling begins
  c.patch(ehPos)
  let eh = n.handler
  if eh != nil:
    # first opcExcept contains the end label of the 'except' block:
    let endExcept = c.xjmp(eh, opcExcept, 0)
    for branch in branches(eh):
      for it in labels(branch):
        assert(it.kind == cgnkType)
        let typ = it.typ.skipTypes(abstractPtrs-{tyTypeDesc})
        c.gABx(it, opcExcept, 0, c.genType(typ))
      if branch.len == 1:
        # general except section:
        c.gABx(branch, opcExcept, 0, 0)
      c.gen(branch.body)
      endings.add(c.xjmp(branch, opcJmp, 0))
      c.patch(endExcept)

  let fin = lastSon(n)
  # we always generate an 'opcFinally' as that pops the safepoint
  # from the stack if no exception is raised in the body.
  c.patch(jumpToFinally)
  c.gABx(fin, opcFinally, 0, 0)
  for endPos in endings: c.patch(endPos)
  let fin = n.finalizer
  if fin != nil:
    c.gen(fin.body)
  c.gABx(fin, opcFinallyEnd, 0, 0)

proc genRaise(c: var TCtx; n: CgNode) =
  if n.value.kind != nkEmpty:
    let
      dest = c.genx(n.value)
      typ = skipTypes(n.value.typ, abstractPtrs)

    # get the exception name
    var name: TDest = c.getTemp(c.graph.getSysType(n.info, tyString))
    c.genLit(n.value, c.toStringCnst(typ.sym.name.s), name)

    # XXX: using an ABxI encoding would make sense here...
    c.gABI(n, opcRaise, dest, name, 0)
    c.freeTemp(name)
    c.freeTemp(dest)
  else:
    # reraise
    c.gABI(n, opcRaise, 0, 0, imm=1)


proc genReturn(c: var TCtx; n: CgNode) =
  c.gABC(n, opcRet)

proc genLit(c: var TCtx; n: PNode; lit: int; dest: var TDest) =
  ## `lit` is the index of a constant as returned by `genLiteral`

  # opcLdConst is now always valid. We produce the necessary copy in the
  # assignments now:
  #var opc = opcLdConst
  if dest.isUnset: dest = c.getTemp(n.typ)
  #elif c.prc.regInfo[dest].kind == slotFixedVar: opc = opcAsgnConst
  c.gABx(n, opcLdConst, dest, lit)

proc genLit(c: var TCtx; n: PNode; dest: var TDest) =
  let lit = genLiteral(c, n)
  genLit(c, n, lit, dest)


proc genProcLit(c: var TCtx, n: CgNode, s: PSym; dest: var TDest) =
  if dest.isUnset:
    dest = c.getTemp(s.typ)

  let idx = c.registerProc(s)

  c.gABx(n, opcLdNull, dest, c.genType(s.typ))
  c.gABx(n, opcWrProc, dest, idx)

proc genArg(c: var TCtx, n: CgNode; dest = TDest(-1)): TRegister =
  var dest = dest
  c.gen(n, dest)
  result = dest

proc genCall(c: var TCtx; n: CgNode; dest: var TDest) =
  # it can happen that due to inlining we have a 'n' that should be
  # treated as a constant (see issue #537).
  #if n.typ != nil and n.typ.sym != nil and n.typ.sym.magic == mPNimrodNode:
  #  genLit(c, n, dest)
  #  return
  if dest.isUnset and not isEmptyType(n.typ): dest = getTemp(c, n.typ)

  let fntyp = skipTypes(n.callee.typ, abstractInst)

  let x = c.getTempRange(n.len, slotTempUnknown)

  # varargs need 'opcSetType' for the FFI support:
  for i, it in arguments(n):
    # skip empty arguments (i.e. arguments to compile-time parameters that
    # were omitted):
    if it.kind == nkEmpty:
      continue

    var r = TDest(x+i)
    if it.typ.skipTypes(abstractInst).kind == tyRef:
      # ``ref`` values are always stored in VM memory, meaning that we're
      # getting a handle to it here. Lots of code is written under the
      # assumption that a ``ref`` is always uses pass-by-value (instead of
      # pass-by-reference), so a copy is explicitly introduced
      # TODO: ``ref`` values should use ``rkAddress`` registers and then we
      #       no longer need the logic here. For the former to work, pseudo
      #       lifetime-hooks would need to be inserted for ``ref``s used in
      #       compile-time code, even when ``--gc:none`` is used
      let tmp = c.genx(it)
      c.gABC(it, opcAsgnComplex, r, tmp)
      c.freeTemp(tmp)
    else:
      c.genArg(it, r)

    if i >= fntyp.len:
      internalAssert(c.config, tfVarargs in fntyp.flags)
      c.gABx(n, opcSetType, r, c.genType(n.typ))

  if dest.isUnset:
    c.gABC(n, opcIndCall, 0, x, n.len)
  else:
    c.gABC(n, opcIndCallAsgn, dest, x, n.len)
  c.freeTempRange(x, n.len)

template isGlobal(s: PSym): bool = sfGlobal in s.flags and s.kind != skForVar
proc isGlobal(n: PNode): bool = n.kind == nkSym and isGlobal(n.sym)

func local(prc: PProc, sym: PSym): TDest {.inline.} =
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
    if s.kind in {skResult, skParam}: s.position + ord(s.kind == skParam)
    else: -1

proc needsAsgnPatch(n: PNode): bool =
  n.kind in {nkBracketExpr, nkDotExpr, nkCheckedFieldExpr,
             nkDerefExpr, nkHiddenDeref} or (n.kind == nkSym and n.sym.isGlobal)

proc genField(c: TCtx; n: CgNode): TRegister =
  assert n.kind == cgnkField

  let s = n.sym
  if s.position > high(TRegister):
    fail(n.info, vmGenDiagTooLargeOffset, sym = s)

  result = s.position

proc genIndex(c: var TCtx; n: CgNode; arr: PType): TRegister =
  if arr.skipTypes(abstractInst).kind == tyArray and (let x = firstOrd(c.config, arr);
      x != Zero):
    let tmp = c.genx(n)
    # freeing the temporary here means we can produce:  regA = regA - Imm
    c.freeTemp(tmp)
    result = c.getTemp(n.typ)
    c.gABI(n, opcSubImmInt, result, tmp, toInt(x))
  else:
    result = c.genx(n)

proc genRegLoad(c: var TCtx, n: CgNode, dest, src: TRegister) =
  c.gABC(n, opcNodeToReg, dest, src)

  let t = n.typ.skipTypes(abstractInst)
  if t.isUnsigned() and t.size < sizeof(BiggestInt):
    c.gABC(n, opcNarrowU, dest, TRegister(t.size * 8))

proc genCheckedObjAccessAux(c: var TCtx; n: CgNode): TRegister
proc genSymAddr(c: var TCtx, n: CgNode): TRegister

proc genAsgnPatch(c: var TCtx; le: CgNode, value: TRegister) =
  case le.kind
  of cgnkArrayAccess:
    let typ = le[0].typ.skipTypes(abstractVarRange-{tyTypeDesc})
    let dest = c.genx(le[0], {gfNode})
    let idx = c.genIndex(le[1], le[0].typ)
    let opc = if typ.kind in {tyString, tyCstring}: opcWrStrIdx
              else: opcWrArr
    c.gABC(le, opc, dest, idx, value)
    c.freeTemp(dest)
    c.freeTemp(idx)
  of nkCheckedFieldExpr:
    let objR = genCheckedObjAccessAux(c, le)
    let idx = genField(c, le[0][1])
    c.gABC(le[0], opcWrObj, objR, idx, value)
    c.freeTemp(objR)
  of nkDotExpr:
    let dest = c.genx(le[0], {gfNode})
    let idx = genField(c, le[1])
    c.gABC(le, opcWrObj, dest, idx, value)
    c.freeTemp(dest)
  of nkDerefExpr, nkHiddenDeref:
    let dest = c.genx(le[0], #[{gfNode}]#)
    c.gABC(le, opcWrDeref, dest, 0, value)
    c.freeTemp(dest)
  of nkSym:
    if le.sym.isGlobal:
      let dest = genSymAddr(c, le)
      c.gABC(le, opcWrDeref, dest, 0, value)
      c.freeTemp(dest)
  else:
    discard

proc genNew(c: var TCtx; n: CgNode) =
  # FIXME: ``opcNew`` stores the allocated ``ref`` in a newly created
  #        ``ref`` location, so we currently have to assign it to the actual
  #        destination after. This is inefficent; instead, ``opcNew``
  #        should treat the destination register as a handle to a ``ref``
  #        location
  let
    dest = c.genx(n[1])
    tmp = c.getTemp(n[1].typ)
  c.gABx(n, opcNew, tmp,
         c.genType(n[1].typ.skipTypes(abstractVar-{tyTypeDesc})))
  c.gABC(n, opcWrLoc, dest, tmp)
  c.freeTemp(tmp)
  c.freeTemp(dest)

proc genNewSeq(c: var TCtx; n: CgNode) =
  # FIXME: ``opcNewSeq`` has the same problem as ``opcNew``. The instruction
  #        should also reuse the location
  let t = n[1].typ.skipTypes(abstractVar-{tyTypeDesc})
  assert t.kind == tySequence
  let
    dest = c.genx(n[1]) # ``seq`` argument
    len = c.genx(n[2])  # length argument
    tmp = c.getTemp(n[1].typ)
  c.gABx(n, opcNewSeq, tmp, c.genType(t))
  c.gABx(n, opcNewSeq, len, 0)
  c.gABC(n, opcWrLoc, dest, tmp)
  c.freeTemp(tmp)
  c.freeTemp(len)
  c.freeTemp(dest)

proc genNewSeqOfCap(c: var TCtx; n: CgNode; dest: var TDest) =
  let t = n.typ
  if dest.isUnset:
    dest = c.getTemp(n.typ)
  let tmp = c.getTemp(n[1].typ)
  c.gABx(n, opcLdNull, dest, c.genType(t))
  c.gABx(n, opcLdImmInt, tmp, 0)
  c.gABx(n, opcNewSeq, dest, c.genType(t.skipTypes(abstractVar-{tyTypeDesc})))
  c.gABx(n, opcNewSeq, tmp, 0)
  c.freeTemp(tmp)

proc genUnaryABC(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  let tmp = c.genx(n[1])
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, tmp)
  c.freeTemp(tmp)

proc genUnaryABI(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode; imm: BiggestInt=0) =
  let tmp = c.genx(n[1])
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABI(n, opc, dest, tmp, imm)
  c.freeTemp(tmp)


proc genBinaryABC(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  let
    tmp = c.genx(n[1])
    tmp2 = c.genx(n[2])
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, tmp, tmp2)
  c.freeTemp(tmp)
  c.freeTemp(tmp2)

proc genBinaryABCD(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  let
    tmp = c.genx(n[1])
    tmp2 = c.genx(n[2])
    tmp3 = c.genx(n[3])
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, tmp, tmp2)
  c.gABC(n, opc, tmp3)
  c.freeTemp(tmp)
  c.freeTemp(tmp2)
  c.freeTemp(tmp3)

proc genNarrow(c: var TCtx; n: CgNode; dest: TDest) =
  let t = skipTypes(n.typ, abstractVar-{tyTypeDesc})
  # uint is uint64 in the VM, we we only need to mask the result for
  # other unsigned types:
  if t.kind in {tyUInt8..tyUInt32} or (t.kind == tyUInt and t.size < 8):
    c.gABC(n, opcNarrowU, dest, TRegister(t.size*8))
  elif t.kind in {tyInt8..tyInt32} or (t.kind == tyInt and t.size < 8):
    c.gABC(n, opcNarrowS, dest, TRegister(t.size*8))

proc genNarrowU(c: var TCtx; n: CgNode; dest: TDest) =
  let t = skipTypes(n.typ, abstractVar-{tyTypeDesc})
  # uint is uint64 in the VM, we we only need to mask the result for
  # other unsigned types:
  if t.kind in {tyUInt8..tyUInt32, tyInt8..tyInt32} or
    (t.kind in {tyUInt, tyInt} and t.size < 8):
    c.gABC(n, opcNarrowU, dest, TRegister(t.size*8))

proc genBinaryABCnarrow(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  genBinaryABC(c, n, dest, opc)
  genNarrow(c, n, dest)

proc genBinaryABCnarrowU(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  genBinaryABC(c, n, dest, opc)
  genNarrowU(c, n, dest)

proc genBinarySet(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  let
    tmp = c.genx(n[1])
    tmp2 = c.genx(n[2])
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, tmp, tmp2)
  c.freeTemp(tmp)
  c.freeTemp(tmp2)

proc genBinaryStmt(c: var TCtx; n: CgNode; opc: TOpcode) =
  let
    dest = c.genx(n[1])
    tmp = c.genx(n[2])
  c.gABC(n, opc, dest, tmp, 0)
  c.freeTemp(tmp)
  c.freeTemp(dest)

proc genBinaryStmtVar(c: var TCtx; n: CgNode; opc: TOpcode) =
  var x = n[1]
  if x.kind in {nkAddr, nkHiddenAddr}: x = x[0]
  let
    dest = c.genx(x)
    tmp = c.genx(n[2])
  c.gABC(n, opc, dest, tmp, 0)
  #c.genAsgnPatch(n[1], dest)
  c.freeTemp(tmp)
  c.freeTemp(dest)

proc genParseOp(c: var TCtx; n: PNode; dest: var TDest,
                opc: range[opcParseExprToAst..opcParseStmtToAst]) =
  ## Generates the code for a ``parseExpr``/``parseStmt`` magic call
  if dest.isUnset:
    dest = c.getTemp(n.typ)

  # the second parameter is a ``var`` parameter. We want to access the
  # register directly, so the used addr operation is skipped (if it hasn't
  # been eliminated by ``transf``)
  var x = n[2]
  if x.kind in {nkAddr, nkHiddenAddr}:
    x = x[0]

  let
    in1 = c.genx(n[1])
    in2 = c.genx(x)

  c.gABC(n, opc, dest, in1, in2)
  c.freeTemp(in1)
  c.freeTemp(in2)

proc genVarargsABC(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  if dest.isUnset: dest = getTemp(c, n.typ)
  var x = c.getTempRange(n.len-1, slotTempStr)
  for i in 1..<n.len:
    var r: TRegister = x+i-1
    c.gen(n[i], r)
  c.gABC(n, opc, dest, x, n.len-1)
  c.freeTempRange(x, n.len-1)

proc isInt8Lit(n: PNode): bool =
  if n.kind in {nkCharLit..nkUInt64Lit}:
    result = n.intVal >= low(int8) and n.intVal <= high(int8)

proc isInt16Lit(n: PNode): bool =
  if n.kind in {nkCharLit..nkUInt64Lit}:
    result = n.intVal >= low(int16) and n.intVal <= high(int16)

proc genAddSubInt(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  if n[2].isInt8Lit:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABI(n, succ(opc), dest, tmp, n[2].intVal)
    c.freeTemp(tmp)
  else:
    genBinaryABC(c, n, dest, opc)
  c.genNarrow(n, dest)

proc genConv(c: var TCtx; n, arg: PNode; dest: var TDest; opc=opcConv) =
  let t2 = n.typ.skipTypes({tyDistinct})
  let targ2 = arg.typ.skipTypes({tyDistinct})

  proc implicitConv(): bool =
    if sameType(t2, targ2): return true
    # xxx consider whether to use t2 and targ2 here
    if n.typ.kind == arg.typ.kind and arg.typ.kind == tyProc:
      # don't do anything for lambda lifting conversions:
      return true

  if implicitConv():
    gen(c, arg, dest)
    return

  let tmp = c.genx(arg)
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, tmp)
  c.gABx(n, opc, 0, c.genTypeInfo(n.typ.skipTypes({tyStatic})))
  c.gABx(n, opc, 0, c.genTypeInfo(arg.typ.skipTypes({tyStatic})))
  c.freeTemp(tmp)

proc genCard(c: var TCtx; n: PNode; dest: var TDest) =
  let tmp = c.genx(n[1])
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABC(n, opcCard, dest, tmp)
  c.freeTemp(tmp)

proc fitsRegister*(t: PType): bool

template needsRegLoad(): untyped =
  gfNode notin flags and
    fitsRegister(n.typ.skipTypes({tyVar, tyLent, tyStatic}))

proc genCastIntFloat(c: var TCtx; n: PNode; dest: var TDest) =
  const allowedIntegers = {tyInt..tyInt64, tyUInt..tyUInt64, tyChar}
  var signedIntegers = {tyInt..tyInt64}
  var unsignedIntegers = {tyUInt..tyUInt64, tyChar}
  let src = n[1].typ.skipTypes(abstractRange)#.kind
  let dst = n[0].typ.skipTypes(abstractRange)#.kind
  let srcSize = getSize(c.config, src)
  let dstSize = getSize(c.config, dst)
  if src.kind in allowedIntegers and dst.kind in allowedIntegers:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n[0].typ)
    c.gABC(n, opcAsgnInt, dest, tmp)
    if dstSize != sizeof(BiggestInt): # don't do anything on biggest int types
      if dst.kind in signedIntegers: # we need to do sign extensions
        if dstSize <= srcSize:
          # Sign extension can be omitted when the size increases.
          c.gABC(n, opcSignExtend, dest, TRegister(dstSize*8))
      elif dst.kind in unsignedIntegers:
        if src.kind in signedIntegers or dstSize < srcSize:
          # Cast from signed to unsigned always needs narrowing. Cast
          # from unsigned to unsigned only needs narrowing when target
          # is smaller than source.
          c.gABC(n, opcNarrowU, dest, TRegister(dstSize*8))
    c.freeTemp(tmp)
  elif srcSize == dstSize and src.kind in allowedIntegers and
                           dst.kind in {tyFloat, tyFloat32, tyFloat64}:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n[0].typ)
    if dst.kind == tyFloat32:
      c.gABC(n, opcCastIntToFloat32, dest, tmp)
    else:
      c.gABC(n, opcCastIntToFloat64, dest, tmp)
    c.freeTemp(tmp)

  elif srcSize == dstSize and src.kind in {tyFloat, tyFloat32, tyFloat64} and
                           dst.kind in allowedIntegers:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n[0].typ)
    if src.kind == tyFloat32:
      c.gABC(n, opcCastFloatToInt32, dest, tmp)
      if dst.kind in unsignedIntegers:
        # integers are sign extended by default.
        # since there is no opcCastFloatToUInt32, narrowing should do the trick.
        c.gABC(n, opcNarrowU, dest, TRegister(32))
    else:
      c.gABC(n, opcCastFloatToInt64, dest, tmp)
      # narrowing for 64 bits not needed (no extended sign bits available).
    c.freeTemp(tmp)
  elif src.kind in PtrLikeKinds + {tyRef} and dst.kind == tyInt:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n[0].typ)
    var imm: BiggestInt = if src.kind in PtrLikeKinds: 1 else: 2
    c.gABI(n, opcCastPtrToInt, dest, tmp, imm)
    c.freeTemp(tmp)
  elif src.kind in PtrLikeKinds + {tyInt} and dst.kind in PtrLikeKinds:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n[0].typ)
    c.gABC(n, opcCastIntToPtr, dest, tmp)
    c.gABx(n, opcSetType, dest, c.genType(dst))
    c.freeTemp(tmp)
  elif src.kind == tyNil and dst.kind in NilableTypes:
    # supports casting nil literals to NilableTypes in VM
    # see #16024
    if dest.isUnset: dest = c.getTemp(n[0].typ)
    let opcode = if fitsRegister(dst): opcLdNullReg else: opcLdNull
    c.gABx(n, opcode, dest, c.genType(dst))
  else:
    # todo: support cast from tyInt to tyRef
    raiseVmGenError:
      VmGenDiag(
        kind: vmGenDiagCannotCast,
        location: n.info,
        instLoc: instLoc(),
        typeMismatch: VmTypeMismatch(actualType: dst, formalType: src))

proc genVoidABC(c: var TCtx, n: PNode, dest: TDest, opcode: TOpcode) =
  unused(c, n, dest)
  var
    tmp1 = c.genx(n[1])
    tmp2 = c.genx(n[2])
    tmp3 = c.genx(n[3])
  c.gABC(n, opcode, tmp1, tmp2, tmp3)
  c.freeTemp(tmp1)
  c.freeTemp(tmp2)
  c.freeTemp(tmp3)

proc genSetElem(c: var TCtx, n: PNode, first: int): TRegister =
  result = c.getTemp(n.typ)

  if first != 0:
    if n.kind in nkIntKinds:
      # a literal value
      c.gABx(n, opcLdImmInt, result, int(n.intVal - first))
    else:
      gen(c, n, result)
      if first > 0:
        c.gABI(n, opcSubImmInt, result, result, first)
      else:
        c.gABI(n, opcAddImmInt, result, result, -first)

  else:
    gen(c, n, result)

proc genSetElem(c: var TCtx, n: PNode, typ: PType): TRegister {.inline.} =
  ## `typ` is the type to derive the lower bound from
  let t = typ.skipTypes(abstractInst)
  assert t.kind == tySet

  # `first` can't be reliably derived from `n.typ` since the type may not
  # match the set element type. This happens with the set in a
  # `nkCheckedFieldExpr` for example
  let first = toInt(c.config.firstOrd(t))
  genSetElem(c, n, first)

proc fitsRegister*(t: PType): bool =
  assert t != nil
  let st = t.skipTypes(abstractInst + {tyStatic} - {tyTypeDesc})
  st.kind in { tyRange, tyEnum, tyBool, tyInt..tyUInt64, tyChar, tyPtr, tyPointer} or
    (st.sym != nil and st.sym.magic == mPNimrodNode) # NimNode goes into register too

proc ldNullOpcode(t: PType): TOpcode =
  assert t != nil
  if fitsRegister(t): opcLdNullReg else: opcLdNull

proc whichAsgnOpc(n: PNode; requiresCopy = true): TOpcode =
  case n.typ.skipTypes(abstractRange-{tyTypeDesc}).kind
  of tyBool, tyChar, tyEnum, tyOrdinal, tyInt..tyInt64, tyUInt..tyUInt64:
    opcAsgnInt
  of tyFloat..tyFloat128:
    opcAsgnFloat
  else:
    # XXX: always require a copy, fastAsgn is broken in the VM
    opcAsgnComplex
    #(if requiresCopy: opcAsgnComplex else: opcFastAsgnComplex)

proc genMagic(c: var TCtx; n: PNode; dest: var TDest; m: TMagic) =
  case m
  of mAnd: c.genAndOr(n, opcFJmp, dest)
  of mOr:  c.genAndOr(n, opcTJmp, dest)
  of mPred, mSubI:
    c.genAddSubInt(n, dest, opcSubInt)
  of mSucc, mAddI:
    c.genAddSubInt(n, dest, opcAddInt)
  of mInc, mDec:
    unused(c, n, dest)
    let isUnsigned = n[1].typ.skipTypes(abstractVarRange).kind in {tyUInt..tyUInt64}
    let opc = if not isUnsigned:
                if m == mInc: opcAddInt else: opcSubInt
              else:
                if m == mInc: opcAddu else: opcSubu
    let d = c.genx(n[1])
    if n[2].isInt8Lit and not isUnsigned:
      c.gABI(n, succ(opc), d, d, n[2].intVal)
    else:
      let tmp = c.genx(n[2])
      c.gABC(n, opc, d, d, tmp)
      c.freeTemp(tmp)
    c.genNarrow(n[1], d)
    c.genAsgnPatch(n[1], d)
    c.freeTemp(d)
  of mOrd, mChr: c.gen(n[1], dest)
  of mArrToSeq:
    let temp = c.genx(n[1])
    let L = c.getTemp(c.graph.getSysType(n.info, tyInt))
    c.gABC(n, opcLenSeq, L, temp)
    let t = n.typ.skipTypes(abstractVar-{tyTypeDesc})
    if dest.isUnset:
      dest = c.getTemp(n.typ)

    c.gABx(n, opcLdNull, dest, c.genType(t))
    c.gABC(n, opcSetLenSeq, dest, L)
    c.gABC(n, opcArrCopy, dest, temp, L)
    c.freeTemp(temp)
    c.freeTemp(L)
  of mIsolate:
    genCall(c, n, dest)
  of mNew, mNewFinalize:
    unused(c, n, dest)
    c.genNew(n)
  of mNewSeq:
    unused(c, n, dest)
    c.genNewSeq(n)
  of mNewSeqOfCap: c.genNewSeqOfCap(n, dest)
  of mNewString:
    genUnaryABC(c, n, dest, opcNewStr)
    # XXX buggy
  of mNewStringOfCap:
    # we ignore the 'cap' argument and translate it as 'newString(0)'.
    # eval n[1] for possible side effects:
    c.freeTemp(c.genx(n[1]))
    var tmp = c.getTemp(n[1].typ)
    c.gABx(n, opcLdImmInt, tmp, 0)
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABC(n, opcNewStr, dest, tmp)
    c.freeTemp(tmp)
    # XXX buggy
  of mLengthOpenArray, mLengthArray, mLengthSeq:
    genUnaryABI(c, n, dest, opcLenSeq)
  of mLengthStr:
    let t = n[1].typ.skipTypes(abstractInst)
    case t.kind
    of tyString:  genUnaryABI(c, n, dest, opcLenStr)
    of tyCstring: genUnaryABI(c, n, dest, opcLenCstring)
    else:         unreachable(t.kind)
  of mIncl, mExcl:
    unused(c, n, dest)
    let
      d = c.genx(n[1])
      tmp = c.genSetElem(n[2], n[1].typ)
    c.gABC(n, if m == mIncl: opcIncl else: opcExcl, d, tmp)
    c.freeTemp(d)
    c.freeTemp(tmp)
  of mCard: genCard(c, n, dest)
  of mMulI: genBinaryABCnarrow(c, n, dest, opcMulInt)
  of mDivI: genBinaryABCnarrow(c, n, dest, opcDivInt)
  of mModI: genBinaryABCnarrow(c, n, dest, opcModInt)
  of mAddF64: genBinaryABC(c, n, dest, opcAddFloat)
  of mSubF64: genBinaryABC(c, n, dest, opcSubFloat)
  of mMulF64: genBinaryABC(c, n, dest, opcMulFloat)
  of mDivF64: genBinaryABC(c, n, dest, opcDivFloat)
  of mShrI:
    # modified: genBinaryABC(c, n, dest, opcShrInt)
    # narrowU is applied to the left operandthe idea here is to narrow the left operand
    let tmp = c.genx(n[1])
    c.genNarrowU(n, tmp)
    let tmp2 = c.genx(n[2])
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABC(n, opcShrInt, dest, tmp, tmp2)
    c.freeTemp(tmp)
    c.freeTemp(tmp2)
  of mShlI:
    genBinaryABC(c, n, dest, opcShlInt)
    # genNarrowU modified
    let t = skipTypes(n.typ, abstractVar-{tyTypeDesc})
    if t.kind in {tyUInt8..tyUInt32} or (t.kind == tyUInt and t.size < 8):
      c.gABC(n, opcNarrowU, dest, TRegister(t.size*8))
    elif t.kind in {tyInt8..tyInt32} or (t.kind == tyInt and t.size < 8):
      c.gABC(n, opcSignExtend, dest, TRegister(t.size*8))
  of mAshrI: genBinaryABC(c, n, dest, opcAshrInt)
  of mBitandI: genBinaryABC(c, n, dest, opcBitandInt)
  of mBitorI: genBinaryABC(c, n, dest, opcBitorInt)
  of mBitxorI: genBinaryABC(c, n, dest, opcBitxorInt)
  of mAddU: genBinaryABCnarrowU(c, n, dest, opcAddu)
  of mSubU: genBinaryABCnarrowU(c, n, dest, opcSubu)
  of mMulU: genBinaryABCnarrowU(c, n, dest, opcMulu)
  of mDivU: genBinaryABCnarrowU(c, n, dest, opcDivu)
  of mModU: genBinaryABCnarrowU(c, n, dest, opcModu)
  of mEqI, mEqB, mEqEnum, mEqCh:
    genBinaryABC(c, n, dest, opcEqInt)
  of mLeI, mLeEnum, mLeCh, mLeB:
    genBinaryABC(c, n, dest, opcLeInt)
  of mLtI, mLtEnum, mLtCh, mLtB:
    genBinaryABC(c, n, dest, opcLtInt)
  of mEqF64: genBinaryABC(c, n, dest, opcEqFloat)
  of mLeF64: genBinaryABC(c, n, dest, opcLeFloat)
  of mLtF64: genBinaryABC(c, n, dest, opcLtFloat)
  of mLePtr, mLeU: genBinaryABC(c, n, dest, opcLeu)
  of mLtPtr, mLtU: genBinaryABC(c, n, dest, opcLtu)
  of mEqProc, mEqRef:
    genBinaryABC(c, n, dest, opcEqRef)
  of mXor: genBinaryABC(c, n, dest, opcXor)
  of mNot: genUnaryABC(c, n, dest, opcNot)
  of mUnaryMinusI, mUnaryMinusI64:
    genUnaryABC(c, n, dest, opcUnaryMinusInt)
    genNarrow(c, n, dest)
  of mUnaryMinusF64: genUnaryABC(c, n, dest, opcUnaryMinusFloat)
  of mUnaryPlusI, mUnaryPlusF64: gen(c, n[1], dest)
  of mBitnotI:
    genUnaryABC(c, n, dest, opcBitnotInt)
    #genNarrowU modified, do not narrow signed types
    let t = skipTypes(n.typ, abstractVar-{tyTypeDesc})
    if t.kind in {tyUInt8..tyUInt32} or (t.kind == tyUInt and t.size < 8):
      c.gABC(n, opcNarrowU, dest, TRegister(t.size*8))
  of mCharToStr, mBoolToStr, mIntToStr, mInt64ToStr, mFloatToStr, mCStrToStr, mStrToStr, mEnumToStr:
    genConv(c, n, n[1], dest)
  of mEqStr, mEqCString: genBinaryABC(c, n, dest, opcEqStr)
  of mLeStr: genBinaryABC(c, n, dest, opcLeStr)
  of mLtStr: genBinaryABC(c, n, dest, opcLtStr)
  of mEqSet: genBinarySet(c, n, dest, opcEqSet)
  of mLeSet: genBinarySet(c, n, dest, opcLeSet)
  of mLtSet: genBinarySet(c, n, dest, opcLtSet)
  of mMulSet: genBinarySet(c, n, dest, opcMulSet)
  of mPlusSet: genBinarySet(c, n, dest, opcPlusSet)
  of mMinusSet: genBinarySet(c, n, dest, opcMinusSet)
  of mConStrStr: genVarargsABC(c, n, dest, opcConcatStr)
  of mInSet:
    let
      tmp = c.genx(n[1])
      tmp2 = c.genSetElem(n[2], n[1].typ)
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABC(n, opcContainsSet, dest, tmp, tmp2)
    c.freeTemp(tmp)
    c.freeTemp(tmp2)
  of mRepr:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABx(n, opcRepr, dest, c.genTypeInfo(n[1].typ))
    c.gABC(n, opcRepr, dest, tmp)
    c.freeTemp(tmp)
  of mExit:
    unused(c, n, dest)
    var tmp = c.genx(n[1])
    c.gABC(n, opcQuit, tmp)
    c.freeTemp(tmp)
  of mSetLengthStr, mSetLengthSeq:
    unused(c, n, dest)
    var d = c.genx(n[1])
    var tmp = c.genx(n[2])
    c.gABC(n, if m == mSetLengthStr: opcSetLenStr else: opcSetLenSeq, d, tmp)
    c.freeTemp(tmp)
    c.freeTemp(d)
  of mSwap:
    unused(c, n, dest)
    c.gen(lowerSwap(c.graph, n, c.idgen, if c.prc == nil or c.prc.sym == nil: c.module else: c.prc.sym))
  of mIsNil: genUnaryABC(c, n, dest, opcIsNil)
  of mParseBiggestFloat:
    if dest.isUnset: dest = c.getTemp(n.typ)
    var d2: TRegister
    # skip 'nkHiddenAddr':
    let d2AsNode = n[2][0]
    if needsAsgnPatch(d2AsNode):
      d2 = c.getTemp(getSysType(c.graph, n.info, tyFloat))
    else:
      d2 = c.genx(d2AsNode)
    var
      tmp1 = c.genx(n[1])
      tmp3 = c.genx(n[3])
    c.gABC(n, opcParseFloat, dest, tmp1, d2)
    c.gABC(n, opcParseFloat, tmp3)
    c.freeTemp(tmp1)
    c.freeTemp(tmp3)
    c.genAsgnPatch(d2AsNode, d2)
    c.freeTemp(d2)
  of mReset:
    unused(c, n, dest)
    var d = c.genx(n[1])
    # XXX use ldNullOpcode() here?
    c.gABx(n, opcLdNull, d, c.genType(n[1].typ))
    c.gABx(n, opcNodeToReg, d, d)
    c.genAsgnPatch(n[1], d)
  of mDefault:
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABx(n, ldNullOpcode(n.typ), dest, c.genType(n.typ))
  of mOf:
    if dest.isUnset: dest = c.getTemp(n.typ)

    let t1 = n[1].typ.skipTypes(abstractRange)
    if t1.kind != tyRef:
      # XXX: the spec for `of` with non-ref types is missing, so we simply
      #      treat it as an `is` for now. If it's decided that this is the
      #      correct behaviour, the `of` should be eliminated in `sem` instead
      #      of down here
      c.gABx(n, opcLdImmInt, dest, ord(sameType(t1, n[2].typ)))
    else:
      var tmp = c.genx(n[1])
      var idx = c.getTemp(getSysType(c.graph, n.info, tyInt))
      let typ = n[2].typ.skipTypes(abstractPtrs)
      c.gABx(n, opcLdImmInt, idx, c.genType(typ))
      c.gABC(n, opcOf, dest, tmp, idx)
      c.freeTemp(tmp)
      c.freeTemp(idx)
  of mHigh:
    if dest.isUnset: dest = c.getTemp(n.typ)
    let tmp = c.genx(n[1])
    case n[1].typ.skipTypes(abstractVar-{tyTypeDesc}).kind:
    of tyString: c.gABI(n, opcLenStr, dest, tmp, 1)
    of tyCstring: c.gABI(n, opcLenCstring, dest, tmp, 1)
    else: c.gABI(n, opcLenSeq, dest, tmp, 1)
    c.freeTemp(tmp)
  of mEcho:
    unused(c, n, dest)
    let n = n[1].skipConv
    if n.kind == nkBracket:
      # can happen for nim check, see bug #9609
      let x = c.getTempRange(n.len, slotTempUnknown)
      for i in 0..<n.len:
        var r: TRegister = x+i
        c.gen(n[i], r)
      c.gABC(n, opcEcho, x, n.len)
      c.freeTempRange(x, n.len)
  of mAppendStrCh:
    unused(c, n, dest)
    genBinaryStmtVar(c, n, opcAddStrCh)
  of mAppendStrStr:
    unused(c, n, dest)
    genBinaryStmtVar(c, n, opcAddStrStr)
  of mAppendSeqElem:
    unused(c, n, dest)
    genBinaryStmtVar(c, n, opcAddSeqElem)
  of mParseExprToAst:
    genParseOp(c, n, dest, opcParseExprToAst)
  of mParseStmtToAst:
    genParseOp(c, n, dest, opcParseStmtToAst)
  of mTypeTrait:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABx(n, opcNSetType, tmp, c.genTypeInfo(n[1].typ))
    c.gABC(n, opcTypeTrait, dest, tmp)
    c.freeTemp(tmp)
  of mSlurp: genUnaryABC(c, n, dest, opcSlurp)
  of mStaticExec: genBinaryABCD(c, n, dest, opcGorge)
  of mNLen: genUnaryABI(c, n, dest, opcLenSeq, nimNodeFlag)
  of mGetImpl: genUnaryABC(c, n, dest, opcGetImpl)
  of mGetImplTransf: genUnaryABC(c, n, dest, opcGetImplTransf)
  of mSymOwner: genUnaryABC(c, n, dest, opcSymOwner)
  of mSymIsInstantiationOf: genBinaryABC(c, n, dest, opcSymIsInstantiationOf)
  of mNChild: genBinaryABC(c, n, dest, opcNChild)
  of mNSetChild: genVoidABC(c, n, dest, opcNSetChild)
  of mNDel: genVoidABC(c, n, dest, opcNDel)
  of mNAdd: genBinaryABC(c, n, dest, opcNAdd)
  of mNAddMultiple: genBinaryABC(c, n, dest, opcNAddMultiple)
  of mNKind: genUnaryABC(c, n, dest, opcNKind)
  of mNSymKind: genUnaryABC(c, n, dest, opcNSymKind)

  of mNccValue: genUnaryABC(c, n, dest, opcNccValue)
  of mNccInc: genBinaryABC(c, n, dest, opcNccInc)
  of mNcsAdd: genBinaryABC(c, n, dest, opcNcsAdd)
  of mNcsIncl: genBinaryABC(c, n, dest, opcNcsIncl)
  of mNcsLen: genUnaryABC(c, n, dest, opcNcsLen)
  of mNcsAt: genBinaryABC(c, n, dest, opcNcsAt)
  of mNctPut: genVoidABC(c, n, dest, opcNctPut)
  of mNctLen: genUnaryABC(c, n, dest, opcNctLen)
  of mNctGet: genBinaryABC(c, n, dest, opcNctGet)
  of mNctHasNext: genBinaryABC(c, n, dest, opcNctHasNext)
  of mNctNext:
    # Load the dest register with a null value of correct type, so that the
    # instruction can write into dest directly without run-time type lookup
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABx(n, opcLdNull, dest, c.genType(n.typ))
    genBinaryABC(c, n, dest, opcNctNext)

  of mNIntVal: genUnaryABC(c, n, dest, opcNIntVal)
  of mNFloatVal: genUnaryABC(c, n, dest, opcNFloatVal)
  of mNGetType:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n.typ)
    let rc = case n[0].sym.name.s:
      of "getType":     0
      of "typeKind":    1
      of "getTypeInst": 2
      of "getTypeImpl": 3
      else: unreachable()
    c.gABC(n, opcNGetType, dest, tmp, rc)
    c.freeTemp(tmp)
    #genUnaryABC(c, n, dest, opcNGetType)
  of mNSizeOf:
    let imm = case n[0].sym.name.s:
      of "getSize":   0
      of "getAlign":  1
      of "getOffset": 2
      else: unreachable()
    c.genUnaryABI(n, dest, opcNGetSize, imm)
  of mNStrVal: genUnaryABC(c, n, dest, opcNStrVal)
  of mNSigHash: genUnaryABC(c, n , dest, opcNSigHash)
  of mNSetIntVal:
    unused(c, n, dest)
    genBinaryStmt(c, n, opcNSetIntVal)
  of mNSetFloatVal:
    unused(c, n, dest)
    genBinaryStmt(c, n, opcNSetFloatVal)
  of mNSetStrVal:
    unused(c, n, dest)
    genBinaryStmt(c, n, opcNSetStrVal)
  of mNNewNimNode: genBinaryABC(c, n, dest, opcNNewNimNode)
  of mNCopyNimNode: genUnaryABC(c, n, dest, opcNCopyNimNode)
  of mNCopyNimTree: genUnaryABC(c, n, dest, opcNCopyNimTree)
  of mStrToIdent: genUnaryABC(c, n, dest, opcStrToIdent)
  of mEqIdent: genBinaryABC(c, n, dest, opcEqIdent)
  of mEqNimrodNode: genBinaryABC(c, n, dest, opcEqNimNode)
  of mSameNodeType: genBinaryABC(c, n, dest, opcSameNodeType)
  of mNLineInfo:
    case n[0].sym.name.s
    of "getFile": genUnaryABI(c, n, dest, opcNGetLineInfo, 0)
    of "getLine": genUnaryABI(c, n, dest, opcNGetLineInfo, 1)
    of "getColumn": genUnaryABI(c, n, dest, opcNGetLineInfo, 2)
    of "copyLineInfo":
      internalAssert(c.config, n.len == 3, "Line info expects tuple with three elements")
      unused(c, n, dest)
      genBinaryStmt(c, n, opcNSetLineInfo)
    else:
      internalAssert(
        c.config, false, "Unexpected mNLineInfo symbol name - " & n[0].sym.name.s)
  of mNHint, mNWarning, mNError:
    unused(c, n, dest)
    c.genCall(n, dest)
  of mNCallSite:
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABC(n, opcCallSite, dest)
  of mNGenSym: genBinaryABC(c, n, dest, opcGenSym)
  of mMinI, mMaxI, mAbsI, mDotDot:
    c.genCall(n, dest)
  of mExpandToAst:
    if n.len != 2:
      fail(n.info, vmGenDiagBadExpandToAstArgRequired)

    let arg = n[1]
    if arg.kind in nkCallKinds:
      #if arg[0].kind != nkSym or arg[0].sym.kind notin {skTemplate, skMacro}:
      #      "ExpandToAst: expanded symbol is no macro or template"
      if dest.isUnset: dest = c.getTemp(n.typ)

      if arg[0].sym.kind == skTemplate:
        let x = c.getTempRange(arg.len, slotTempUnknown)

        # Pass the call expression as the first value
        var tmp = TDest(x)
        c.genLit(n, c.toNodeCnst(arg), tmp)

        # Call arguments
        for i in 1..<arg.len:
          var d = TDest(x+i)
          c.gen(arg[i], d)

        c.gABC(arg, opcExpandToAst, dest, x, arg.len)
        c.freeTempRange(x, arg.len)
      else:
        # macros are still invoked via the opcIndCall mechanism
        c.genCall(arg, dest)

      # do not call clearDest(n, dest) here as getAst has a meta-type as such
      # produces a value
    else:
      fail(n.info, vmGenDiagBadExpandToAstCallExprRequired)

  of mSizeOf, mAlignOf, mOffsetOf:
    fail(n.info, vmGenDiagMissingImportcCompleteStruct, m)

  of mRunnableExamples:
    discard "just ignore any call to runnableExamples"
  of mDestroy, mTrace: discard "ignore calls to the default destructor"
  of mMove:
    let arg = n[1]
    let a = c.genx(arg)
    if dest.isUnset: dest = c.getTemp(arg.typ)
    gABC(c, arg, whichAsgnOpc(arg, requiresCopy=false), dest, a)
    # XXX use ldNullOpcode() here?
    # Don't zero out the arg for now #17199
    # c.gABx(n, opcLdNull, a, c.genType(arg.typ))
    # c.gABx(n, opcNodeToReg, a, a)
    # c.genAsgnPatch(arg, a)
    c.freeTemp(a)
  of mNodeId:
    c.genUnaryABC(n, dest, opcNodeId)
  else:
    # mGCref, mGCunref, mFinished, etc.
    fail(n.info, vmGenDiagCodeGenUnhandledMagic, m)

proc genDeref(c: var TCtx, n: CgNode, dest: var TDest, flags: TGenFlags) =
    let tmp = c.genx(n[0])
    if dest.isUnset: dest = c.getTemp(n.typ)
    gABC(c, n, opcLdDeref, dest, tmp)
    assert n.typ != nil
    if needsRegLoad():
      c.genRegLoad(n, dest, dest)
    c.freeTemp(tmp)

proc genAsgn(c: var TCtx; dest: TDest; ri: CgNode; requiresCopy: bool) =
  let tmp = c.genx(ri)
  assert dest >= 0
  gABC(c, ri, whichAsgnOpc(ri, requiresCopy), dest, tmp)
  c.freeTemp(tmp)

func setSlot(c: var TCtx; v: PSym): TRegister {.discardable.} =
  # XXX generate type initialization here?
  result = getFreeRegister(c, if v.kind == skLet: slotFixedLet else: slotFixedVar, start = 1)
  c.prc.locals[v.id] = result

func cannotEval(c: TCtx; n: CgNode) {.noinline, noreturn.} =
  raiseVmGenError(vmGenDiagCannotEvaluateAtComptime, n)

func getOwner(c: TCtx): PSym =
  result = c.prc.sym
  if result.isNil: result = c.module

proc importcCondVar*(s: PSym): bool {.inline.} =
  # see also importcCond
  if sfImportc in s.flags:
    return s.kind in {skVar, skLet, skConst}

proc checkCanEval(c: TCtx; n: CgNode) =
  # we need to ensure that we don't evaluate 'x' here:
  # proc foo() = var x ...
  let s = n.sym
  if {sfCompileTime, sfGlobal} <= s.flags: return
  if s.importcCondVar:
    # Defining importc'ed variables is allowed and since `checkCanEval` is
    # also used by `genVarSection`, don't fail here
    return
  if s.kind in {skVar, skTemp, skLet, skParam, skResult} and
      not s.isOwnedBy(c.prc.sym) and s.owner != c.module and
      c.mode notin {emRepl, emStandalone}:
    # little hack ahead for bug #12612: assume gensym'ed variables
    # are in the right scope:
    if sfGenSym in s.flags and c.prc.sym == nil: discard
    else: cannotEval(c, n)
  elif s.kind in {skProc, skFunc, skConverter, skMethod,
                  skIterator} and sfForward in s.flags:
    cannotEval(c, n)


proc genDiscrVal(c: var TCtx, discr: PSym, n: CgNode, oty: PType): TRegister =
  ## Generate the code for preparing and loading the discriminator value
  ## as expected by the execution engine

  let oty = oty.skipTypes(abstractPtrs)
  assert oty.kind == tyObject

  let discrTyp = block:
    let (o, idx) =
      getFieldAndOwner(
        c.getOrCreate(oty),
        fpos(discr.position))
    o.fieldAt(idx).typ

  let recCase = findRecCase(oty, discr)
  assert recCase != nil

  if n.kind == cgnkLiteralData:
    # Discriminator value is known at compile-time
    let b = findMatchingBranch(recCase, n)
    assert b != -1 # no matching branch; should have been caught already

    assert n.data.intVal <= (1 shl discrTyp.numBits) - 1
    let v = bitor(b shl discrTyp.numBits, int(n.data.intVal))

    result = c.getTemp(n.typ)
    var tmp = TDest(result)
    c.genLit(n, c.toIntCnst(v), tmp)
  else:
    result = c.getTemp(discr.typ)

    var endings: seq[TPosition] = @[]
    let bIReg = c.getTemp(discr.typ)
    let tmp = c.getTemp(discr.typ)
    # XXX: this is mostly just copied from `genCase`
    let dt = discr.typ.skipTypes(abstractVarRange)
    c.gen(n, tmp)
    # branch tmp, codeIdx
    # fjmp   elseLabel

    # iterate of/else branches
    for i in 1..<recCase.len:
      let branch = recCase[i]
      let bI = i - 1
      assert bI <= int(high(uint16))
      if branch.len == 1:
        # else branch:
        c.gABx(n, opcLdImmInt, bIReg, bI)
      else:
        # of branch
        let b = genBranchLit(c, branch, dt)
        c.gABx(branch, opcBranch, tmp, b)
        let elsePos = c.xjmp(branch.lastSon, opcFJmp, tmp)
        c.gABx(n, opcLdImmInt, bIReg, bI)
        if i < recCase.len-1:
          endings.add(c.xjmp(branch.lastSon, opcJmp, 0))
        c.patch(elsePos)

    for endPos in endings: c.patch(endPos)

    let tmp2 = c.getTemp(discr.typ)

    c.gABx(n, opcLdImmInt, tmp2, discrTyp.numBits)
    c.gABC(n, opcShlInt, result, bIReg, tmp2)
    c.gABC(n, opcAsgnInt, tmp2, result)
    c.gABC(n, opcBitorInt, result, tmp2, tmp)

    c.freeTemp(tmp)
    c.freeTemp(tmp2)
    c.freeTemp(bIReg)


proc genFieldAsgn(c: var TCtx, obj: TRegister; le, ri: CgNode) =
  c.config.internalAssert(le.kind == cgnkObjAccess)

  let idx = c.genField(le.field)
  let s = le.field.sym

  let tmp = c.genx(ri)
  c.gABC(le, opcWrObj, obj, idx, tmp)
  c.freeTemp(tmp)

proc genBranchSwitch(c: var TCtx, obj: TRegister; le, ri: CgNode)=
    # Can't use `s.owner.typ` since it may be a `tyGenericBody`
    let
      idx = c.genField(le.field)
      s = le.field.sym

    var tmp = c.genx(ri)

    tmp = c.genDiscrVal(le.field, ri, le.path.typ)
    c.gABC(le, opcSetDisc, obj, idx, tmp)


    c.freeTemp(tmp)

proc genAsgn(c: var TCtx; le, ri: CgNode) =
  case le.kind
  of cgnkArrayAccess:
    let typ = le.path.typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind
    let dest = c.genx(le.path, {gfNode})
    let idx = c.genIndex(le.indexExpr, le.path.typ)
    let tmp = c.genx(ri)
    let opc = if typ in {tyString, tyCstring}: opcWrStrIdx
              else: opcWrArr

    c.gABC(le, opc, dest, idx, tmp)
    c.freeTemp(tmp)
    c.freeTemp(idx)
    c.freeTemp(dest)
  of cgnkTupleAccess:
    let dest = c.genx(le[0], {gfNode})
    let tmp = c.genx(ri)
    c.gABC(le, opcWrObj, dest, tmp, ri)
    c.freeTemp(dest)
    c.freeTemp(tmp)
  of cgnkObjAccess:
    let dest = c.genx(le[0], {gfNode})
    c.genFieldAsgn(dest, le, ri)
    # c.freeTemp(idx) # BUGFIX: idx is an immediate (field position), not a register
    c.freeTemp(dest)
  of cgnkDeref, cgnkDerefView:
    let dest = c.genx(le[0], #[{gfNode}]#)
    let tmp = c.genx(ri)
    c.gABC(le, opcWrDeref, dest, 0, tmp)
    c.freeTemp(dest)
    c.freeTemp(tmp)
  of cgnkGlobal:
      let s = le.sym
      checkCanEval(c, le)
        let
          tmp = genSymAddr(c, le)
          val = c.genx(ri)
        c.gABC(le, opcWrDeref, tmp, 0, val)
        c.freeTemp(val)
        c.freeTemp(tmp)

  of cgnkLocal, cgnkParam:
      let s = le.sym
      checkCanEval(c, le)
      var dest = c.prc.local(s)
      c.config.internalAssert dest >= 0

      assert le.typ != nil
      if fitsRegister(le.typ):
        # the value fits into a register; we can use the local's register as
        # the destination directly
        gen(c, ri, dest)
      else:
        # an assignment is required
        let cc = genx(c, ri)
        c.gABC(le, opcAsgnComplex, dest, cc)
        c.freeTemp(cc)
  else:
    unreachable(n.kind)

proc genTypeLit(c: var TCtx; t: PType; dest: var TDest) =
  var n = newNode(nkType)
  n.typ = t
  genLit(c, n, dest)

proc importcCond*(c: TCtx; s: PSym): bool {.inline.} =
  ## return true to importc `s`, false to execute its body instead (refs #8405)
  if sfImportc in s.flags:
    if s.kind in routineKinds:
      return getBody(c.graph, s).kind == nkEmpty

func local(c: TCtx, n: PNode): TRegister =
  ## Looks up and returns the register that stores the local named by symbol
  ## node `n`.
  let local = c.prc.local(n.sym)
  if local >= 0:
    result = local
  else:
    # TODO: semantic analysis currently makes it the responsibility of the
    #       code-generators to check whether an enitity (a local in this
    #       case) is accesible in the current context. For example:
    #
    #         var a = 0
    #         const b = a # `a` isn't really in scope
    #
    #       is not rejected by sem, so we have to reject it here (i.e. the
    #       ``cannotEval``). See tests/t99bott for an example that triggers
    #       it
    cannotEval(c, n)

proc useGlobal(c: var TCtx, n: PNode): int =
    ## Resolves the global identified by symbol node `n` to the ID that
    ## identifies it at run-time. If using the global is illegal (because
    ## it's an importc'ed variable, for example), an error is raised.
    let s = n.sym

    if importcCondVar(s) or c.importcCond(s):
      # Using importc'ed symbols on the left or right side of an expression is
      # not allowed
      fail(n.info, vmGenDiagCannotImportc, sym = s)

    if s.id in c.symToIndexTbl:
      # XXX: double table lookup
      result = c.symToIndexTbl[s.id].int
    else:
      if c.mode in {emRepl, emStandalone}:
        # for REPL and standalone mode, allow the ad-hoc setup of globals. For
        # the VM back-end (standalone mode), the modules aren't necessarily
        # processed in a meaningfull order, so the global's var section might
        # have been not visited yet
        result = c.registerGlobal(s)
      else:
        # a global that is not accessible in the current context
        cannotEval(c, n)

proc genSym(c: var TCtx; n: PNode; dest: var TDest; flags: TGenFlags) =
  ## Generates and emits the code for loading either the value or handle of
  ## the location named by symbol node `n` into the `dest` register.
  let s = n.sym
  if s.isGlobal:
    let pos = useGlobal(c, n)
    if dest.isUnset:
      dest = c.getTemp(s.typ)

    if fitsRegister(s.typ) and gfNode notin flags:
      let cc = c.getTemp(n.typ)
      c.gABx(n, opcLdGlobal, cc, pos)
      c.genRegLoad(n, dest, cc)
      c.freeTemp(cc)
    else:
      c.gABx(n, opcLdGlobal, dest, pos)
  else:
      let local = local(c, n)
      if dest.isUnset:
        dest = local
        internalAssert(c.config, c.prc.regInfo[dest].kind < slotSomeTemp)
      else:
        # despite the name, ``opcFastAsgnComplex`` currently performs a
        # register copy, which is exactly what we need here
        c.gABC(n, opcFastAsgnComplex, dest, local)

proc genSymAddr(c: var TCtx, n: PNode, dest: var TDest) =
  ## Generates and emits the code for taking the address of the location
  ## identified by the symbol node `n`.
  let s = n.sym
  if dest.isUnset:
    dest = c.getTemp(s.typ)

  if s.isGlobal:
    let pos = useGlobal(c, n)
    c.gABx(n, opcLdGlobalAddr, dest, pos)
  else:
    let local = local(c, n)
    c.gABC(n, opcAddrReg, dest, local)

proc genSymAddr(c: var TCtx, n: PNode): TRegister =
  var dest = TDest(-1)
  genSymAddr(c, n, dest)
  result = dest

proc genArrAccessOpcode(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode;
                        flags: TGenFlags) =
  let a = c.genx(n[0], {gfNode})
  let b = c.genIndex(n[1], n[0].typ)
  if dest.isUnset: dest = c.getTemp(n.typ)
  if opc in {opcLdArrAddr, opcLdStrIdxAddr}:
    c.gABC(n, opc, dest, a, b)
  elif opc == opcLdStrIdx:
    c.gABC(n, opc, dest, a, b)
  elif needsRegLoad():
    var cc = c.getTemp(n.typ)
    c.gABC(n, opc, cc, a, b)
    c.genRegLoad(n, dest, cc)
    c.freeTemp(cc)
  else:
    #message(c.config, n.info, warnUser, "argh")
    #echo "FLAGS ", flags, " ", fitsRegister(n.typ), " ", typeToString(n.typ)
    c.gABC(n, opc, dest, a, b)
    c.makeHandleReg(dest, a)
  c.freeTemp(a)
  c.freeTemp(b)

proc genFieldAccessAux(c: var TCtx; n: PNode; a, b: TRegister, dest: var TDest; flags: TGenFlags) =
  ## Emits the code for loading either the value or handle of an
  ## ``object``/``tuple`` field into the `dest` register. The `a` register
  ## holds the handle to the object location, while `b` is an immedate value
  ## representing the position of the accessed field.
  prepare(c, dest, n.typ)
  if needsRegLoad():
    var cc = c.getTemp(n.typ)
    c.gABC(n, opcLdObj, cc, a, b)
    c.genRegLoad(n, dest, cc)
    c.freeTemp(cc)
  else:
    c.gABC(n, opcLdObj, dest, a, b)
    c.makeHandleReg(dest, a)

proc genFieldAccess(c: var TCtx; n: PNode; dest: var TDest; flags: TGenFlags) =
  ## Generates and emits the code for a dot-expression `n` (i.e. field access).
  ## The resulting value/handle is stored to `dest`.
  assert n.kind == nkDotExpr
  let
    a = c.genx(n[0], {gfNode})
    b = genField(c, n[1])

  genFieldAccessAux(c, n, a, b, dest, flags)
  c.freeTemp(a)

proc genFieldAddr(c: var TCtx, n, obj: PNode, fieldPos: int, dest: TDest) =
  let obj = c.genx(obj)
  c.gABC(n, opcLdObjAddr, dest, obj, fieldPos)
  c.freeTemp(obj)

proc genCheckedObjAccessAux(c: var TCtx; n: PNode): TRegister =
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
  result = c.genx(accessExpr[0], {gfNode})
  # Load the discriminant
  var discVal = c.getTemp(disc.typ)
  var discValTemp = c.getTemp(disc.typ)
  c.gABC(n, opcLdObj, discValTemp, result, genField(c, disc))
  c.gABC(n, opcNodeToReg, discVal, discValTemp)
  c.freeTemp(discValTemp)
  # Check if its value is contained in the supplied set
  let setLit = c.genx(checkExpr[1])
  var rs = c.getTemp(getSysType(c.graph, n.info, tyBool))
  c.gABC(n, opcContainsSet, rs, setLit, discVal)
  c.freeTemp(setLit)
  # If the check fails let the user know
  let lab1 = c.xjmp(n, if negCheck: opcFJmp else: opcTJmp, rs)
  c.freeTemp(rs)
  let strType = getSysType(c.graph, n.info, tyString)
  var msgReg: TDest = c.getTemp(strType)
  var discrStrReg: TDest = c.getTemp(strType)
  let fieldName = $accessExpr[1]
  let msg = genFieldDefect(c.config, fieldName, disc.sym)
  let strLit = newStrNode(msg, accessExpr[1].info)
  strLit.typ = strType
  c.genLit(strLit, msgReg)
  # repr for discriminator value
  c.gABx(n, opcRepr, discrStrReg, c.genTypeInfo(disc.typ))
  c.gABC(n, opcRepr, discrStrReg, discVal)
  c.gABC(n, opcInvalidField, msgReg, discrStrReg)
  c.freeTemp(discVal)
  c.freeTemp(msgReg)
  c.freeTemp(discrStrReg)
  c.patch(lab1)

proc genCheckedObjAccess(c: var TCtx; n: PNode; dest: var TDest; flags: TGenFlags) =
  let objR = genCheckedObjAccessAux(c, n)

  let accessExpr = n[0]
  # Field symbol
  var field = accessExpr[1]
  internalAssert(
    c.config,
    field.sym.kind == skField,
    "Access expression must be a field, but found " & $field.sym.kind)

  # Load the content now
  if dest.isUnset: dest = c.getTemp(n.typ)
  let fieldPos = genField(c, field)

  if needsRegLoad():
    var cc = c.getTemp(accessExpr.typ)
    c.gABC(n, opcLdObj, cc, objR, fieldPos)
    c.genRegLoad(n, dest, cc)
    c.freeTemp(cc)
  else:
    c.gABC(n, opcLdObj, dest, objR, fieldPos)
    c.makeHandleReg(dest, objR)

  c.freeTemp(objR)

proc genArrAccess(c: var TCtx; n: PNode; dest: var TDest; flags: TGenFlags) =
  let arrayType = n[0].typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind
  case arrayType
  of tyString, tyCstring:
    genArrAccessOpcode(c, n, dest, opcLdStrIdx, flags)
  of tyTuple:
    let a = genx(c, n[0], {gfNode})
    genFieldAccessAux(c, n, a, n[1].intVal, dest, flags)
    c.freeTemp(a)
  of tyArray, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray:
    genArrAccessOpcode(c, n, dest, opcLdArr, flags)
  else:
    unreachable()

proc genBracketAddr(c: var TCtx, n: PNode, dest: var TDest) =
  ## Generates the code for loading the address of a bracket expression (i.e.
  ## ``addr x[0]``)
  assert not dest.isUnset
  case n[0].typ.skipTypes(abstractInst-{tyTypeDesc}).kind
  of tyTuple:
    genFieldAddr(c, n, n[0], n[1].intVal.int, dest)
  of tyString, tyCstring:
    genArrAccessOpcode(c, n, dest, opcLdStrIdxAddr, {})
  of tyArray, tySequence, tyOpenArray, tyVarargs:
    genArrAccessOpcode(c, n, dest, opcLdArrAddr, {})
  else:
    unreachable()

proc genAddr(c: var TCtx, src, n: PNode, dest: var TDest) =
  ## Generates and emits the code for taking the address of lvalue expression
  ## `n`. `src` provides the line information to use for the emitted
  ## address-of instruction.
  case n.kind
  of nkSym:
    prepare(c, dest, src.typ)
    genSymAddr(c, n, dest)
  of nkDotExpr:
    prepare(c, dest, src.typ)
    genFieldAddr(c, n, n[0], genField(c, n[1]), dest)
  of nkBracketExpr:
    prepare(c, dest, src.typ)
    genBracketAddr(c, n, dest)
  of nkCheckedFieldExpr:
    prepare(c, dest, src.typ)

    let obj = genCheckedObjAccessAux(c, n)
    c.gABC(src, opcLdObjAddr, dest, obj, genField(c, n[0][1]))
    c.freeTemp(obj)
  of nkHiddenDeref:
    # a dereference of a view. Since the register stores either a handle,
    # an address, or a register address, we can safely treat the ``addr``
    # operation as a no-op
    # NOTE: because of ``rkRegAddr``, this is currently not only an
    #       optimization, but necessary
    gen(c, n[0], dest)
  of nkDerefExpr:
    prepare(c, dest, src.typ)

    var tmp = TDest(-1)
    genDeref(c, n, tmp, {gfNode})
    c.gABC(src, opcAddrReg, dest, tmp)
    c.freeTemp(tmp)
  of nkConv:
    # an l-value conversion. Apply the operation to the source expression
    genAddr(c, src, n[1], dest)
  of nkObjDownConv, nkObjUpConv:
    genAddr(c, src, n[0], dest)
  of nkStmtListExpr:
    for i in 0..<n.len-1:
      gen(c, n[i])

    genAddr(c, src, n.lastSon, dest)
  else:
    unreachable(n.kind)

proc genVarSection(c: var TCtx; n: PNode) =
  for a in n:
    case a.kind
    of nkCommentStmt:
      continue
    of nkVarTuple:
      for i in 0..<a.len-2:
        case a[i].kind
        of nkSym:
          checkCanEval(c, a[i])
          let s = a[i].sym

          if s.isGlobal:
            # No need to check for function-level globals here, as var
            # tuples with the `{.global.}` pragma are currently forbidden
            discard c.registerGlobal(s)
            discard c.getOrCreate(s.typ)
          else:
            setSlot(c, s)
        else:
          discard # xxx: seems weird we don't know what to expect here

      c.gen(lowerTupleUnpacking(c.graph, a, c.idgen, c.getOwner))
    of nkIdentDefs:
      if a[0].kind == nkSym:
        let s = a[0].sym
        checkCanEval(c, a[0])
        if s.isGlobal:
          if importcCondVar(s):
            # Ignore definitions of importc'ed variables
            continue

          # XXX: during NimScript execution, top-level ``.compileTime``
          #      variables are code-gen'ed twice, once via `setupCompileTimeVar`
          #      called from `sem.semVarOrLet` and once through `vm.myProcess`.
          #      This leads to the global's symbol already being present in the
          #      table. For the VM back-end, the symbol is also already present
          #      if the global was used somewhere in another module
          #c.config.internalAssert(s.id notin c.symToIndexTbl, a[0].info)
          discard c.registerGlobal(s)
          discard c.getOrCreate(s.typ)

          # no need to generate an assignment if the global has no initializer
          if a[2].kind == nkEmpty:
            continue

          # for globals, ``vmgen`` trusts the callsite (i.e. the place where
          # ``genProcBody`` is invoked) to make sure that globals defined
          # inside procedures are extracted / otherwise taken care of. Thus, we
          # emit the initialization logic here without further checks
          let tmp = genSymAddr(c, a[0])
          let val = c.genx(a[2])
          c.gABC(a, opcWrDeref, tmp, 0, val)
          c.freeTemp(val)
          c.freeTemp(tmp)
        else:
          let reg = setSlot(c, s)
          if a[2].kind == nkEmpty:
            # no initializer; only setup the location
            c.gABx(a, ldNullOpcode(s.typ), reg, c.genType(s.typ))
          else:
            if fitsRegister(s.typ):
              # the value fits into a register; load it directly
              gen(c, a[2], reg)
            else:
              let rhs = genx(c, a[2])
              # assign the source value (the assignment takes care of
              # allocating the location):
              c.gABC(a, opcAsgnComplex, reg, rhs)
              c.freeTemp(rhs)

      elif a[2].kind == nkEmpty:
        # The closure's captured value is automatically zero-initialized when
        # creating the closure environment object; nothing left to do here
        discard
      else:
        # assign to a[0]; happens for closures
        genAsgn(c, a[0], a[2], true)
    else:
      # xxx: error out
      discard

proc genArrayConstr(c: var TCtx, n: PNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABx(n, opcLdNull, dest, c.genType(n.typ))

  let intType = getSysType(c.graph, n.info, tyInt)
  let seqType = n.typ.skipTypes(abstractVar-{tyTypeDesc})
  if seqType.kind == tySequence:
    var tmp = c.getTemp(intType)
    c.gABx(n, opcLdImmInt, tmp, n.len)
    c.gABx(n, opcNewSeq, dest, c.genType(seqType))
    c.gABx(n, opcNewSeq, tmp, 0)
    c.freeTemp(tmp)

  if n.len > 0:
    var tmp = getTemp(c, intType)
    c.gABx(n, opcLdNullReg, tmp, c.genType(intType))
    for x in n:
      let a = c.genx(x)
      c.gABC(n, opcWrArr, dest, tmp, a)
      c.gABI(n, opcAddImmInt, tmp, tmp, 1)
      c.freeTemp(a)
    c.freeTemp(tmp)

proc genSetConstr(c: var TCtx, n: PNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABx(n, opcLdNull, dest, c.genType(n.typ))
  # XXX: since `first` stays the same across the loop, we could invert
  #      the loop around `genSetElem`'s logic...
  let first = firstOrd(c.config, n.typ.skipTypes(abstractInst)).toInt()
  for x in n:
    if x.kind == nkRange:
      let a = c.genSetElem(x[0], first)
      let b = c.genSetElem(x[1], first)
      c.gABC(n, opcInclRange, dest, a, b)
      c.freeTemp(b)
      c.freeTemp(a)
    else:
      let a = c.genSetElem(x, first)
      c.gABC(n, opcIncl, dest, a)
      c.freeTemp(a)

proc genObjConstr(c: var TCtx, n: PNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)
  let t = n.typ.skipTypes(abstractRange-{tyTypeDesc})
  var refTemp: TDest
  if t.kind == tyRef:
    refTemp = c.getTemp(t[0]) # The temporary register to hold the
                              # dereferenced location
    c.gABx(n, opcNew, dest, c.genType(t))
    c.gABC(n, opcLdDeref, refTemp, dest)
    swap(refTemp, dest)
  else:
    c.gABx(n, opcLdNull, dest, c.genType(n.typ))
  for i in 1..<n.len:
    let it = n[i]
    if it.kind == nkExprColonExpr and it[0].kind == nkSym:
      let idx = genField(c, it[0])
      var tmp: TRegister
      var opcode: TOpcode
      if sfDiscriminant notin it[0].sym.flags:
        tmp = c.genx(it[1])
        opcode = opcWrObj
        let
          le = it[0].sym.typ
          ri = it[1].typ
        if le.kind == tyOpenArray and not sameType(le, ri):
          # XXX: this is a hack to make `tests/vm/tconst_views` work for now.
          #      `transf` removes `nkHiddenStdConv` for array/seq to openArray
          #      conversions, which we could have otherwise relied on
          let tmp2 = c.getTemp(le)
          c.gABC(n, opcConv, tmp2, tmp)
          c.gABx(n, opcConv, 0, c.genTypeInfo(le))
          c.gABx(n, opcConv, 0, c.genTypeInfo(ri))
          c.freeTemp(tmp)
          tmp = tmp2
      else:
        tmp = c.genDiscrVal(it[0], it[1], n.typ)
        opcode = opcInitDisc
      c.gABC(it[1], opcode, dest, idx, tmp)
      c.freeTemp(tmp)
    else:
      fail(n.info, vmGenDiagInvalidObjectConstructor, it)

  if t.kind == tyRef:
    swap(refTemp, dest)
    c.freeTemp(refTemp)

proc genTupleConstr(c: var TCtx, n: PNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)
  if n.typ.kind != tyTypeDesc:
    c.gABx(n, opcLdNull, dest, c.genType(n.typ))
    # XXX x = (x.old, 22)  produces wrong code ... stupid self assignments
    for i in 0..<n.len:
      let it = n[i]
      if it.kind == nkExprColonExpr:
        let idx = genField(c, it[0])
        let tmp = c.genx(it[1])
        c.gABC(it[1], opcWrObj, dest, idx, tmp)
        c.freeTemp(tmp)
      else:
        let tmp = c.genx(it)
        c.gABC(it, opcWrObj, dest, i.TRegister, tmp)
        c.freeTemp(tmp)

proc genClosureConstr(c: var TCtx, n: PNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)

  c.gABx(n, opcLdNull, dest, c.genType(n.typ))
  let tmp = c.genx(n[0])
  if n[1].kind == nkNilLit:
    # no environment
    c.gABC(n, opcWrClosure, dest, tmp, dest)
  else:
    let envTmp = c.genx(n[1])
    c.gABC(n, opcWrClosure, dest, tmp, envTmp)
    c.freeTemp(envTmp)

  c.freeTemp(tmp)

proc gen(c: var TCtx; n: CgNode; dest: var TDest; flags: TGenFlags = {}) =
  when defined(nimCompilerStacktraceHints):
    setFrameMsg c.config$n.info & " " & $n.kind & " " & $flags

  case n.kind
  of cgnkLocal:
    let s = n.sym
    checkCanEval(c, n)
    genSym(c, n, dest, flags)
  of cgnkProc:
    let s = n.sym
    checkCanEval(c, n)
    if s.kind == skIterator and s.typ.callConv == TCallingConvention.ccClosure:
      fail(n.info, vmGenDiagNoClosureIterators, sym = s)
    if importcCond(c, s) and lookup(c.callbackKeys, s) == -1:
      fail(n.info, vmGenDiagCannotImportc, sym = s)

    genProcLit(c, n, s, dest)
  of cgnkConst:
    let s = n.sym
    checkCanEval(c, n)

    if dest.isUnset: dest = c.getTemp(s.typ)

    if s.ast.kind in nkLiterals:
      let lit = genLiteral(c, s.ast)
      c.genLit(n, lit, dest)
    else:
      let idx = c.registerConst(s)
      discard c.getOrCreate(s.typ)
      c.gABx(n, opcLdCmplxConst, dest, idx)
  of cgnkParam:
    if
      if c.getOwner().kind == skMacro:
        genSym(c, n, dest, flags)
      else:
        # note: this can't be replaced with an assert. ``tryConstExpr`` is
        # sometimes used to check whether an expression can be evaluated
        # at compile-time, in which case we need to report an error when
        # encountering an unresolved generic parameter
        fail(n.info, vmGenDiagCannotGenerateCode, n)

  of cgnkStmtListExpr:
    for i in 0..<n.len-1:
      gen(c, n[i])
    gen(c, n.lastSon, dest, flags)
  of nkCall:
    genCall(c, n, dest)
    clearDest(c, n, dest)
  of cgnkCharLit..cgnkInt64Lit:
    if isInt16Lit(n):
      if dest.isUnset: dest = c.getTemp(n.typ)
      c.gABx(n, opcLdImmInt, dest, n.intVal.int)
    else:
      genLit(c, n, dest)
  of cgnkUIntLit..pred(cgnkNilLit): genLit(c, n, dest)
  of cgnkNilLit:
    if dest.isUnset: dest = c.getTemp(t)
    c.gABx(n, ldNullOpcode(t), dest, c.genType(n.typ))
  of cgnkNimNodeLit:
    # the VM does not copy the tree when loading a ``PNode`` constant (which
    # is correct). ``NimNode``s not marked with `nfSem` can be freely modified
    # inside macros, so in order to prevent mutations of the AST part of the
    # constant, we perform a defensive tree copy before assigning the literal
    # to the destination
    if dest.isUnset:
      dest = c.getTemp(n.typ)

    var tmp: TDest = c.getTemp(n.typ)
    c.genLit(n, c.toNodeCnst(n[0]), tmp)
    c.gABC(n, opcNCopyNimTree, dest, tmp)
    freeTemp(c, tmp)
  of cgnkObjAccess: genFieldAccess(c, n, dest, flags)
  of cgnkVariantAccess: genCheckedObjAccess(c, n, dest, flags)
  of cgnkArrayAccess: genArrAccess(c, n, dest, flags)
  of cgnkDeref, cgnkDerefView: genDeref(c, n, dest, flags)
  of cgnkAddr, cgnkView: genAddr(c, n, n[0], dest)
  of cgnkConv, cgnkLConv, cgnkObjDownConv, cgnkObjUpConv:
    genConv(c, n, n[0], dest)
  of cgnkChckRangeF, cgnkChckRange64, cgnkChckRange:
    let
      tmp0 = c.genx(n[0])
      tmp1 = c.genx(n[1])
      tmp2 = c.genx(n[2])
    c.gABC(n, opcRangeChck, tmp0, tmp1, tmp2)
    c.freeTemp(tmp1)
    c.freeTemp(tmp2)
    if dest >= 0:
      gABC(c, n, whichAsgnOpc(n), dest, tmp0)
      c.freeTemp(tmp0)
    else:
      dest = tmp0
  of nkStringToCString, nkCStringToString:
    gen(c, n[0], dest)
  of cgnkSetConstr:     genSetConstr(c, n, dest)
  of cgnkArrayConstr:   genArrayConstr(c, n, dest)
  of cgnkTupleConstr:   genTupleConstr(c, n, dest)
  of cgnkClosureConstr: genClosureConstr(c, n, dest)
  of cgnkObjConstr:     genObjConstr(c, n, dest)
  of nkCast:
    if allowCast in c.features:
      genConv(c, n, n[1], dest, opcCast)
    else:
      genCastIntFloat(c, n, dest)
  of cgnkType:
    genTypeLit(c, n.typ, dest)

proc gen(c: var TCtx, n: CgNode) =
  case n.kind
  of nkCall:
    genCall(c, n, dest)
  of cgnkAsgn, cgnkBlitCopy:
    genAsgn(c, n[0], n[1])
  of cgnkSwitch: genBranchSwitch(c, n)
  of cgnkIf:     genIf(c, n)
  of cgnkCase:   genCase(c, n, dest)
  of cgnkRepeat: genWhile(c, n)
  of cgnkBlock:  genBlock(c, n, dest)
  of cgnkReturn: genReturn(c, n)
  of cgnkRaise:  genRaise(c, n)
  of cgnkBreak:  genBreak(c, n)
  of cgnkTry:    genTry(c, n, dest)
  of cgnkStmtList:
    for x in n:
      gen(c, x)
  of cgnkVoidStmt:
    discard genx(c, n[0])
  of cgnkDef:
    genVarSection(c, n)
  else:
    unreachable(n.kind)

proc genStmt*(c: var TCtx; n: PNode): Result[void, VmGenDiag] =
  var d: TDest = -1
  try:
    c.gen(n, d)
  except VmGenError as e:
    return typeof(result).err(move e.diag)

  c.config.internalAssert(d < 0, n.info, "VM problem: dest register is set")
  result = typeof(result).ok()

proc genExpr*(c: var TCtx; n: PNode, requiresValue = true): VmGenResult =
  let start = c.code.len
  var d: TDest = -1
  tryOrReturn:
    c.gen(n, d)

  if d < 0:
    c.config.internalAssert(not requiresValue, n.info, "VM problem: dest register is not set")

    d = 0

  c.gABC(n, opcEof, d)
  # TODO: use `opcRet` for expressions that yield something, instead of
  #       overloading opcEof with this behaviour
  #[
  if requiresValue:
    assert d == 0
    c.gABC(n, opcRet)

  c.gABC(n, opcEof)
  ]#

  result = VmGenResult.ok:
    (start: start, regCount: c.prc.regInfo.len)

proc genParams(prc: PProc; s: PSym) =
  let
    params = s.typ.n
    res = if resultPos < s.ast.len: s.ast[resultPos] else: nil

  setLen(prc.regInfo, max(params.len, 1))

  if res != nil:
    prc.locals[res.sym.id] = 0
    prc.regInfo[0] = RegInfo(refCount: 1, kind: slotFixedVar)

  for i in 1..<params.len:
    prc.locals[params[i].sym.id] = i
    prc.regInfo[i] = RegInfo(refCount: 1, kind: slotFixedLet)

proc finalJumpTarget(c: var TCtx; pc, diff: int) =
  internalAssert(
    c.config,
    regBxMin < diff and diff < regBxMax,
    "Jump target is not in range of min/max registers - $1 < $2 < $3 failed" % [
      $regBxMin, $diff, $regBxMax])

  let oldInstr = c.code[pc]
  # opcode and regA stay the same:
  c.code[pc] = ((
    oldInstr.TInstrType and
    ((regOMask shl regOShift) or (regAMask shl regAShift))).TInstrType or
                TInstrType(diff+wordExcess) shl regBxShift).TInstr

proc genGenericParams(c: var TCtx; gp: PNode) =
  var base = c.prc.regInfo.len
  setLen c.prc.regInfo, base + gp.len
  for i in 0..<gp.len:
    var param = gp[i].sym
    c.prc.locals[param.id] = base + i # XXX: fix this earlier; make it
                                      # consistent with templates
    c.prc.regInfo[base + i] = RegInfo(refCount: 1, kind: slotFixedLet)

proc optimizeJumps(c: var TCtx; start: int) =
  const maxIterations = 10
  for i in start..<c.code.len:
    let opc = c.code[i].opcode
    case opc
    of opcTJmp, opcFJmp:
      var reg = c.code[i].regA
      var d = i + c.code[i].jmpDiff
      for iters in countdown(maxIterations, 0):
        case c.code[d].opcode
        of opcJmp:
          d += c.code[d].jmpDiff
        of opcTJmp, opcFJmp:
          if c.code[d].regA != reg: break
          # tjmp x, 23
          # ...
          # tjmp x, 12
          # -- we know 'x' is true, and so can jump to 12+13:
          if c.code[d].opcode == opc:
            d += c.code[d].jmpDiff
          else:
            # tjmp x, 23
            # fjmp x, 22
            # We know 'x' is true so skip to the next instruction:
            d += 1
        else: break
      if d != i + c.code[i].jmpDiff:
        c.finalJumpTarget(i, d - i)
    of opcJmp, opcJmpBack:
      var d = i + c.code[i].jmpDiff
      var iters = maxIterations
      while c.code[d].opcode == opcJmp and iters > 0:
        d += c.code[d].jmpDiff
        dec iters
      if c.code[d].opcode == opcRet:
        # optimize 'jmp to ret' to 'ret' here
        c.code[i] = c.code[d]
      elif d != i + c.code[i].jmpDiff:
        c.finalJumpTarget(i, d - i)
    else: discard

proc genProcBody(c: var TCtx; s: PSym, body: PNode): int =
    var p = PProc(blocks: @[], sym: s)
    let oldPrc = c.prc
    c.prc = p
    # iterate over the parameters and allocate space for them:
    genParams(c.prc, s)

    # allocate additional space for any generically bound parameters
    if s.kind == skMacro and s.isGenericRoutineStrict:
      genGenericParams(c, s.ast[genericParamsPos])

    if s.typ.callConv == ccClosure:
      # reserve a slot for the hidden environment parameter
      c.prc.regInfo.add RegInfo(refCount: 1, kind: slotFixedLet)

    gen(c, body)

    # generate final 'return' statement:
    c.gABC(body, opcRet)

    result = c.prc.regInfo.len
    c.prc = oldPrc

proc genProc*(c: var TCtx; s: PSym, body: CgNode): VmGenResult =
  # thanks to the jmp we can add top level statements easily and also nest
  # procs easily:
  let
    start = c.code.len+1 # skip the jump instruction
    procStart = c.xjmp(body, opcJmp, 0)

  let regCount = tryOrReturn:
    c.genProcBody(s, body)

  c.patch(procStart)
  c.optimizeJumps(start)

  result = VmGenResult.ok:
    (start: start, regCount: regCount)

func vmGenDiagToAstDiagVmGenError*(diag: VmGenDiag): AstDiagVmGenError {.inline.} =
  let kind =
    case diag.kind
    of vmGenDiagBadExpandToAstArgRequired: adVmGenBadExpandToAstArgRequired
    of vmGenDiagBadExpandToAstCallExprRequired: adVmGenBadExpandToAstCallExprRequired
    of vmGenDiagTooManyRegistersRequired: adVmGenTooManyRegistersRequired
    of vmGenDiagCannotFindBreakTarget: adVmGenCannotFindBreakTarget
    of vmGenDiagNotUnused: adVmGenNotUnused
    of vmGenDiagNotAFieldSymbol: adVmGenNotAFieldSymbol
    of vmGenDiagCannotGenerateCode: adVmGenCannotGenerateCode
    of vmGenDiagCannotEvaluateAtComptime: adVmGenCannotEvaluateAtComptime
    of vmGenDiagInvalidObjectConstructor: adVmGenInvalidObjectConstructor
    of vmGenDiagMissingImportcCompleteStruct: adVmGenMissingImportcCompleteStruct
    of vmGenDiagCodeGenUnhandledMagic: adVmGenCodeGenUnhandledMagic
    of vmGenDiagCodeGenGenericInNonMacro: adVmGenCodeGenGenericInNonMacro
    of vmGenDiagCodeGenUnexpectedSym: adVmGenCodeGenUnexpectedSym
    of vmGenDiagCannotImportc: adVmGenCannotImportc
    of vmGenDiagTooLargeOffset: adVmGenTooLargeOffset
    of vmGenDiagNoClosureIterators: adVmGenNoClosureIterators
    of vmGenDiagCannotCallMethod: adVmGenCannotCallMethod
    of vmGenDiagCannotCast: adVmGenCannotCast

  {.cast(uncheckedAssign).}: # discriminants on both sides lead to saddness
    result =
      case diag.kind
      of vmGenDiagCodeGenGenericInNonMacro,
          vmGenDiagCodeGenUnexpectedSym,
          vmGenDiagCannotImportc,
          vmGenDiagTooLargeOffset,
          vmGenDiagNoClosureIterators,
          vmGenDiagCannotCallMethod:
        AstDiagVmGenError(
          kind: kind,
          sym: diag.sym)
      of vmGenDiagCannotCast:
        AstDiagVmGenError(
          kind: kind,
          formalType: diag.typeMismatch.formalType,
          actualType: diag.typeMismatch.actualType)
      of vmGenDiagMissingImportcCompleteStruct,
          vmGenDiagCodeGenUnhandledMagic:
        AstDiagVmGenError(
          kind: kind,
          magic: diag.magic)
      of vmGenDiagNotUnused,
          vmGenDiagNotAFieldSymbol,
          vmGenDiagCannotGenerateCode,
          vmGenDiagCannotEvaluateAtComptime,
          vmGenDiagInvalidObjectConstructor:
        AstDiagVmGenError(
          kind: kind,
          ast: diag.ast)
      of vmGenDiagBadExpandToAstArgRequired,
          vmGenDiagBadExpandToAstCallExprRequired,
          vmGenDiagTooManyRegistersRequired,
          vmGenDiagCannotFindBreakTarget:
        AstDiagVmGenError(kind: kind)
