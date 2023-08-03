#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the code generator for the VM.

# FIXME: the below comment doesn't reflect reality anymore; it needs to be
#        updated

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
    intsets,
    strutils,
    tables
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
  compiler/utils/[
    idioms
  ],
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

import std/options as std_options

from std/bitops import bitor

when defined(nimCompilerStacktraceHints):
  import std/stackframes
  import compiler/utils/debugutils


type
  VmGenResult* = Result[CodeInfo, VmGenDiag] ## Result of a vmgen invocation

  VmGenError = object of CatchableError
    diag: VmGenDiag

  Loc = object
    ## An encapsulation that associates the register storing the value with
    ## the register storing the handle of the location it was loaded from.
    ##
    ## `Loc` is used for convenient write-back handling. A write-back is
    ## necessary where an instruction operates on and modifies a register that
    ## stores a simple value (e.g. int, float), and the modification needs to
    ## be reflected at the memory location the value originated from
    handleReg: TDest ## the register holding the handle to the location
    val: TRegister   ## the register holding the loaded value

const
  IrrelevantTypes = abstractInst + {tyStatic} - {tyTypeDesc}
    ## the set of types that are not relevant to the VM. ``tyTypeDesc``, while
    ## not relevant right now, is likely going to be in the future.

  MagicsToKeep* = {mNone, mIsolate, mNHint, mNWarning, mNError, mMinI, mMaxI,
                   mAbsI, mDotDot}
    ## the set of magics that are kept as normal procedure calls and thus need
    ## an entry in the function table. For convenience, the ``mNone`` magic is
    ## also included

  noDest = TDest(-1)

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

# forward declarations
proc genLit(c: var TCtx; n: PNode; lit: int; dest: var TDest)
proc genTypeLit(c: var TCtx; t: PType; dest: var TDest)
proc genType(c: var TCtx, typ: PType): int
func fitsRegister(t: PType): bool
func local(prc: PProc, sym: PSym): TDest {.inline.}
proc genRegLoad(c: var TCtx, n: PNode, dest, src: TRegister)

template isUnset(x: TDest): bool = x < 0


proc routineSignature(s: PSym): PType {.inline.} =
  ## Returns the signature type of the routine `s`
  if s.kind == skMacro: s.internal
  else:                 s.typ

func underlyingLoc(n: PNode): PSym =
  ## Computes and returns the symbol of the complete location (i.e., a location
  ## not part of a compound location) that l-value expression `n` names. If no
  ## complete location is named, ``nil`` is returned.
  var root {.cursor.} = n
  # skip nodes that don't change the location until we arrive at either one
  # that does, or a symbol
  while root.kind in {nkConv, nkStmtListExpr}:
    root = root.lastSon

  result =
    if root.kind == nkSym: root.sym
    else:                  nil

func analyseIfAddressTaken(n: PNode, locs: var IntSet) =
  ## Recursively traverses the tree `n` and collects the symbols IDs of all
  ## complete locations of which the address is taken. Note that this analysis
  ## doesn't rely on the ``sfAddrTaken`` flag (because it's not reliable).
  # TODO: turn this into a MIR analysis. Doing so will simplify the code, make
  #       it less error-prone, and likely also faster
  case n.kind
  of nkHiddenAddr, nkAddr:
    # the nodes we're interested
    let loc = underlyingLoc(n[0])
    if loc != nil:
      # we're only interested in locals
      if sfGlobal notin loc.flags:
        locs.incl(loc.id)
    else:
      # the operand expression must still be anaylsed
      analyseIfAddressTaken(n[0], locs)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv, nkCast:
    analyseIfAddressTaken(n[1], locs)
  of nkVarSection, nkLetSection:
    for it in n.items:
      case it.kind
      of nkCommentStmt: discard
      of nkIdentDefs, nkVarTuple:
        # only analyse the initializer expression, the other parts are
        # declarative
        analyseIfAddressTaken(n.lastSon, locs)
      else:
        unreachable(it.kind)

  of nkEmpty, nkCommentStmt, nkTypeSection, nkConstSection, nkPragma,
     nkIncludeStmt, nkImportStmt, nkFromStmt, nkExportStmt, nkMixinStmt,
     nkBindStmt, nkLambdaKinds, routineDefs:
    discard "ignore declarative nodes"
  of nkLiterals, nkNimNodeLit, nkSym, nkNone, nkIdent, nkError:
    discard "ignore"
  elif n.kind in nkWithSons:
    for it in n.items:
      analyseIfAddressTaken(it, locs)


func lookupConst(c: TCtx, sym: PSym): int {.inline.} =
  c.symToIndexTbl[sym.id].int

func isNimNode(t: PType): bool =
  ## Returns whether `t` is the ``NimNode`` magic type
  let t = skipTypes(t, IrrelevantTypes)
  t.sym != nil and t.sym.magic == mPNimrodNode

func gABC*(ctx: var TCtx; n: PNode; opc: TOpcode; a, b, c: TRegister = 0) =
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

proc gABI(c: var TCtx; n: PNode; opc: TOpcode; a, b: TRegister; imm: BiggestInt) =
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

proc gABx*(c: var TCtx; n: PNode; opc: TOpcode; a: TRegister = 0; bx: int) =
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

proc xjmp(c: var TCtx; n: PNode; opc: TOpcode; a: TRegister = 0): TPosition =
  #assert opc in {opcJmp, opcFJmp, opcTJmp}
  result = TPosition(c.code.len)
  gABx(c, n, opc, a, 0)

func genLabel(c: TCtx): TPosition =
  result = TPosition(c.code.len)
  #c.jumpTargets.incl(c.code.len)

proc jmpBack(c: var TCtx, n: PNode, p = TPosition(0)) =
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

func getTemp(cc: var TCtx; kind: TSlotKind): TRegister {.inline.} =
  getFreeRegister(cc, kind, start = 0)

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

proc getFullTemp(c: var TCtx, n: PNode, t: PType): TRegister =
  ## Allocates a register for a value of type `t`, and, if the value doesn't
  ## fit into a register, emits the bytecode for allocating and setting up a
  ## temporary location.
  ##
  ## The difference compared to ``getTemp`` is that ``getFullTemp`` also
  ## allocates a location. While ``getFullTemp`` should ideally replace
  ## ``getTemp``, doing so is currently not possible because ``getTemp`` gets
  ## often passed an incorrect type
  let kind = getSlotKind(t.skipTypes(abstractInst + {tyStatic} - {tyTypeDesc}))
  result = getFreeRegister(c, kind, start = 0)

  if not fitsRegister(t):
    c.gABx(n, opcLdNull, result, c.genType(t))

func prepare(c: var TCtx, dest: var TDest, typ: PType) =
  ## Initializes `dest` to a temporary register if it's not already set. `typ`
  ## is passed to register allocation logic to improve allocation behaviour.
  ## Do note that this is only a hint, no invalid code will be generated if
  ## `typ` doesn't matches what's stored in the register.
  if dest.isUnset:
    dest = c.getTemp(typ)

func isLocView(t: PType): bool {.inline.} =
  ## Returns whether `t` is a a direct single-location view-type.
  classifyBackendView(t) == bvcSingle

func isDirectView(t: PType): bool {.inline.} =
  ## Returns whether `t` is a direct view-type.
  classifyBackendView(t) != bvcNone

proc prepare(c: var TCtx, dest: var TDest, n: PNode, typ: PType) =
  ## If `dest` is not already set or refers to an argument slot, allocates a
  ## location of `typ` and assigns the register holding the resulting handle or
  ## value to it. `n` only provides the line information to use for emitted
  ## instructions
  if dest.isUnset:
    dest = c.getFullTemp(n, typ)
  elif c.prc.regInfo[dest].kind == slotTempUnknown and not fitsRegister(typ):
    # the destination is a register slot used for argument passing, and the
    # value doesn't fit into a register -> setup a temporary location
    c.gABx(n, opcLdNull, dest, c.genType(typ))

template withTemp(tmp, n, typ, body: untyped) {.dirty.} =
  var tmp = getFullTemp(c, n, typ)
  body
  c.freeTemp(tmp)

proc popBlock(c: var TCtx; oldLen: int) =
  for f in c.prc.blocks[oldLen].fixups:
    c.patch(f)
  c.prc.blocks.setLen(oldLen)

template withBlock(labl: PSym; body: untyped) {.dirty.} =
  var oldLen {.gensym.} = c.prc.blocks.len
  c.prc.blocks.add TBlock(label: labl, fixups: @[])
  body
  popBlock(c, oldLen)

proc gen(c: var TCtx; n: PNode; dest: var TDest)
proc gen(c: var TCtx; n: PNode; dest: TRegister) =
  var d: TDest = dest
  gen(c, n, d)
  #internalAssert c.config, d == dest # issue #7407

proc gen(c: var TCtx; n: PNode) =
  var tmp: TDest = -1
  gen(c, n, tmp)
  if tmp >= 0:
    freeTemp(c, tmp)
  #if n.typ.isEmptyType: internalAssert tmp < 0

proc genx(c: var TCtx; n: PNode): TRegister =
  var tmp: TDest = -1
  gen(c, n, tmp)
  #internalAssert c.config, tmp >= 0 # 'nim check' does not like this internalAssert.
  if tmp >= 0:
    result = TRegister(tmp)

proc genLvalue(c: var TCtx, n: PNode, dest: var TDest)
proc genLvalue(c: var TCtx, n: PNode): TRegister {.inline.} =
  var dest = noDest
  genLvalue(c, n, dest)
  result = TRegister(dest)

proc clearDest(c: var TCtx; n: PNode; dest: var TDest) {.inline.} =
  # stmt is different from 'void' in meta programming contexts.
  # So we only set dest to -1 if 'void':
  if dest >= 0 and (n.typ.isNil or n.typ.kind == tyVoid):
    c.freeTemp(dest)
    dest = -1

proc isNotOpr(n: PNode): bool =
  n.kind in nkCallKinds and n[0].kind == nkSym and
    n[0].sym.magic == mNot

proc isTrue(n: PNode): bool =
  n.kind == nkSym and n.sym.kind == skEnumField and n.sym.position != 0 or
    n.kind == nkIntLit and n.intVal != 0

proc genWhile(c: var TCtx; n: PNode) =
  # lab1:
  #   body
  #   jmp lab1
  # lab2:
  let lab1 = c.genLabel
  withBlock(nil):
    assert isTrue(n[0])
    c.gen(n[1])
    c.jmpBack(n, lab1)

proc genBlock(c: var TCtx; n: PNode) =
  let oldRegisterCount = c.prc.regInfo.len
  withBlock(n[0].sym):
    c.gen(n[1])

  for i in oldRegisterCount..<c.prc.regInfo.len:
      when not defined(release):
        if c.prc.regInfo[i].inUse and c.prc.regInfo[i].kind in {slotTempUnknown,
                                  slotTempInt,
                                  slotTempFloat,
                                  slotTempStr,
                                  slotTempComplex,
                                  slotTempHandle}:
          doAssert false, "leaking temporary " & $i & " " & $c.prc.regInfo[i].kind
      c.prc.regInfo[i] = RegInfo(kind: slotEmpty)

proc genBreak(c: var TCtx; n: PNode) =
  let lab1 = c.xjmp(n, opcJmp)
  if n[0].kind == nkSym:
    #echo cast[int](n[0].sym)
    for i in countdown(c.prc.blocks.len-1, 0):
      if c.prc.blocks[i].label == n[0].sym:
        c.prc.blocks[i].fixups.add lab1
        return
    fail(n.info, vmGenDiagCannotFindBreakTarget)
  else:
    c.prc.blocks[c.prc.blocks.high].fixups.add lab1

proc genIf(c: var TCtx, n: PNode) =
  #  if (!expr1) goto lab1;
  #    thenPart
  #  lab1:
  assert n.len == 1
  block:
      let it = n[0]
      withTemp(tmp, it[0], it[0].typ):
        var elsePos: TPosition
        if isNotOpr(it[0]):
          c.gen(it[0][1], tmp)
          elsePos = c.xjmp(it[0][1], opcTJmp, tmp) # if true
        else:
          c.gen(it[0], tmp)
          elsePos = c.xjmp(it[0], opcFJmp, tmp) # if false

      c.gen(it[1]) # then part
      c.patch(elsePos)

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

proc toIntCnst(c: var TCtx, val: Int128): int =
  # integer constants are stored as their raw bit representation
  toIntCnst(c, BiggestInt(toInt64(val)))

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

template fillSliceList[T](sl: var seq[Slice[T]], nodes: openArray[PNode],
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

proc genBranchLit(c: var TCtx, n: PNode, t: PType): int =
  ## Turns the slice-list or single literal of the given `nkOfBranch` into
  ## a constant and returns it's index in `c.constant`.
  ##
  ## slice-lists are always added as a new constant while single literals
  ## are reused

  # XXX: slice-list constants (maybe `VmConstant`s in general) should be
  #      stored in a `BiTable` so that it can be easily detected if they
  #      already exist
  assert t.kind in IntegralTypes+{tyString}

  if n.len == 2 and n[0].kind in nkLiterals:
    # It's an 'of' branch with a single value
    result = c.genLiteral(n[0])
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


proc unused(c: TCtx; n: PNode; x: TDest) {.inline.} =
  if x >= 0:
    fail(n.info, vmGenDiagNotUnused, n)

proc genCase(c: var TCtx; n: PNode) =
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
  let selType = n[0].typ.skipTypes(abstractVarRange)
  var endings: seq[TPosition] = @[]
  withTemp(tmp, n[0], n[0].typ):
    c.gen(n[0], tmp)
    # branch tmp, codeIdx
    # fjmp   elseLabel

    # iterate of/else branches
    for i in 1..<n.len:
      let branch = n[i]
      case branch.kind
      of nkOfBranch:
        let b = genBranchLit(c, branch, selType)
        c.gABx(branch, opcBranch, tmp, b)
        let elsePos = c.xjmp(branch.lastSon, opcFJmp, tmp)
        c.gen(branch.lastSon)
        if i < n.len-1:
          endings.add(c.xjmp(branch.lastSon, opcJmp, 0))
        c.patch(elsePos)
      of nkElse:
        # else stmt:
        c.gen(branch[0])
      else:
        unreachable(branch.kind)
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
  ## Returns the stable index into `TCtx.rtti` where `typ`'s corresponding
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
  c.gen(n[0])
  # Add a jump past the exception handling code
  let jumpToFinally = c.xjmp(n, opcJmp, 0)
  # This signals where the body ends and where the exception handling begins
  c.patch(ehPos)
  for i in 1..<n.len:
    let it = n[i]
    if it.kind != nkFinally:
      # first opcExcept contains the end label of the 'except' block:
      let endExcept = c.xjmp(it, opcExcept, 0)
      for j in 0..<it.len - 1:
        assert(it[j].kind == nkType)
        let typ = it[j].typ.skipTypes(abstractPtrs-{tyTypeDesc})
        c.gABx(it, opcExcept, 0, c.genType(typ))
      if it.len == 1:
        # general except section:
        c.gABx(it, opcExcept, 0, 0)
      c.gen(it.lastSon)
      if i < n.len:
        endings.add(c.xjmp(it, opcJmp, 0))
      c.patch(endExcept)
  let fin = lastSon(n)
  # we always generate an 'opcFinally' as that pops the safepoint
  # from the stack if no exception is raised in the body.
  c.patch(jumpToFinally)
  c.gABx(fin, opcFinally, 0, 0)
  for endPos in endings: c.patch(endPos)
  if fin.kind == nkFinally:
    c.gen(fin[0])
  c.gABx(fin, opcFinallyEnd, 0, 0)

proc genRaise(c: var TCtx; n: PNode) =
  if n[0].kind != nkEmpty:
    let
      dest = c.genx(n[0])
      typ = skipTypes(n[0].typ, abstractPtrs)

    # get the exception name
    var name: TDest = c.getTemp(c.graph.getSysType(n.info, tyString))
    c.genLit(n[0], c.toStringCnst(typ.sym.name.s), name)

    # XXX: using an ABxI encoding would make sense here...
    c.gABI(n, opcRaise, dest, name, 0)
    c.freeTemp(name)
    c.freeTemp(dest)
  else:
    # reraise
    c.gABI(n, opcRaise, 0, 0, imm=1)

proc writeBackResult(c: var TCtx, info: PNode) =
  ## If the result value fits into a register but is not stored in one
  ## (because it has its address taken, etc.), emits the code for storing it
  ## back into a register. `info` is only used to provide line information.
  if not isEmptyType(c.prc.sym.routineSignature[0]):
    let
      res = c.prc.sym.ast[resultPos]
      typ = res.typ

    if fitsRegister(typ) and not isDirectView(typ) and
       res.sym.id in c.prc.addressTaken:
      # a write-back is required. Load the value into temporary register and
      # then do a register move
      let
        tmp = c.getTemp(typ)
        dest = local(c.prc, res.sym)
      c.genRegLoad(res, tmp, dest)
      c.gABC(info, opcFastAsgnComplex, dest, tmp)
      c.freeTemp(tmp)

proc genReturn(c: var TCtx; n: PNode) =
  writeBackResult(c, n)
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


proc genProcLit(c: var TCtx, n: PNode, s: PSym; dest: var TDest) =
  if dest.isUnset:
    dest = c.getTemp(s.typ)

  let idx = c.lookupProc(s).int

  c.gABx(n, opcLdNull, dest, c.genType(s.typ))
  c.gABx(n, opcWrProc, dest, idx)

proc genCall(c: var TCtx; n: PNode; dest: var TDest) =
  # it can happen that due to inlining we have a 'n' that should be
  # treated as a constant (see issue #537).
  #if n.typ != nil and n.typ.sym != nil and n.typ.sym.magic == mPNimrodNode:
  #  genLit(c, n, dest)
  #  return
  # bug #10901: do not produce code for wrong call expressions:
  if n.len == 0 or n[0].typ.isNil: return

  if not isEmptyType(n.typ):
    prepare(c, dest, n, n.typ)

  let
    fntyp = skipTypes(n[0].typ, abstractInst)
    x = c.getTempRange(n.len, slotTempUnknown)

  # the procedure to call:
  c.gen(n[0], x+0)

  # varargs need 'opcSetType' for the FFI support:
  for i in 1..<n.len:
    # skip empty arguments (i.e. arguments to compile-time parameters that
    # were omitted):
    if n[i].kind == nkEmpty:
      continue

    var r: TRegister = x+i
    if n[i].typ.skipTypes(abstractInst).kind == tyRef:
      # ``ref`` values are always stored in VM memory, meaning that we're
      # getting a handle to it here. Lots of code is written under the
      # assumption that a ``ref`` is always uses pass-by-value (instead of
      # pass-by-reference), so a copy is explicitly introduced
      # TODO: ``ref`` values should use ``rkAddress`` registers and then we
      #       no longer need the logic here. For the former to work, pseudo
      #       lifetime-hooks would need to be inserted for ``ref``s used in
      #       compile-time code, even when ``--gc:none`` is used
      let tmp = c.genx(n[i])
      c.gABC(n[i], opcAsgnComplex, r, tmp)
      c.freeTemp(tmp)
    else:
      c.gen(n[i], r)

    if i >= fntyp.len:
      internalAssert(c.config, tfVarargs in fntyp.flags)
      c.gABx(n, opcSetType, r, c.genType(n[i].typ))
  if dest.isUnset:
    c.gABC(n, opcIndCall, 0, x, n.len)
  else:
    c.gABC(n, opcIndCallAsgn, dest, x, n.len)
  c.freeTempRange(x, n.len)

template isGlobal(s: PSym): bool = sfGlobal in s.flags

func local(prc: PProc, sym: PSym): TDest {.inline.} =
  ## Returns the register associated with the local variable `sym` (or -1 if
  ## `sym` is not a local)
  prc.locals.getOrDefault(sym.id, -1)

proc genField(c: TCtx; n: PNode): TRegister =
  assert n.kind == nkSym and n.sym.kind == skField

  let s = n.sym
  if s.position > high(typeof(result)):
    fail(n.info, vmGenDiagTooLargeOffset, sym = s)

  result = s.position

proc genIndex(c: var TCtx; n: PNode; arr: PType): TRegister =
  if arr.skipTypes(abstractInst).kind == tyArray and (let x = firstOrd(c.config, arr);
      x != Zero):
    let tmp = c.genx(n)
    # freeing the temporary here means we can produce:  regA = regA - Imm
    c.freeTemp(tmp)
    result = c.getTemp(n.typ)
    c.gABI(n, opcSubImmInt, result, tmp, toInt(x))
  else:
    result = c.genx(n)

proc genRegLoad(c: var TCtx, n: PNode, dest, src: TRegister) =
  c.gABC(n, opcNodeToReg, dest, src)

  let t = n.typ.skipTypes(abstractInst)
  if t.isUnsigned() and t.size < sizeof(BiggestInt):
    c.gABC(n, opcNarrowU, dest, TRegister(t.size * 8))

proc genCheckedObjAccessAux(c: var TCtx; n: PNode): TRegister
proc genSym(c: var TCtx, n: PNode, dest: var TDest, load = true)

func usesRegister(p: PProc, s: PSym): bool =
  ## Returns whether the location identified by `s` is backed by a register
  ## (that is, whether the value is stored in a register directly)
  fitsRegister(s.typ) and s.id notin p.addressTaken

proc genNew(c: var TCtx; n: PNode) =
  let dest = c.genLvalue(n[1])
  c.gABx(n, opcNew, dest,
         c.genType(n[1].typ.skipTypes(abstractVar-{tyTypeDesc})))
  c.freeTemp(dest)

proc genNewSeq(c: var TCtx; n: PNode) =
  let t = n[1].typ.skipTypes(abstractVar)
  assert t.kind == tySequence
  let
    dest = c.genLvalue(n[1]) # ``seq`` argument
    len = c.genx(n[2])  # length argument
  c.gABx(n, opcNewSeq, dest, c.genType(t))
  c.gABx(n, opcNewSeq, len, 0)
  c.freeTemp(len)
  c.freeTemp(dest)

proc genNewSeqOfCap(c: var TCtx; n: PNode; dest: var TDest) =
  prepare(c, dest, n, n.typ)

  let tmp = c.getTemp(n[1].typ)
  c.gABx(n, opcLdImmInt, tmp, 0)
  c.gABx(n, opcNewSeq, dest, c.genType(n.typ))
  c.gABx(n, opcNewSeq, tmp, 0)
  c.freeTemp(tmp)

proc genUnaryABC(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  prepare(c, dest, n, n.typ)
  let tmp = c.genx(n[1])
  c.gABC(n, opc, dest, tmp)
  c.freeTemp(tmp)

proc genUnaryABI(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode; imm: BiggestInt=0) =
  prepare(c, dest, n, n.typ)
  let tmp = c.genx(n[1])
  c.gABI(n, opc, dest, tmp, imm)
  c.freeTemp(tmp)


proc genBinaryABC(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  prepare(c, dest, n, n.typ)
  let
    tmp = c.genx(n[1])
    tmp2 = c.genx(n[2])
  c.gABC(n, opc, dest, tmp, tmp2)
  c.freeTemp(tmp)
  c.freeTemp(tmp2)

proc genBinaryABCD(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  prepare(c, dest, n, n.typ)
  let
    tmp = c.genx(n[1])
    tmp2 = c.genx(n[2])
    tmp3 = c.genx(n[3])
  c.gABC(n, opc, dest, tmp, tmp2)
  c.gABC(n, opc, tmp3)
  c.freeTemp(tmp)
  c.freeTemp(tmp2)
  c.freeTemp(tmp3)

proc genNarrow(c: var TCtx; n: PNode; dest: TDest) =
  let t = skipTypes(n.typ, abstractVar-{tyTypeDesc})
  # uint is uint64 in the VM, we we only need to mask the result for
  # other unsigned types:
  if t.kind in {tyUInt8..tyUInt32} or (t.kind == tyUInt and t.size < 8):
    c.gABC(n, opcNarrowU, dest, TRegister(t.size*8))
  elif t.kind in {tyInt8..tyInt32} or (t.kind == tyInt and t.size < 8):
    c.gABC(n, opcNarrowS, dest, TRegister(t.size*8))

proc genNarrowU(c: var TCtx; n: PNode; dest: TDest) =
  let t = skipTypes(n.typ, abstractVar-{tyTypeDesc})
  # uint is uint64 in the VM, we we only need to mask the result for
  # other unsigned types:
  if t.kind in {tyUInt8..tyUInt32, tyInt8..tyInt32} or
    (t.kind in {tyUInt, tyInt} and t.size < 8):
    c.gABC(n, opcNarrowU, dest, TRegister(t.size*8))

proc genBinaryABCnarrow(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  genBinaryABC(c, n, dest, opc)
  genNarrow(c, n, dest)

proc genBinaryABCnarrowU(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  genBinaryABC(c, n, dest, opc)
  genNarrowU(c, n, dest)

proc genBinarySet(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  prepare(c, dest, n, n.typ)
  let
    tmp = c.genx(n[1])
    tmp2 = c.genx(n[2])
  c.gABC(n, opc, dest, tmp, tmp2)
  c.freeTemp(tmp)
  c.freeTemp(tmp2)

proc genBinaryStmt(c: var TCtx; n: PNode; opc: TOpcode) =
  let
    dest = c.genx(n[1])
    tmp = c.genx(n[2])
  c.gABC(n, opc, dest, tmp, 0)
  c.freeTemp(tmp)
  c.freeTemp(dest)

proc genBinaryStmtVar(c: var TCtx; n: PNode; opc: TOpcode) =
  let
    dest = c.genLvalue(n[1])
    tmp = c.genx(n[2])
  c.gABC(n, opc, dest, tmp, 0)
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

proc genVarargsABC(c: var TCtx; n: PNode; dest: TRegister; opc: TOpcode) =
  var x = c.getTempRange(n.len-1, slotTempUnknown)
  for i in 1..<n.len:
    var r: TRegister = x+i-1
    c.gen(n[i], r)
  c.gABC(n, opc, dest, x, n.len-1)
  c.freeTempRange(x, n.len-1)

proc isInt8Lit(n: PNode): bool =
  ## Returns whether `n` represents an integer value (signed or
  ## unsigned) that fits into the range of an 8-bit signed integer.
  if n.kind in nkIntKinds:
    let val = getInt(n)
    result = val >= low(int8) and val <= high(int8)

proc genAddSubInt(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  if n[2].isInt8Lit:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABI(n, succ(opc), dest, tmp, n[2].intVal)
    c.freeTemp(tmp)
  else:
    genBinaryABC(c, n, dest, opc)
  c.genNarrow(n, dest)

proc genNumberConv(c: var TCtx, info: PNode, dest, src: TRegister,
                   desttype, srctype: PType) =
  ## Generates and emits the code for an *unchecked* conversion between two
  ## numeric types.
  const
    Floats = {tyFloat..tyFloat64}
    Signed = {tyInt..tyInt64, tyEnum}
    Unsigned = {tyUInt..tyUInt64, tyChar, tyBool}

  template payload(op: NumericConvKind): uint16 =
    packedConvDesc(op, desttype.size.int, srctype.size.int)

  # things to keep in mind:
  # - registers storing integer values have no notion of signed vs. unsigned
  # - registers only store full-width integers and floats
  case desttype.kind
  of Signed:
    case srctype.kind
    of Floats:
      c.gABC(info, opcNumConv, dest, src, payload(nckFToI))
    of Signed, Unsigned:
      c.gABC(info, opcFastAsgnComplex, dest, src)
      if desttype.size < srctype.size:
        c.gABC(info, opcSignExtend, dest, TRegister(desttype.size * 8))
    else:
      unreachable()
  of Unsigned - {tyBool}:
    case srctype.kind
    of Floats:
      c.gABC(info, opcNumConv, dest, src, payload(nckFToU))
    of Unsigned:
      c.gABC(info, opcFastAsgnComplex, dest, src)
      if desttype.size < srctype.size:
        # truncate to the new size:
        c.gABC(info, opcNarrowU, dest, TRegister(desttype.size * 8))
    of Signed:
      # similar to the unsigned-to-unsigned case, but a narrow is also required
      # to "drop" the bits not part of the destination's bit range
      # XXX: this behaviour matches that of the C target, but truncating to the
      #      *source size* would make more sense
      c.gABC(info, opcFastAsgnComplex, dest, src)
      if desttype.size < 8:
        # drop the bits past the destination's size
        c.gABC(info, opcNarrowU, dest, TRegister(desttype.size * 8))
    else:
      unreachable()
  of Floats:
    let op =
      case srctype.kind
      of Floats:   nckFToF
      of Signed:   nckIToF
      of Unsigned: nckUToF
      else:        unreachable()

    c.gABC(info, opcNumConv, dest, src, payload(op))
  of tyBool:
    c.gABC(info, opcNumConv, dest, src, payload(nckToB))
  else:
    unreachable()

proc genConv(c: var TCtx; n, arg: PNode; dest: var TDest) =
  let
    a = skipTypes(n.typ, IrrelevantTypes + {tyRange})
    b = skipTypes(arg.typ, IrrelevantTypes + {tyRange})
  # we're not interested in range types here -- range checks are already
  # handled via ``opcRangeChck``

  if sameLocationType(a, b) or {a.kind, b.kind} == {tyProc} or
     a.kind == tyPointer:
    # don't do anything for conversions that don't change the run-time type
    # and lambda-lifting conversions
    gen(c, arg, dest)
  else:
    # a normal conversion that produces a new value of different type
    prepare(c, dest, n, n.typ)
    let tmp = c.genx(arg)
    case a.kind
    of IntegralTypes:
      # numeric type conversions don't use ``opcConv````
      genNumberConv(c, n, dest, tmp, a, b)
    of ConcreteTypes - IntegralTypes:
      c.gABx(n, opcConv, dest, c.genTypeInfo(a))
      c.gABx(n, opcConv, tmp, c.genTypeInfo(b))
    else:
      unreachable()

    c.freeTemp(tmp)

proc genToStr(c: var TCtx, n, arg: PNode, dest: var TDest) =
  # TODO: don't use ``opcConv`` for to-string conversions
  prepare(c, dest, n, n.typ)
  let tmp = c.genx(arg)
  c.gABx(n, opcConv, dest, c.genTypeInfo(n.typ.skipTypes(IrrelevantTypes)))
  c.gABx(n, opcConv, tmp, c.genTypeInfo(arg.typ.skipTypes(IrrelevantTypes)))
  c.freeTemp(tmp)

proc genObjConv(c: var TCtx, n: PNode, dest: var TDest) =
  prepare(c, dest, n.typ)
  let
    tmp = genx(c, n[0])
    desttyp = n.typ.skipTypes(IrrelevantTypes + {tyVar, tyLent})
  # XXX: var and lent in conversions should not end up here -- fix-up
  #      the conversions in ``mirgen``

  case desttyp.kind
  of tyRef, tyObject:
    c.gABC(n, opcObjConv, dest, tmp)
    c.gABx(n, opcObjConv, 0, c.genType(desttyp))
    c.makeHandleReg(dest, tmp)
  of tyPtr:
    c.gABC(n, opcFastAsgnComplex, dest, tmp) # register copy
    c.gABx(n, opcSetType, dest, c.genType(desttyp)) # set the new type
  else:
    unreachable()

  c.freeTemp(tmp)

proc genCard(c: var TCtx; n: PNode; dest: var TDest) =
  let tmp = c.genx(n[1])
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABC(n, opcCard, dest, tmp)
  c.freeTemp(tmp)

func fitsRegisterConsiderView(t: PType): bool =
  ## Returns whether a value of type `t` fits into a register, also
  ## considering view-types that map to pointers.
  # XXX: introduce a ``mapType`` (similar to the one used by the other code
  #      generators) and base the "fits register" queries on that
  let t = t.skipTypes(IrrelevantTypes)
  fitsRegister(t) or
    # is it a direct single-location view?:
    t.kind in {tyVar, tyLent} and t.base.kind != tyOpenArray

template needsRegLoad(): untyped =
  mixin load
  load and fitsRegisterConsiderView(n.typ)

proc genCast(c: var TCtx, n, arg: PNode, dest: var TDest) =
  let
    a = skipTypes(n.typ, IrrelevantTypes + {tyRange})
    b = skipTypes(arg.typ, IrrelevantTypes + {tyRange})

  if sameLocationType(a, b):
    # treat the cast as a no-op if there's no change in run-time type
    gen(c, arg, dest)
  else:
    # not supported yet
    raiseVmGenError:
      VmGenDiag(
        kind: vmGenDiagCannotCast,
        location: n.info,
        instLoc: instLoc(-1),
        typeMismatch: VmTypeMismatch(actualType: arg.typ, formalType: n.typ))

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

proc genDataToAst(c: var TCtx, n: PNode, dest: TRegister) =
  ## Generates and emits the bytecode for evaluating the expression `n` and
  ## deserializing the result to ``NimNode`` AST
  let tmp = c.genx(n)
  var typLit = TDest(-1)
  c.genTypeLit(n.typ, typLit)
  c.gABC(n, opcDataToAst, dest, tmp, typLit)
  c.freeTemp(typLit)
  c.freeTemp(tmp)

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

proc genVoidBC(c: var TCtx, n: PNode, dest: TDest, opcode: TOpcode) =
  ## Special convention used by some macrocache-related ops
  unused(c, n, dest)
  var
    tmp1 = c.genx(n[1])
    tmp2 = c.genx(n[2])
  c.gABC(n, opcode, 0, tmp1, tmp2)
  c.freeTemp(tmp1)
  c.freeTemp(tmp2)

proc loadInt(c: var TCtx, n: PNode, dest: TRegister, val: Int128) =
  ## Loads the integer `val` into `dest`, choosing the most efficient way to
  ## do so.
  if val in regBxMin-1..regBxMax:
    # can be loaded as an immediate
    c.gABx(n, opcLdImmInt, dest, toInt(val))
  else:
    # requires a constant
    c.gABx(n, opcLdConst, dest, c.toIntCnst(val))

proc genSetElem(c: var TCtx, n: PNode, first: Int128): TRegister =
  result = c.getTemp(n.typ)

  if first != 0:
    if n.kind in nkIntKinds:
      # a literal value. Since sem makes sure sets cannot store elements
      # with an adjusted value of >= 2^16, we know that the result of the
      # subtraction fits into the encodable range for ABX
      c.gABx(n, opcLdImmInt, result, toInt(getInt(n) - first))
    else:
      gen(c, n, result)
      if first notin -127..127:
        # too large for the ABI encoding; we need to load a constant
        let
          typ = skipTypes(n.typ, IrrelevantTypes + {tyRange})
          tmp = c.getTemp(typ)
          opc =
            if isUnsigned(typ): opcSubu
            else:               opcSubInt
        c.loadInt(n, tmp, first)
        c.gABC(n, opc, result, result, tmp)
        c.freeTemp(tmp)
      elif first > 0:
        c.gABI(n, opcSubImmInt, result, result, toInt(first))
      else:
        c.gABI(n, opcAddImmInt, result, result, toInt(-first))

  else:
    gen(c, n, result)

proc genSetElem(c: var TCtx, n: PNode, typ: PType): TRegister {.inline.} =
  ## `typ` is the type to derive the lower bound from
  let t = typ.skipTypes(abstractInst)
  assert t.kind == tySet

  # `first` can't be reliably derived from `n.typ` since the type may not
  # match the set element type. This happens with the set in a
  # `nkCheckedFieldExpr` for example
  let first = c.config.firstOrd(t)
  genSetElem(c, n, first)

func fitsRegister(t: PType): bool =
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

func usesRegister(p: PProc, n: PNode): bool =
  ## Analyses and returns whether the value of the location named by l-value
  ## expression `n` is stored in a register instead of a memory location
  # XXX: instead of using a separate analysis, compute and return this as part
  #      of ``genLValue`` and
  case n.kind
  of nkSym:
    let s = n.sym
    not s.isGlobal and usesRegister(p, s)
  of nkDerefExpr, nkHiddenDeref, nkDotExpr, nkBracketExpr, nkCheckedFieldExpr,
     nkConv, nkObjDownConv, nkObjUpConv:
    false
  of nkStmtListExpr:
    usesRegister(p, n.lastSon)
  else:
    unreachable(n.kind)

proc genNoLoad(c: var TCtx, n: PNode): tuple[reg: TRegister, isDirect: bool] =
  ## Similar to ``genLValue``, but also returns whether the register storing
  ## the result stores a handle or a value.
  var dest = noDest
  genLvalue(c, n, dest)
  result = (TRegister(dest), usesRegister(c.prc, n))

proc genLoc(c: var TCtx, n: PNode): Loc =
  ## Generates and emits the code for evaluating the l-value expression `n`.
  ## The returned ``Loc`` holds the register information.
  assert fitsRegister(n.typ), "`genLoc` is not needed"

  let (reg, isDirect) = genNoLoad(c, n)
  if isDirect:
    # the location is backed by a register. No write-back needs to be performed
    # when the modification is done
    result.handleReg = -1
    result.val = reg
  else:
    # the location is backed by VM memory. Load its value and remember both
    # registers
    result.handleReg = reg
    result.val = c.getTemp(n.typ)
    genRegLoad(c, n, result.val, result.handleReg)

proc finish(c: var TCtx, info: PNode, loc: sink Loc) =
  ## Wraps up the modification to `loc` by writing the register-stored
  ## value back to the source memory location.
  if loc.handleReg != -1:
    # a write-back is required
    c.gABC(info, opcWrLoc, loc.handleReg, loc.val)
    c.freeTemp(loc.handleReg)

  c.freeTemp(loc.val)

proc genMagic(c: var TCtx; n: PNode; dest: var TDest; m: TMagic) =
  case m
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
    let loc = genLoc(c, n[1])

    if n[2].isInt8Lit and not isUnsigned:
      c.gABI(n, succ(opc), loc.val, loc.val, n[2].intVal)
    else:
      let tmp = c.genx(n[2])
      c.gABC(n, opc, loc.val, loc.val, tmp)
      c.freeTemp(tmp)
    c.genNarrow(n[1], loc.val)

    # write back:
    finish(c, n, loc)
  of mOrd, mChr: c.gen(n[1], dest)
  of mArrToSeq:
    prepare(c, dest, n, n.typ)

    let temp = c.genx(n[1])
    let L = c.getTemp(c.graph.getSysType(n.info, tyInt))
    c.gABC(n, opcLenSeq, L, temp)

    c.gABC(n, opcSetLenSeq, dest, L)
    c.gABC(n, opcArrCopy, dest, temp, L)
    c.freeTemp(temp)
    c.freeTemp(L)
  of mIsolate:
    genCall(c, n, dest)
  of mNew:
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
    prepare(c, dest, n, n.typ)
    c.freeTemp(c.genx(n[1]))
    var tmp = c.getTemp(n[1].typ)
    c.gABx(n, opcLdImmInt, tmp, 0)
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
      d = c.genLvalue(n[1])
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
    genToStr(c, n, n[1], dest)
  of mEqStr, mEqCString: genBinaryABC(c, n, dest, opcEqStr)
  of mLeStr: genBinaryABC(c, n, dest, opcLeStr)
  of mLtStr: genBinaryABC(c, n, dest, opcLtStr)
  of mEqSet: genBinarySet(c, n, dest, opcEqSet)
  of mLeSet: genBinarySet(c, n, dest, opcLeSet)
  of mLtSet: genBinarySet(c, n, dest, opcLtSet)
  of mMulSet: genBinarySet(c, n, dest, opcMulSet)
  of mPlusSet: genBinarySet(c, n, dest, opcPlusSet)
  of mMinusSet: genBinarySet(c, n, dest, opcMinusSet)
  of mConStrStr:
    # we need to assign to a temporary first, as the operands might alias
    # with the destination
    # TODO: the "use temporary if result aliases with arguments" transformation
    #       needs to happen as a MIR pass instead
    let tmp = c.getFullTemp(n, n.typ)
    genVarargsABC(c, n, tmp, opcConcatStr)
    if dest.isUnset:
      dest = tmp
    else:
      assert c.prc.regInfo[dest].kind == slotTempUnknown
      # XXX: this is shaky, and depends on the destination only being already
      #      set in argument contexts
      c.gABC(n, opcAsgnComplex, dest, tmp)
      c.freeTemp(tmp)
  of mInSet:
    let
      tmp = c.genx(n[1])
      tmp2 = c.genSetElem(n[2], n[1].typ)
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABC(n, opcContainsSet, dest, tmp, tmp2)
    c.freeTemp(tmp)
    c.freeTemp(tmp2)
  of mRepr:
    prepare(c, dest, n, n.typ)
    let tmp = c.genx(n[1])
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
    var d = c.genLvalue(n[1])
    var tmp = c.genx(n[2])
    c.gABC(n, if m == mSetLengthStr: opcSetLenStr else: opcSetLenSeq, d, tmp)
    c.freeTemp(tmp)
    c.freeTemp(d)
  of mSwap:
    unused(c, n, dest)
    let tmp = getTemp(c, n.typ)
    # XXX: swap doesn't need to be implemented here; lower it into assignment
    #      with a MIR pass
    # there's no ``nkHiddenAddr`` on the operands, so we don't need to skip
    # var types
    if fitsRegister(n[1].typ):
      # we need to account for the fact that either operand could be
      # stored in a register already (because it's a local variable)
      let
        a = c.genLoc(n[1])
        b = c.genLoc(n[2])
      # register copies are enough, here
      c.gABC(n, opcFastAsgnComplex, tmp, a.val)
      c.gABC(n, opcFastAsgnComplex, a.val, b.val)
      c.gABC(n, opcFastAsgnComplex, b.val, tmp)
      # write back (if necessary):
      finish(c, n, b)
      finish(c, n, a)
    else:
      let
        a = c.genLvalue(n[1])
        b = c.genLvalue(n[2])
      # XXX: this currently creates a full copy; the VM is still missing the
      #      ability to create shallow copies
      c.gABC(n, opcAsgnComplex, tmp, a)
      c.gABC(n, opcWrLoc, a, b)
      c.gABC(n, opcWrLoc, b, tmp)
      c.freeTemp(b)
      c.freeTemp(a)

    c.freeTemp(tmp)
  of mIsNil: genUnaryABC(c, n, dest, opcIsNil)
  of mParseBiggestFloat:
    if dest.isUnset: dest = c.getTemp(n.typ)
    var
      tmp1 = c.genx(n[1])
      tmp2 = c.genx(n[2])
      tmp3 = c.genx(n[3])
    c.gABC(n, opcParseFloat, dest, tmp1, tmp2)
    c.gABC(n, opcParseFloat, tmp3)
    c.freeTemp(tmp1)
    c.freeTemp(tmp2)
    c.freeTemp(tmp3)
  of mReset, mWasMoved:
    unused(c, n, dest)
    let
      (dest, isDirect) = genNoLoad(c, n[1])
      typ = n[1].typ.skipTypes({tyVar, tyLent})

    if isDirect:
      # the location uses a register -> load it with the empty value
      c.gABx(n, opcLdNullReg, dest, c.genType(typ))
    else:
      let tmp = c.getTemp(typ)
      if fitsRegister(typ):
        # optimization: the location isn't backed by a register, but its value
        # fits in one. Don't unnecessarily allocate a temporary memory location
        c.gABx(n, opcLdNullReg, tmp, c.genType(typ))
      else:
        # FIXME: this is a very inefficient way of implementing ``reset``. We're
        #        allocating a temporary location for just its zero
        #        representation. A dedicated instruction is probably needed
        c.gABx(n, opcLdNull, tmp, c.genType(typ))

      c.gABC(n, opcWrLoc, dest, tmp)
      c.freeTemp(tmp)

    c.freeTemp(dest)
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
    prepare(c, dest, n, n.typ)
    let tmp = c.genx(n[1])
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
  of mNccInc: genVoidBC(c, n, dest, opcNccInc)
  of mNcsAdd: genVoidBC(c, n, dest, opcNcsAdd)
  of mNcsIncl: genVoidBC(c, n, dest, opcNcsIncl)
  of mNcsLen: genUnaryABC(c, n, dest, opcNcsLen)
  of mNcsAt: genBinaryABC(c, n, dest, opcNcsAt)
  of mNctPut: genVoidABC(c, n, dest, opcNctPut)
  of mNctLen: genUnaryABC(c, n, dest, opcNctLen)
  of mNctGet: genBinaryABC(c, n, dest, opcNctGet)
  of mNctHasNext: genBinaryABC(c, n, dest, opcNctHasNext)
  of mNctNext: genBinaryABC(c, n, dest, opcNctNext)
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
    # only transformed ``getAst`` calls that expand templates reach here
    prepare(c, dest, n.typ)

    let
      call = n
      numArgs = call.len - 1
      x = c.getTempRange(numArgs, slotTempUnknown)

    # pass the template symbol as the first argument
    var callee = TDest(x)
    c.genLit(call[1], c.toNodeCnst(call[1]), callee)

    # the arguments to the template are used as arguments to the
    # `ExpandToAst` operation
    for i in 1..<numArgs:
      let it = call[i + 1]
      var d = TDest(x+i)
      # small optimization: don't use ``DataToAst` if the argument is
      # already a NimNode
      if it.typ.isNimNode():
        c.gen(it, d)
      else:
        # evaluate the argument and deserialize the result to ``NimNode``
        # AST
        c.genDataToAst(it, d)

    c.gABC(n, opcExpandToAst, dest, x, numArgs)
    c.freeTempRange(x, numArgs)
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
  of mFinished:
    # XXX: the implementation is a hack -- it makes a lot of implicit
    #      assumptions and is thus very brittle. However, don't attempt to
    #      fix it here; implement the lowering of the ``mFinished`` magic as a
    #      MIR pass that is used for all backends
    prepare(c, dest, n.typ)
    let
      tmp = c.genx(n[1]) # the operand
      env = c.getTemp(n[1].typ) # XXX: wrong type
      state = c.getTemp(n.typ)  # XXX: also wrong
      imm = c.getTemp(n.typ)    # XXX: this one too

    c.gABC(n, opcAccessEnv, env, tmp)

    # load the state value into a register. The :state field is always
    # located at position 0
    c.gABC(n, opcLdObj, state, env, 0)
    c.gABC(n, opcNodeToReg, state, state)

    c.gABx(n, opcLdImmInt, imm, 0)        # load 0
    c.gABC(n, opcLtInt, dest, state, imm) # compare

    c.freeTemp(imm)
    c.freeTemp(state)
    c.freeTemp(env)
    c.freeTemp(tmp)
  else:
    # mGCref, mGCunref, mFinished, etc.
    fail(n.info, vmGenDiagCodeGenUnhandledMagic, m)

proc genDeref(c: var TCtx, n: PNode, dest: var TDest; load = true) =
    let tmp = c.genx(n[0])
    if dest.isUnset: dest = c.getTemp(n.typ)
    gABC(c, n, opcLdDeref, dest, tmp)

    if needsRegLoad():
      c.genRegLoad(n, dest, dest)
    c.freeTemp(tmp)

func setSlot(c: var TCtx; v: PSym): TRegister {.discardable.} =
  # XXX generate type initialization here?
  result = getFreeRegister(c, if v.kind == skLet: slotFixedLet else: slotFixedVar, start = 1)
  c.prc.locals[v.id] = result

func cannotEval(c: TCtx; n: PNode) {.noinline, noreturn.} =
  raiseVmGenError(vmGenDiagCannotEvaluateAtComptime, n)

proc importcCondVar*(s: PSym): bool {.inline.} =
  # see also importcCond
  if sfImportc in s.flags:
    return s.kind in {skVar, skLet, skConst}

proc checkCanEval(c: TCtx; n: PNode) =
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


proc genDiscrVal(c: var TCtx, discr, n: PNode, oty: PType): TRegister =
  ## Generate the code for preparing and loading the discriminator value
  ## as expected by the execution engine

  let oty = oty.skipTypes(abstractPtrs)
  assert oty.kind == tyObject

  let discrTyp = block:
    let (o, idx) =
      getFieldAndOwner(
        c.getOrCreate(oty),
        fpos(discr.sym.position))
    o.fieldAt(idx).typ

  let recCase = findRecCase(oty, discr.sym)
  assert recCase != nil

  if n.kind in nkCharLit..nkUInt64Lit:
    # Discriminator value is known at compile-time

    let b = findMatchingBranch(recCase, n)
    assert b != -1 # no matching branch; should have been caught already

    assert n.intVal <= (1 shl discrTyp.numBits) - 1
    let v = bitor(b shl discrTyp.numBits, int(n.intVal))

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


proc genAsgnSource(c: var TCtx, n: PNode, wantsPtr: bool): TRegister

proc genFieldAsgn(c: var TCtx, obj: TRegister; le, ri: PNode) =
  c.config.internalAssert(le.kind == nkDotExpr)

  let idx = c.genField(le[1])
  let s = le[1].sym

  var tmp: TRegister

  if sfDiscriminant notin s.flags:
    tmp = c.genAsgnSource(ri, wantsPtr = true)
    c.gABC(le, opcWrObj, obj, idx, tmp)
  else:
    # Can't use `s.owner.typ` since it may be a `tyGenericBody`
    tmp = c.genDiscrVal(le[1], ri, le[0].typ)
    c.gABC(le, opcSetDisc, obj, idx, tmp)


  c.freeTemp(tmp)

func isPtrView(n: PNode): bool =
  ## Analyses whether the expression `n` evaluates to a direct view that is
  ## represented via an address value instead of a handle. The former is the
  ## case for both globals that are direct views, and for direct views stored
  ## in compound types
  case n.kind
  of nkSym:
    sfGlobal in n.sym.flags
  of nkDotExpr, nkBracketExpr, nkCheckedFieldExpr:
    true
  of nkHiddenAddr, nkCallKinds:
    false
  of nkStmtListExpr:
    isPtrView(n.lastSon)
  else:
    unreachable(n.kind)

proc genAsgnSource(c: var TCtx, n: PNode, wantsPtr: bool): TRegister =
  ## Generates and emits the code for evaluating the expression `n`, which
  ## is the source operand to an assignment.
  ##
  ## Because there are two types of views (handle vs. address), special
  ## handling is required when the source operand is a view. ``wantsPtr``
  ## indicates which one the consumer expects.
  result = c.genx(n)
  if isLocView(n.typ):
    let isPtr = isPtrView(n)
    # if necessary, convert the view to the representation the destination
    # expects:
    if not wantsPtr and isPtr:
      # produce a handle by dereferencing the pointer
      # note: we can reuse the register because we know it's a temporary
      # one
      assert c.isTemp(result)
      c.gABC(n, opcLdDeref, result, result)
    elif wantsPtr and not isPtr:
      # turn the handle into an address. The register can't be reused
      # because it might be non-temporary one
      let tmp = result
      result = c.getTemp(n.typ)
      c.gABC(n, opcAddr, result, tmp)
      c.freeTemp(tmp)

proc genSymAsgn(c: var TCtx, le, ri: PNode) =
  ## Generates and emits the code for an assignment where the RHS is a symbol
  ## node.
  let s = le.sym
  var dest = noDest
  # we're only interested in the *handle* (i.e. identity) of the location, so
  # don't load it
  c.genSym(le, dest, load=false)

  if sfGlobal in s.flags:
    # global views use pointers internally
    let b = genAsgnSource(c, ri, wantsPtr=true)
    c.gABC(le, opcWrLoc, dest, b)
    c.freeTemp(b)
  else:
    if usesRegister(c.prc, s):
      # if the location is backed by a register (i.e., is not in stored
      # in a memory cell), we don't use a temporary register + assignment
      # but directly write to the destination register
      # XXX: we can't. If the right-hand side is a call and it raises an
      #      exception, the way the VM currently implements ``IndCallAsgn``
      #      would result in the local's register becoming uninitialized. In
      #      other words, we have to also use a temporary here, at least until
      #      the VM no longer clears out the destination register
      #gen(c, ri, local(c.prc, s))
      let b = c.genx(ri)
      c.gABC(le, whichAsgnOpc(le), dest, b)
      c.freeTemp(b)
    else:
      # an assignment is required to the local. Views are always stored as
      # handles in this case, so a register move is used for assigning them
      let
        opc = (if isDirectView(s.typ): opcFastAsgnComplex else: opcWrLoc)
        b = c.genx(ri)
      c.gABC(le, opc, dest, b)
      c.freeTemp(b)

  c.freeTemp(dest)

proc genDerefView(c: var TCtx, n: PNode, dest: var TDest; load = true) =
  ## Generates and emits the code for a view dereference, where `n` is the
  ## expression that evaluates to a view. `load` indicates whether the
  ## *handle* of the underlying location or the value stored in it should be
  ## put into `dest`.
  let
    isPtr = isPtrView(n)
    needsLoad = load and fitsRegister(n.typ.skipTypes(abstractVar))

  if isPtr or needsLoad:
    # we need to process the operand further, and thus need a temporary
    prepare(c, dest, n.typ) # XXX: the passed type is incorrect
    let tmp = c.genx(n)
    var src = tmp

    if isPtr:
      # the operand is view stored as a pointer; turn it into a handle first
      # by dereferencing it
      c.gABC(n, opcLdDeref, dest, tmp)
      src = dest # the handle is now in `dest`

    if needsLoad:
      # load the value into `dest`
      c.genRegLoad(n, dest, src)

    c.freeTemp(tmp)
  else:
    # no processing required; load the handle directly into `dest`
    c.gen(n, dest)

proc genAsgn(c: var TCtx; le, ri: PNode; requiresCopy: bool) =
  case le.kind
  of nkBracketExpr:
    let typ = le[0].typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind
    let dest = c.genx(le[0])
    let idx = if typ != tyTuple: c.genIndex(le[1], le[0].typ) else: le[1].intVal
    let tmp = c.genAsgnSource(ri, wantsPtr = true)
    let opc = if typ in {tyString, tyCstring}: opcWrStrIdx
              elif typ == tyTuple: opcWrObj
              else: opcWrArr

    c.gABC(le, opc, dest, idx, tmp)
    c.freeTemp(tmp)
    if typ != tyTuple:
      c.freeTemp(idx)
    c.freeTemp(dest)
  of nkCheckedFieldExpr:
    let objR = genCheckedObjAccessAux(c, le)
    c.genFieldAsgn(objR, le[0], ri)
    # c.freeTemp(idx) # BUGFIX, see nkDotExpr
    c.freeTemp(objR)
  of nkDotExpr:
    let dest = c.genx(le[0])
    c.genFieldAsgn(dest, le, ri)
    # c.freeTemp(idx) # BUGFIX: idx is an immediate (field position), not a register
    c.freeTemp(dest)
  of nkHiddenDeref:
    # an assignment to a view's underlying location. The source cannot be a
    # view, so using ``genAsgnSource`` is unnecessary
    var dest = noDest
    genDerefView(c, le[0], dest, load=false) # we need a handle, hence ``false``
    let tmp = c.genx(ri)

    c.gABC(le, opcWrLoc, dest, tmp)
    c.freeTemp(tmp)
    c.freeTemp(dest)
  of nkDerefExpr:
    # same as for ``nkHiddenDeref``, the source cannot be a view
    let
      dest = c.genx(le[0])
      tmp = c.genx(ri)
    c.gABC(le, opcWrDeref, dest, 0, tmp)
    c.freeTemp(dest)
    c.freeTemp(tmp)
  of nkObjDownConv, nkObjUpConv:
    # assignment to an lvalue-converted object, ref, or ptr
    case le.typ.skipTypes(IrrelevantTypes).kind
    of tyPtr:
      # not supported yet. ``vmgen`` first needs some architectural changes
      cannotEval(c, le)
    of tyRef, tyObject:
      var dest = TDest(-1)
      genObjConv(c, le, dest)
      let tmp = c.genx(ri)

      c.gABC(le, opcWrLoc, dest, tmp)
      c.freeTemp(tmp)
      c.freeTemp(dest)
    else:
      unreachable()
  of nkConv, nkHiddenStdConv:
    # these conversions don't result in a lvalue of different run-time type, so
    # they're skipped
    genAsgn(c, le[0], ri, requiresCopy)
  of nkSym:
    checkCanEval(c, le)
    genSymAsgn(c, le, ri)
  else:
    unreachable(le.kind)

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
      # a global that is not accessible in the current context
      cannotEval(c, n)

proc genSym(c: var TCtx; n: PNode; dest: var TDest; load = true) =
  ## Generates and emits the code for loading either the value or handle of
  ## the location named by symbol node `n` into the `dest` register.
  let s = n.sym
  if s.isGlobal:
    let pos = useGlobal(c, n)
    if dest.isUnset:
      dest = c.getTemp(s.typ)

    if load and (isLocView(s.typ) or fitsRegister(s.typ)):
      let cc = c.getTemp(n.typ)
      c.gABx(n, opcLdGlobal, cc, pos)
      c.genRegLoad(n, dest, cc)
      c.freeTemp(cc)
    else:
      c.gABx(n, opcLdGlobal, dest, pos)
  else:
      let local = local(c, n)
      internalAssert(c.config, c.prc.regInfo[local].kind < slotSomeTemp)
      if usesRegister(c.prc, s) or not load or not fitsRegister(s.typ):
        if dest.isUnset:
          dest = local
        else:
          # despite the name, ``opcFastAsgnComplex`` currently performs a
          # register copy, which is exactly what we need here
          c.gABC(n, opcFastAsgnComplex, dest, local)
      else:
        prepare(c, dest, s.typ)
        c.genRegLoad(n, dest, local)

proc genSymAddr(c: var TCtx, n: PNode, dest: var TDest) =
  ## Generates and emits the code for taking the address of the location
  ## identified by the symbol node `n`.
  let s = n.sym
  if dest.isUnset:
    dest = c.getTemp(s.typ)

  if s.isGlobal:
    let
      pos = useGlobal(c, n)
      tmp = c.getTemp(slotTempComplex)
    c.gABx(n, opcLdGlobal, tmp, pos)
    c.gABC(n, opcAddr, dest, tmp)
    c.freeTemp(tmp)
  else:
    let local = local(c, n)
    c.gABC(n, opcAddr, dest, local)

proc genArrAccessOpcode(c: var TCtx; n: PNode; dest: var TDest; opc: TOpcode; load = true) =
  let
    a = c.genx(n[0])
    b = c.genIndex(n[1], n[0].typ)

  prepare(c, dest, n.typ)
  if opc in {opcLdArrAddr, opcLdStrIdx, opcLdStrIdxAddr}:
    # the result is already stored in a register; no special handling
    # required
    c.gABC(n, opc, dest, a, b)
  elif needsRegLoad():
    var cc = c.getTemp(n.typ)
    c.gABC(n, opc, cc, a, b)
    c.genRegLoad(n, dest, cc)
    c.freeTemp(cc)
  else:
    c.gABC(n, opc, dest, a, b)
    c.makeHandleReg(dest, a)
  c.freeTemp(a)
  c.freeTemp(b)

proc genFieldAccessAux(c: var TCtx; n: PNode; a, b: TRegister, dest: var TDest; load = true) =
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

proc genFieldAccess(c: var TCtx; n: PNode; dest: var TDest; load = true) =
  ## Generates and emits the code for a dot-expression `n` (i.e. field access).
  ## The resulting value/handle is stored to `dest`.
  assert n.kind == nkDotExpr
  let
    a = c.genx(n[0])
    b = genField(c, n[1])

  genFieldAccessAux(c, n, a, b, dest, load)
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
  result = c.genx(accessExpr[0])
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
  var discrStrReg = c.getFullTemp(n, strType)
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

proc genCheckedObjAccess(c: var TCtx; n: PNode; dest: var TDest; load = true) =
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

proc genArrAccess(c: var TCtx; n: PNode; dest: var TDest; load = true) =
  let arrayType = n[0].typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind
  case arrayType
  of tyString, tyCstring:
    if load:
      # no need to pass `load`; the result of a string access is always
      # stored in a register
      genArrAccessOpcode(c, n, dest, opcLdStrIdx)
    else:
      # use the ``opcLdArr`` operation to get a handle to the ``char``
      # location
      genArrAccessOpcode(c, n, dest, opcLdArr, load=false)
  of tyTuple:
    let a = genx(c, n[0])
    genFieldAccessAux(c, n, a, n[1].intVal, dest, load)
    c.freeTemp(a)
  of tyArray, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray:
    genArrAccessOpcode(c, n, dest, opcLdArr, load)
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
    genArrAccessOpcode(c, n, dest, opcLdStrIdxAddr)
  of tyArray, tySequence, tyOpenArray, tyVarargs:
    genArrAccessOpcode(c, n, dest, opcLdArrAddr)
  else:
    unreachable()

proc genAddr(c: var TCtx, src, n: PNode, dest: var TDest) =
  ## Generates and emits the code for taking the address of lvalue expression
  ## `n`. `src` provides the type information of the destination, plus the line
  ## information to use.
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
    # taking the address of a view's or ``var`` parameter's underlying
    # location
    assert isLocView(n[0].typ)
    if isPtrView(n[0]):
      # the view is stored as an address; treat the deref as a no-op
      genLvalue(c, n[0], dest)
    else:
      prepare(c, dest, src.typ)
      let tmp = genx(c, n[0]) # skip the deref
      c.gABC(src, opcAddr, dest, tmp)
      c.freeTemp(tmp)
  of nkDerefExpr:
    prepare(c, dest, src.typ)
    # after transformation (i.e. ``transf``), the only ``addr(deref(x))``
    # sequences left are those that can't be collapsed, and since a ``ptr``
    # is not interchangeable with ``ref`` we need the deref
    var tmp = noDest
    genDeref(c, n, tmp, load=false)
    c.gABC(src, opcAddr, dest, tmp)
    c.freeTemp(tmp)
  of nkConv:
    # an l-value conversion. Take the address of the source expression
    genAddr(c, src, n[1], dest)
  of nkObjDownConv, nkObjUpConv:
    case n.typ.skipTypes(IrrelevantTypes).kind
    of tyPtr:
      # not supported at this time
      cannotEval(c, src)
    of tyRef, tyObject:
      # the operations produce a new handle (to the same location, but with
      # a different type), so we must not skip them
      prepare(c, dest, src.typ)
      var tmp = noDest
      # perform the conversion and then take the address of the result
      genObjConv(c, n, tmp)
      c.gABC(src, opcAddr, dest, tmp)
      c.freeTemp(tmp)
    else:
      unreachable()
  of nkStmtListExpr:
    for i in 0..<n.len-1:
      gen(c, n[i])

    genAddr(c, src, n[^1], dest)
  else:
    unreachable(n.kind)

proc genLvalue(c: var TCtx, n: PNode, dest: var TDest) =
  ## Generates and emits the code for computing the handle of the location
  ## named by l-value expression `n`. If the expression names a location that
  ## is stored in a VM memory cell, `dest` will always store a handle --
  ## address-based views are dereferenced.
  ##
  ## Note that in the case of locals backed by registers, `dest` will store
  ## its value instead of a handle.
  case n.kind
  of nkSym:
    c.genSym(n, dest, load=false)
  of nkDotExpr:
    genFieldAccess(c, n, dest, load=false)
  of nkCheckedFieldExpr:
    genCheckedObjAccess(c, n, dest, load=false)
  of nkBracketExpr:
    genArrAccess(c, n, dest, load=false)
  of nkConv:
    # if a conversion reaches here, it must be an l-value conversion. They
    # don't map to any bytecode, so we skip them
    genLvalue(c, n[1], dest)
  of nkObjDownConv, nkObjUpConv:
    # these conversions are *not* no-ops, as they produce a handle of different
    # type
    gen(c, n, dest)
  of nkHiddenDeref:
    assert isLocView(n[0].typ)
    if isPtrView(n[0]):
      # we want a handle (``rkHandle``), but the input view uses a pointer
      # (``rkAddress``) internally. Turn it into a handle by dereferencing it
      prepare(c, dest, n[0].typ)
      let tmp = c.genx(n[0])
      c.gABC(n, opcLdDeref, dest, tmp)
      c.freeTemp(tmp)
    else:
      # the operand is a handle already; treat the deref as a no-op
      genLvalue(c, n[0], dest)
  of nkDerefExpr:
    genDeref(c, n, dest, load=false)
  of nkCallKinds:
    # we only reach this case for ``HiddenAddr (HiddenDeref (Call ...))``.
    # Generate the call returning a view as is
    # XXX: ``astgen`` should not emit these instead
    assert isLocView(n.typ)
    gen(c, n, dest)
  of nkStmtListExpr:
    for i in 0..<n.len-1:
      gen(c, n[i])

    genLvalue(c, n.lastSon, dest)
  else:
    unreachable(n.kind)

proc genVarSection(c: var TCtx; n: PNode) =
  for a in n:
    case a.kind
    of nkIdentDefs:
        assert a[0].kind == nkSym
        let s = a[0].sym
        checkCanEval(c, a[0])
        assert not s.isGlobal
        if true:
          let reg = setSlot(c, s)
          if a[2].kind == nkEmpty:
            # no initializer; only setup the register (and memory location,
            # if used)
            let opc = if usesRegister(c.prc, s): opcLdNullReg
                      else: opcLdNull

            c.gABx(a, opc, reg, c.genType(s.typ))
          else:
            # XXX: checking for views here is wrong but necessary
            if not usesRegister(c.prc, s) and not isDirectView(s.typ):
              # only setup a memory location if the local uses one
              c.gABx(a, opcLdNull, reg, c.genType(s.typ))

            # views and locals backed by registers don't need explicit
            # initialization logic here -- the assignment takes care of that
            genSymAsgn(c, a[0], a[2])

    else:
      unreachable(a.kind)

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
      let a = c.genAsgnSource(x, wantsPtr = true)
      c.gABC(n, opcWrArr, dest, tmp, a)
      c.gABI(n, opcAddImmInt, tmp, tmp, 1)
      c.freeTemp(a)
    c.freeTemp(tmp)

proc genSetConstr(c: var TCtx, n: PNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABx(n, opcLdNull, dest, c.genType(n.typ))
  # XXX: since `first` stays the same across the loop, we could invert
  #      the loop around `genSetElem`'s logic...
  let first = firstOrd(c.config, n.typ.skipTypes(abstractInst))
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
  prepare(c, dest, n, n.typ)
  let t = n.typ.skipTypes(abstractRange-{tyTypeDesc})
  var refTemp: TDest
  if t.kind == tyRef:
    refTemp = c.getTemp(t[0]) # The temporary register to hold the
                              # dereferenced location
    c.gABx(n, opcNew, dest, c.genType(t))
    c.gABC(n, opcLdDeref, refTemp, dest)
    swap(refTemp, dest)

  for i in 1..<n.len:
    let it = n[i]
    assert it.kind == nkExprColonExpr and it[0].kind == nkSym
    if true:
      let idx = genField(c, it[0])
      var tmp: TRegister
      var opcode: TOpcode
      if sfDiscriminant notin it[0].sym.flags:
        tmp = c.genAsgnSource(it[1], wantsPtr = true)
        opcode = opcWrObj
        let
          le = it[0].sym.typ
          ri = it[1].typ
        if le.kind == tyOpenArray and not sameType(le, ri):
          # XXX: this is a hack to make `tests/vm/tconst_views` work for now.
          #      `transf` removes `nkHiddenStdConv` for array/seq to openArray
          #      conversions, which we could have otherwise relied on
          let tmp2 = c.getFullTemp(it[0], le)
          c.gABx(n, opcConv, tmp2, c.genTypeInfo(le))
          c.gABx(n, opcConv, tmp, c.genTypeInfo(ri))
          c.freeTemp(tmp)
          tmp = tmp2
      else:
        tmp = c.genDiscrVal(it[0], it[1], n.typ)
        opcode = opcInitDisc
      c.gABC(it[1], opcode, dest, idx, tmp)
      c.freeTemp(tmp)

  if t.kind == tyRef:
    swap(refTemp, dest)
    c.freeTemp(refTemp)

proc genTupleConstr(c: var TCtx, n: PNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)
  if n.typ.kind != tyTypeDesc:
    c.gABx(n, opcLdNull, dest, c.genType(n.typ))
    # XXX x = (x.old, 22)  produces wrong code ... stupid self assignments
    for i, it in n.pairs:
      let tmp = c.genAsgnSource(it, wantsPtr = true)
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

proc gen(c: var TCtx; n: PNode; dest: var TDest) =
  when defined(nimCompilerStacktraceHints):
    frameMsg c.config, n

  case n.kind
  of nkSym:
    let s = n.sym
    checkCanEval(c, n)
    case s.kind
    of skVar, skForVar, skTemp, skLet, skParam, skResult:
      genSym(c, n, dest)
    of skProc, skFunc, skConverter, skMacro, skMethod, skIterator:
      if importcCond(c, s) and lookup(c.callbackKeys, s) == -1:
        fail(n.info, vmGenDiagCannotImportc, sym = s)

      genProcLit(c, n, s, dest)
    of skConst:
      if dest.isUnset: dest = c.getTemp(s.typ)

      if s.ast.kind in nkLiterals:
        let lit = genLiteral(c, s.ast)
        c.genLit(n, lit, dest)
      else:
        let idx = c.lookupConst(s)
        discard c.getOrCreate(s.typ)
        c.gABx(n, opcLdCmplxConst, dest, idx)
    else:
      unreachable(s.kind)
  of nkCall:
    if n[0].kind == nkSym:
      let s = n[0].sym
      if s.magic != mNone:
        genMagic(c, n, dest, s.magic)
      elif s.kind == skMethod and c.mode != emStandalone:
        # XXX: detect and reject this earlier -- it's not a code
        #      generation error
        fail(n.info, vmGenDiagCannotCallMethod, sym = s)
      else:
        genCall(c, n, dest)
        clearDest(c, n, dest)
    else:
      genCall(c, n, dest)
      clearDest(c, n, dest)
  of nkIntKinds:
    prepare(c, dest, n.typ)
    c.loadInt(n, dest, getInt(n))
  of nkFloatKinds, nkStrKinds: genLit(c, n, dest)
  of nkNilLit:
    if true:
      let t = n.typ.skipTypes(abstractInst)
      internalAssert(c.config,
        t.kind in {tyPtr, tyRef, tyPointer, tyNil, tyProc, tyCstring},
        n.info,
        $t.kind)
      if dest.isUnset: dest = c.getTemp(t)
      c.gABx(n, ldNullOpcode(t), dest, c.genType(n.typ))
  of nkNimNodeLit:
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
  of nkAsgn, nkFastAsgn:
    unused(c, n, dest)
    genAsgn(c, n[0], n[1], n.kind == nkAsgn)
  of nkDotExpr: genFieldAccess(c, n, dest)
  of nkCheckedFieldExpr: genCheckedObjAccess(c, n, dest)
  of nkBracketExpr: genArrAccess(c, n, dest)
  of nkDerefExpr: genDeref(c, n, dest)
  of nkAddr: genAddr(c, n, n[0], dest)
  of nkHiddenDeref:
    assert isLocView(n[0].typ)
    # a view indirection
    genDerefView(c, n[0], dest)
  of nkHiddenAddr:
    assert isLocView(n.typ)
    # load the source operand as a handle
    genLvalue(c, n[0], dest)
  of nkIfStmt:
    unused(c, n, dest)
    genIf(c, n)
  of nkCaseStmt:
    unused(c, n, dest)
    genCase(c, n)
  of nkWhileStmt:
    unused(c, n, dest)
    genWhile(c, n)
  of nkBlockStmt:
    unused(c, n, dest)
    genBlock(c, n)
  of nkReturnStmt:
    genReturn(c, n)
  of nkRaiseStmt:
    genRaise(c, n)
  of nkBreakStmt:
    genBreak(c, n)
  of nkTryStmt:
    unused(c, n, dest)
    genTry(c, n)
  of nkStmtList:
    unused(c, n, dest)
    for x in n: gen(c, x)
  of nkStmtListExpr:
    for i in 0..<n.len-1: gen(c, n[i])
    gen(c, n[^1], dest)
  of nkDiscardStmt:
    unused(c, n, dest)
    gen(c, n[0])
  of nkHiddenStdConv, nkConv:
    genConv(c, n, n[1], dest)
  of nkObjDownConv, nkObjUpConv:
    genObjConv(c, n, dest)
  of nkLetSection, nkVarSection:
    unused(c, n, dest)
    genVarSection(c, n)
  of nkChckRangeF, nkChckRange64, nkChckRange:
    let tmp0 = c.genx(n[0])
    # XXX: range checks currently always happen, even if disabled by the user.
    #      Once the range check injection logic is an MIR pass, this should be
    #      reconsidered, at least for code not running at compile-time
    # note: don't skip ``tyRange``
    case n.typ.skipTypes(IrrelevantTypes + {tyVar, tyLent}).kind
    of tyUInt..tyUInt64:
      # use a normal conversion instead of a range check for unsigned integers
      let
        a = n.typ.skipTypes(IrrelevantTypes + {tyVar, tyLent, tyRange})
        b = n[0].typ.skipTypes(IrrelevantTypes + {tyVar, tyLent, tyRange})
      prepare(c, dest, n.typ)
      genNumberConv(c, n, dest, tmp0, a, b)
      c.freeTemp(tmp0)
    else:
      let
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
  of nkEmpty:
    unused(c, n, dest)
  of nkStringToCString, nkCStringToString:
    gen(c, n[0], dest)
  of nkBracket: genArrayConstr(c, n, dest)
  of nkCurly: genSetConstr(c, n, dest)
  of nkObjConstr: genObjConstr(c, n, dest)
  of nkTupleConstr: genTupleConstr(c, n, dest)
  of nkClosure: genClosureConstr(c, n, dest)
  of nkCast:
    if allowCast in c.features:
      genCast(c, n, n[1], dest)
    else:
      genCastIntFloat(c, n, dest)
  of nkType:
    genTypeLit(c, n.typ, dest)
  of nkPragma, nkAsmStmt:
    unused(c, n, dest)
  of nkWithSons + nkWithoutSons - codegenExprNodeKinds:
    unreachable(n.kind)

proc genStmt*(c: var TCtx; n: PNode): Result[void, VmGenDiag] =
  analyseIfAddressTaken(n, c.prc.addressTaken)

  var d: TDest = -1
  try:
    c.gen(n, d)
  except VmGenError as e:
    return typeof(result).err(move e.diag)

  c.config.internalAssert(d < 0, n.info, "VM problem: dest register is set")
  result = typeof(result).ok()

proc genExpr*(c: var TCtx; n: PNode): Result[TRegister, VmGenDiag] =
  analyseIfAddressTaken(n, c.prc.addressTaken)

  var d: TDest = -1
  try:
    c.gen(n, d)
  except VmGenError as e:
    return typeof(result).err(move e.diag)

  # the destination register not being set likely indicate that `n` is not an
  # expression
  c.config.internalAssert(d != noDest, n.info):
    "VM problem: dest register is not set"

  result = typeof(result).ok(TRegister(d))


proc genParams(prc: PProc; s: PSym) =
  let
    params = s.routineSignature.n

  setLen(prc.regInfo, max(params.len, 1))

  if not isEmptyType(s.routineSignature[0]):
    prc.locals[s.ast[resultPos].sym.id] = 0
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

proc transitionToLocation(c: var TCtx, info: PNode, typ: PType, reg: TRegister) =
  ## Transitions the register `reg`, which is expected to store a value of
  ## type `typ` directly, to store an owning handle to a location instead.
  ## The location is initialized with the original value. `info` is only used
  ## for providing line information.
  assert fitsRegister(typ)
  let tmp = c.getTemp(typ)
  # make a temporary copy of the register:
  c.gABC(info, opcFastAsgnComplex, tmp, reg)
  c.gABx(info, opcLdNull, reg, c.genType(typ)) # allocate a location
  c.gABC(info, opcWrLoc, reg, tmp) # write the orignal value
  c.freeTemp(tmp)

proc prepareParameters(c: var TCtx, info: PNode) =
  ## Prepares immutable parameters backed by registers for having their address
  ## taken. If an immutable parameter has its address taken, it is transitioned
  ## to a VM memory location at the start of the procedure.
  let typ = c.prc.sym.routineSignature # the procedure's type

  template setupParam(c: var TCtx, s: PSym) =
    if fitsRegister(s.typ) and s.id in c.prc.addressTaken:
      transitionToLocation(c, info, s.typ, local(c.prc, s))

  if typ.n.len > 1: # does it have more than zero parameters?
    # if getReturnType(c.prc.sym).skipTypes(abstractInst).kind == tyLent:
    #   # the first parameter of a procedure returning an immutable view (i.e.
    #   # ``lent T``) is special: the argument is always passed by handle, even
    #   # if the value would fit into a register. To communicate this to the rest
    #   # of the code-generator, the parameter is marked as not being backed by a
    #   # register
    #   c.prc.addressTaken.incl(typ.n[1].sym.id)
    # else:
    # XXX: the above is correct, but the expection that to-be-borrowed-from
    #      parameters use pass-by-handle isn't met yet. For now, what this
    #      means is that ``proc borrow(x: int): lent int`` won't work
    setupParam(c, typ.n[1].sym)

  # setup the remaining parameters:
  for i in 2..<typ.n.len:
    setupParam(c, typ.n[i].sym)

proc genProcBody(c: var TCtx; s: PSym, body: PNode): int =
    assert s.kind in routineKinds - {skTemplate}

    var p = PProc(blocks: @[], sym: s)
    let oldPrc = c.prc
    c.prc = p

    # collect which locations have their address taken or a view to them
    # created:
    analyseIfAddressTaken(body, p.addressTaken)

    # iterate over the parameters and allocate space for them:
    genParams(c.prc, s)

    if s.routineSignature.callConv == ccClosure:
      # reserve a slot for the hidden environment parameter and register the
      # mapping
      c.prc.regInfo.add RegInfo(refCount: 1, kind: slotFixedLet)
      c.prc.locals[s.ast[paramsPos][^1].sym.id] = c.prc.regInfo.high

    # setup the result register, if necessary. Watch out for macros! Their
    # result register is setup at the start of macro evaluation
    # XXX: initializing the ``result`` of a macro should be handled through
    #      inserting the necessary code either in ``sem` or here
    let rt = s.routineSignature[0]
    if not isEmptyType(rt) and fitsRegister(rt):
      # initialize the register holding the result
      let r = s.ast[resultPos].sym

      if s.kind == skMacro:
        if r.id in c.prc.addressTaken:
          # the result variable for macros is currently initialized from outside
          # the VM, so we can't use ``opcLdNull`` here
          transitionToLocation(c, body, rt, local(c.prc, r))
      else:
        let opcode =
          if r.id in c.prc.addressTaken:
            # the result variable has its address taken or a reference/view
            # to it created -> we need a VM memory location. In order for this
            # to be opaque to the callsite, a write-back is injected before
            # returns
            opcLdNull
          else:
            opcLdNullReg

        # only setup a location/register if the procedure's result is not a view:
        if not isDirectView(r.typ):
          gABx(c, body, opcode, local(c.prc, r), c.genType(r.typ))

    prepareParameters(c, body)
    gen(c, body)

    # generate final 'return' statement:
    writeBackResult(c, body)
    c.gABC(body, opcRet)

    result = c.prc.regInfo.len
    c.prc = oldPrc

proc genProc*(c: var TCtx; s: PSym, body: PNode): VmGenResult =
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
    of vmGenDiagTooManyRegistersRequired: adVmGenTooManyRegistersRequired
    of vmGenDiagCannotFindBreakTarget: adVmGenCannotFindBreakTarget
    of vmGenDiagNotUnused: adVmGenNotUnused
    of vmGenDiagCannotEvaluateAtComptime: adVmGenCannotEvaluateAtComptime
    of vmGenDiagMissingImportcCompleteStruct: adVmGenMissingImportcCompleteStruct
    of vmGenDiagCodeGenUnhandledMagic: adVmGenCodeGenUnhandledMagic
    of vmGenDiagCannotImportc: adVmGenCannotImportc
    of vmGenDiagTooLargeOffset: adVmGenTooLargeOffset
    of vmGenDiagCannotCallMethod: adVmGenCannotCallMethod
    of vmGenDiagCannotCast: adVmGenCannotCast
  
  {.cast(uncheckedAssign).}: # discriminants on both sides lead to saddness
    result =
      case diag.kind
      of vmGenDiagCannotImportc,
          vmGenDiagTooLargeOffset,
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
          vmGenDiagCannotEvaluateAtComptime:
        AstDiagVmGenError(
          kind: kind,
          ast: diag.ast)
      of vmGenDiagTooManyRegistersRequired,
          vmGenDiagCannotFindBreakTarget:
        AstDiagVmGenError(kind: kind)
 