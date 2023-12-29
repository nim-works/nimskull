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
  compiler/backend/[
    cgir
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
    containers,
    idioms
  ],
  compiler/vm/[
    vmaux,
    vmdef,
    vmlinker,
    vmobjects,
    vmtypegen,
    vmtypes,
  ],
  experimental/[
    results
  ]

import std/options as std_options

from compiler/backend/compat import getInt, isOfBranch, skipConv, lastSon,
  getMagic

from std/bitops import bitor

when defined(nimCompilerStacktraceHints):
  import std/stackframes
  import compiler/utils/debugutils


type
  VmGenResult* = Result[CodeInfo, VmGenDiag] ## Result of a vmgen invocation

  VmGenError = object of CatchableError
    diag: VmGenDiag

  TPosition = distinct int
  TDest = range[-1..regAMask.int]

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

  TSlotKind = enum    # We try to re-use slots in a smart way to
                      # minimize allocations; however the VM supports arbitrary
                      # temporary slot usage. This is required for the parameter
                      # passing implementation.
    slotEmpty,        ## slot is unused
    slotFixedVar,     ## slot is used for a fixed var/result (requires copy then)
    slotFixedLet,     ## slot is used for a fixed param/let
    slotTempUnknown,  ## slot but type unknown (argument of proc call)
    slotTempInt,      ## some temporary int
    slotTempFloat,    ## some temporary float
    slotTempStr,      ## some temporary string
    slotTempComplex,  ## some complex temporary (s.node field is used)
    slotTempHandle,   ## some temporary handle into an object/array
    slotTempPerm      ## slot is temporary but permanent (hack)

  RegInfo = object
    inUse: bool
    kind: TSlotKind

  LocalLoc = object
    ## The current state for a local.
    reg: TRegister   ## the register that holds either the handle or value
    isIndirect: bool ## whether the local uses a handle while its value
                     ## would fit it into a register

  BProc = object
    blocks: seq[seq[TPosition]]
      ## for each block, the jump instructions targeting the block's exit.
      ## These need to be patched once the code for the block is generated
    sym: PSym
    body: Body
      ## the full body of the current procedure/statement/expression
    bestEffort: TLineInfo
      ## the source-code position to point to for internal failures where
      ## no line information is directly available
    regInfo: seq[RegInfo]

    locals: OrdinalSeq[LocalId, LocalLoc]
      ## current state of all locals

  CodeGenCtx* = object
    ## Bundles all input, output, and other contextual data needed for the
    ## code generator
    prc: BProc

    # immutable input parameters:
    graph*: ModuleGraph
    config*: ConfigRef
    mode*: TEvalMode
    features*: TSandboxFlags
    module*: PSym

    linking*: LinkerData

    # input-output parameters:
    code*: seq[TInstr]
    debug*: seq[TLineInfo]
    constants*: seq[VmConstant]
    typeInfoCache*: TypeInfoCache
    rtti*: seq[VmTypeInfo]

  TCtx = CodeGenCtx ## legacy alias

const
  IrrelevantTypes = abstractInst + {tyStatic, tyEnum} - {tyTypeDesc}
    ## the set of types that are not relevant to the VM. ``tyTypeDesc``, while
    ## not relevant right now, is likely going to be in the future.

  MagicsToKeep* = {mNone, mIsolate, mNHint, mNWarning, mNError, mMinI, mMaxI,
                   mAbsI, mDotDot}
    ## the set of magics that are kept as normal procedure calls and thus need
    ## an entry in the function table. For convenience, the ``mNone`` magic is
    ## also included

  noDest = TDest(-1)
  slotSomeTemp* = slotTempUnknown

proc getOrCreate*(c: var TCtx, typ: PType;
                  noClosure = false): PVmType {.inline.} =
  var cl: GenClosure
  getOrCreate(c.typeInfoCache, c.config, typ, noClosure, cl)

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
proc genLit(c: var TCtx; n: CgNode; lit: int; dest: var TDest)
proc genTypeLit(c: var TCtx; info: CgNode, t: PType; dest: var TDest)
proc genType(c: var TCtx, typ: PType; noClosure = false): int
func fitsRegister(t: PType): bool
proc genRegLoad(c: var TCtx, n: CgNode, dest, src: TRegister) {.inline.}
proc genRegLoad(c: var TCtx, n: CgNode, typ: PType, dest, src: TRegister)

template isUnset(x: TDest): bool = x < 0

template `[]`(p: BProc, id: LocalId): LocalLoc =
  ## Convenience shorthand.
  p.locals[id]

proc routineSignature(s: PSym): PType {.inline.} =
  ## Returns the signature type of the routine `s`
  if s.kind == skMacro: s.internal
  else:                 s.typ

func underlyingLoc(n: CgNode): CgNode =
  ## Computes and returns the symbol of the complete location (i.e., a location
  ## not part of a compound location) that l-value expression `n` names. If no
  ## complete location is named, ``nil`` is returned.
  var root {.cursor.} = n
  # skip nodes that don't change the location until we arrive at either one
  # that does, or a symbol
  while root.kind == cnkConv:
    root = root.operand

  result = root

func analyseIfAddressTaken(n: CgNode, prc: var BProc) =
  ## Recursively traverses the tree `n` and marks locals of which the address is
  ## taken as requiring indirection.
  case n.kind
  of cnkHiddenAddr, cnkAddr:
    # the nodes we're interested
    let loc = underlyingLoc(n.operand)
    # we're only interested in locals
    if loc.kind == cnkLocal:
      prc[loc.local].isIndirect = true
    else:
      # the operand expression must still be anaylsed
      analyseIfAddressTaken(n.operand, prc)
  of cnkAtoms:
    discard "ignore"
  of cnkWithOperand - {cnkHiddenAddr, cnkAddr}:
    analyseIfAddressTaken(n.operand, prc)
  of cnkWithItems:
    for it in n.items:
      analyseIfAddressTaken(it, prc)


func isNimNode(t: PType): bool =
  ## Returns whether `t` is the ``NimNode`` magic type
  let t = skipTypes(t, IrrelevantTypes)
  t.sym != nil and t.sym.magic == mPNimrodNode

func gABC*(ctx: var TCtx; i: TLineInfo; opc: TOpcode; a, b, c: TRegister = 0) =
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
  ctx.debug.add(i)

proc gABI(c: var TCtx; i: TLineInfo; opc: TOpcode; a, b: TRegister; imm: BiggestInt) =
  # Takes the `b` register and the immediate `imm`, applies the operation `opc`,
  # and stores the output value into `a`.
  # `imm` is signed and must be within [-128, 127]
  c.config.internalAssert(imm in -128..127, i,
    "VM: immediate value does not fit into an int8")

  let ins = (opc.TInstrType or (a.TInstrType shl regAShift) or
                           (b.TInstrType shl regBShift) or
                           (imm+byteExcess).TInstrType shl regCShift).TInstr
  c.code.add(ins)
  c.debug.add(i)

proc gABx*(c: var TCtx; i: TLineInfo; opc: TOpcode; a: TRegister = 0; bx: int) =
  # Applies `opc` to `bx` and stores it into register `a`
  # `bx` must be signed and in the range [regBxMin, regBxMax]

  #[
  when false:
    if c.code.len == 43:
      writeStackTrace()
      echo "generating ", opc
      ]#

  c.config.internalAssert(bx in regBxMin-1..regBxMax, i,
    "VM: immediate value does not fit into regBx")

  let ins = (opc.TInstrType or a.TInstrType shl regAShift or
            (bx+wordExcess).TInstrType shl regBxShift).TInstr
  c.code.add(ins)
  c.debug.add(i)

# convenience templates that take the line information from the node
template gABC*(ctx: var TCtx; n: CgNode; opc: TOpcode; a, b, c: TRegister = 0) =
  gABC(ctx, n.info, opc, a, b, c)

template gABI(c: var TCtx; n: CgNode; opc: TOpcode; a, b: TRegister; imm: BiggestInt) =
  gABI(c, n.info, opc, a, b, imm)

template gABx(c: var TCtx, n: CgNode, opc: TOpcode; a: TRegister, bx: int) =
  gABx(c, n.info, opc, a, bx)

proc xjmp(c: var TCtx; n: CgNode; opc: TOpcode; a: TRegister = 0): TPosition =
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
  case t.skipTypes(IrrelevantTypes+{tyRange}).kind
  of tyBool, tyChar, tyInt..tyInt64, tyUInt..tyUInt64:
    slotTempInt
  of tyString, tyCstring:
    slotTempStr
  of tyFloat..tyFloat64:
    slotTempFloat
  else:
    slotTempComplex

const
  HighRegisterPressure = 40

proc getFreeRegister(c: var BProc; k: TSlotKind; start: int): TRegister =
  # we prefer the same slot kind here for efficiency. Unfortunately for
  # discardable return types we may not know the desired type. This can happen
  # for e.g. mNAdd[Multiple]:
  for i in start..c.regInfo.len-1:
    if c.regInfo[i].kind == k and not c.regInfo[i].inUse:
      c.regInfo[i].inUse = true
      return TRegister(i)

  # if register pressure is high, we re-use more aggressively:
  if c.regInfo.len >= high(TRegister):
    for i in start..c.regInfo.len-1:
      if not c.regInfo[i].inUse:
        c.regInfo[i] = RegInfo(inUse: true, kind: k)
        return TRegister(i)
  if c.regInfo.len >= high(TRegister):
    fail(c.bestEffort, vmGenDiagTooManyRegistersRequired)

  result = TRegister(max(c.regInfo.len, start))
  c.regInfo.setLen int(result)+1
  c.regInfo[result] = RegInfo(inUse: true, kind: k)

func getTemp(cc: var TCtx; kind: TSlotKind): TRegister {.inline.} =
  getFreeRegister(cc.prc, kind, start = 0)

proc getTemp(cc: var TCtx; tt: PType): TRegister =
  let typ = tt.skipTypesOrNil({tyStatic})
  # we prefer the same slot kind here for efficiency. Unfortunately for
  # discardable return types we may not know the desired type. This can happen
  # for e.g. mNAdd[Multiple]:
  let k = if typ.isNil: slotTempComplex else: typ.getSlotKind
  result = getFreeRegister(cc.prc, k, start = 0)

  when false:
    # enable this to find "register" leaks:
    if result == 4:
      echo "begin ---------------"
      writeStackTrace()
      echo "end ----------------"

func freeTemp(c: var TCtx; r: TRegister) =
  case c.prc.regInfo[r].kind
  of slotSomeTemp..slotTempHandle:
    # this seems to cause https://github.com/nim-lang/Nim/issues/10647
    c.prc.regInfo[r].inUse = false
  else:
    discard # do nothing


proc getTempRange(c: var BProc; n: int; kind: TSlotKind): TRegister =
  # if register pressure is high, we re-use more aggressively:
  # we could also customize via the following (with proper caching in ConfigRef):
  # let highRegisterPressure = cc.config.getConfigVar("vm.highRegisterPressure", "40").parseInt
  if c.regInfo.len >= HighRegisterPressure or c.regInfo.len+n >= high(TRegister):
    for i in 0..c.regInfo.len-n:
      if not c.regInfo[i].inUse:
        block search:
          for j in i+1..i+n-1:
            if c.regInfo[j].inUse: break search
          result = TRegister(i)
          for k in result..result+n-1:
            c.regInfo[k] = RegInfo(inUse: true, kind: kind)
          return
  if c.regInfo.len+n >= high(TRegister):
    fail(c.bestEffort, vmGenDiagTooManyRegistersRequired)

  result = TRegister(c.regInfo.len)
  setLen c.regInfo, c.regInfo.len+n
  for k in result .. result + n - 1:
    c.regInfo[k] = RegInfo(inUse: true, kind: kind)

proc freeTempRange(c: var TCtx; start: TRegister, n: int) =
  for i in start .. start + n - 1:
    c.freeTemp(TRegister(i))

proc getFullTemp(c: var TCtx, n: CgNode, t: PType): TRegister =
  ## Allocates a register for a value of type `t`, and, if the value doesn't
  ## fit into a register, emits the bytecode for allocating and setting up a
  ## temporary location.
  ##
  ## The difference compared to ``getTemp`` is that ``getFullTemp`` also
  ## allocates a location. While ``getFullTemp`` should ideally replace
  ## ``getTemp``, doing so is currently not possible because ``getTemp`` gets
  ## often passed an incorrect type
  let kind = getSlotKind(t.skipTypes(abstractInst + {tyStatic} - {tyTypeDesc}))
  result = getFreeRegister(c.prc, kind, start = 0)

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

proc prepare(c: var TCtx, dest: var TDest, n: CgNode, typ: PType) =
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

proc gen(c: var TCtx; n: CgNode; dest: var TDest)
proc gen(c: var TCtx; n: CgNode; dest: TRegister) =
  var d: TDest = dest
  gen(c, n, d)
  #internalAssert c.config, d == dest # issue #7407

proc gen(c: var TCtx; n: CgNode) =
  var tmp: TDest = -1
  gen(c, n, tmp)
  if tmp >= 0:
    freeTemp(c, tmp)
  #if n.typ.isEmptyType: internalAssert tmp < 0

proc genx(c: var TCtx; n: CgNode): TRegister =
  var tmp: TDest = -1
  gen(c, n, tmp)
  #internalAssert c.config, tmp >= 0 # 'nim check' does not like this internalAssert.
  if tmp >= 0:
    result = TRegister(tmp)

proc genLvalue(c: var TCtx, n: CgNode, dest: var TDest)
proc genLvalue(c: var TCtx, n: CgNode): TRegister {.inline.} =
  var dest = noDest
  genLvalue(c, n, dest)
  result = TRegister(dest)

proc clearDest(c: var TCtx; n: CgNode; dest: var TDest) {.inline.} =
  # stmt is different from 'void' in meta programming contexts.
  # So we only set dest to -1 if 'void':
  if dest >= 0 and (n.typ.isNil or n.typ.kind == tyVoid):
    c.freeTemp(dest)
    dest = -1

func isNotOpr(n: CgNode): bool {.inline.} =
  getMagic(n) == mNot

proc genRepeat(c: var TCtx; n: CgNode) =
  # lab1:
  #   body
  #   jmp lab1
  # lab2:
  let lab1 = c.genLabel
  c.gen(n[0])
  c.jmpBack(n, lab1)

proc genBlock(c: var TCtx; n: CgNode) =
  let oldRegisterCount = c.prc.regInfo.len

  c.prc.blocks.add @[] # push a new block
  c.gen(n[1])
  # fixup the jumps:
  for pos in c.prc.blocks[^1].items:
    c.patch(pos)
  # pop the block again:
  c.prc.blocks.setLen(c.prc.blocks.len - 1)

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

proc genBreak(c: var TCtx; n: CgNode) =
  let lab1 = c.xjmp(n, opcJmp)
  c.prc.blocks[n[0].label.int].add lab1

proc genIf(c: var TCtx, n: CgNode) =
  #  if (!expr1) goto lab1;
  #    thenPart
  #  lab1:
  block:
      let it = n
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

proc genLiteral(c: var TCtx, n: CgNode): int =
  case n.kind
  of cnkIntLit:   toIntCnst(c, n.intVal)
  of cnkUIntLit:  toIntCnst(c, n.intVal)
  of cnkFloatLit: toFloatCnst(c, n.floatVal)
  of cnkStrLit:   toStringCnst(c, n.strVal)
  else:           unreachable(n.kind)

template fillSliceList[T](sl: var seq[Slice[T]], nodes: openArray[CgNode],
                          get: untyped) =
  sl.newSeq(nodes.len)

  template getIt(n): untyped =
    block:
      let it {.cursor, inject.} = n
      get

  for (i, n) in nodes.pairs:
    sl[i] =
      if n.kind == cnkRange:
        getIt(n[0]) .. getIt(n[1])
      else:
        let e = getIt(n)
        e .. e

proc genBranchLit(c: var TCtx, n: CgNode, t: PType): int =
  ## Turns the slice-list or single literal of the given `cnkBranch` into
  ## a constant and returns its index in `c.constant`.
  ##
  ## slice-lists are always added as a new constant while single literals
  ## are reused

  # XXX: slice-list constants (maybe `VmConstant`s in general) should be
  #      stored in a `BiTable` so that it can be easily detected if they
  #      already exist
  assert t.kind in IntegralTypes+{tyString}

  if n.len == 2 and n[0].kind in cnkLiterals:
    # It's an 'of' branch with a single value
    result = c.genLiteral(n[0])
  else:
    # It's an 'of' branch with multiple and/or range values
    var cnst: VmConstant

    template values: untyped =
      n.kids.toOpenArray(0, n.kids.high - 1) # -1 for the branch body

    case t.kind
    of IntegralTypes-{tyFloat..tyFloat64}:
      cnst = VmConstant(kind: cnstSliceListInt)
      cnst.intSlices.fillSliceList(values):
        it.intVal

    of tyFloat..tyFloat64:
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

proc genBranchLit(c: var TCtx, n: PNode): int =
  ## Compatibility overload that accepts a ``PNode``. Only branches with
  ## integer values are supported.
  if n.len == 2 and n[0].kind != nkRange:
    toIntCnst(c, n[0].intVal)
  else:
    var sl = VmConstant(kind: cnstSliceListInt)
    sl.intSlices.newSeq(n.len - 1)
    for i, it in sliceIt(n.sons, 0, n.len-2):
      sl.intSlices[i] =
        if it.kind == nkRange: it[0].intVal .. it[1].intVal
        else:                  it.intVal .. it.intVal

    c.rawGenLiteral(sl)

proc unused(c: TCtx; n: CgNode; x: TDest) {.inline.} =
  if x >= 0:
    fail(n.info, vmGenDiagNotUnused, PNode(nil))

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
  let selType = n[0].typ.skipTypes(abstractVarRange)
  var endings: seq[TPosition] = @[]
  withTemp(tmp, n[0], n[0].typ):
    c.gen(n[0], tmp)
    # branch tmp, codeIdx
    # fjmp   elseLabel

    # iterate of/else branches
    for i in 1..<n.len:
      let branch = n[i]
      if isOfBranch(branch):
        let b = genBranchLit(c, branch, selType)
        c.gABx(branch, opcBranch, tmp, b)
        let elsePos = c.xjmp(branch.lastSon, opcFJmp, tmp)
        c.gen(branch.lastSon)
        if i < n.len-1:
          endings.add(c.xjmp(branch.lastSon, opcJmp, 0))
        c.patch(elsePos)
      else:
        # else stmt:
        c.gen(branch[0])
  for endPos in endings: c.patch(endPos)

proc genType(c: var TCtx; typ: PType; noClosure = false): int =
  ## Returns the ID of `typ`'s corresponding `VmType` as an `int`. The
  ## `VmType` is created first if it doesn't exist already
  let t = c.getOrCreate(typ, noClosure)
  # XXX: `getOrCreate` doesn't return the id directly yet. Once it does, the
  #      linear search below can be removed
  result = c.typeInfoCache.types.find(t)
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

proc genTry(c: var TCtx; n: CgNode) =
  var endings: seq[TPosition] = @[]
  let ehPos = c.xjmp(n, opcTry, 0)
  c.gen(n[0])
  # Add a jump past the exception handling code
  let jumpToFinally = c.xjmp(n, opcJmp, 0)
  # This signals where the body ends and where the exception handling begins
  c.patch(ehPos)
  for i in 1..<n.len:
    let it = n[i]
    if it.kind != cnkFinally:
      # first opcExcept contains the end label of the 'except' block:
      let endExcept = c.xjmp(it, opcExcept, 0)
      for j in 0..<it.len - 1:
        assert(it[j].kind == cnkType)
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
  if fin.kind == cnkFinally:
    c.gen(fin[0])
  c.gABx(fin, opcFinallyEnd, 0, 0)

proc genRaise(c: var TCtx; n: CgNode) =
  if n[0].kind != cnkEmpty:
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

proc writeBackResult(c: var TCtx, info: CgNode) =
  ## If the result value fits into a register but is not stored in one
  ## (because it has its address taken, etc.), emits the code for storing it
  ## back into a register. `info` is only used to provide line information.
  let typ = c.prc.body[resultId].typ
  if not isEmptyType(typ) and fitsRegister(typ) and not isDirectView(typ) and
     c.prc[resultId].isIndirect:
      # a write-back is required. Load the value into temporary register and
      # then do a register move
      let
        tmp = c.getTemp(typ)
        dest = c.prc[resultId].reg
      c.genRegLoad(info, typ, tmp, dest)
      c.gABC(info, opcFastAsgnComplex, dest, tmp)
      c.freeTemp(tmp)

proc genReturn(c: var TCtx; n: CgNode) =
  writeBackResult(c, n)
  c.gABC(n, opcRet)


proc genLit(c: var TCtx; n: CgNode; lit: int; dest: var TDest) =
  ## `lit` is the index of a constant as returned by `genLiteral`

  # opcLdConst is now always valid. We produce the necessary copy in the
  # assignments now:
  #var opc = opcLdConst
  if dest.isUnset: dest = c.getTemp(n.typ)
  #elif c.prc.regInfo[dest].kind == slotFixedVar: opc = opcAsgnConst
  c.gABx(n, opcLdConst, dest, lit)

proc genLit(c: var TCtx; n: CgNode; dest: var TDest) =
  let lit = genLiteral(c, n)
  genLit(c, n, lit, dest)


proc genProcLit(c: var TCtx, n: CgNode, s: PSym; dest: var TDest) =
  if dest.isUnset:
    dest = c.getTemp(s.typ)

  let idx = c.linking.symToIndexTbl[s.id].int

  c.gABx(n, opcLdNull, dest, c.genType(s.typ, noClosure=true))
  c.gABx(n, opcWrProc, dest, idx)

proc genCall(c: var TCtx; n: CgNode; dest: var TDest) =
  if not isEmptyType(n.typ):
    prepare(c, dest, n, n.typ)

  let
    fntyp = skipTypes(n[0].typ, abstractInst)
    regCount = n.len + ord(fntyp.callConv == ccClosure)
    x = c.prc.getTempRange(regCount, slotTempUnknown)

  # generate the code for the callee:
  if fntyp.callConv == ccClosure:
    # unpack the closure (i.e., tuple): the proc goes into the callee
    # slot, while the environment pointer goes into the last argument
    # slot
    if n[0].kind == cnkClosureConstr:
      # optimization: don't allocate a temporary but place the values into
      # the respective registers directly
      # XXX: dead code, but should be restored
      c.gen(n[0][0], x+0)
      c.gen(n[0][1], x+n.len)
    else:
      let
        tmp = c.genx(n[0])
        tmp2 = c.getTemp(slotTempComplex)
      c.gABC(n[0], opcLdObj, x+0, tmp, 0)
      # use a full assignment in order for the environment to stay alive during
      # the call
      c.gABC(n[0], opcLdObj, tmp2, tmp, 1)
      c.gABC(n[0], opcAsgnComplex, x+n.len, tmp2)
      c.freeTemp(tmp2)
      c.freeTemp(tmp)
  else:
    c.gen(n[0], x+0)

  # varargs need 'opcSetType' for the FFI support:
  for i in 1..<n.len:
    # skip empty arguments (i.e. arguments to compile-time parameters that
    # were omitted):
    if n[i].kind == cnkEmpty:
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
  c.freeTempRange(x, regCount)

proc genField(c: TCtx; n: CgNode): TRegister =
  assert n.kind == cnkField

  let s = n.sym
  if s.position > high(typeof(result)):
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

proc genRegLoad(c: var TCtx, n: CgNode, typ: PType, dest, src: TRegister) =
  c.gABC(n, opcNodeToReg, dest, src)

  let t = typ.skipTypes(abstractInst)
  if t.isUnsigned() and t.size < sizeof(BiggestInt):
    c.gABC(n, opcNarrowU, dest, TRegister(t.size * 8))

proc genRegLoad(c: var TCtx, n: CgNode, dest, src: TRegister) {.inline.} =
  genRegLoad(c, n, n.typ, dest, src)

proc genCheckedObjAccessAux(c: var TCtx; n: CgNode): TRegister
proc genSym(c: var TCtx, n: CgNode, dest: var TDest, load = true)

func usesRegister(p: BProc, s: LocalId): bool =
  ## Returns whether the location identified by `s` is backed by a register
  ## (that is, whether the value is stored in a register directly)
  fitsRegister(p.body[s].typ) and not p[s].isIndirect

proc genNew(c: var TCtx; n: CgNode, dest: var TDest) =
  prepare(c, dest, n, n.typ)
  c.gABx(n, opcNew, dest,
         c.genType(n.typ.skipTypes(abstractInst-{tyTypeDesc})))
  c.freeTemp(dest)

proc genNewSeq(c: var TCtx; n: CgNode) =
  let t = n[1].typ.skipTypes(abstractVar)
  assert t.kind == tySequence
  let
    dest = c.genLvalue(n[1]) # ``seq`` argument
    len = c.genx(n[2])  # length argument
  c.gABx(n, opcNewSeq, dest, c.genType(t))
  c.gABx(n, opcNewSeq, len, 0)
  c.freeTemp(len)
  c.freeTemp(dest)

proc genNewSeqOfCap(c: var TCtx; n: CgNode; dest: var TDest) =
  prepare(c, dest, n, n.typ)

  let tmp = c.getTemp(n[1].typ)
  c.gABx(n, opcLdImmInt, tmp, 0)
  c.gABx(n, opcNewSeq, dest, c.genType(n.typ))
  c.gABx(n, opcNewSeq, tmp, 0)
  c.freeTemp(tmp)

proc genUnaryABC(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  prepare(c, dest, n, n.typ)
  let tmp = c.genx(n[1])
  c.gABC(n, opc, dest, tmp)
  c.freeTemp(tmp)

proc genUnaryABI(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode; imm: BiggestInt=0) =
  prepare(c, dest, n, n.typ)
  let tmp = c.genx(n[1])
  c.gABI(n, opc, dest, tmp, imm)
  c.freeTemp(tmp)


proc genBinaryABC(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  prepare(c, dest, n, n.typ)
  let
    tmp = c.genx(n[1])
    tmp2 = c.genx(n[2])
  c.gABC(n, opc, dest, tmp, tmp2)
  c.freeTemp(tmp)
  c.freeTemp(tmp2)

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
  prepare(c, dest, n, n.typ)
  let
    tmp = c.genx(n[1])
    tmp2 = c.genx(n[2])
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
  let
    dest = c.genLvalue(n[1])
    tmp = c.genx(n[2])
  c.gABC(n, opc, dest, tmp, 0)
  c.freeTemp(tmp)
  c.freeTemp(dest)

proc genParseOp(c: var TCtx; n: CgNode; dest: var TDest,
                opc: range[opcParseExprToAst..opcParseStmtToAst]) =
  ## Generates the code for a ``parseExpr``/``parseStmt`` magic call
  if dest.isUnset:
    dest = c.getTemp(n.typ)

  # the second parameter is a ``var`` parameter. We want to access the
  # register directly, so the used addr operation is skipped (if it hasn't
  # been eliminated by ``transf``)
  var x = n[2]
  if x.kind == cnkHiddenAddr:
    x = x.operand

  let
    in1 = c.genx(n[1])
    in2 = c.genx(x)

  c.gABC(n, opc, dest, in1, in2)
  c.freeTemp(in1)
  c.freeTemp(in2)

proc genVarargsABC(c: var TCtx; n: CgNode; dest: TRegister; opc: TOpcode) =
  var x = c.prc.getTempRange(n.len-1, slotTempUnknown)
  for i in 1..<n.len:
    var r: TRegister = x+i-1
    c.gen(n[i], r)
  c.gABC(n, opc, dest, x, n.len-1)
  c.freeTempRange(x, n.len-1)

proc isInt8Lit(n: CgNode): bool =
  ## Returns whether `n` represents an integer value (signed or
  ## unsigned) that fits into the range of an 8-bit signed integer.
  if n.kind in {cnkIntLit, cnkUIntLit}:
    let val = getInt(n)
    result = val >= low(int8) and val <= high(int8)

proc genAddSubInt(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode) =
  if n[2].isInt8Lit:
    let tmp = c.genx(n[1])
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABI(n, succ(opc), dest, tmp, n[2].intVal)
    c.freeTemp(tmp)
  else:
    genBinaryABC(c, n, dest, opc)
  c.genNarrow(n, dest)

proc genNumberConv(c: var TCtx, info: CgNode, dest, src: TRegister,
                   desttype, srctype: PType) =
  ## Generates and emits the code for an *unchecked* conversion between two
  ## numeric types.
  const
    Floats = {tyFloat..tyFloat64}
    Signed = {tyInt..tyInt64}
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

proc genConv(c: var TCtx; n, arg: CgNode; dest: var TDest) =
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

proc genToStr(c: var TCtx, n, arg: CgNode, dest: var TDest) =
  # TODO: don't use ``opcConv`` for to-string conversions
  # don't skip enum types
  const Skip = IrrelevantTypes - {tyEnum}
  prepare(c, dest, n, n.typ)
  let tmp = c.genx(arg)
  c.gABx(n, opcConv, dest, c.genTypeInfo(n.typ.skipTypes(Skip)))
  c.gABx(n, opcConv, tmp, c.genTypeInfo(arg.typ.skipTypes(Skip)))
  c.freeTemp(tmp)

proc genToSlice(c: var TCtx, val: TRegister, typ: PType, info: TLineInfo,
                dest: TRegister) =
  let L = c.getTemp(c.graph.getSysType(info, tyInt))
  c.gABC(info, opcLenSeq, L, val) # fetch the length of the input array
  c.gABC(info, opcSetLenSeq, dest, L) # resize the destination
  c.gABC(info, opcArrCopy, dest, val, L) # copy the contents

proc genObjConv(c: var TCtx, n: CgNode, dest: var TDest) =
  prepare(c, dest, n.typ)
  let
    tmp = genx(c, n.operand)
    desttyp = n.typ.skipTypes(IrrelevantTypes)

  case desttyp.kind
  of tyRef, tyObject:
    c.gABC(n, opcObjConv, dest, tmp)
    c.gABx(n, opcObjConv, 0, c.genType(desttyp))
  of tyPtr:
    c.gABC(n, opcFastAsgnComplex, dest, tmp) # register copy
    c.gABx(n, opcSetType, dest, c.genType(desttyp)) # set the new type
  else:
    unreachable()

  c.freeTemp(tmp)

proc genCard(c: var TCtx; n: CgNode; dest: var TDest) =
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

proc genCast(c: var TCtx, n, arg: CgNode, dest: var TDest) =
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

proc genCastIntFloat(c: var TCtx; n: CgNode; dest: var TDest) =
  const allowedIntegers = {tyInt..tyInt64, tyUInt..tyUInt64, tyChar}
  var signedIntegers = {tyInt..tyInt64}
  var unsignedIntegers = {tyUInt..tyUInt64, tyChar}
  let src = n.operand.typ.skipTypes(abstractRange)#.kind
  let dst = n.typ.skipTypes(abstractRange)#.kind
  let srcSize = getSize(c.config, src)
  let dstSize = getSize(c.config, dst)
  if src.kind in allowedIntegers and dst.kind in allowedIntegers:
    let tmp = c.genx(n.operand)
    if dest.isUnset: dest = c.getTemp(n.typ)
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
    let tmp = c.genx(n.operand)
    if dest.isUnset: dest = c.getTemp(n.typ)
    if dst.kind == tyFloat32:
      c.gABC(n, opcCastIntToFloat32, dest, tmp)
    else:
      c.gABC(n, opcCastIntToFloat64, dest, tmp)
    c.freeTemp(tmp)

  elif srcSize == dstSize and src.kind in {tyFloat, tyFloat32, tyFloat64} and
                           dst.kind in allowedIntegers:
    let tmp = c.genx(n.operand)
    if dest.isUnset: dest = c.getTemp(n.typ)
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
    let tmp = c.genx(n.operand)
    if dest.isUnset: dest = c.getTemp(n.typ)
    var imm: BiggestInt = if src.kind in PtrLikeKinds: 1 else: 2
    c.gABI(n, opcCastPtrToInt, dest, tmp, imm)
    c.freeTemp(tmp)
  elif src.kind in PtrLikeKinds + {tyInt} and dst.kind in PtrLikeKinds:
    let tmp = c.genx(n.operand)
    if dest.isUnset: dest = c.getTemp(n.typ)
    c.gABC(n, opcCastIntToPtr, dest, tmp)
    c.gABx(n, opcSetType, dest, c.genType(dst))
    c.freeTemp(tmp)
  elif src.kind == tyNil and dst.kind in NilableTypes:
    # supports casting nil literals to NilableTypes in VM
    # see #16024
    if dest.isUnset: dest = c.getTemp(n.typ)
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

proc genDataToAst(c: var TCtx, n: CgNode, dest: TRegister) =
  ## Generates and emits the bytecode for evaluating the expression `n` and
  ## deserializing the result to ``NimNode`` AST
  let tmp = c.genx(n)
  var typLit = TDest(-1)
  c.genTypeLit(n, n.typ, typLit)
  c.gABC(n, opcDataToAst, dest, tmp, typLit)
  c.freeTemp(typLit)
  c.freeTemp(tmp)

proc genVoidABC(c: var TCtx, n: CgNode, dest: TDest, opcode: TOpcode) =
  unused(c, n, dest)
  var
    tmp1 = c.genx(n[1])
    tmp2 = c.genx(n[2])
    tmp3 = c.genx(n[3])
  c.gABC(n, opcode, tmp1, tmp2, tmp3)
  c.freeTemp(tmp1)
  c.freeTemp(tmp2)
  c.freeTemp(tmp3)

proc genVoidBC(c: var TCtx, n: CgNode, dest: TDest, opcode: TOpcode) =
  ## Special convention used by some macrocache-related ops
  unused(c, n, dest)
  var
    tmp1 = c.genx(n[1])
    tmp2 = c.genx(n[2])
  c.gABC(n, opcode, 0, tmp1, tmp2)
  c.freeTemp(tmp1)
  c.freeTemp(tmp2)

proc loadInt(c: var TCtx, n: CgNode, dest: TRegister, val: Int128) =
  ## Loads the integer `val` into `dest`, choosing the most efficient way to
  ## do so.
  if val in regBxMin-1..regBxMax:
    # can be loaded as an immediate
    c.gABx(n, opcLdImmInt, dest, toInt(val))
  else:
    # requires a constant
    c.gABx(n, opcLdConst, dest, c.toIntCnst(val))

proc genSetElem(c: var TCtx, n: CgNode, first: Int128): TRegister =
  result = c.getTemp(n.typ)

  if first != 0:
    if n.kind in {cnkIntLit, cnkUIntLit}:
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

proc genSetElem(c: var TCtx, n: CgNode, typ: PType): TRegister {.inline.} =
  ## `typ` is the type to derive the lower bound from
  let t = typ.skipTypes(abstractInst)
  assert t.kind == tySet

  # `first` can't be reliably derived from `n.typ` since the type may not
  # match the set element type. This happens with the set in a
  # `nkCheckedFieldExpr` for example
  let first = c.config.firstOrd(t)
  genSetElem(c, n, first)

func fitsRegister(t: PType): bool =
  let st = t.skipTypes(IrrelevantTypes + {tyRange})
  st.kind in { tyBool, tyInt..tyUInt64, tyChar, tyPtr, tyPointer} or
    (st.sym != nil and st.sym.magic == mPNimrodNode) # NimNode goes into register too

proc ldNullOpcode(t: PType): TOpcode =
  assert t != nil
  if fitsRegister(t): opcLdNullReg else: opcLdNull

proc whichAsgnOpc(n: CgNode; requiresCopy = true): TOpcode =
  case n.typ.skipTypes(IrrelevantTypes + {tyRange}).kind
  of tyBool, tyChar, tyInt..tyInt64, tyUInt..tyUInt64:
    opcAsgnInt
  of tyFloat..tyFloat64:
    opcAsgnFloat
  else:
    # XXX: always require a copy, fastAsgn is broken in the VM
    opcAsgnComplex
    #(if requiresCopy: opcAsgnComplex else: opcFastAsgnComplex)

func usesRegister(p: BProc, n: CgNode): bool =
  ## Analyses and returns whether the value of the location named by l-value
  ## expression `n` is stored in a register instead of a memory location
  # XXX: instead of using a separate analysis, compute and return this as part
  #      of ``genLValue`` and
  case n.kind
  of cnkLocal:
    usesRegister(p, n.local)
  of cnkProc, cnkConst, cnkGlobal:
    false
  of cnkDeref, cnkDerefView, cnkFieldAccess, cnkArrayAccess, cnkTupleAccess,
     cnkCheckedFieldAccess, cnkConv, cnkObjDownConv, cnkObjUpConv:
    false
  else:
    unreachable(n.kind)

proc genNoLoad(c: var TCtx, n: CgNode): tuple[reg: TRegister, isDirect: bool] =
  ## Similar to ``genLValue``, but also returns whether the register storing
  ## the result stores a handle or a value.
  var dest = noDest
  genLvalue(c, n, dest)
  result = (TRegister(dest), usesRegister(c.prc, n))

proc genLoc(c: var TCtx, n: CgNode): Loc =
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

proc finish(c: var TCtx, info: CgNode, loc: sink Loc) =
  ## Wraps up the modification to `loc` by writing the register-stored
  ## value back to the source memory location.
  if loc.handleReg != -1:
    # a write-back is required
    c.gABC(info, opcWrLoc, loc.handleReg, loc.val)
    c.freeTemp(loc.handleReg)

  c.freeTemp(loc.val)

proc genMagic(c: var TCtx; n: CgNode; dest: var TDest; m: TMagic) =
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
    c.genNew(n, dest)
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
  of mEqRef:
    genBinaryABC(c, n, dest, opcEqRef)
  of mEqProc:
    # XXX: lower this as a MIR pass
    if skipTypes(n[1].typ, abstractInst).callConv == ccClosure:
      prepare(c, dest, n.typ)
      # compare both tuple elements:
      let
        a = c.genx(n[1])
        b = c.genx(n[2])
        fieldA = c.getTemp(slotTempComplex)
        fieldB = c.getTemp(slotTempComplex)

      # load the prc fields:
      c.gABC(n, opcLdObj, fieldA, a, 0)
      c.gABC(n, opcLdObj, fieldB, b, 0)
      # ``dest = fieldA == fieldB``
      c.gABC(n, opcEqRef, dest, fieldA, fieldB)
      # ``if not dest: goto end``
      let jmp = c.xjmp(n, opcFJmp, dest)
      # load the env fields:
      c.gABC(n, opcLdObj, fieldA, a, 1)
      c.gABC(n, opcLdObj, fieldB, b, 1)
      # ``dest = fieldA == fieldB``
      c.gABC(n, opcEqRef, dest, fieldA, fieldB)
      c.patch(jmp)

      c.freeTemp(fieldB)
      c.freeTemp(fieldA)
      c.freeTemp(b)
      c.freeTemp(a)
    else:
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
  of mIsNil:
    # XXX: lower this earlier
    if skipTypes(n[1].typ, abstractInst).callConv == ccClosure:
      # test wether the procedure address is nil
      prepare(c, dest, n.typ)
      let
        tmp = c.genx(n[1])
        tmp2 = c.getTemp(slotTempComplex)
      c.gABC(n, opcLdObj, tmp2, tmp, 0) # load the field handle
      c.gABC(n, opcIsNil, dest, tmp2)
      c.freeTemp(tmp2)
      c.freeTemp(tmp)
    else:
      genUnaryABC(c, n, dest, opcIsNil)
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
  of mWasMoved:
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
    let
      numArgs = n.len - 2
      x = c.prc.getTempRange(numArgs, slotTempUnknown)
    for i in 0..<numArgs:
      var r: TRegister = x+i
      c.gen(n[i + 2], r)
    c.gABC(n, opcEcho, x, numArgs)
    c.freeTempRange(x, numArgs)
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
      x = c.prc.getTempRange(numArgs, slotTempUnknown)

    # pass the template symbol as the first argument
    var callee = TDest(x)
    c.genLit(call[1], c.toNodeCnst(newSymNode(call[1].sym)), callee)
    # XXX: don't create a new symbol node here; in ``transformExpandToAst``,
    #      emit an ``nkNimNodeLit`` for the callee instead

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

    # load the env reference and dereference it:
    c.gABC(n, opcLdObj, env, tmp, 1)
    c.gABC(n, opcLdDeref, env, env)

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
  of mChckRange:
    let
      tmp0 = c.genx(n[1])
      tmp1 = c.genx(n[2])
      tmp2 = c.genx(n[3])
    c.gABC(n, opcRangeChck, tmp0, tmp1, tmp2)
    c.freeTemp(tmp1)
    c.freeTemp(tmp2)
    if dest >= 0:
      gABC(c, n, whichAsgnOpc(n), dest, tmp0)
      c.freeTemp(tmp0)
    else:
      dest = tmp0
  else:
    # mGCref, mGCunref, mFinished, etc.
    fail(n.info, vmGenDiagCodeGenUnhandledMagic, m)

proc genDeref(c: var TCtx, n: CgNode, dest: var TDest; load = true) =
    let tmp = c.genx(n.operand)
    if dest.isUnset: dest = c.getTemp(n.typ)
    gABC(c, n, opcLdDeref, dest, tmp)

    if needsRegLoad():
      c.genRegLoad(n, dest, dest)
    c.freeTemp(tmp)

func setSlot(p: var BProc; v: LocalId): TRegister {.discardable.} =
  result = getFreeRegister(p, slotFixedVar, start = 1)
  p[v].reg = result

func cannotEval(c: TCtx; n: CgNode) {.noinline, noreturn.} =
  # best-effort translation to ``PNode`` for improved error messages
  # XXX: move this kind of error reporting outside of vmgen instead
  {.cast(noSideEffect).}:
    let ast =
      if n.kind in {cnkProc, cnkConst, cnkGlobal}:
        newSymNode(n.sym, n.info)
      else:
        nil # give up

  raiseVmGenError(vmGenDiagCannotEvaluateAtComptime, ast)

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
  if s.kind in {skProc, skFunc, skConverter, skMethod,
                  skIterator} and sfForward in s.flags:
    cannotEval(c, n)


proc genDiscrVal(c: var TCtx, discr, n: CgNode, oty: PType): TRegister =
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

  if n.kind in {cnkIntLit, cnkUIntLit}:
    # Discriminator value is known at compile-time

    let b = findMatchingBranch(recCase, getInt(n))
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
        let b = genBranchLit(c, branch)
        c.gABx(n, opcBranch, tmp, b)
        let elsePos = c.xjmp(n, opcFJmp, tmp)
        c.gABx(n, opcLdImmInt, bIReg, bI)
        if i < recCase.len-1:
          endings.add(c.xjmp(n, opcJmp, 0))
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


proc genAsgnSource(c: var TCtx, n: CgNode, wantsPtr: bool): TRegister

proc genFieldAsgn(c: var TCtx, obj: TRegister; le, ri: CgNode) =
  c.config.internalAssert(le.kind == cnkFieldAccess)

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

func isPtrView(n: CgNode): bool =
  ## Analyses whether the expression `n` evaluates to a direct view that is
  ## represented via an address value instead of a handle. The former is the
  ## case for both globals that are direct views, and for direct views stored
  ## in compound types
  case n.kind
  of cnkConst, cnkGlobal:
    true
  of cnkLocal:
    false
  of cnkFieldAccess, cnkArrayAccess, cnkTupleAccess, cnkCheckedFieldAccess:
    true
  of cnkHiddenAddr, cnkCall, cnkCheckedCall:
    false
  else:
    unreachable(n.kind)

proc genAsgnSource(c: var TCtx, n: CgNode, wantsPtr: bool): TRegister =
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
      let tmp = result
      result = c.getTemp(n.typ)
      c.gABC(n, opcLdDeref, result, tmp)
      c.freeTemp(tmp)
    elif wantsPtr and not isPtr:
      # turn the handle into an address. The register can't be reused
      # because it might be non-temporary one
      let tmp = result
      result = c.getTemp(n.typ)
      c.gABC(n, opcAddr, result, tmp)
      c.freeTemp(tmp)

proc genAsgnToGlobal(c: var TCtx, le, ri: CgNode) =
  ## Generates and emits the code for an assignment where the LHS is the symbol
  ## of a global location.
  var dest = noDest
  # we're only interested in the *handle* (i.e. identity) of the location, so
  # don't load it
  c.genSym(le, dest, load=false)

  if true:
    # global views use pointers internally
    let b = genAsgnSource(c, ri, wantsPtr=true)
    c.gABC(le, opcWrLoc, dest, b)
    c.freeTemp(b)

  c.freeTemp(dest)

proc genAsgnToLocal(c: var TCtx, le, ri: CgNode) =
  let dest = c.prc[le.local].reg
  if true:
    if usesRegister(c.prc, le.local):
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
        opc = (if isDirectView(le.typ): opcFastAsgnComplex else: opcWrLoc)
        b = genAsgnSource(c, ri, wantsPtr = false)
      c.gABC(le, opc, dest, b)
      c.freeTemp(b)

proc genDerefView(c: var TCtx, n: CgNode, dest: var TDest; load = true) =
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

proc genAsgn(c: var TCtx; le, ri: CgNode; requiresCopy: bool) =
  case le.kind
  of cnkArrayAccess:
    let
      typ = le[0].typ.skipTypes(abstractVar).kind
      dest = c.genx(le[0])
      idx = c.genIndex(le[1], le[0].typ)
      tmp = c.genAsgnSource(ri, wantsPtr = true)
      opc =
        case typ
        of tyString, tyCstring: opcWrStrIdx
        else:                   opcWrArr

    c.gABC(le, opc, dest, idx, tmp)
    c.freeTemp(tmp)
    c.freeTemp(idx)
    c.freeTemp(dest)
  of cnkTupleAccess:
    let
      dest = c.genx(le[0])
      tmp = c.genAsgnSource(ri, wantsPtr = true)
    c.gABC(le, opcWrObj, dest, le[1].intVal.TRegister, tmp)
    c.freeTemp(tmp)
    c.freeTemp(dest)
  of cnkCheckedFieldAccess:
    let objR = genCheckedObjAccessAux(c, le)
    c.genFieldAsgn(objR, le[0], ri)
    # c.freeTemp(idx) # BUGFIX, see nkDotExpr
    c.freeTemp(objR)
  of cnkFieldAccess:
    let dest = c.genx(le[0])
    c.genFieldAsgn(dest, le, ri)
    # c.freeTemp(idx) # BUGFIX: idx is an immediate (field position), not a register
    c.freeTemp(dest)
  of cnkDerefView:
    # an assignment to a view's underlying location. The source cannot be a
    # view, so using ``genAsgnSource`` is unnecessary
    var dest = noDest
    genDerefView(c, le.operand, dest, load=false) # we need a handle, hence ``false``
    let tmp = c.genx(ri)

    c.gABC(le, opcWrLoc, dest, tmp)
    c.freeTemp(tmp)
    c.freeTemp(dest)
  of cnkDeref:
    # same as for ``nkHiddenDeref``, the source cannot be a view
    let
      dest = c.genx(le.operand)
      tmp = c.genx(ri)
    c.gABC(le, opcWrDeref, dest, 0, tmp)
    c.freeTemp(dest)
    c.freeTemp(tmp)
  of cnkObjDownConv, cnkObjUpConv:
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
  of cnkConv, cnkHiddenConv:
    # these conversions don't result in a lvalue of different run-time type, so
    # they're skipped
    genAsgn(c, le.operand, ri, requiresCopy)
  of cnkGlobal:
    checkCanEval(c, le)
    genAsgnToGlobal(c, le, ri)
  of cnkLocal:
    genAsgnToLocal(c, le, ri)
  else:
    unreachable(le.kind)

proc genTypeLit(c: var TCtx; info: CgNode, t: PType; dest: var TDest) =
  var n = newNode(nkType)
  n.typ = t
  genLit(c, info, toNodeCnst(c, n), dest)

proc importcCond*(c: TCtx; s: PSym): bool {.inline.} =
  ## return true to importc `s`, false to execute its body instead (refs #8405)
  if sfImportc in s.flags:
    if s.kind in routineKinds:
      return getBody(c.graph, s).kind == nkEmpty

proc useGlobal(c: var TCtx, n: CgNode): int =
    ## Resolves the global identified by symbol node `n` to the ID that
    ## identifies it at run-time. If using the global is illegal (because
    ## it's an importc'ed variable, for example), an error is raised.
    let s = n.sym

    if importcCondVar(s) or c.importcCond(s):
      # Using importc'ed symbols on the left or right side of an expression is
      # not allowed
      fail(n.info, vmGenDiagCannotImportc, sym = s)

    if s.id in c.linking.symToIndexTbl:
      # XXX: double table lookup
      result = c.linking.symToIndexTbl[s.id].int
    else:
      # a global that is not accessible in the current context
      cannotEval(c, n)

proc genSym(c: var TCtx; n: CgNode; dest: var TDest; load = true) =
  ## Generates and emits the code for loading either the value or handle of
  ## the location named by symbol or local node `n` into the `dest` register.
  case n.kind
  of cnkConst:
    let s = n.sym
    prepare(c, dest, n.typ)

    let pos = int c.linking.lookup(s)
    if load and fitsRegister(n.typ):
      let cc = c.getTemp(n.typ)
      c.gABx(n, opcLdCmplxConst, cc, pos)
      c.genRegLoad(n, dest, cc)
      c.freeTemp(cc)
    else:
      c.gABx(n, opcLdCmplxConst, dest, pos)

    discard genType(c, n.typ) # make sure the type exists
  of cnkGlobal:
    # a global location
    let s = n.sym
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
  of cnkLocal:
      let local = c.prc[n.local].reg
      internalAssert(c.config, c.prc.regInfo[local].kind < slotSomeTemp)
      if usesRegister(c.prc, n.local) or not load or not fitsRegister(n.typ):
        if dest.isUnset:
          dest = local
        else:
          # despite the name, ``opcFastAsgnComplex`` currently performs a
          # register copy, which is exactly what we need here
          c.gABC(n, opcFastAsgnComplex, dest, local)
      else:
        prepare(c, dest, n.typ)
        c.genRegLoad(n, dest, local)
  else:
    unreachable()

proc genSymAddr(c: var TCtx, n: CgNode, dest: var TDest) =
  ## Generates and emits the code for taking the address of the location
  ## identified by the symbol or local node `n`.
  assert dest != noDest
  case n.kind
  of cnkConst:
    let
      pos = int c.linking.lookup(n.sym)
      tmp = c.getTemp(slotTempComplex)
    c.gABx(n, opcLdCmplxConst, tmp, pos)
    c.gABC(n, opcAddr, dest, tmp)
    c.freeTemp(tmp)
    discard genType(c, n.typ) # make sure the type exists
  of cnkGlobal:
    let
      pos = useGlobal(c, n)
      tmp = c.getTemp(slotTempComplex)
    c.gABx(n, opcLdGlobal, tmp, pos)
    c.gABC(n, opcAddr, dest, tmp)
    c.freeTemp(tmp)
  of cnkLocal:
    let local = c.prc[n.local].reg
    c.gABC(n, opcAddr, dest, local)
  else:
    unreachable()

proc genArrAccessOpcode(c: var TCtx; n: CgNode; dest: var TDest; opc: TOpcode; load = true) =
  let
    a = c.genx(n[0])
    b = c.genIndex(n[1], n[0].typ)

  prepare(c, dest, n.typ)
  if opc in {opcLdArrAddr, opcLdStrIdx}:
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
  c.freeTemp(a)
  c.freeTemp(b)

proc genFieldAccessAux(c: var TCtx; n: CgNode; a, b: TRegister, dest: var TDest; load = true) =
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

proc genFieldAccess(c: var TCtx; n: CgNode; pos: int, dest: var TDest;
                    load = true) =
  ## Generates and emits the code for the record-like access `n`. The handle
  ## value is written to the `dest` register.
  assert n.kind in {cnkFieldAccess, cnkTupleAccess}
  let a = c.genx(n[0])
  genFieldAccessAux(c, n, a, pos, dest, load)
  c.freeTemp(a)

proc genFieldAddr(c: var TCtx, n, obj: CgNode, fieldPos: int, dest: TDest) =
  let obj = c.genx(obj)
  c.gABC(n, opcLdObjAddr, dest, obj, fieldPos)
  c.freeTemp(obj)

proc genCheckedObjAccessAux(c: var TCtx; n: CgNode): TRegister =
  internalAssert(
    c.config,
    n.kind == cnkCheckedFieldAccess,
    "genCheckedObjAccessAux requires checked field node")

  # ``cnkObjAccess`` to access the requested field
  let accessExpr = n[0]
  # the call to check if the discriminant is valid
  var checkExpr = n[1]

  let negCheck = checkExpr[0].magic == mNot
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
  let fieldName = accessExpr[1].sym.name.s
  let msg = genFieldDefect(c.config, fieldName, disc.sym)
  c.genLit(accessExpr[1], toStringCnst(c, msg), msgReg)
  # repr for discriminator value
  c.gABx(n, opcRepr, discrStrReg, c.genTypeInfo(disc.typ))
  c.gABC(n, opcRepr, discrStrReg, discVal)
  c.gABC(n, opcInvalidField, msgReg, discrStrReg)
  c.freeTemp(discVal)
  c.freeTemp(msgReg)
  c.freeTemp(discrStrReg)
  c.patch(lab1)

proc genCheckedObjAccess(c: var TCtx; n: CgNode; dest: var TDest; load = true) =
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

  c.freeTemp(objR)

proc genArrAccess(c: var TCtx; n: CgNode; dest: var TDest; load = true) =
  case n[0].typ.skipTypes(abstractVar).kind
  of tyString, tyCstring:
    if load:
      # no need to pass `load`; the result of a string access is always
      # stored in a register
      genArrAccessOpcode(c, n, dest, opcLdStrIdx)
    else:
      # use the ``opcLdArr`` operation to get a handle to the ``char``
      # location
      genArrAccessOpcode(c, n, dest, opcLdArr, load=false)
  of tyArray, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray:
    genArrAccessOpcode(c, n, dest, opcLdArr, load)
  else:
    unreachable()

proc genArrayAddr(c: var TCtx, n: CgNode, dest: var TDest) =
  ## Generates the code for loading the address of a bracket expression (i.e.
  ## ``addr x[0]``)
  assert not dest.isUnset
  case n[0].typ.skipTypes(abstractInst).kind
  of tyString, tyCstring, tyArray, tySequence, tyOpenArray, tyVarargs:
    genArrAccessOpcode(c, n, dest, opcLdArrAddr)
  else:
    unreachable()

proc genAddr(c: var TCtx, src, n: CgNode, dest: var TDest) =
  ## Generates and emits the code for taking the address of lvalue expression
  ## `n`. `src` provides the type information of the destination, plus the line
  ## information to use.
  case n.kind
  of cnkConst, cnkGlobal, cnkLocal:
    prepare(c, dest, src.typ)
    genSymAddr(c, n, dest)
  of cnkFieldAccess:
    prepare(c, dest, src.typ)
    genFieldAddr(c, n, n[0], genField(c, n[1]), dest)
  of cnkArrayAccess:
    prepare(c, dest, src.typ)
    genArrayAddr(c, n, dest)
  of cnkTupleAccess:
    prepare(c, dest, src.typ)
    genFieldAddr(c, n, n[0], n[1].intVal.TRegister, dest)
  of cnkCheckedFieldAccess:
    prepare(c, dest, src.typ)

    let obj = genCheckedObjAccessAux(c, n)
    c.gABC(src, opcLdObjAddr, dest, obj, genField(c, n[0][1]))
    c.freeTemp(obj)
  of cnkDerefView:
    # taking the address of a view's or ``var`` parameter's underlying
    # location
    assert isLocView(n.operand.typ)
    if isPtrView(n.operand):
      # the view is stored as an address; treat the deref as a no-op
      genLvalue(c, n.operand, dest)
    else:
      prepare(c, dest, src.typ)
      let tmp = genx(c, n.operand) # skip the deref
      c.gABC(src, opcAddr, dest, tmp)
      c.freeTemp(tmp)
  of cnkDeref:
    prepare(c, dest, src.typ)
    # after transformation (i.e. ``transf``), the only ``addr(deref(x))``
    # sequences left are those that can't be collapsed, and since a ``ptr``
    # is not interchangeable with ``ref`` we need the deref
    var tmp = noDest
    genDeref(c, n, tmp, load=false)
    c.gABC(src, opcAddr, dest, tmp)
    c.freeTemp(tmp)
  of cnkConv:
    # an l-value conversion. Take the address of the source expression
    genAddr(c, src, n.operand, dest)
  of cnkObjDownConv, cnkObjUpConv:
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
  else:
    unreachable(n.kind)

proc genLvalue(c: var TCtx, n: CgNode, dest: var TDest) =
  ## Generates and emits the code for computing the handle of the location
  ## named by l-value expression `n`. If the expression names a location that
  ## is stored in a VM memory cell, `dest` will always store a handle --
  ## address-based views are dereferenced.
  ##
  ## Note that in the case of locals backed by registers, `dest` will store
  ## its value instead of a handle.
  case n.kind
  of cnkConst, cnkGlobal, cnkLocal:
    c.genSym(n, dest, load=false)
  of cnkFieldAccess:
    genFieldAccess(c, n, genField(c, n[1]), dest, load=false)
  of cnkCheckedFieldAccess:
    genCheckedObjAccess(c, n, dest, load=false)
  of cnkArrayAccess:
    genArrAccess(c, n, dest, load=false)
  of cnkTupleAccess:
    genFieldAccess(c, n, n[1].intVal.int, dest, load=false)
  of cnkConv:
    # if a conversion reaches here, it must be an l-value conversion. They
    # don't map to any bytecode, so we skip them
    genLvalue(c, n.operand, dest)
  of cnkObjDownConv, cnkObjUpConv:
    # these conversions are *not* no-ops, as they produce a handle of different
    # type
    gen(c, n, dest)
  of cnkDerefView:
    assert isLocView(n.operand.typ)
    if isPtrView(n.operand):
      # we want a handle (``rkHandle``), but the input view uses a pointer
      # (``rkAddress``) internally. Turn it into a handle by dereferencing it
      prepare(c, dest, n.operand.typ)
      let tmp = c.genx(n.operand)
      c.gABC(n, opcLdDeref, dest, tmp)
      c.freeTemp(tmp)
    else:
      # the operand is a handle already; treat the deref as a no-op
      genLvalue(c, n.operand, dest)
  of cnkDeref:
    genDeref(c, n, dest, load=false)
  else:
    unreachable(n.kind)

proc genDef(c: var TCtx; a: CgNode) =
        let
          s   = a[0].local
          typ = a[0].typ
        if true:
          if a[1].kind == cnkEmpty:
            # no initializer; only setup the register (and memory location,
            # if used)
            let reg = setSlot(c.prc, s)
            let opc = if usesRegister(c.prc, s): opcLdNullReg
                      else: opcLdNull

            c.gABx(a, opc, reg, c.genType(typ))
          elif classifyBackendView(typ) == bvcSequence:
            # XXX: either a shallow copy or construction of an ``openArray``
            #      should take place here instead, but both are things not yet
            #      supported by the VM
            let src = genx(c, a[1])
            if c.prc.regInfo[src].kind in {slotFixedVar, slotFixedLet}:
              # the register cannot be reused. However, since the source must
              # outlive the slice, copying the handle is fine
              let dst = setSlot(c.prc, s)
              c.gABC(a, opcFastAsgnComplex, dst, src)
            else:
              # promote to local
              c.prc.regInfo[src].kind = slotFixedLet
              c.prc[a[0].local].reg = src
          else:
            let reg = setSlot(c.prc, s)
            # XXX: checking for views here is wrong but necessary
            if not usesRegister(c.prc, s) and not isDirectView(typ):
              # only setup a memory location if the local uses one
              c.gABx(a, opcLdNull, reg, c.genType(typ))

            # views and locals backed by registers don't need explicit
            # initialization logic here -- the assignment takes care of that
            genAsgnToLocal(c, a[0], a[1])

proc genArrayConstr(c: var TCtx, n: CgNode, dest: var TDest) =
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

proc genSetConstr(c: var TCtx, n: CgNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)
  c.gABx(n, opcLdNull, dest, c.genType(n.typ))
  # XXX: since `first` stays the same across the loop, we could invert
  #      the loop around `genSetElem`'s logic...
  let first = firstOrd(c.config, n.typ.skipTypes(abstractInst))
  for x in n:
    if x.kind == cnkRange:
      let a = c.genSetElem(x[0], first)
      let b = c.genSetElem(x[1], first)
      c.gABC(n, opcInclRange, dest, a, b)
      c.freeTemp(b)
      c.freeTemp(a)
    else:
      let a = c.genSetElem(x, first)
      c.gABC(n, opcIncl, dest, a)
      c.freeTemp(a)

proc genObjConstr(c: var TCtx, n: CgNode, dest: var TDest) =
  prepare(c, dest, n, n.typ)
  let t = n.typ.skipTypes(abstractRange-{tyTypeDesc})
  var refTemp: TDest
  if t.kind == tyRef:
    refTemp = c.getTemp(t[0]) # The temporary register to hold the
                              # dereferenced location
    c.gABx(n, opcNew, dest, c.genType(t))
    c.gABC(n, opcLdDeref, refTemp, dest)
    swap(refTemp, dest)

  for it in n.items:
    assert it.kind == cnkBinding and it[0].kind == cnkField
    if true:
      let idx = genField(c, it[0])
      var tmp: TRegister
      var opcode: TOpcode
      if sfDiscriminant notin it[0].sym.flags:
        tmp = c.genAsgnSource(it[1], wantsPtr = true)
        opcode = opcWrObj
        let
          le = it[0].sym.typ
        if le.kind == tyOpenArray:
          # XXX: once the to-slice operator is passed to ``vmgen``, integrate
          #      the conversion into ``genAsgnSource``
          let tmp2 = c.getFullTemp(it[0], le)
          c.genToSlice(tmp, le, it[1].info, tmp2)
          c.freeTemp(tmp)
          tmp = TRegister(tmp2)
      else:
        tmp = c.genDiscrVal(it[0], it[1], n.typ)
        opcode = opcInitDisc
      c.gABC(it[1], opcode, dest, idx, tmp)
      c.freeTemp(tmp)

  if t.kind == tyRef:
    swap(refTemp, dest)
    c.freeTemp(refTemp)

proc genTupleConstr(c: var TCtx, n: CgNode, dest: var TDest) =
  if dest.isUnset: dest = c.getTemp(n.typ)
  if n.typ.kind != tyTypeDesc:
    c.gABx(n, opcLdNull, dest, c.genType(n.typ))
    # XXX x = (x.old, 22)  produces wrong code ... stupid self assignments
    for i, it in n.pairs:
      let tmp = c.genAsgnSource(it, wantsPtr = true)
      c.gABC(it, opcWrObj, dest, i.TRegister, tmp)
      c.freeTemp(tmp)

proc genClosureConstr(c: var TCtx, n: CgNode, dest: var TDest) =
  prepare(c, dest, n, n.typ)
  let tmp = c.genx(n[0])
  c.gABC(n, opcWrObj, dest, 0, tmp)
  c.freeTemp(tmp)

  let typ = c.typeInfoCache.types.find(c.typeInfoCache.rootRef)
  # the type of the environment value is wrong, the VM expects
  # the value to be of ``RootRef`` type. We're correcting this
  # here by emitting a conversion
  if n[1].kind == cnkNilLit:
    let tmp = c.getTemp(slotTempComplex)
    c.gABx(n[1], opcLdNull, tmp, typ)
    c.gABC(n[1], opcWrObj, dest, 1, tmp)
    c.freeTemp(tmp)
  else:
    let
      envTmp = c.genx(n[1])
      tmp2 = c.getTemp(slotTempComplex)
    c.gABC(n[1], opcObjConv, tmp2, envTmp)
    c.gABx(n[1], opcObjConv, 0, typ)
    c.gABC(n[1], opcWrObj, dest, 1, tmp2)
    c.freeTemp(tmp2)
    c.freeTemp(envTmp)

proc gen(c: var TCtx; n: CgNode; dest: var TDest) =
  when defined(nimCompilerStacktraceHints):
    frameMsg c.config, n

  case n.kind
  of cnkProc:
    let s = n.sym
    checkCanEval(c, n)
    if importcCond(c, s) and lookup(c.linking.callbackKeys, s) == -1:
      fail(n.info, vmGenDiagCannotImportc, sym = s)

    genProcLit(c, n, s, dest)
  of cnkConst, cnkGlobal, cnkLocal:
    genSym(c, n, dest)
  of cnkCall, cnkCheckedCall:
    let magic = getMagic(n)
    if magic != mNone:
      genMagic(c, n, dest, magic)
    elif n[0].kind == cnkProc and n[0].sym.kind == skMethod and
         c.mode != emStandalone:
        # XXX: detect and reject this earlier -- it's not a code
        #      generation error
        fail(n.info, vmGenDiagCannotCallMethod, sym = n[0].sym)
    else:
      genCall(c, n, dest)
      clearDest(c, n, dest)
  of cnkIntLit, cnkUIntLit:
    prepare(c, dest, n.typ)
    c.loadInt(n, dest, getInt(n))
  of cnkFloatLit, cnkStrLit: genLit(c, n, dest)
  of cnkNilLit:
    if true:
      let t = n.typ.skipTypes(abstractInst)
      internalAssert(c.config,
        t.kind in {tyPtr, tyRef, tyPointer, tyNil, tyProc, tyCstring},
        n.info,
        $t.kind)
      if dest.isUnset: dest = c.getTemp(t)
      c.gABx(n, ldNullOpcode(t), dest, c.genType(n.typ))
  of cnkAstLit:
    # the VM does not copy the tree when loading a ``PNode`` constant (which
    # is correct). ``NimNode``s not marked with `nfSem` can be freely modified
    # inside macros, so in order to prevent mutations of the AST part of the
    # constant, we perform a defensive tree copy before assigning the literal
    # to the destination
    if dest.isUnset:
      dest = c.getTemp(n.typ)

    var tmp: TDest = c.getTemp(n.typ)
    c.genLit(n, c.toNodeCnst(n.astLit), tmp)
    c.gABC(n, opcNCopyNimTree, dest, tmp)
    freeTemp(c, tmp)
  of cnkAsgn, cnkFastAsgn:
    unused(c, n, dest)
    genAsgn(c, n[0], n[1], n.kind == cnkAsgn)
  of cnkFieldAccess: genFieldAccess(c, n, genField(c, n[1]), dest)
  of cnkCheckedFieldAccess: genCheckedObjAccess(c, n, dest)
  of cnkArrayAccess: genArrAccess(c, n, dest)
  of cnkTupleAccess: genFieldAccess(c, n, n[1].intVal.int, dest)
  of cnkDeref: genDeref(c, n, dest)
  of cnkAddr: genAddr(c, n, n.operand, dest)
  of cnkDerefView:
    assert isLocView(n.operand.typ)
    # a view indirection
    genDerefView(c, n.operand, dest)
  of cnkHiddenAddr:
    assert isLocView(n.typ)
    # load the source operand as a handle
    genLvalue(c, n.operand, dest)
  of cnkIfStmt:
    unused(c, n, dest)
    genIf(c, n)
  of cnkCaseStmt:
    unused(c, n, dest)
    genCase(c, n)
  of cnkRepeatStmt:
    unused(c, n, dest)
    genRepeat(c, n)
  of cnkBlockStmt:
    unused(c, n, dest)
    genBlock(c, n)
  of cnkReturnStmt:
    genReturn(c, n)
  of cnkRaiseStmt:
    genRaise(c, n)
  of cnkBreakStmt:
    genBreak(c, n)
  of cnkTryStmt:
    unused(c, n, dest)
    genTry(c, n)
  of cnkStmtList:
    unused(c, n, dest)
    for x in n: gen(c, x)
  of cnkVoidStmt:
    unused(c, n, dest)
    gen(c, n[0])
  of cnkHiddenConv, cnkConv:
    genConv(c, n, n.operand, dest)
  of cnkObjDownConv, cnkObjUpConv:
    genObjConv(c, n, dest)
  of cnkDef:
    unused(c, n, dest)
    genDef(c, n)
  of cnkEmpty:
    unused(c, n, dest)
  of cnkStringToCString, cnkCStringToString:
    gen(c, n.operand, dest)
  of cnkArrayConstr: genArrayConstr(c, n, dest)
  of cnkSetConstr: genSetConstr(c, n, dest)
  of cnkObjConstr: genObjConstr(c, n, dest)
  of cnkTupleConstr: genTupleConstr(c, n, dest)
  of cnkClosureConstr: genClosureConstr(c, n, dest)
  of cnkCast:
    if allowCast in c.features:
      genCast(c, n, n.operand, dest)
    else:
      genCastIntFloat(c, n, dest)
  of cnkType:
    genTypeLit(c, n, n.typ, dest)
  of cnkPragmaStmt, cnkAsmStmt, cnkEmitStmt:
    unused(c, n, dest)
  of cnkInvalid, cnkMagic, cnkRange, cnkExcept, cnkFinally, cnkBranch,
     cnkBinding, cnkLabel, cnkStmtListExpr, cnkField:
    unreachable(n.kind)

proc initProc(c: TCtx, owner: PSym, body: sink Body): BProc =
  ## `owner` is the procedure the body belongs to, or nil, if its something
  ## standalone.
  result = BProc(sym: owner, body: body)
  result.bestEffort =
    if owner != nil: owner.info
    else:            c.module.info
  result.locals.synchronize(result.body.locals)

  # analyse what locals require indirections:
  analyseIfAddressTaken(result.body.code, result)

proc genStmt*(c: var TCtx; body: sink Body): Result[int, VmGenDiag] =
  c.prc = initProc(c, nil, body)
  let n = c.prc.body.code

  var d: TDest = -1
  try:
    c.gen(n, d)
  except VmGenError as e:
    return typeof(result).err(move e.diag)

  c.config.internalAssert(d < 0, n.info, "VM problem: dest register is set")
  result = typeof(result).ok(c.prc.regInfo.len)

proc genExpr*(c: var TCtx; body: sink Body): Result[int, VmGenDiag] =
  ## Generates and emits the code for a standalone expression.
  c.prc = initProc(c, nil, body)
  let n = c.prc.body.code

  var d: TDest = -1
  try:
    if n.kind == cnkStmtListExpr:
      # special case the expression here so that ``gen`` doesn't have to
      for i in 0..<n.len-1:
        c.gen(n[i])

      c.gen(n[^1], d)
    else:
      c.gen(n, d)
  except VmGenError as e:
    return typeof(result).err(move e.diag)

  # the destination register not being set likely indicate that `n` is not an
  # expression
  c.config.internalAssert(d != noDest, n.info):
    "VM problem: dest register is not set"
  # standalone expressions are treated as nullary procedures that
  # directly return the value
  c.gABC(n, opcRet, d)

  result = typeof(result).ok(c.prc.regInfo.len)


proc genParams(prc: var BProc; signature: PType) =
  ## Allocates the registers for the parameters and associates them with the
  ## corresponding locs.
  let
    params = signature.n
    isClosure = signature.callConv == ccClosure

  # closure procedures have an additional, hidden parameter
  prc.regInfo.newSeq(params.len + ord(isClosure))

  for i, it in prc.regInfo.mpairs:
    it = RegInfo(inUse: true, kind: slotFixedVar)
    prc[LocalId i].reg = i

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

proc transitionToLocation(c: var TCtx, info: CgNode, typ: PType, reg: TRegister) =
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

proc prepareParameters(c: var TCtx, info: CgNode) =
  ## Prepares immutable parameters backed by registers for having their address
  ## taken. If an immutable parameter has its address taken, it is transitioned
  ## to a VM memory location at the start of the procedure.
  let typ = c.prc.sym.routineSignature # the procedure's type

  template setupParam(c: var TCtx, typ: PType, loc: LocalLoc) =
    if fitsRegister(typ) and loc.isIndirect:
      transitionToLocation(c, info, typ, loc.reg)

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
    setupParam(c, typ[1], c.prc[LocalId(1)])

  # setup the remaining parameters:
  for i in 2..<typ.n.len:
    setupParam(c, typ[i], c.prc[LocalId(i)])

proc genProcBody(c: var TCtx): int =
    let
      s    = c.prc.sym
      body = c.prc.body.code

    genParams(c.prc, s.routineSignature)

    # setup the result register, if necessary. Watch out for macros! Their
    # result register is setup at the start of macro evaluation
    # XXX: initializing the ``result`` of a macro should be handled through
    #      inserting the necessary code either in ``sem` or here
    let rt = c.prc.body[resultId].typ
    if not isEmptyType(rt) and fitsRegister(rt):
      # initialize the register holding the result
      if s.kind == skMacro:
        if c.prc[resultId].isIndirect:
          # the result variable for macros is currently initialized from outside
          # the VM, so we can't use ``opcLdNull`` here
          transitionToLocation(c, body, rt, c.prc[resultId].reg)
      else:
        let opcode =
          if c.prc[resultId].isIndirect:
            # the result variable has its address taken or a reference/view
            # to it created -> we need a VM memory location. In order for this
            # to be opaque to the callsite, a write-back is injected before
            # returns
            opcLdNull
          else:
            opcLdNullReg

        # only setup a location/register if the procedure's result is not a view:
        if not isDirectView(rt):
          gABx(c, body, opcode, c.prc[resultId].reg, c.genType(rt))

    prepareParameters(c, body)
    if s.routineSignature.callConv == ccClosure:
      # convert the environment reference to the expected type, the caller
      # may pass it as a super type
      let env = TRegister(s.routineSignature.n.len)
      c.gABC(body, opcObjConv, env, env)
      c.gABx(body, opcObjConv, 0, c.genType(c.prc.body[LocalId env].typ))

    gen(c, body)

    # generate final 'return' statement:
    writeBackResult(c, body)
    c.gABC(body, opcRet)

    result = c.prc.regInfo.len

proc genProc*(c: var TCtx; s: PSym, body: sink Body): VmGenResult =
  # thanks to the jmp we can add top level statements easily and also nest
  # procs easily:
  let
    start = c.code.len+1 # skip the jump instruction
    procStart = c.xjmp(body.code, opcJmp, 0)

  c.prc = initProc(c, s, body)

  let regCount = tryOrReturn:
    c.genProcBody()

  c.patch(procStart)
  c.optimizeJumps(start)

  result = VmGenResult.ok:
    (start: start, regCount: regCount)

func vmGenDiagToAstDiagVmGenError*(diag: VmGenDiag): AstDiagVmGenError {.inline.} =
  let kind =
    case diag.kind
    of vmGenDiagTooManyRegistersRequired: adVmGenTooManyRegistersRequired
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
      of vmGenDiagTooManyRegistersRequired:
        AstDiagVmGenError(kind: kind)
