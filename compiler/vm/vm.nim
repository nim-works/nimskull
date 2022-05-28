#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This file implements the new evaluation engine for Nim code.
## An instruction is 1-3 int32s in memory, it is a register based VM.

import
  std/[
    strutils,
    tables,
    parseutils
  ],
  compiler/ast/[
    ast,
    errorhandling,
    errorreporting,
    lineinfos,
    renderer, # toStrLit implementation
    trees,
    idents,
    reports,
    typesrenderer,
    types,
    parser # `parseExpr()` and `parseStmt()`
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs,
    cli_reporter # Imported to generate literal error message for VM exception
                 # handling
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/utils/[
    debugutils,
    int128,
    btrees,
    bitsets
  ],
  compiler/sem/[
    sighashes,
    macrocacheimpl,
    transf,
    passes,
    evaltempl
  ],
  compiler/vm/[
    vmprofiler,
    gorgeimpl,
    vmchecks,
    vmcompilerserdes,
    vmdeps,
    vmerrors,
    vmgen,
    vmdef,
    vmmemory,
    vmobjects,
    vmtypegen,
    vmtypes
  ],
  experimental/[
    results
  ]

# XXX: instead of `assert` a special `vmAssert` could be used for internal
#      checks. This would allow for also reporting things like the current
#      executed instruction

import std/options as stdoptions
from std/math import round

const
  traceCode = defined(nimVMDebugExecute)

const
  errIllegalConvFromXtoY = "illegal conversion from '$1' to '$2'"



proc createStackTrace(
    c:          TCtx,
    sframe:     StackFrameIndex,
    pc:         int,
    recursionLimit: int = 100
  ): SemReport =
  ## Generates a stack-trace report starting at frame `sframe` (inclusive).
  ## The amount of entries in the trace is limited to `recursionLimit`, further
  ## entries are skipped, though the entry function is always included in the
  ## trace
  assert c.sframes.len > 0

  result = SemReport(kind: rsemVmStackTrace)
  # Just leave the exceptions empty, they aren't used anyways
  result.currentExceptionA = nil
  result.currentExceptionB = nil

  block:
    var i = sframe
    var count = 0
    var pc = pc
    # Traverse the stack formed via the `next` field
    while i >= 0:
      # XXX: unsafeAddr is used due to nocopy-let-in-loop issue
      let f = unsafeAddr c.sframes[i]
      i = f.next

      if count < recursionLimit - 1 or i == 0:
        # The `i == 0` is to make sure that we're always including the
        # bottom of the stack (the entry function) in the trace

        # Since we're walking the stack from top to bottom,
        # the elements are added to the trace in reverse order
        # (the most recent function is first in the list, not last).
        # This needs to be accounted for by the actual reporting logic
        result.stacktrace.add((sym: f.prc, location: c.debug[pc]))

      pc = f.comesFrom
      inc count

    if count > recursionLimit:
      result.skipped = count - recursionLimit

  assert result.stacktrace.len() <= recursionLimit # post condition check


func setNodeValue(dest: LocHandle, node: PNode) =
  assert dest.typ.kind == akPNode
  deref(dest).nodeVal = node

proc copyToLocation(dest, src: LocHandle, mm: var VmMemoryManager, reset: static[bool] = true) =
  assert dest.typ == src.typ
  copyToLocation(mm, dest.byteView(), src.h, src.typ, reset)

# XXX: in the future all atoms should be stored in registers
const RegisterAtomKinds =
  {akInt, akFloat, akPtr, akPNode, akDiscriminator} ##
  ## The set of atom kinds that are stored directly in registers

func loadFromLoc(reg: var TFullReg, h: LocHandle) =
  ## Loads the value from `h` into `reg`. If the value doesn't not fit into a
  ## register, the handle is put into the register instead
  let atom = deref(h)
  case h.typ.kind
  of akInt:
    # Always sign-extend on load (even for unsigned integers). Ints are
    # stored as signed-integers in registers
    let val = signExtended(readIntBits(h.byteView()),
                            BiggestInt(h.typ.sizeInBytes))
    reg = TFullReg(kind: rkInt, intVal: val)
  of akFloat:
    reg = TFullReg(kind: rkFloat, floatVal: readFloat(h.byteView()))
  of akPtr:
    let a = makeMemPtr(atom.ptrVal, 0)
    reg = TFullReg(kind: rkAddress, addrVal: a, addrTyp: h.typ.targetType)
  of akDiscriminator:
    reg = TFullReg(kind: rkInt, intVal: readDiscriminant(h))
  of akPNode:
    # TODO: (maybe) separate AST from types in the macros API
    reg = TFullReg(kind: rkNimNode, nimNode: atom.nodeVal)

    # XXX: To make the  implementation simpler for now, NimNodes are always
    #      non-nil `PNode`s. Since complex values are zero-initialized, we
    #      have to translate the nil-PNode into an nkNilLit here. This is only
    #      temporary until the NimNode handling gets reworked
    if reg.nimNode == nil:
      reg.nimNode = newNode(nkNilLit)
  else:
    reg = TFullReg(kind: rkHandle, handle: h)


proc toVmError(c: DerefFailureCode, inst: InstantiationInfo): ref VmError =
  ## Creates a `VmError` for the failure code
  new(result)
  result.report = SemReport(
    kind: FailureCodeToReport[c],
    reportInst: toReportLineInfo(inst))

template toException(x: DerefFailureCode): untyped =
  ## `Result` -> exception translation
  toVmError(x, instLoc())

proc reportException(c: TCtx; sframe: StackFrameIndex, raised: LocHandle) =
  ## Reports the exception represented by `raised` by raising a `VmError`

  let name = $raised.getFieldHandle(1.fpos).deref().strVal
  let msg = $raised.getFieldHandle(2.fpos).deref().strVal

  # The reporter expects the exception as a deserialized PNode-tree. Only the
  # 2nd (name) and 3rd (msg) field are actually used, so instead of running
  # full deserialization (which is also not possible due to no `PType` being
  # available), we just create the necessary parts manually

  # TODO: the report should take the two strings directly instead
  let empty = newNode(nkEmpty)
  let ast = newTree(nkObjConstr,
                    empty, # constructor type; unused
                    empty, # unused
                    newStrNode(nkStrLit, name),
                    newStrNode(nkStrLit, msg))
  raiseVmError(reportAst(rsemVmUnhandledException, ast))

when not defined(nimComputedGoto):
  {.pragma: computedGoto.}


func cleanUpReg(r: var TFullReg, mm: var VmMemoryManager) =
  ## Cleans up and frees a location register's location. If `r` is not a
  ## location register, nothing is done.
  ##
  ## The register is left in an invalid state and should be transitioned to a
  ## new state immediately afterwards.

  # XXX: the cleaning up and state transitioning of registers will be lifted
  #      into `vmgen` once it's rewritten
  if r.kind == rkLocation:
    resetLocation(mm, r.handle.byteView(), r.handle.typ)
    mm.allocator.dealloc(r.handle)

proc cleanUpLocations(mm: var VmMemoryManager, frame: var TStackFrame) =
  ## Cleans up and deallocates all locations belonging to `frame`. Registers
  ## are left in an invalid state, as this function is meant to be called
  ## prior to leaving a frame
  for s in frame.slots.items:
    if s.kind == rkLocation:
      mm.resetLocation(s.handle.byteView(), s.handle.typ)
      mm.allocator.dealloc(s.handle)

func cleanUpPending(mm: var VmMemoryManager) =
  ## Cleans up all managed ref-counted locations marked for clean-up.
  var i = 0
  # `resetLocation` might add new entries to the `pending` list, which is why
  # we have to iterate the list manually like this
  while i < mm.heap.pending.len:
    let idx = mm.heap.pending[i]
    let slot {.cursor.} = mm.heap.slots[idx] # A deep-copy is not necessary
                # here, since the underlying `HeapSlot` is only moved around

    assert not slot.handle.h.isNil

    resetLocation(mm, slot.handle.byteView(), slot.handle.typ)
    mm.allocator.dealloc(slot.handle)

    mm.heap.slots[idx].reset()

    inc i

  mm.heap.pending.setLen(0)

# XXX: ensureKind (register transition) will be moved into a dedicated
#      instruction

func ensureKind(n: var TFullReg, k: TRegisterKind, mm: var VmMemoryManager) {.inline.} =
  if n.kind != k:
    cleanUpReg(n, mm)
    n = TFullReg(kind: k)

func initLocReg(r: var TFullReg, typ: PVmType, mm: var VmMemoryManager) =
  ## Transitions `r` to a location register storing a location of type `typ`
  cleanUpReg(r, mm)

  r = TFullReg(kind: rkLocation)
  r.handle = mm.allocator.allocSingleLocation(typ)

func initIntReg(r: var TFullReg, i: BiggestInt) =
  r = TFullReg(kind: rkInt, intVal: i)

func ensureAtomKind(n: var TFullReg, typ: PVmType, mm: var VmMemoryManager) {.inline.} =
  ## If necessary, transitions register to a location register of type `typ`

  assert typ.kind in (realAtomKinds-RegisterAtomKinds)

  if n.kind != rkLocation or n.handle.typ.kind != typ.kind:
    n.initLocReg(typ, mm)

template ensureKind(k: untyped) {.dirty.} =
  ensureKind(regs[ra], k, c.memory)

template ensureAtomKind(k: AtomKind) {.dirty.} =
  ensureAtomKind(regs[ra], kindToTypeLut[k], c.memory)

template decodeB(k: TRegisterKind) {.dirty.} =
  let rb = instr.regB
  ensureKind(k)

template decodeB(k: AtomKind) {.dirty.} =
  let rb = instr.regB
  ensureAtomKind(k)

template decodeBC(k: TRegisterKind) {.dirty.} =
  let rb = instr.regB
  let rc = instr.regC
  ensureKind(k)

template decodeBC(k: AtomKind) {.dirty.} =
  let rb = instr.regB
  let rc = instr.regC
  ensureAtomKind(k)

template declBC() {.dirty.} =
  let rb = instr.regB
  let rc = instr.regC

template decodeBImm(k: TRegisterKind) {.dirty.} =
  let rb = instr.regB
  let imm = instr.regC - byteExcess
  ensureKind(k)

template decodeBImm(k: AtomKind) {.dirty.} =
  let rb = instr.regB
  let imm = instr.regC - byteExcess
  ensureAtomKind(k)


template decodeBImm() {.dirty.} =
  let rb = instr.regB
  let imm = instr.regC - byteExcess

template decodeBx(k: TRegisterKind) {.dirty.} =
  let rbx = instr.regBx - wordExcess
  ensureKind(k)

template decodeBx(k: AtomKind) {.dirty.} =
  let rbx = instr.regBx - wordExcess
  ensureAtomKind(k)

template decodeBx() {.dirty.} =
  let rbx = instr.regBx - wordExcess

template move(a, b: untyped) {.dirty.} = system.shallowCopy(a, b)
# XXX fix minor 'shallowCopy' overloading bug in compiler

include vmhooks

template rawPointer(x: LocHandle): untyped =
  x.h.rawPointer

proc asgnValue(mm: var VmMemoryManager, dest: var TFullReg, src: TFullReg) =
  ## Assigns the value in `src` to `dest`, performing a register transition if
  ## necessary. An assignment in user code (`a = b`) maps directly to this
  ## function

  # Since vmgen doesn't take care of register/location lifetime management, it
  # has to be done here

  var tmp: TFullReg
  case src.kind
  of rkNone, rkInt, rkFloat, rkAddress, rkNimNode, rkRegAddr:
    cleanUpReg(dest, mm)
    dest = src
  of rkHandle, rkLocation:
    if dest.kind == rkLocation and dest.handle.typ == src.handle.typ:
      # Reuse the location. This happens when either 1) a variable is assigned
      # to or 2) a location register is reused for a new location and the types
      # happen to match

      # XXX: the reusing here can lead to ptr values staying valid past
      #      the lifetime of the location they were derived from
      if dest.handle.rawPointer != src.handle.rawPointer:
        # prevent self-assignment
        copyToLocation(dest = dest.handle, src = src.handle, mm, true)
    else:
      # XXX: in theory, it could happen that src is a handle to a location
      #      derived from dest - in which case cleaning up dest would render src
      #      invalid. Let's hope this doesn't happen in practice (until `vmgen`
      #      properly handles locations)
      cleanUpReg(dest, mm)

      let loc = mm.allocator.allocSingleLocation(src.handle.typ)
      copyToLocation(dest = loc, src = src.handle, mm, reset = false)

      dest = TFullReg(kind: rkLocation, handle: loc)

func fastAsgnComplex(x: var TFullReg, y: TFullReg) =
  # XXX: `fastAsgnComplex` is more or less broken right now. The correct
  #      behaviour would be to perform a shallow copy when `x` stores a
  #      location, but this is currently not possible due to various reasons.
  #
  #      Resolved once location lifetime management gets moved to `vmgen`.
  case y.kind
  of rkNone: x.reset()
  of rkInt, rkFloat, rkAddress, rkNimNode, rkRegAddr: x = y
  of rkHandle, rkLocation:
    {.cast(noSideEffect).}: # erroneous side-effect
      x = TFullReg(kind: rkHandle, handle: y.handle)


proc writeLoc(h: LocHandle, x: TFullReg, mm: var VmMemoryManager) =
  ## Writes the value stored in register `x` to location `h`. The handle is
  ## expected to be valid. If the register stores a location itself, a copy is
  ## performed
  case x.kind
  of rkNone: unreachable() # This hints at a programming error somewhere
  of rkInt:
    assert h.typ.kind == akInt
    # Narrow/NarrowU made sure that the integer is in the  correct range
    writeInt(h.byteView(), x.intVal)
  of rkFloat:
    assert h.typ.kind == akFloat
    writeFloat(h, x.floatVal)
  of rkAddress:
    assert h.typ.kind == akPtr # but not akRef
    deref(h).ptrVal = x.addrVal.rawPointer
  of rkHandle, rkLocation:
    assert x.handle.typ == h.typ
    # Prevent writing to self (this can happen via opcWrDeref)
    # XXX: this is mostly due to how vmgen handles var parameters
    if h.rawPointer != x.handle.rawPointer:
      copyToLocation(dest=h, src=x.handle, mm)
  of rkNimNode:
    assert h.typ.kind == akPNode
    setNodeValue(h, x.nimNode)

  of rkRegAddr:
    # The following code won't work in the vm (until rkRegAddr replaced)
    # .. code-block:: nim
    #   var x: int
    #   var o: ...
    #   o.p = addr x
    doAssert false, "Trying to write register address to location"


proc regToNode(c: TCtx, x: TFullReg; typ: PType, info: TLineInfo): PNode =
  ## Deserializes the value stored by `x` to a `PNode` of type `typ`
  case x.kind
  of rkNone: unreachable()
  of rkInt:
    result = newNodeIT(nkIntLit, TLineInfo(), typ)
    result.intVal = x.intVal
  of rkFloat:
    result = newNodeIT(nkFloatLit, TLineInfo(), typ)
    result.floatVal = x.floatVal
  of rkAddress:
    if x.addrVal.isNil:
      result = newNode(nkNilLit)
      result.typ = typ
    else:
      # TODO: validate the address
      result = c.deserialize(makeLocHandle(x.addrVal, x.addrTyp), typ, info)
  of rkHandle, rkLocation: result = c.deserialize(x.handle, typ, info)
  of rkNimNode: result = x.nimNode

  of rkRegAddr: result = c.regToNode(x.regAddr[], typ, info)

proc pushSafePoint(f: var TStackFrame; pc: int) =
  f.safePoints.add(pc)

proc popSafePoint(f: var TStackFrame) =
  discard f.safePoints.pop()

type
  ExceptionGoto = enum
    ExceptionGotoHandler,
    ExceptionGotoFinally,
    ExceptionGotoUnhandled

proc findExceptionHandler(c: TCtx, f: var TStackFrame, raisedType: PVmType):
    tuple[why: ExceptionGoto, where: int] =

  while f.safePoints.len > 0:
    var pc = f.safePoints.pop()

    var matched = false
    var pcEndExcept = pc

    # Scan the chain of exceptions starting at pc.
    # The structure is the following:
    # pc - opcExcept, <end of this block>
    #      - opcExcept, <pattern1>
    #      - opcExcept, <pattern2>
    #        ...
    #      - opcExcept, <patternN>
    #      - Exception handler body
    #    - ... more opcExcept blocks may follow
    #    - ... an optional opcFinally block may follow
    #
    # Note that the exception handler body already contains a jump to the
    # finally block or, if that's not present, to the point where the execution
    # should continue.
    # Also note that opcFinally blocks are the last in the chain.
    while c.code[pc].opcode == opcExcept:
      # Where this Except block ends
      pcEndExcept = pc + c.code[pc].regBx - wordExcess
      inc pc

      # A series of opcExcept follows for each exception type matched
      while c.code[pc].opcode == opcExcept:
        let excIndex = c.code[pc].regBx - wordExcess
        let exceptType =
          if excIndex > 0: c.types[excIndex]
          else: nil

        # echo typeToString(exceptType), " ", typeToString(raisedType)

        # Determine if the exception type matches the pattern
        if exceptType.isNil or getTypeRel(raisedType, exceptType) in {vtrSub, vtrSame}:
          matched = true
          break

        inc pc

      # Skip any further ``except`` pattern and find the first instruction of
      # the handler body
      while c.code[pc].opcode == opcExcept:
        inc pc

      if matched:
        break

      # If no handler in this chain is able to catch this exception we check if
      # the "parent" chains are able to. If this chain ends with a `finally`
      # block we must execute it before continuing.
      pc = pcEndExcept

    # Where the handler body starts
    let pcBody = pc

    if matched:
      return (ExceptionGotoHandler, pcBody)
    elif c.code[pc].opcode == opcFinally:
      # The +1 here is here because we don't want to execute it since we've
      # already pop'd this statepoint from the stack.
      return (ExceptionGotoFinally, pc + 1)

  return (ExceptionGotoUnhandled, 0)

proc cleanUpOnReturn(c: TCtx; f: var TStackFrame): int =
  # Walk up the chain of safepoints and return the PC of the first `finally`
  # block we find or -1 if no such block is found.
  # Note that the safepoint is removed once the function returns!
  result = -1

  # Traverse the stack starting from the end in order to execute the blocks in
  # the intended order
  for i in 1..f.safePoints.len:
    var pc = f.safePoints[^i]
    # Skip the `except` blocks
    while c.code[pc].opcode == opcExcept:
      pc += c.code[pc].regBx - wordExcess
    if c.code[pc].opcode == opcFinally:
      discard f.safePoints.pop
      return pc + 1

template atomVal(r: TFullReg): untyped =
  cast[ptr Atom](r.handle.rawPointer)[]

template strVal*(r: TFullReg): untyped =
  {.line.}:
    assert r.handle.typ.kind == akString
  deref(r.handle).strVal

template `strVal=`*(r: TFullReg, s: VmString) =
  {.line.}:
    assert r.handle.typ.kind == akString
  asgnVmString(deref(r.handle).strVal, s, c.allocator)

template `strVal=`*(r: TFullReg, s: string) =
  {.line.}:
    assert r.handle.typ.kind == akString
  newVmString(deref(r.handle).strVal, s, c.allocator)

proc opConv(c: var TCtx; dest: var TFullReg, src: TFullReg, dt, st: (PType, PVmType)): bool =
  ## Convert the value in register `src` from `st` to `dt` and write the result
  ## to register `dest`

  let desttyp = dt[0]
  let srctyp = st[0]

  # TODO: conversions are rather fuzzy right now. `opConv` needs to be revisited
  if desttyp.kind == tyString:
    # TODO: this part needs some refactoring...
    initLocReg(dest, c.typeInfoCache.stringType, c.memory)

    let styp = srctyp.skipTypes(abstractRange)
    case styp.kind
    of tyEnum:
      let n = styp.n
      let x = src.intVal.int
      if x <% n.len and (let f = n[x].sym; f.position == x):
        dest.strVal = if f.ast.isNil: f.name.s else: f.ast.strVal
      else:
        for i in 0..<n.len:
          c.config.internalAssert(n[i].kind == nkSym, "opConv for enum")

          let f = n[i].sym
          if f.position == x:
            dest.strVal = if f.ast.isNil: f.name.s else: f.ast.strVal
            return
        dest.strVal = styp.sym.name.s & " " & $x
    of tyInt..tyInt64:
      dest.strVal = $src.intVal
    of tyUInt..tyUInt64:
      dest.strVal = $uint64(src.intVal)
    of tyBool:
      dest.strVal = if src.intVal == 0: "false" else: "true"
    of tyFloat..tyFloat128:
      dest.strVal = $src.floatVal
    of tyString:
      dest.strVal = src.strVal
    of tyCstring:
      dest.strVal = src.strVal
    of tyChar:
      dest.strVal = $chr(src.intVal)
    else:
      internalError(c.config, "cannot convert to string " & desttyp.typeToString)
  else:
    let desttyp = skipTypes(desttyp, abstractVarRange)
    case desttyp.kind
    of tyInt..tyInt64:
      dest = TFullReg(kind: rkInt)
      case skipTypes(srctyp, abstractRange).kind
      of tyFloat..tyFloat64:
        dest.intVal = int(src.floatVal)
      else:
        dest.intVal = src.intVal
      if toInt128(dest.intVal) < firstOrd(c.config, desttyp) or toInt128(dest.intVal) > lastOrd(c.config, desttyp):
        return true
    of tyUInt..tyUInt64:
      dest = TFullReg(kind: rkInt)
      let styp = srctyp.skipTypes(abstractRange) # skip distinct types(dest type could do this too if needed)
      case styp.kind
      of tyFloat..tyFloat64:
        dest.intVal = int(src.floatVal)
      else:
        let srcDist = (sizeof(src.intVal) - styp.size) * 8
        let destDist = (sizeof(dest.intVal) - desttyp.size) * 8
        var value = cast[BiggestUInt](src.intVal)
        value = (value shl srcDist) shr srcDist
        value = (value shl destDist) shr destDist
        dest.intVal = cast[BiggestInt](value)
    of tyBool:
      dest = TFullReg(kind: rkInt)
      dest.intVal =
        case skipTypes(srctyp, abstractRange).kind
          of tyFloat..tyFloat64: int(src.floatVal != 0.0)
          else: int(src.intVal != 0)
    of tyFloat, tyFloat64:
      dest = TFullReg(kind: rkFloat)
      case skipTypes(srctyp, abstractRange).kind
      of tyInt..tyInt64, tyUInt..tyUInt64, tyEnum, tyBool, tyChar:
        dest.floatVal = toBiggestFloat(src.intVal)
      else:
        dest.floatVal = src.floatVal
    of tyFloat32:
      # convert to `float32` first and then to `BiggestFloat`
      dest = TFullReg(kind: rkFloat)
      case skipTypes(srctyp, abstractRange).kind
      of tyInt..tyInt64, tyUInt..tyUInt64, tyEnum, tyBool, tyChar:
        dest.floatVal = src.intVal.float32.BiggestFloat
      of tyFloat, tyFloat64:
        dest.floatVal = src.floatVal.float32.BiggestFloat
      else:
        dest.floatVal = src.floatVal
    of tyObject:
      c.config.internalAssert(srctyp.skipTypes(abstractVarRange).kind == tyObject, "invalid object-to-object conversion")
      # XXX: fastAsgnComplex is wrong. A handle conversion should be performed
      #      instead
      fastAsgnComplex(dest, src)
    of tyRef:
      assert getTypeRel(st[1], dt[1]) != vtrUnrelated, "invalid ref-to-ref conversion" # vmgen issue
      assert src.handle.typ.kind == akRef # vmgen issue

      let refVal = deref(src.handle).refVal

      if refVal.isNil:
        discard "always allow conversion for nil ref"
      else:
        let loc = c.heap.tryDeref(refVal, noneType).value()
        if getTypeRel(loc.typ, dt[1].targetType) notin {vtrSame, vtrSub}:
          return true

      block:
        # src might be dependent on dest so copy first before cleaning up
        var tmp: TFullReg
        swap(tmp, dest)

        initLocReg(dest, dt[1], c.memory)
        asgnRef(deref(dest.handle).refVal, refVal, c.memory, reset=false)

        cleanUpReg(tmp, c.memory)

    of tyOpenArray:
      assert dt[1].kind == akSeq

      let srckind = srctyp.skipTypes(abstractRange).kind
      case srckind
      of tySequence:
        # XXX: `asgnComplex` is wrong and leads to `experimental:views`
        #      more or less not working at all for VM code. Since
        #      first-class openArray usage is probably very rare, I believe
        #      it's okay for now.
        asgnValue(c.memory, dest, src)
      of tyArray, tyString:
        dest.initLocReg(dt[1], c.memory)
        let
          stride = dt[1].seqElemStride
          t = dt[1].seqElemType
          L = arrayLen(src.handle)

        deref(dest.handle).seqVal.newVmSeq(dt[1], L, c.memory)

        let sd =
          if srckind == tyArray: src.handle.h
          else: src.strVal.data
        let dd = deref(dest.handle).seqVal.data

        # XXX: does the same thing as `asgnComplex` above and is thus
        #      equally wrong
        arrayCopy(c.memory, dd, sd, L, t, false)
      else:
        unreachable() # vmgen issue

    else:
      asgnValue(c.memory, dest, src)

func toException(r: sink SemReport): ref VmError {.noinline.} =
  new(result)
  result.report = r

proc compile(c: var TCtx, s: PSym): int =
  result = vmgen.genProc(c, s).value()

  when debugEchoCode:
    c.codeListing(s, nil, start = result)

template handleJmpBack() {.dirty.} =
  if c.loopIterations <= 0:
    if allowInfiniteLoops in c.features:
      c.loopIterations = c.config.maxLoopIterationsVM
    else:
      raiseVmError(reportSem(rsemVmTooManyIterations))

  dec(c.loopIterations)


func setAddress(r: var TFullReg, p: MemRegionPtr, typ: PVmType) =
  assert r.kind == rkAddress
  r.addrVal = p
  r.addrTyp = typ

func setAddress(r: var TFullReg, handle: LocHandle) =
  assert r.kind == rkAddress
  r.addrVal = handle.h
  r.addrTyp = handle.typ

func setHandle(r: var TFullReg, handle: LocHandle) =
  assert r.kind == rkHandle # Not rkLocation
  assert not handle.h.isNil
  assert handle.typ.isValid
  r.handle = handle

func copyRegister(r: var TFullReg, other: TFullReg, mm: var VmMemoryManager) =
  # TODO: remove `copyRegister` once rkRegAddr is gone
  cleanUpReg(r, mm)
  fastAsgnComplex(r, other)


proc putIntoReg(dest: var TFullReg; c: var TCtx, n: PNode, formal: PType) =
  ## Put the value that is represented by `n` (but not the node itself) into
  ## `dest`. Implicit conversion is also performed, if necessary.
  let t = formal.skipTypes(abstractInst+{tyStatic, tyOwned}-{tyTypeDesc})

  # XXX: instead of performing conversion here manually, sem could generate a
  #      small thunk for macro invocations that sets up static arguments and
  #      then invokes the macro. The thunk would be executed in the VM, making
  #      the code here obsolete while also eliminating unnecessary
  #      deserialize/serialize round-trips

  case t.kind
  of tyBool, tyChar, tyEnum, tyInt..tyInt64, tyUInt..tyUInt64:
    assert n.kind in nkCharLit..nkUInt64Lit
    dest.ensureKind(rkInt, c.memory)
    dest.intVal = n.intVal
  of tyFloat..tyFloat128:
    assert n.kind in nkFloatLit..nkFloat128Lit
    dest.ensureKind(rkFloat, c.memory)
    dest.floatVal = n.floatVal
  of tyNil, tyPtr, tyPointer:
    dest.ensureKind(rkAddress, c.memory)
    # XXX: it's currently forbidden to pass non-nil pointer to static
    #      parameters. `deserialize` already reports an error, so an
    #      assert is used here to make sure that it really got reported
    #      earlier
    assert n.kind == nkNilLit
  of tyOpenArray:
    # Handle `openArray` parameters the same way they're handled elsewhere
    # in the VM: simply pass the argument without an conversion
    let typ = c.getOrCreate(n.typ)
    dest.initLocReg(typ, c.memory)
    c.serialize(n, dest.handle)
  of tyProc:
    # XXX: a hack required to uphold some expectations. For example,
    #      `genEnumCaseStmt` would fail without this. Procedural types as
    #      static macro arguments are underspecified
    let pt =
      if t.callConv == ccClosure and n.kind == nkSym:
        # Force the location to be of non-closure type. This breaks other
        # assumptions!
        n.sym.typ
      else:
        t

    let typ = c.getOrCreate(pt)
    dest.initLocReg(typ, c.memory)
    c.serialize(n, dest.handle, pt)

  else:
    if t.kind == tyRef and t.sym != nil and t.sym.magic == mPNimrodNode:
      # A NimNode
      dest.ensureKind(rkNimNode, c.memory)
      dest.nimNode = n
    else:
      let typ = c.getOrCreate(formal)
      dest.initLocReg(typ, c.memory)
      # XXX: overriding the type (passing `formal`), leads to issues (internal
      #      compiler error) when passing an empty set to a static parameter
      c.serialize(n, dest.handle)#, formal)

func loadEmptyReg(r: var TFullReg, typ: PVmType, info: TLineInfo, mm: var VmMemoryManager): bool =
  ## If a value of `typ` fits into a register, transitions `r` to the correct
  ## state, loads the default value and returns true. Returns false otherwise
  case typ.kind
  of akFloat:
    ensureKind(r, rkFloat, mm)
    r.floatVal = 0.0
  of akInt:
    ensureKind(r, rkInt, mm)
    r.intVal = 0
  of akPtr:
    ensureKind(r, rkAddress, mm)
    r.setAddress(makeMemPtr(nil, 0), typ)
  of akPNode:
    ensureKind(r, rkNimNode, mm)
    r.nimNode = newNodeI(nkNilLit, info)
  else:
    # value isn't stored in a register directly
    return false

  return true

func raiseAccessViolation(
  reason: AccessViolationReason,
  inst: InstantiationInfo) {.noinline.} =
  let k =
    case reason
    of avrNoError: unreachable()
    of avrOutOfBounds: rsemVmAccessOutOfBounds
    of avrTypeMismatch: rsemVmAccessTypeMismatch
    of avrNoLocation: rsemVmAccessNoLocation

  raiseVmError(SemReport(kind: k), inst)

template checkHandle(a: VmAllocator, re: TFullReg) =
  ## Tests if the handle in `re` is valid and raises an access violation error
  ## if it's not valid
  let reg = addr re
  if reg.kind == rkHandle:
    let r = checkValid(a, reg.handle.h, reg.handle.typ)
    if unlikely(r != avrNoError):
      const L = instLoc()
      {.line: L.}: raiseAccessViolation(r, L)

template checkHandle(a: VmAllocator, handle: LocHandle) =
  ## Tests if `handle` is valid and raises an access violation error if it's
  ## not valid
  let r = checkValid(a, handle.h, handle.typ)
  if unlikely(r != avrNoError):
    const L = instLoc()
    {.line: L.}: raiseAccessViolation(r, L)


when not defined(nimHasSinkInference):
  {.pragma: nosinks.}


type RegisterIndex = int

proc rawExecute(c: var TCtx, pc: var int, tos: var StackFrameIndex): RegisterIndex =
  ## Runs the execution loop, starting in frame `tos` at program counter `pc`.
  ## In the case of an error, raises an exception of type `VmError`.
  ##
  ## If the loop was exited due to an error, `pc` and `tos` will point to the
  ## faulting instruction and the active stack-frame respectively.
  ##
  ## If the loop exits without errors, `pc` points to the last executed
  ## instruction and `tos` refers to the stack-frame it executed on
  ##
  ## tos means top-of-stack

  # Used to keep track of where the execution is resumed.
  var savedPC = -1
  var savedFrame: StackFrameIndex
  when defined(gcArc) or defined(gcOrc):
    # Use {.cursor.} as a way to get a shallow copy of the seq. This is safe,
    # since `slots` is never changed in length (no add/delete)
    var regs {.cursor.}: seq[TFullReg]
    template updateRegsAlias =
      regs = c.sframes[tos].slots
    updateRegsAlias
  else:
    var regs: seq[TFullReg] # alias to tos.slots for performance
    template updateRegsAlias =
      shallowCopy(regs, c.sframes[tos].slots)
    updateRegsAlias

  template pushFrame(p: TStackFrame) =
    c.sframes.add(p)
    tos = c.sframes.high
    updateRegsAlias

  template popFrame() =
    tos = c.sframes[tos].next
    # Possibly throws aways all frames left alive for `raise`
    # handling, but since the exception is swallowed anyway,
    # it doesn't really matter
    for i in (tos+1)..<c.sframes.len:
      cleanUpLocations(c.memory, c.sframes[i])
    c.sframes.setLen(tos + 1)
    updateRegsAlias

  template gotoFrame(f: int) =
    tos = f
    assert tos in 0..c.sframes.high
    updateRegsAlias

  template unwindToFrame(f: int) =
    let nf = f
    assert nf in 0..c.sframes.high
    for i in (nf+1)..<c.sframes.len:
      cleanUpLocations(c.memory, c.sframes[i])
    c.sframes.setLen(nf + 1)
    tos = nf
    updateRegsAlias

  template guestValidate(cond: bool, msg: string) =
    ## Ensures that a guest-input related condition holds true and raises
    ## a `VmError` if it doesn't
    if unlikely(not cond): # Treat input violations as unlikely
      # TODO: don't use a string message here; use proper reports
      raiseVmError(reportStr(rsemVmErrInternal, msg))

  template checkHandle(re: TFullReg) =
    {.line.}:
      checkHandle(c.allocator, re)

  proc reportVmIdx(usedIdx, maxIdx: SomeInteger): SemReport =
    SemReport(
      kind: rsemVmIndexError,
      indexSpec: (
        usedIdx: toInt128(usedIdx),
        minIdx: toInt128(0),
        maxIdx: toInt128(maxIdx)))


  let kindToTypeLut = [
    akInt: c.typeInfoCache.intTypes[tyInt],
    akFloat: c.typeInfoCache.floatTypes[tyFloat],
    akPtr: nil,
    akSet: nil,
    akString: c.typeInfoCache.stringType,
    akSeq: nil
  ]

  #echo "NEW RUN ------------------------"
  while true:
    #{.computedGoto.}
    let instr = c.code[pc]
    let ra = instr.regA

    when traceCode:
      template regDescr(r): TRegisterKind =
        if r < regs.len: regs[r].kind else: rkNone

      c.config.localReport(DebugReport(
        kind: rdbgVmExecTraceFull,
        vmgenExecFull: (
          pc: pc,
          opc: c.code[pc].opcode,
          info: c.debug[pc],
          ra: regDescr(instr.regA),
          rb: regDescr(instr.regB),
          rc: regDescr(instr.regC)
      )))

    if c.config.active.isVmTrace:
      # unlike nimVMDebug, this doesn't require re-compiling nim and is
      # controlled by user code
      c.config.localReport(DebugReport(
        kind: rdbgVmExecTraceMinimal,
        vmgenExecMinimal: (
          info: c.debug[pc],
          opc: instr.opcode
      )))

    c.profiler.enter(c, tos)
    case instr.opcode
    of opcEof:
      # XXX: eof shouldn't be used to return a register
      return ra
    of opcRet:
      let newPc = c.cleanUpOnReturn(c.sframes[tos])
      # Perform any cleanup action before returning
      if newPc < 0:
        pc = c.sframes[tos].comesFrom
        if tos == 0:
          return 0 # opcRet always returns it's value in register zero

        assert c.code[pc].opcode in {opcIndCall, opcIndCallAsgn}
        if c.code[pc].opcode == opcIndCallAsgn:
          # Move the return value to the destination register on the
          # caller's frame
          let p = c.sframes[tos].next
          let i = c.code[pc].regA
          let slot = addr c.sframes[p].slots[i]
          slot[].cleanUpReg(c.memory)
          slot[] = move regs[0]

        popFrame()
      else:
        savedPC = pc
        savedFrame = tos
        # The -1 is needed because at the end of the loop we increment `pc`
        pc = newPc - 1
    of opcYldYoid: assert false
    of opcYldVal: assert false
    of opcAsgnInt:
      decodeB(rkInt)
      regs[ra].intVal = regs[rb].intVal
    of opcAsgnFloat:
      decodeB(rkFloat)
      regs[ra].floatVal = regs[rb].floatVal
    of opcCastFloatToInt32:
      let rb = instr.regB
      ensureKind(rkInt)
      regs[ra].intVal = cast[int32](float32(regs[rb].floatVal))
    of opcCastFloatToInt64:
      let rb = instr.regB
      ensureKind(rkInt)
      regs[ra].intVal = cast[int64](regs[rb].floatVal)
    of opcCastIntToFloat32:
      let rb = instr.regB
      ensureKind(rkFloat)
      regs[ra].floatVal = cast[float32](regs[rb].intVal)
    of opcCastIntToFloat64:
      let rb = instr.regB
      ensureKind(rkFloat)
      regs[ra].floatVal = cast[float64](regs[rb].intVal)

    of opcCastPtrToInt: # RENAME opcCastPtrOrRefToInt
      decodeBImm(rkInt)
      var intVal : int
      case imm
      of 1: # PtrLikeKinds
        case regs[rb].kind
        of rkAddress:
          intVal = cast[int](regs[rb].addrVal.rawPointer)
        else:
          raiseVmError(reportStr(
            rsemVmErrInternal,
            "opcCastPtrToInt: got " & $regs[rb].kind))

      of 2: # tyRef
        case regs[rb].kind
        of rkNimNode:
          intVal = cast[int](regs[rb].nimNode)
        of rkHandle, rkLocation:
          checkHandle(regs[rb])
          assert regs[rb].handle.typ.kind == akRef # vmgen issue
          # XXX: this is very likely wrong. Instead, the address of the target
          #      should be taken and then cast to int
          intVal = cast[int](regs[rb].atomVal.refVal)
        else:
          assert false # vmgen issue
      else: assert false, $imm
      regs[ra].intVal = intVal
    of opcCastIntToPtr:
      decodeB(rkAddress)

      var ptrVal: pointer
      case regs[rb].kind
      of rkInt:
        ptrVal = cast[pointer](regs[rb].intVal)
      of rkAddress:
        # vmgen abuses this opcode for ptr-to-ptr casting
        ptrVal = regs[rb].addrVal.rawPointer
      else:
        raiseVmError(reportStr(
          rsemVmErrInternal,
          "opcCastIntToPtr: regs[rb].kind: " & $regs[rb].kind))

      regs[ra].setAddress(makeMemPtr(ptrVal, 0), nil)
    of opcAsgnComplex:
      checkHandle(regs[instr.regB])
      asgnValue(c.memory, regs[ra], regs[instr.regB])
    of opcFastAsgnComplex:
      cleanUpReg(regs[ra], c.memory)
      fastAsgnComplex(regs[ra], regs[instr.regB])
    of opcNodeToReg:
      let ra = instr.regA
      let rb = instr.regB

      if regs[rb].kind in {rkLocation, rkHandle}:
        checkHandle(regs[rb])
        if regs[rb].handle.typ.kind in RegisterAtomKinds:
          loadFromLoc(regs[ra], regs[rb].handle)
        else:
          assert false # vmgen issue
      else:
        # This case has to be temporarily treated as valid, as a side-effect
        # of rkRegAddr. `LdDeref` should always produce a handle, but due
        # to `rkRegAddr` it can't.
        discard

    of opcLdArr:
      # a = b[c]
      decodeBC(rkHandle)
      checkHandle(regs[rb])

      let
        idx = regs[rc].intVal.int
        srcTyp = regs[rb].handle.typ
        L = arrayLen(regs[rb].handle)

      if unlikely(idx >=% L):
        raiseVmError(reportVmIdx(idx, L-1))

      case srcTyp.kind
      of akString:
        # XXX: `vmgen` uses `opcLdArr` for openArrays. Since strings are
        #       directly passed into openArray parameters, we have to handle
        #       akString here too
        # TODO: Remove this case once openArray handling is reworked
        let
          t = c.typeInfoCache.charType
          h = regs[rb].strVal.data.getSubHandle(idx * t.sizeInBytes.int)
        regs[ra].setHandle(makeLocHandle(h, t))
      of akSeq, akArray:
        regs[ra].setHandle(getItemHandle(regs[rb].handle, idx))
      else:
        unreachable($srcTyp.kind)

    of opcLdArrAddr:
      # a = addr(b[c])
      decodeBC(rkAddress)
      let idx = regs[rc].intVal.int

      checkHandle(regs[rb])
      let src = regs[rb].handle

      if unlikely(idx >=% arrayLen(src)):
        raiseVmError(reportVmIdx(idx, arrayLen(src) - 1))

      case src.typ.kind
      of akString:
        # XXX: see todo and comment in `opcLdArr` for the reasons why this
        #      case is needed
        let t = c.typeInfoCache.charType
        regs[ra].setAddress(
          regs[rb].strVal.data.getSubHandle(idx * t.sizeInBytes.int),
          t)
      of akSeq, akArray:
        let h = getItemHandle(src, idx)
        regs[ra].setAddress(h.h, h.typ)
      else:
        unreachable($src.typ.kind) # vmgen issue

    of opcLdStrIdx:
      decodeBC(rkInt)
      let idx = regs[rc].intVal.int
      checkHandle(regs[rb])
      let s = regs[rb].atomVal.strVal
      if idx <% s.len:
        regs[ra].intVal = s[idx].ord
      else:
        raiseVmError(reportVmIdx(idx, s.len-1))
    of opcLdStrIdxAddr:
      # a = addr(b[c]); similar to opcLdArrAddr
      decodeBC(rkAddress)
      if regs[rc].intVal > high(int):
        raiseVmError(reportVmIdx(regs[rc].intVal, high(int)))

      checkHandle(regs[rb])

      let idx = regs[rc].intVal.int
      let s = regs[rb].atomVal.strVal.addr # or `byaddr`
      if idx <% s[].len:
        regs[ra].setAddress(s[].data.getSubHandle(idx), c.typeInfoCache.charType)
      else:
        raiseVmError(reportVmIdx(idx, s[].len-1))

    of opcWrArr:
      # a[b] = c
      let
        rb = instr.regB
        rc = instr.regC

      checkHandle(regs[ra])

      let
        dest = regs[ra].handle
        idx = regs[rb].intVal.int
        dTyp = dest.typ

      let (mHandle, maxLen, stride, eTyp) =
        case dest.typ.kind
        of akString:
          # XXX: see todo and comment in `opcLdArr` for the reasons why we need
          #      this case
          (deref(dest).strVal.data,
           deref(dest).strVal.len,
           1, c.typeInfoCache.charType)
        of akSeq:
          (deref(dest).seqVal.data,
           deref(dest).seqVal.length,
           dTyp.seqElemStride, dTyp.seqElemType)
        of akArray:
          (dest.h,
           dTyp.elementCount,
           dTyp.elementStride, dTyp.elementType)
        of akInt, akFloat, akSet, akPtr, akRef, akObject, akPNode, akCallable, akClosure, akDiscriminator:
          unreachable($dTyp.kind)

      if idx <% maxLen:
        writeLoc(makeLocHandle(mHandle.getSubHandle(stride * idx), eTyp), regs[rc], c.memory)
      else:
        raiseVmError(reportVmIdx(idx, maxLen))

    of opcLdObj:
      # a = b.c
      decodeBC(rkHandle)

      # Handles are only validated when using them to access locations. No
      # location access happens here, thus no validation is done
      regs[ra].setHandle(regs[rb].handle.getFieldHandle(FieldPosition(rc)))
    of opcLdObjAddr:
      # a = addr(b.c)
      decodeBC(rkAddress)

      # No location is accessed here, so no validation is done
      regs[ra].setAddress(regs[rb].handle.getFieldHandle(FieldPosition(rc)))

    of opcWrObj:
      # a.b = c
      let rb = instr.regB
      let rc = instr.regC

      # rb is treated as an immediate value instead of an index into the
      # register list

      assert regs[ra].kind in {rkHandle, rkLocation}
      assert regs[ra].handle.typ.kind == akObject

      let h = regs[ra].handle.getFieldHandle(FieldPosition(rb))
      if regs[ra].kind == rkHandle:
        # Location registers' handles are always valid (same goes for
        # sub-handles derived from them), so we don't need to validate
        # the handle to the field for those
        checkHandle(c.allocator, h)

      writeLoc(h, regs[rc], c.memory)

    of opcWrStrIdx:
      # a[b] = c
      let rb = instr.regB
      let rc = instr.regC

      checkHandle(regs[ra])

      let idx = regs[rb].intVal.int
      if idx <% regs[ra].atomVal.strVal.len:
        regs[ra].atomVal.strVal[idx] = chr(regs[rc].intVal)
      else:
        raiseVmError(reportVmIdx(idx, regs[ra].atomVal.strVal.len-1))

    of opcInitDisc, opcSetDisc:
      let rb = instr.regB
      let rc = instr.regC

      # To keep things simpler, we validate the object handle instead of
      # each field that's part of the source and target branch
      checkHandle(regs[ra])

      let
        (t, idx) = getFieldAndOwner(regs[ra].handle.typ, FieldPosition(rb))
        discr = regs[ra].handle.getSubHandle(0, t).getFieldHandle(idx)
        combined = regs[rc].intVal

      assert discr.typ.kind == akDiscriminator
      let (v, branch) = unpackDiscr(combined.int, discr.typ.numBits)

      if instr.opcode == opcSetDisc:
        let oldBranch = readDiscrBranch(discr, t, idx)
        if oldBranch != branch:
          let h = regs[ra].handle.getSubHandle(0, t)
          resetBranch(c.memory, h, idx, oldBranch)

      writeDiscrField(discr, t, idx, v, branch)

    of opcWrProc:
      # TODO: once all atoms go into registers, rename to `opcLdProc`
      let fncIdx = FunctionIndex(instr.regBx - wordExcess)

      # No need to validate the handle. Due to the usage of `opcWrProc` in
      # vmgen, it's always valid here

      let handle = regs[ra].handle
      assert handle.typ.kind in {akCallable, akClosure}

      assert handle.typ.funcTypeId == c.functions[fncIdx.int].typ

      # XXX: as a direct consequence of the state of vmgen, opcWrProc and
      #      opcWrClosure are both implemented in a rather hacky way

      case handle.typ.kind
      of akCallable:
        deref(handle).callableVal = toFuncPtr(fncIdx)
      of akClosure:
        # XXX: once function pointers are stored in registers, we won't need
        # any special handling regarding the ptr/closure difference
        deref(handle).closureVal.fnc = toFuncPtr(fncIdx)
      else:
        assert false # vmgen issue

    of opcWrClosure:
      let rb = instr.regB
      let rc = instr.regC

      # No need to validate the handle. Due to the usage of `opcWrClosure` in
      # vmgen, it's always valid here

      let h = regs[ra].handle
      assert h.typ.kind == akClosure # vmgen issue

      let env: HeapSlotHandle =
        if rc == ra:
          # Use nil environment
          0
        else:
          # rc holds the environment ref
          assert regs[rc].handle.typ.kind == akRef # vmgen issue
          deref(regs[rc].handle).refVal

      let fPtr =
        case regs[rb].handle.typ.kind
        of akCallable: deref(regs[rb].handle).callableVal
        of akClosure: deref(regs[rb].handle).closureVal.fnc
        else: assert false; default(VmFunctionPtr) # vmgen issue

      assert fPtr.isNil or env == 0 or
             c.functions[fPtr.toFuncIndex.int].envParamType != nil,
             "environment must not be nil" # vmgen issue

      deref(h).closureVal.asgnClosure(
        VmClosure(fnc: fPtr, env: env),
        c.memory,
        reset=true)

    of opcAddrReg:
      let rb = instr.regB
      case regs[rb].kind
      of rkLocation:
        ensureKind(rkAddress)
        regs[ra].setAddress(regs[rb].handle)
      of rkHandle:
        # XXX: due to various vmgen issues, this case has to be allowed for now
        #unreachable() # vmgen issue
        ensureKind(rkAddress)
        regs[ra].setAddress(regs[rb].handle.h, regs[rb].handle.typ)
      else:
        ensureKind(rkRegAddr)
        regs[ra].regAddr = addr regs[rb]

    of opcAddrNode:
      # XXX: once vmgen is improved, this instruction will be merged into
      #      `opcAddrReg`
      decodeB(rkAddress)
      case regs[rb].kind
      of rkHandle:
        regs[ra].setAddress(regs[rb].handle.h, regs[rb].handle.typ)
      else:
        raiseVmError(reportStr(
          rsemVmErrInternal,
          "limited VM support for 'addr', got kind: " & $regs[rb].kind))
    of opcLdDeref:
      # a = b[]
      decodeB(rkHandle)

      case regs[rb].kind
      of rkAddress:
        let r = addr regs[rb]
        assert r.addrTyp != nil, "Deref of untyped pointer"
        if not r.addrVal.isNil:
          # it's not an error to create an invalid handle from a pointer,
          # using the resulting handle is
          let p = makeMemPtr(r.addrVal.rawPointer, r.addrTyp.sizeInBytes)
          regs[ra].setHandle(makeLocHandle(p, r.addrTyp))
        else:
          raiseVmError(reportSem(rsemVmNilAccess))

      of rkRegAddr:
        # HACK: rkRegAddr is a hack
        assert regs[rb].regAddr.kind notin { rkLocation, rkHandle }
        copyRegister(regs[ra], regs[rb].regAddr[], c.memory)
      of rkHandle, rkLocation:
        checkHandle(regs[rb])
        assert regs[rb].handle.typ.kind == akRef

        let slot = deref(regs[rb].handle).refVal
        let h = c.heap.tryDeref(slot, regs[rb].handle.typ.targetType).value()

        regs[ra].setHandle(h)
      else:
        raiseVmError(reportStr(
          rsemVmNilAccess,
          " kind: " & $regs[rb].kind))

    of opcWrDeref:
      # a[] = c; b unused
      let rc = instr.regC

      # XXX: vmgen could use `opcLdDeref` and then mutate the target through
      #      the resulting handle (requires a new opcode: opcWrLoc).
      #      This would make `opcWrDeref` obsolete

      # Copies c into the location pointed to by a

      let r = addr regs[ra]
      case r.kind
      of rkAddress:
        assert r.addrTyp != nil, "Deref of untyped pointer" # vmgen issue
        if not r.addrVal.isNil:
          let cr = c.allocator.checkValid(r.addrVal, r.addrTyp)
          if cr == avrNoError:
            let p = makeMemPtr(r.addrVal.rawPointer, r.addrTyp.sizeInBytes)
            writeLoc(makeLocHandle(p, r.addrTyp), regs[rc], c.memory)
          else:
            raiseAccessViolation(cr, instLoc(0))

        elif r.addrVal.isNil:
          raiseVmError(reportSem(rsemVmNilAccess))

      of rkRegAddr:
        # HACK: remove this once vmgen is adjusted
        assert regs[rc].kind notin { rkLocation, rkHandle }

        copyRegister(regs[ra].regAddr[], regs[rc], c.memory)

      of rkHandle, rkLocation:
        checkHandle(regs[ra])
        assert r.handle.typ.kind == akRef

        let slot = deref(r.handle).refVal
        let h = c.heap.tryDeref(slot, r.handle.typ.targetType).value()

        writeLoc(h, regs[rc], c.memory)

      else:
        raiseVmError(reportStr(
          rsemVmErrInternal,
          "opcWrDeref: regs[ra].kind: " & $r.kind))

    of opcAddInt:
      decodeBC(rkInt)
      let
        bVal = regs[rb].intVal
        cVal = regs[rc].intVal
        sum = bVal +% cVal
      if (sum xor bVal) >= 0 or (sum xor cVal) >= 0:
        regs[ra].intVal = sum
      else:
        raiseVmError(reportSem(rsemVmOverOrUnderflow))
    of opcAddImmInt:
      decodeBImm(rkInt)
      #message(c.config, c.debug[pc], warnUser, "came here")
      #debug regs[rb].node
      let
        bVal = regs[rb].intVal
        cVal = imm
        sum = bVal +% cVal
      if (sum xor bVal) >= 0 or (sum xor cVal) >= 0:
        regs[ra].intVal = sum
      else:
        raiseVmError(reportSem(rsemVmOverOrUnderflow))
    of opcSubInt:
      decodeBC(rkInt)
      let
        bVal = regs[rb].intVal
        cVal = regs[rc].intVal
        diff = bVal -% cVal
      if (diff xor bVal) >= 0 or (diff xor not cVal) >= 0:
        regs[ra].intVal = diff
      else:
        raiseVmError(reportSem(rsemVmOverOrUnderflow))
    of opcSubImmInt:
      decodeBImm(rkInt)
      let
        bVal = regs[rb].intVal
        cVal = imm
        diff = bVal -% cVal
      if (diff xor bVal) >= 0 or (diff xor not cVal) >= 0:
        regs[ra].intVal = diff
      else:
        raiseVmError(reportSem(rsemVmOverOrUnderflow))
    of opcLenSeq:
      decodeBImm(rkInt)
      #assert regs[rb].kind == nkBracket
      let high = (imm and 1) # discard flags
      if (imm and nimNodeFlag) != 0:
        # TODO: there should be a standalone opcode for NimNode.len
        # used by mNLen (NimNode.len)
        regs[ra].intVal = regs[rb].nimNode.safeLen - high
      else:
        checkHandle(regs[rb])
        regs[ra].intVal = arrayLen(regs[rb].handle) - high
    of opcLenStr:
      # a = b.len - imm
      decodeBImm(rkInt)
      assert regs[rb].kind in {rkHandle, rkLocation}
      checkHandle(regs[rb])
      regs[ra].intVal = regs[rb].strVal.len - imm
    of opcLenCstring:
      # a = b.len - imm
      decodeBImm(rkInt)
      assert regs[rb].kind in {rkHandle, rkLocation}
      checkHandle(regs[rb])
      # XXX: cstring could be implemented in a C-like fashion now (i.e. as
      #      pointers)
      regs[ra].intVal = regs[rb].strVal.asCString().len - imm
    of opcIncl:
      # a = a incl b
      let rb = instr.regB

      assert regs[ra].handle.typ.kind == akSet

      checkHandle(regs[ra])
      bitSetIncl(mbitSet(regs[ra].handle), regs[rb].intVal)
    of opcInclRange:
      # a = a incl b..c
      let rb = instr.regB
      let rc = instr.regC

      assert regs[ra].handle.typ.kind == akSet

      checkHandle(regs[ra])
      bitSetInclRange(mbitSet(regs[ra].handle), regs[rb].intVal..regs[rc].intVal)

    of opcExcl:
      # a excl b
      let rb = instr.regB

      assert regs[ra].handle.typ.kind == akSet

      checkHandle(regs[ra])
      bitSetExcl(mbitSet(regs[ra].handle), regs[rb].intVal)

    of opcCard:
      # a = card(b)
      decodeB(rkInt)
      checkHandle(regs[rb])
      regs[ra].intVal = bitSetCard(bitSet(regs[rb].handle))
    of opcMulInt:
      decodeBC(rkInt)
      let
        bVal = regs[rb].intVal
        cVal = regs[rc].intVal
        product = bVal *% cVal
        floatProd = toBiggestFloat(bVal) * toBiggestFloat(cVal)
        resAsFloat = toBiggestFloat(product)
      if resAsFloat == floatProd:
        regs[ra].intVal = product
      elif 32.0 * abs(resAsFloat - floatProd) <= abs(floatProd):
        regs[ra].intVal = product
      else:
        raiseVmError(reportSem(rsemVmOverOrUnderflow))
    of opcDivInt:
      decodeBC(rkInt)
      if regs[rc].intVal == 0:
        raiseVmError(reportSem(rsemVmDivisionByConstZero))
      else: regs[ra].intVal = regs[rb].intVal div regs[rc].intVal
    of opcModInt:
      decodeBC(rkInt)
      if regs[rc].intVal == 0:
        raiseVmError(reportSem(rsemVmDivisionByConstZero))
      else:
        regs[ra].intVal = regs[rb].intVal mod regs[rc].intVal
    of opcAddFloat:
      decodeBC(rkFloat)
      regs[ra].floatVal = regs[rb].floatVal + regs[rc].floatVal
    of opcSubFloat:
      decodeBC(rkFloat)
      regs[ra].floatVal = regs[rb].floatVal - regs[rc].floatVal
    of opcMulFloat:
      decodeBC(rkFloat)
      regs[ra].floatVal = regs[rb].floatVal * regs[rc].floatVal
    of opcDivFloat:
      decodeBC(rkFloat)
      regs[ra].floatVal = regs[rb].floatVal / regs[rc].floatVal
    of opcShrInt:
      decodeBC(rkInt)
      let b = cast[uint64](regs[rb].intVal)
      let c = cast[uint64](regs[rc].intVal)
      let a = cast[int64](b shr c)
      regs[ra].intVal = a
    of opcShlInt:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal shl regs[rc].intVal
    of opcAshrInt:
      decodeBC(rkInt)
      regs[ra].intVal = ashr(regs[rb].intVal, regs[rc].intVal)
    of opcBitandInt:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal and regs[rc].intVal
    of opcBitorInt:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal or regs[rc].intVal
    of opcBitxorInt:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal xor regs[rc].intVal
    of opcAddu:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal +% regs[rc].intVal
    of opcSubu:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal -% regs[rc].intVal
    of opcMulu:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal *% regs[rc].intVal
    of opcDivu:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal /% regs[rc].intVal
    of opcModu:
      decodeBC(rkInt)
      regs[ra].intVal = regs[rb].intVal %% regs[rc].intVal
    of opcEqInt:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].intVal == regs[rc].intVal)
    of opcLeInt:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].intVal <= regs[rc].intVal)
    of opcLtInt:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].intVal < regs[rc].intVal)
    of opcEqFloat:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].floatVal == regs[rc].floatVal)
    of opcLeFloat:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].floatVal <= regs[rc].floatVal)
    of opcLtFloat:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].floatVal < regs[rc].floatVal)
    of opcLeu:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].intVal <=% regs[rc].intVal)
    of opcLtu:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].intVal <% regs[rc].intVal)
    of opcEqRef:
      # a = b == c
      decodeBC(rkInt)

      let a = addr regs[rb]
      let b = addr regs[rc]

      let ret =
        case a.kind
        of rkAddress:
          assert b.kind == rkAddress
          a.addrVal.rawPointer == b.addrVal.rawPointer
        of rkLocation, rkHandle:
          assert b.kind in {rkHandle, rkLocation}
          assert a.handle.typ.kind == b.handle.typ.kind
          checkHandle(regs[rb])
          checkHandle(regs[rc])

          let
            aAtom = deref(a.handle)
            bAtom = deref(b.handle)

          template cmpF(n): bool = aAtom.n == bAtom.n

          case a.handle.typ.kind
          of akRef:      cmpF(refVal)
          of akCallable: cmpF(callableVal)
          of akClosure:  cmpF(closureVal)
          else: unreachable() # vmgen issue
        of rkRegAddr:
          assert b.kind == rkRegAddr
          a.regAddr == b.regAddr
        of rkNimNode:
          assert b.kind == rkNimNode
          a.nimNode == b.nimNode
        else:
          unreachable() # vmgen issue

      regs[ra].intVal = ord(ret)
    of opcEqNimNode:
      decodeBC(rkInt)
      regs[ra].intVal =
        ord(exprStructuralEquivalent(regs[rb].nimNode, regs[rc].nimNode,
                                     strictSymEquality=true))
    of opcSameNodeType:
      decodeBC(rkInt)
      # TODO: Look into me!
      #[
        Thanks to @Elegantbeef for being a bro and figuring out what this should be...
        This is the VM instruction for sameType and there is a bug where:
          `type A[T] = int sameType(A[int], int) != sameType(int, A[int])`
        Generic aliases are only done for generic -> generic
        There is no logic for generic into non generic
        Skipping over generic insts results in some generics failing
        Realistically we need a tyGenericAlias instead of using tyGenericInst then reasoning into it
      ]#
      regs[ra].intVal = ord(regs[rb].nimNode.typ.sameTypeOrNil(regs[rc].nimNode.typ, {ExactTypeDescValues, ExactGenericParams}))
      # The types should exactly match which is why we pass `{ExactTypeDescValues, ExactGenericParams}`.
      # This solves checks with generics.
    of opcXor:
      decodeBC(rkInt)
      regs[ra].intVal = ord(regs[rb].intVal != regs[rc].intVal)
    of opcNot:
      decodeB(rkInt)
      #assert regs[rb].kind == rkInt
      regs[ra].intVal = 1 - regs[rb].intVal
    of opcUnaryMinusInt:
      decodeB(rkInt)

      #assert regs[rb].kind == rkInt
      let val = regs[rb].intVal
      if val != int64.low:
        regs[ra].intVal = -val
      else:
        raiseVmError(reportSem(rsemVmOverOrUnderflow))
    of opcUnaryMinusFloat:
      decodeB(rkFloat)

      #assert regs[rb].kind == rkFloat
      regs[ra].floatVal = -regs[rb].floatVal
    of opcBitnotInt:
      decodeB(rkInt)

      #assert regs[rb].kind == rkInt
      regs[ra].intVal = not regs[rb].intVal
    of opcEqStr:
      decodeBC(rkInt)
      checkHandle(regs[rb])
      checkHandle(regs[rc])
      # XXX: once `cast` is fully supported in the VM, the strings need to be
      #      validated as well (not just here, but everywhere)
      regs[ra].intVal = ord(regs[rb].strVal == regs[rc].strVal)
    of opcLeStr:
      decodeBC(rkInt)
      checkHandle(regs[rb])
      checkHandle(regs[rc])
      regs[ra].intVal = ord(regs[rb].strVal <= regs[rc].strVal)
    of opcLtStr:
      decodeBC(rkInt)
      checkHandle(regs[rb])
      checkHandle(regs[rc])
      regs[ra].intVal = ord(regs[rb].strVal < regs[rc].strVal)
    of opcLeSet:
      # a = b is subset of c
      decodeBC(rkInt)
      checkHandle(regs[rb])
      checkHandle(regs[rc])
      regs[ra].intVal = ord(bitSetContains(bitSet(regs[rb].handle), bitSet(regs[rc].handle)))
    of opcEqSet:
      # a = b == c
      decodeBC(rkInt)
      checkHandle(regs[rb])
      checkHandle(regs[rc])

      regs[ra].intVal = ord(
        bitSetEquals(
          bitSet(regs[rb].handle),
          bitSet(regs[rc].handle),
        ))
    of opcLtSet:
      # a = b is proper subset of c
      decodeBC(rkInt)
      checkHandle(regs[rb])
      checkHandle(regs[rc])

      let a = regs[rb].handle
      let b = regs[rc].handle

      # TODO: bitsets.nim is missing an "is-proper-subset" test...
      regs[ra].intVal = ord(
        bitSetContains(bitSet(a), bitSet(b)) and not bitSetEquals(bitSet(a), bitSet(b)))
    of opcMulSet:
      # a = intersection(b, c)
      let rb = instr.regB
      let rc = instr.regC

      checkHandle(regs[rb])
      checkHandle(regs[rc])

      regs[ra].initLocReg(regs[rb].handle.typ, c.memory)

      safeCopyMemSrc(mbitSet(regs[ra].handle), bitSet(regs[rb].handle))
      bitSetIntersect(mbitSet(regs[ra].handle), bitSet(regs[rc].handle))

    of opcPlusSet:
      # a = union(b, c)
      let rb = instr.regB
      let rc = instr.regC

      checkHandle(regs[rb])
      checkHandle(regs[rc])

      regs[ra].initLocReg(regs[rb].handle.typ, c.memory)

      safeCopyMemSrc(mbitSet(regs[ra].handle), bitSet(regs[rb].handle))
      bitSetUnion(mbitSet(regs[ra].handle), bitSet(regs[rc].handle))

    of opcMinusSet:
      # a = diff(b, c)
      let rb = instr.regB
      let rc = instr.regC

      checkHandle(regs[rb])
      checkHandle(regs[rc])

      regs[ra].initLocReg(regs[rb].handle.typ, c.memory)

      safeCopyMemSrc(mbitSet(regs[ra].handle), bitSet(regs[rb].handle))
      bitSetDiff(mbitSet(regs[ra].handle), bitSet(regs[rc].handle))

    of opcConcatStr:
      # Concat all values from register range rb..<rc together. Integer values
      # are automatically treated as char
      let rb = instr.regB
      let rc = instr.regC

      func appendstr(str: var VmString, r: TFullReg, a: var VmAllocator) =
        case r.kind
        of rkInt:
          str.add(chr(r.intVal), a)
        of rkHandle, rkLocation:
          checkHandle(a, r.handle)
          assert r.handle.typ.kind == akString # vmgen issue
          str.add(r.strVal, a)
        else:
          assert false # vmgen issue

      # TODO: first resize the string to the computed new length, then do the
      #       concatenation in place
      regs[ra].initLocReg(c.typeInfoCache.stringType, c.memory)

      for i in rb..rb+rc-1:
        regs[ra].strVal.appendstr(regs[i], c.allocator)
    of opcAddStrCh:
      # a += b
      let rb = instr.regB
      checkHandle(regs[ra])
      regs[ra].strVal.add(regs[rb].intVal.chr, c.allocator)
    of opcAddStrStr:
      # a += b
      let rb = instr.regB
      checkHandle(regs[ra])
      checkHandle(regs[rb])
      regs[ra].strVal.add(regs[rb].strVal, c.allocator)
    of opcAddSeqElem:
      # a.add(b)
      let rb = instr.regB
      checkHandle(regs[ra])
      checkHandle(regs[rb])

      # XXX: could encode in immediate value whether b should be moved
      #      or copied

      assert regs[ra].kind in {rkLocation, rkHandle}

      let typ = regs[ra].handle.typ
      assert typ.kind == akSeq

      let s = addr regs[ra].atomVal.seqVal
      growBy(s[], typ, 1, c.memory)
      getItemHandle(s[], typ, s.length - 1).writeLoc(regs[rb], c.memory)
    of opcGetImpl:
      decodeB(rkNimNode)
      var a = regs[rb].nimNode
      if a.kind == nkVarTy: a = a[0]
      if a.kind == nkSym:
        regs[ra].nimNode = if a.sym.ast.isNil: newNode(nkNilLit)
                          else: copyTree(a.sym.ast)
      else:
        raiseVmError(reportSem(rsemVmNodeNotASymbol))

    of opcGetImplTransf:
      decodeB(rkNimNode)
      let a = regs[rb].nimNode
      if a.kind == nkSym:
        regs[ra].nimNode =
          if a.sym.ast.isNil:
            newNode(nkNilLit)
          else:
            let ast = a.sym.ast.shallowCopy
            for i in 0..<a.sym.ast.len:
              ast[i] = a.sym.ast[i]
            ast[bodyPos] = transformBody(c.graph, c.idgen, a.sym, cache=true)
            ast.copyTree()
      else:
        raiseVmError(reportSem(rsemVmNodeNotASymbol))

    of opcExpandToAst:
      decodeBC(rkNimNode)

      let
        callExprAst = regs[rb].nimNode
        prc =         callExprAst[0].sym
        prevFrame =   c.sframes[tos].next

      assert callExprAst.kind in nkCallKinds
      assert prc.kind == skTemplate

      let genSymOwner = if prevFrame > 0 and c.sframes[prevFrame].prc != nil:
                          c.sframes[prevFrame].prc
                        else:
                          c.module
      var templCall = newNodeI(nkCall, c.debug[pc])
      templCall.add(newSymNode(prc))
      for i in 1..rc-1:
        let node = c.regToNode(regs[rb+i], callExprAst[i].typ, c.debug[pc])
        node.info = c.debug[pc]
        templCall.add(node)

      var a = evalTemplate(templCall, prc, genSymOwner, c.config, c.cache, c.templInstCounter, c.idgen)
      if a.kind == nkStmtList and a.len == 1: # flatten if a single statement
        a = a[0]

      regs[ra].nimNode = a

    of opcSymOwner:
      decodeB(rkNimNode)
      let a = regs[rb].nimNode
      if a.kind == nkSym:
        regs[ra].nimNode = if a.sym.owner.isNil: newNode(nkNilLit)
                          else: newSymNode(a.sym.skipGenericOwner)
      else:
        raiseVmError(reportSem(rsemVmNodeNotASymbol))
    of opcSymIsInstantiationOf:
      decodeBC(rkInt)
      let a = regs[rb].nimNode
      let b = regs[rc].nimNode
      if a.kind == nkSym and a.sym.kind in skProcKinds and
         b.kind == nkSym and b.sym.kind in skProcKinds:
        regs[ra].intVal =
          if sfFromGeneric in a.sym.flags and a.sym.owner == b.sym: 1
          else: 0
      else:
        raiseVmError(reportSem(rsemVmNodeNotAProcSymbol))

    of opcEcho:
      # XXX: could be a candidate for turning into a callback

      let rb = instr.regB
      template fn(s: string) =
        localReport(c.config, InternalReport(msg: s, kind: rintEchoMessage))

      if rb == 1:
        checkHandle(regs[ra])
        fn($regs[ra].strVal)

      else:
        var outp = ""
        for i in ra..ra+rb-1:
          checkHandle(regs[i])
          #if regs[i].kind != rkNode: debug regs[i]
          outp.add(regs[i].strVal)

        fn(outp)
    of opcContainsSet:
      # a = c in b
      decodeBC(rkInt)
      let val = regs[rc].intVal
      assert val >= 0 and val <= high(uint16).BiggestInt

      checkHandle(regs[rb])
      regs[ra].intVal = ord(bitSetIn(bitSet(regs[rb].handle), regs[rc].intVal))
    of opcParseFloat:
      # TODO: this op has really unusual semantics. Turn it into a callback?

      # a = number of chars read
      # rc[] = parseFloat(rb, rd)
      decodeBC(rkInt)
      inc pc
      assert c.code[pc].opcode == opcParseFloat
      let rd = c.code[pc].regA

      var rcAddr = addr(regs[rc])
      if rcAddr.kind == rkRegAddr: rcAddr = rcAddr.regAddr
      else:
        ensureKind(regs[rc], rkFloat, c.memory)

      checkHandle(regs[rb])
      # TODO: don't do a string copy here
      regs[ra].intVal = parseBiggestFloat($regs[rb].strVal,
                                          rcAddr[].floatVal, regs[rd].intVal.int)
    of opcRangeChck:
      # Checks if a is in range [b, c], aborts execution otherwise
      let rb = instr.regB
      let rc = instr.regC

      func isLessEqual(a, b: ptr TFullReg): bool =
        ## Returns whether a <= b, converting the operands prior, if necessary

        # This has the same behaviour as `semfold.leValueConv` (which was used
        # previously)
        case a.kind
        of rkInt:
          case b.kind
          of rkInt: a.intVal <= b.intVal
          of rkFloat: a.intVal <= round(b.floatVal).int
          else: false
        of rkFloat:
          case b.kind
          of rkInt: a.floatVal <= toFloat64(toInt128(b.intVal))
          of rkFloat: a.floatVal <= b.floatVal
          else: false
        else: false

      func toStr(x: TFullReg): string =
        case x.kind
        of rkInt: $x.intVal
        of rkFloat: $x.floatVal
        else: unreachable($x.kind)

      let a = addr regs[ra]
      let l = addr regs[rb]
      let h = addr regs[rc]

      if not (isLessEqual(l, a) and
              isLessEqual(a, h)):
        # TODO: are range errors `Exception`s or `Defect`s in normal Nim? If
        #       they are `Exception`s we should raise a user catchable error
        #       here
        raiseVmError(reportStr(
          rsemVmIllegalConv,
          errIllegalConvFromXtoY % [
            regs[ra].toStr,
            "[" & regs[rb].toStr & ".." & regs[rc].toStr & "]"
          ]))

    of opcArrCopy:
      let rb = instr.regB
      let rc = instr.regC

      proc collectInfo(h: LocHandle): tuple[size: int, data: MemRegionPtr, eT: PVmType] =
        let typ = h.typ
        case typ.kind
        of akSeq:
          result = (
            deref(h).seqVal.length,
            deref(h).seqVal.data,
            typ.seqElemType
          )
        of akArray:
          result = (
            typ.elementCount,
            h.h,
            typ.elementType
          )
        else:
          assert false

      checkHandle(regs[ra])
      checkHandle(regs[rb])

      let dest = collectInfo(regs[ra].handle)
      let src = collectInfo(regs[rb].handle)

      assert dest.eT == src.eT

      let L = regs[rc].intVal

      if L > dest.size:
        # XXX: since opcArrCopy is only used internally be vmgen, this should
        #      probably be an assert
        raiseVmError(reportVmIdx(L, dest.size - 1))

      if L > src.size:
        # XXX: same comment as aboves
        raiseVmError(reportVmIdx(L, src.size - 1))

      c.memory.arrayCopy(dest.data, src.data, L, src.eT, true)

    of opcIndCall, opcIndCallAsgn:
      # dest = call regStart, n; where regStart = fn, arg1, ...
      let rb = instr.regB
      let rc = instr.regC

      checkHandle(regs[rb])

      let h = regs[rb].handle
      let (fPtr, isClosure) =
        case h.typ.kind
        of akCallable: (deref(h).callableVal, false)
        of akClosure: (deref(h).closureVal.fnc, true)
        else:
          assert false # vmgen issue
          (default(VmFunctionPtr), false)

      if unlikely(fPtr.isNil):
        raiseVmError(reportSem(rsemVmNilAccess))

      let bb = c.functions[int toFuncIndex(fPtr)]
      assert bb.typ == h.typ.funcTypeId
      let retType = bb.retValDesc

      let prc = bb.prc
      case bb.kind:
      of ckCallback:
        # it's a callback:
        if instr.opcode == opcIndCallAsgn:
          # XXX: instead of doing this manually here, vmgen will emit
          #      `opcLdNull(Reg)` in the future instead
          # setup register that will store the result
          if not loadEmptyReg(regs[ra], retType, c.debug[pc], c.memory):
            regs[ra].initLocReg(retType, c.memory)

        # We have to assume that the callback makes use of it's parameters and
        # thus need to validate them here
        for i in (rb+1)..<(rb+rc):
          checkHandle(regs[i])

        c.callbacks[bb.cbOffset].value(
          VmArgs(ra: ra, rb: rb, rc: rc, slots: cast[ptr UncheckedArray[TFullReg]](addr regs[0]),
                 currentException: c.currentExceptionA,
                 currentLineInfo: c.debug[pc],
                 typeCache: addr c.typeInfoCache,
                 mem: addr c.memory,
                 heap: addr c.heap))
      of ckDefault:
        let newPc = compile(c, prc)
        # tricky: a recursion is also a jump back, so we use the same
        # logic as for loops:
        if newPc < pc: handleJmpBack()
        #echo "new pc ", newPc, " calling: ", prc.name.s
        var newFrame = TStackFrame(prc: prc, comesFrom: pc, next: tos)
        newFrame.slots.newSeq(prc.offset+ord(isClosure))
        if retType.isValid:
          # TODO: instead of initializing the result register here, it should
          #       be done by vmgen via `opcLdNull`
          if not loadEmptyReg(newFrame.slots[0], retType, c.debug[pc], c.memory):
            newFrame.slots[0].initLocReg(retType, c.memory)
        for i in 1..rc-1:
          newFrame.slots[i].fastAsgnComplex(regs[rb+i])

        let envPType = bb.envParamType
        if isClosure:
          let env = deref(h).closureVal.env
          if not env.isNil:
            # slots[rc] = closure env
            assert envPType != noneType # vmgen should've prevented this
            # TODO: also check if the env type is valid
            if c.heap.isValid(env):
              newFrame.slots[rc].initLocReg(envPType, c.memory)
              deref(newFrame.slots[rc].handle).refVal.asgnRef(env, c.memory, reset=false)
            else:
              # Since the closure env is protected from to the guest, it
              # should not be possible that it becomes invalid.
              raiseVmError(reportStr(
                rsemVmErrInternal,
                "closure env is invalid"))

          else:
            assert envPType == noneType # A programming error that should have
                                        # already been caught by `opWrClosure`

        pushFrame(newFrame)
        # -1 for the following 'inc pc'
        pc = newPc-1

    of opcTJmp:
      # jump Bx if A != 0
      let rbx = instr.regBx - wordExcess - 1 # -1 for the following 'inc pc'
      if regs[ra].intVal != 0:
        inc pc, rbx
    of opcFJmp:
      # jump Bx if A == 0
      let rbx = instr.regBx - wordExcess - 1 # -1 for the following 'inc pc'
      if regs[ra].intVal == 0:
        inc pc, rbx
    of opcJmp:
      # jump Bx
      let rbx = instr.regBx - wordExcess - 1 # -1 for the following 'inc pc'
      inc pc, rbx
    of opcJmpBack:
      let rbx = instr.regBx - wordExcess - 1 # -1 for the following 'inc pc'
      inc pc, rbx
      handleJmpBack()
    of opcBranch:
      # we know the next instruction is a 'fjmp':
      let value = c.constants[instr.regBx-wordExcess]

      checkHandle(regs[ra])

      func contains[T](list: openArray[Slice[T]], v: T): bool =
        for s in list.items:
          if v in s: return true

      var cond = false
      case value.kind
      of cnstInt:      cond = regs[ra].intVal == value.intVal
      of cnstString:   cond = regs[ra].strVal == deref(value.strVal).strVal
      of cnstFloat:    cond = regs[ra].floatVal == value.floatVal
      of cnstSliceListInt:   cond = regs[ra].intVal in value.intSlices
      of cnstSliceListFloat: cond = regs[ra].floatVal in value.floatSlices
      of cnstSliceListStr:
        # string slice-lists don't store the strings directly, but the ID of
        # a constant instead
        let str = regs[ra].strVal
        for s in value.strSlices.items:
          let a = deref(c.constants[s.a].strVal).strVal
          let r = cmp(a, str)
          if s.a == s.b:
            # no need to compare the string with both slice elements if
            # they're the same
            if r == 0:
              cond = true
              break
          else:
            let b = deref(c.constants[s.b].strVal).strVal
            if r <= 0 and cmp(str, b) <= 0:
              cond = true
              break

      else:
        unreachable($value.kind)

      assert c.code[pc+1].opcode == opcFJmp
      inc pc
      # we skip this instruction so that the final 'inc(pc)' skips
      # the following jump
      if not cond:
        let instr2 = c.code[pc]
        let rbx = instr2.regBx - wordExcess - 1 # -1 for the following 'inc pc'
        inc pc, rbx
    of opcTry:
      let rbx = instr.regBx - wordExcess
      c.sframes[tos].pushSafePoint(pc + rbx)
      assert c.code[pc+rbx].opcode in {opcExcept, opcFinally}
    of opcExcept:
      # This opcode is never executed, it only holds information for the
      # exception handling routines.
      doAssert(false)
    of opcFinally:
      # Pop the last safepoint introduced by a opcTry. This opcode is only
      # executed _iff_ no exception was raised in the body of the `try`
      # statement hence the need to pop the safepoint here.
      doAssert(savedPC < 0)
      c.sframes[tos].popSafePoint()
    of opcFinallyEnd:
      # The control flow may not resume at the next instruction since we may be
      # raising an exception or performing a cleanup.
      if savedPC >= 0:
        pc = savedPC - 1
        savedPC = -1
        if tos != savedFrame:
          gotoFrame(savedFrame)
    of opcRaise:
      decodeBImm()
      checkHandle(regs[ra])

      # `imm == 0` -> raise; `imm == 1` -> reraise current exception
      let isReraise = imm == 1

      let raisedRef =
        if isReraise:
          # TODO: must raise a defect when there's no current exception
          c.currentExceptionA
        else:
          assert regs[ra].handle.typ.kind == akRef
          regs[ra].atomVal.refVal

      let raised = c.heap.tryDeref(raisedRef, noneType).value()
      let excType = raised.typ

      # XXX: the exception is never freed right now

      # Keep the exception alive during exception handling
      c.heap.heapIncRef(raisedRef)
      if not c.currentExceptionA.isNil:
        c.heap.heapDecRef(c.allocator, c.currentExceptionA)

      c.currentExceptionA = raisedRef
      c.exceptionInstr = pc

      let name = deref(raised.getFieldHandle(1.fpos))
      if not isReraise and name.strVal.len == 0:
        # XXX: the VM doesn't distinguish between a `nil` cstring and an empty
        #      `cstring`, leading to the name erroneously being overridden if
        #      it was explicitly initialized with `""`
        # Set the `name` field of the exception. No need to valdiate the
        # handle in `regs[rb]`, since it's a constant loaded prior to the
        # raise
        name.strVal.asgnVmString(regs[rb].strVal, c.allocator)

      var frame = tos
      var jumpTo = findExceptionHandler(c, c.sframes[frame], excType)
      while jumpTo.why == ExceptionGotoUnhandled and frame > 0:
        dec frame
        jumpTo = findExceptionHandler(c, c.sframes[frame], excType)

      case jumpTo.why:
      of ExceptionGotoHandler:
        # Jump to the handler, do nothing when the `finally` block ends.
        savedPC = -1
        pc = jumpTo.where - 1
        if tos != frame:
          unwindToFrame(frame)
      of ExceptionGotoFinally:
        # Jump to the `finally` block first then re-jump here to continue the
        # traversal of the exception chain
        savedPC = pc
        savedFrame = tos
        pc = jumpTo.where - 1
        if tos != frame:
          gotoFrame(frame)
      of ExceptionGotoUnhandled:
        # Nobody handled this exception, error out.
        reportException(c, tos, raised)

    of opcNew:
      let typ = c.types[instr.regBx - wordExcess]
      assert typ.kind == akRef

      # typ is the ref type, not the target type
      let slot = c.heap.heapNew(c.allocator, typ.targetType)
      regs[ra].initLocReg(typ, c.memory)
      regs[ra].atomVal.refVal = slot
    of opcNewSeq:
      let typ = c.types[instr.regBx - wordExcess]
      inc pc
      let instr2 = c.code[pc]
      let count = regs[instr2.regA].intVal.int

      assert typ.kind == akSeq
      regs[ra].initLocReg(typ, c.memory)
      newVmSeq(regs[ra].atomVal.seqVal, typ, count, c.memory)
    of opcNewStr:
      let rb = instr.regB
      regs[ra].initLocReg(c.typeInfoCache.stringType, c.memory)
      newVmString(regs[ra].atomVal.strVal, regs[rb].intVal.int, c.allocator)
    of opcLdImmInt:
      # dest = immediate value
      decodeBx(rkInt)
      regs[ra].intVal = rbx
    of opcLdNull:
      let typ = c.types[instr.regBx - wordExcess]
      regs[ra].initLocReg(typ, c.memory)
    of opcLdNullReg:
      let typ = c.types[instr.regBx - wordExcess]
      if loadEmptyReg(regs[ra], typ, c.debug[pc], c.memory):
        discard
      else:
        # XXX: this case was previously just ignored
        assert false, "vmgen issue"
    of opcLdConst:
      decodeBx()
      # XXX: register clean-up/transition will be done via a dedicated
      #      instruction in the future
      cleanUpReg(regs[ra], c.memory)

      let cnst = c.constants[rbx]
      case cnst.kind
      of cnstInt:
        regs[ra] = TFullReg(kind: rkInt, intVal: cnst.intVal)
      of cnstFloat:
        regs[ra] = TFullReg(kind: rkFloat, floatVal: cnst.floatVal)
      of cnstString:
        regs[ra] = TFullReg(kind: rkHandle, handle: cnst.strVal)
      of cnstNode:
        # XXX: cnstNode is also used for non-NimNodes, so using `rkNimNode` is
        #      somewhat wrong. Introducing a new register kind just for the
        #      cases where non-NimNode PNodes need to be stored in registers
        #      seems unnecessary however.
        regs[ra] = TFullReg(kind: rkNimNode, nimNode: cnst.node)
      of cnstSliceListInt..cnstSliceListStr:
        # A slice-list must not be used with `LdConst`
        assert false

    of opcAsgnConst:
      # XXX: currently unused, but might be revived
      assert false
      #[
      decodeBx()
      let cnst = c.constants[rbx]
      if cnst.typ.kind in RegisterAtomKinds:
        putIntoReg(regs[ra], cnst)
      else:
        regs[ra].initLocReg(cnst.typ, c.memory)
        copyToLocation(regs[ra].handle, cnst, c.memory, false)
      ]#
    of opcLdGlobal:
      let rb = instr.regBx - wordExcess - 1
      let slot = c.globals[rb]
      ensureKind(rkHandle)
      regs[ra].setHandle(c.heap.slots[slot].handle)
    of opcLdGlobalAddr:
      # a = addr to globals[b]
      let rb = instr.regBx - wordExcess - 1 # -1 since global indices are
                          # 1-based (zero indicates not-initialized state)

      let handle = c.heap.slots[c.globals[rb]].handle
      ensureKind(rkAddress)
      regs[ra].setAddress(handle.h, handle.typ)
    of opcMatConst:
      # Load the materialized value of `const c` into 'ra'. If it hasn't been
      # materialized yet, materialize it. Materialize here means translating
      # the PNode-tree structure into VM data via `vmcompilerserdes`
      decodeBx(rkHandle)

      let id = LitId(rbx)
      let mc = addr c.materializedConsts[id]
      if not mc.handle.isValid:
        # The previous code in `vmgen` tested whether sym.ast is nil and used
        # `sym.typ.n` if it was. However, `sym.ast` seems to never be nil (none
        # of the current tests are failing)
        let n = mc.sym.ast
        assert n != nil

        # The type was already generated by `vmgen`, we just need to look it up
        let typ = c.typeInfoCache.lookup(c.config, mc.sym.typ)

        mc.handle = c.allocator.allocConstantLocation(typ.unsafeGet)
        # TODO: strings, seq and other values using allocation also need to be
        #       allocated with `allocConstLocation` inside `serialize` here
        c.serialize(n, mc.handle)

      regs[ra].setHandle(mc.handle)

    of opcRepr:
      # Depending on whether or not arc/orc is used for the active compilation.
      # opcRepr is:
      # - arc/orc: used only for `repr(NimNode)` (see `system/repr_v2`)
      # - refc: used for every non user-supplied `repr` call

      # HACK: We need type information to deserialize. But this is very
      #       inefficient. Not only is opcRepr a two-instruction-word opcode
      #       now, we're also deserializing an object just to throw it away
      #       after rendering
      let typ = c.rtti[instr.regBx - wordExcess]
      inc pc
      let instr2 = c.code[pc]
      let rb = instr2.regB

      checkHandle(regs[rb])

      let str = renderTree(c.regToNode(regs[rb], typ.nimType, c.debug[pc]),
                           {renderNoComments, renderDocComments})

      regs[ra].initLocReg(c.typeInfoCache.stringType, c.memory)
      regs[ra].strVal.newVmString(str, c.allocator)
    of opcQuit:
      if c.mode in {emRepl, emStaticExpr, emStaticStmt}:
        localReport(c.config, c.debug[pc], InternalReport(kind: rintQuitCalled))
        # TODO: this will crash the compiler/vm (RangeDefect) if `quit` is
        #       called with a value outside of int8 range!
        msgQuit(int8(regs[ra].intVal))
      else:
        # TODO: we need to tell the caller that register zero isn't valid in
        #       this case. In general, the caller must be informed of
        #       non-default vm exits via `rawExecute`'s return value
        return 0
    of opcInvalidField:
      # REFACTOR this opcode is filled in the
      # `vmgen.genCheckedObjAccessAux` and contains expression for the
      # incorrect node kind (generated using `astmsgs.genFieldDefect`
      # call). For now I left it as-is, but in the future this needs to be
      # cleaned up - hardcoded error message for missing field is insanely
      # verbose.

      # No need to validate the string handles (they're directly supplied by
      # constants)
      let msg = $regs[ra].strVal
      let discr = $regs[instr.regB].strVal
      let msg2 = formatFieldDefect(msg, discr)
      raiseVmError(reportStr(rsemVmFieldInavailable, msg2))
    of opcSetLenStr:
      # a.setLen(b)
      let rb = instr.regB
      #createStrKeepNode regs[ra]
      checkHandle(regs[ra])
      regs[ra].strVal.setLen(regs[rb].intVal.int, c.allocator)
    of opcOf:
      # a == b of c:type
      decodeBC(rkInt)
      checkHandle(regs[rb])

      let handle = regs[rb].handle
      let typ = c.types[regs[rc].intVal.int]

      assert handle.typ.kind == akRef
      let refVal = deref(handle).refVal

      let ret =
        if not refVal.isNil:
          let h = c.heap.tryDeref(refVal, noneType).value()
          getTypeRel(h.typ, typ) in {vtrSame, vtrSub}
        else:
          false

      regs[ra].intVal = ord(ret)

    of opcSetLenSeq:
      # a.setLen(b)
      let rb = instr.regB
      checkHandle(regs[ra])

      let newLen = regs[rb].intVal.int
      assert regs[ra].handle.typ.kind == akSeq, "opcSetLenSeq: Not a seq"

      regs[ra].atomVal.seqVal.setLenSeq(regs[ra].handle.typ, newLen, c.memory)
    of opcNarrowS:
      decodeB(rkInt)
      let min = -(1.BiggestInt shl (rb-1))
      let max = (1.BiggestInt shl (rb-1))-1
      if regs[ra].intVal < min or regs[ra].intVal > max:
        raiseVmError(reportSem(rsemVmOutOfRange))
    of opcNarrowU:
      decodeB(rkInt)
      regs[ra].intVal = regs[ra].intVal and ((1'i64 shl rb)-1)
    of opcSignExtend:
      # like opcNarrowS, but no out of range possible
      decodeB(rkInt)
      let imm = 64 - rb
      regs[ra].intVal = ashr(regs[ra].intVal shl imm, imm)
    of opcIsNil:
      decodeB(rkInt)
      var res = false

      case regs[rb].kind
      of rkHandle, rkLocation:
        checkHandle(regs[ra])

        let atom = deref(regs[rb].handle)
        case regs[rb].handle.typ.kind
        of akSeq, akString:
          # opcIsNil is to implement the `mIsNil` magic, used by the `isNil`
          # system procs. The `isNil` overloads for string and seq still exist
          # in system.nim, so we have to account for that here

          # XXX: turn this case into an error once the `isNil` overloads for string and seq
          # are removed
          res = false
        of akRef:
          res = atom.refVal == 0
        of akPtr:
          res = atom.ptrVal == nil
        of akCallable:
          res = atom.callableVal.isNil
        of akClosure:
          res = atom.closureVal.fnc.isNil
        of akInt, akFloat, akSet, akObject, akArray, akPNode, akDiscriminator:
          unreachable($regs[rb].kind)
      of rkNimNode:
        res = regs[rb].nimNode.kind == nkNilLit
      of rkAddress:
        res = regs[rb].addrVal.isNil
      of rkRegAddr:
        # A reg address can never be nil
        res = true
      of rkNone, rkInt, rkFloat:
        assert false # vmgen issue


      regs[ra].intVal = ord(res)
    of opcNBindSym:
      # cannot use this simple check
      # if dynamicBindSym notin c.config.features:

      # bindSym with static input
      decodeBx(rkNimNode)
      regs[ra].nimNode = copyTree(c.constants[rbx].node)
    of opcNDynBindSym:
      # experimental bindSym
      ensureKind(rkNimNode)
      let
        rb = instr.regB
        rc = instr.regC
        idx = int(regs[rb+rc-1].intVal)
        callback = c.callbacks[idx].value
        args = VmArgs(ra: ra, rb: rb, rc: rc, slots: cast[ptr UncheckedArray[TFullReg]](addr regs[0]),
                currentException: c.currentExceptionA,
                currentLineInfo: c.debug[pc],
                typeCache: addr c.typeInfoCache,
                mem: addr c.memory)
      callback(args)
    of opcNChild:
      decodeBC(rkNimNode)
      let idx = regs[rc].intVal.int
      let src = regs[rb].nimNode
      # TODO: This if-else block should be reordered so as to match the
      #       expectation of occurence
      if src.kind in {nkEmpty..nkNilLit}:
        raiseVmError(reportAst(rsemVmCannotGetChild, src))
      elif idx >=% src.len:
        raiseVmError(reportVmIdx(idx, src.len - 1))
      else:
        regs[ra].nimNode = src[idx]
    of opcNSetChild:
      decodeBC(rkNimNode)
      let idx = regs[rb].intVal.int
      var dest = regs[ra].nimNode
      if nfSem in dest.flags and allowSemcheckedAstModification notin c.config.legacyFeatures:
        raiseVmError(reportSem(rsemVmCannotModifyTypechecked))
      elif dest.kind in {nkEmpty..nkNilLit}:
        raiseVmError(reportAst(rsemVmCannotSetChild, dest))
      elif idx >=% dest.len:
        raiseVmError(reportVmIdx(idx, dest.len - 1))
      else:
        dest[idx] = regs[rc].nimNode
    of opcNAdd:
      decodeBC(rkNimNode)
      var u = regs[rb].nimNode
      if nfSem in u.flags and allowSemcheckedAstModification notin c.config.legacyFeatures:
        raiseVmError(reportSem(rsemVmCannotModifyTypechecked))
      elif u.kind in {nkEmpty..nkNilLit}:
        echo c.config $ c.debug[pc]
        raiseVmError(reportAst(rsemVmCannotAddChild, u))
      else:
        u.add(regs[rc].nimNode)
      regs[ra].nimNode = u

    of opcNAddMultiple:
      decodeBC(rkNimNode)
      checkHandle(regs[rc])
      let typ = regs[rc].handle.typ
      assert typ.kind in {akSeq, akArray} # varargs
      assert typ.elemType().kind == akPNode
      let x = regs[rc].handle
      var u = regs[rb].nimNode
      if nfSem in u.flags and allowSemcheckedAstModification notin c.config.legacyFeatures:
        raiseVmError(reportSem(rsemVmCannotModifyTypechecked))
      elif u.kind in {nkEmpty..nkNilLit}:
        raiseVmError(reportAst(rsemVmCannotAddChild, u))
      else:
        let L = arrayLen(x)
        for i in 0..<L:
          u.add(deref(getItemHandle(x, i)).nodeVal)
      regs[ra].nimNode = u

    of opcNKind:
      decodeB(rkInt)
      regs[ra].intVal = ord(regs[rb].nimNode.kind)
      c.comesFromHeuristic = regs[rb].nimNode.info

    of opcNSymKind:
      decodeB(rkInt)
      let a = regs[rb].nimNode
      if a.kind == nkSym:
        regs[ra].intVal = ord(a.sym.kind)
      else:
        raiseVmError(reportSem(rsemVmNodeNotASymbol))
      c.comesFromHeuristic = a.info

    of opcNIntVal:
      decodeB(rkInt)
      let a = regs[rb].nimNode
      if a.kind in {nkCharLit..nkUInt64Lit}:
        regs[ra].intVal = a.intVal
      elif a.kind == nkSym and a.sym.kind == skEnumField:
        regs[ra].intVal = a.sym.position
      else:
        raiseVmError(reportStr(rsemVmFieldNotFound, "intVal"))
    of opcNFloatVal:
      decodeB(rkFloat)
      let a = regs[rb].nimNode
      case a.kind
      of nkFloatLit..nkFloat64Lit: regs[ra].floatVal = a.floatVal
      else: raiseVmError(reportStr(rsemVmFieldNotFound, "floatVal"))
    of opcNodeId:
      decodeB(rkInt)
      regs[ra].intVal = regs[rb].nimNode.id
    of opcNGetType:
      let rb = instr.regB
      let rc = instr.regC

      assert regs[rb].kind == rkNimNode
      let b = regs[rb].nimNode

      # TODO: this could use some refactoring
      case rc
      of 0:
        # getType opcode:
        ensureKind(rkNimNode)
        if b.typ != nil:
          regs[ra].nimNode = opMapTypeToAst(c.cache, b.typ, c.debug[pc], c.idgen)
        elif b.kind == nkSym and b.sym.typ != nil:
          regs[ra].nimNode = opMapTypeToAst(c.cache, b.sym.typ, c.debug[pc], c.idgen)
        else:
          raiseVmError(reportSem(rsemVmNoType))
      of 1:
        # typeKind opcode:
        ensureKind(rkInt)
        assert regs[rb].kind == rkNimNode
        let b = regs[rb].nimNode
        if b.typ != nil:
          regs[ra].intVal = ord(b.typ.kind)
        elif b.kind == nkSym and b.sym.typ != nil:
          regs[ra].intVal = ord(b.sym.typ.kind)
        #else:
        #  stackTrace(c, tos, pc, "node has no type")
      of 2:
        # getTypeInst opcode:
        ensureKind(rkNimNode)
        if b.typ != nil:
          regs[ra].nimNode = opMapTypeInstToAst(c.cache, b.typ, c.debug[pc], c.idgen)
        elif b.kind == nkSym and b.sym.typ != nil:
          regs[ra].nimNode = opMapTypeInstToAst(c.cache, b.sym.typ, c.debug[pc], c.idgen)
        else:
          raiseVmError(reportSem(rsemVmNoType))
      else:
        # getTypeImpl opcode:
        ensureKind(rkNimNode)
        if b.typ != nil:
          regs[ra].nimNode = opMapTypeImplToAst(c.cache, b.typ, c.debug[pc], c.idgen)
        elif b.kind == nkSym and b.sym.typ != nil:
          regs[ra].nimNode = opMapTypeImplToAst(c.cache, b.sym.typ, c.debug[pc], c.idgen)
        else:
          raiseVmError(reportSem(rsemVmNoType))
    of opcNGetSize:
      decodeBImm(rkInt)
      let n = regs[rb].nimNode
      case imm
      of 0: # size
        if n.typ == nil:
          raiseVmError(reportAst(rsemVmNoType, n))
        else:
          regs[ra].intVal = getSize(c.config, n.typ)
      of 1: # align
        if n.typ == nil:
          raiseVmError(reportAst(rsemVmNoType, n))
        else:
          regs[ra].intVal = getAlign(c.config, n.typ)
      else: # offset
        if n.kind != nkSym:
          raiseVmError(reportAst(rsemVmNodeNotASymbol, n))
        elif n.sym.kind != skField:
          raiseVmError(reportSym(rsemVmNotAField, n.sym))
        else:
          regs[ra].intVal = n.sym.offset

    of opcNStrVal:
      decodeB(akString)
      let a = regs[rb].nimNode
      case a.kind
      of nkStrLit..nkTripleStrLit:
        regs[ra].strVal = a.strVal
      of nkCommentStmt:
        regs[ra].strVal = a.comment
      of nkIdent:
        regs[ra].strVal = a.ident.s
      of nkSym:
        regs[ra].strVal = a.sym.name.s
      else:
        raiseVmError(reportStr(rsemVmFieldNotFound, "strVal"))

    of opcNSigHash:
      decodeB(akString)
      if regs[rb].nimNode.kind == nkSym:
        regs[ra].strVal = $sigHash(regs[rb].nimNode.sym)
      else:
        raiseVmError(reportAst(rsemVmNodeNotASymbol, regs[rb].nimNode))

    of opcSlurp:
      decodeB(akString)
      checkHandle(regs[rb])
      regs[ra].strVal = opSlurp($regs[rb].strVal, c.debug[pc],
                                     c.module, c.config)
    of opcGorge:
      decodeBC(akString)
      inc pc
      let rd = c.code[pc].regA
      checkHandle(regs[ra])
      if defined(nimsuggest) or c.config.cmd == cmdCheck:
        discard "don't run staticExec for 'nim suggest'"
        regs[ra].strVal = ""
      else:
        when defined(nimcore):
          checkHandle(regs[rb])
          checkHandle(regs[rc])
          checkHandle(regs[rd])
          regs[ra].strVal = opGorge($regs[rb].strVal,
                                        $regs[rc].strVal, $regs[rd].strVal,
                                        c.debug[pc], c.config)[0]
        else:
          regs[ra].strVal = ""
          globalReport(c.config, c.debug[pc], InternalReport(
            kind: rintNotUsingNimcore,
            msg: "VM is not built with 'gorge' support"))

    of opcNError, opcNWarning, opcNHint:
      let rb = instr.regB
      checkHandle(regs[ra])
      let a = $regs[ra].strVal
      let b = regs[rb].nimNode
      let info = if b.kind == nkNilLit: c.debug[pc] else: b.info
      case instr.opcode:
        of opcNError:
          raiseVmError(
            reportStr(rsemUserError, a), info)

        of opcNWarning:
          localReport(c.config, info, reportStr(rsemUserWarning, a))

        of opcNHint:
          localReport(c.config, info, reportStr(rsemUserHint, a))

        else: discard
    of opcParseExprToAst, opcParseStmtToAst:
      # REFACTOR This branch implements handling of the `parseExpr()` and
      # `parseStmt()` calls in the compiler API - because parsing can
      # expectedly fail (for example `"{a+}"` in strformat module) we need
      # to handle failure and report it as a *VM* exception, so user can
      # properly handle this.
      #
      # Previous implementation also relied on the report hook override -
      # in the future this need to be completely removed, since I
      # personally find exceptions-as-control-flow extremely ugly and unfit
      # for any serious work. Technically it does the job, one might argue
      # this is a good compromized, but I'd much rather have a paser
      # template that (1) writes out error message with correction location
      # information and (2) if parser is in 'speculative' mode, aborts the
      # parsing completely (via early exit checks in the parser module).
      # Yes, more work, but it avoids rebouncing exceptions through two (or
      # even three) compiler subsystems (`Lexer?->Parser->Vm`)
      decodeB(rkNimNode)

      type TemporaryExceptionHack = ref object of CatchableError

      var error = reportEmpty
      let oldHook = c.config.structuredReportHook
      var ast: PNode
      c.config.structuredReportHook = proc(
          conf: ConfigRef, report: Report
      ): TErrorHandling =
        # QUESTION This check might be affected by current severity
        # configurations, maybe it makes sense to do a hack-in that would
        # ignore all user-provided CLI otions?
        if report.category in {repParser, repLexer} and
           conf.severity(report) == rsevError:
          error = report
          raise TemporaryExceptionHack()

        else:
          return oldHook(conf, report)

      checkHandle(regs[rb])
      try:
        ast = parseString(
          $regs[rb].strVal,
          c.cache,
          c.config,
          toFullPath(c.config, c.debug[pc]),
          c.debug[pc].line.int
        )

      except TemporaryExceptionHack:
        discard

      c.config.structuredReportHook = oldHook

      if error.kind > repNone:
        c.errorFlag = error

      # Handling both statement and expression in this case branch, so
      # first checking for `parseExpr()` postconditions and then repacking
      # the ast as needed.
      elif ast.len != 1 and instr.opcode == opcParseExprToAst:
        c.errorFlag = SemReport(kind: rsemVmOpcParseExpectedExpression).wrap()

      elif instr.opcode == opcParseStmtToAst:
        regs[ra].nimNode = ast

      else:
        # Seems like parser always generates `stmtList`, so taking first
        # node here (checked for correct lenght earlier)
        regs[ra].nimNode = ast[0]
    of opcQueryErrorFlag:
      # REFACTOR HACK Only `parseExpr()` and `parseStmt()` appear to be
      # using error flag - we might as well set them directly? Anyway,
      # previous error formatting reused the same error message as
      # generated by reporting system, but now I need to convert the
      # `Report` object into string that can be read later by user VM code.

      regs[ra].initLocReg(c.typeInfoCache.stringType, c.memory)
      #createStr regs[ra]
      if c.errorFlag.kind != repNone:
        # Not sure if there is any better solution - I /do/ want to make
        # sure that error reported to the user in the VM is the same as one
        # I would report on the CLI, but at the same time this still looks
        # like an overly hacky approach
        regs[ra].strVal = "Error: " & c.config.reportBody(c.errorFlag)
                             # ^ `reportBody()` only returns main part of
                             # the report, so need to add `"Error: "`
                             # manally to stay consistent with the old
                             # output.

      c.errorFlag = reportEmpty


    of opcCallSite:
      ensureKind(rkNimNode)
      if c.callsite != nil: regs[ra].nimNode = c.callsite
      else: raiseVmError(reportStr(rsemVmFieldNotFound, "callsite"))
    of opcNGetLineInfo:
      decodeBImm()
      let n = regs[rb].nimNode
      case imm
      of 0: # getFile
        regs[ra].initLocReg(c.typeInfoCache.stringType, c.memory)
        regs[ra].strVal.newVmString(toFullPath(c.config, n.info), c.allocator)
      of 1: # getLine
        regs[ra].initIntReg(n.info.line.int)
      of 2: # getColumn
        regs[ra].initIntReg(n.info.col)
      else:
        unreachable($imm) # vmgen issue
    of opcNSetLineInfo:
      decodeB(rkNimNode)
      regs[ra].nimNode.info = regs[rb].nimNode.info
    of opcEqIdent:
      decodeBC(rkInt)

      func asCString(a: TFullReg): cstring =
        case a.kind
        of rkLocation, rkHandle:
          if a.handle.typ.kind == akString:
            result = deref(a.handle).strVal.asCString()
        of rkNimNode:
          var aNode = a.nimNode

          # Skipping both, `nkPostfix` and `nkAccQuoted` for both
          # arguments.  `nkPostfix` exists only to tag exported symbols
          # and therefor it can be safely skipped. Nim has no postfix
          # operator. `nkAccQuoted` is used to quote an identifier that
          # wouldn't be allowed to use in an unquoted context.

          if aNode.kind == nkPostfix:
            aNode = aNode[1]
          if aNode.kind == nkAccQuoted:
            aNode = aNode[0]

          case aNode.kind
          of nkIdent:
            result = aNode.ident.s.cstring
          of nkSym:
            result = aNode.sym.name.s.cstring
          of nkOpenSymChoice, nkClosedSymChoice:
            result = aNode[0].sym.name.s.cstring
          else:
            discard
        else:
          unreachable($a.kind) # vmgen issue

      checkHandle(regs[rb])
      checkHandle(regs[rc])

      # These vars are of type `cstring` to prevent unnecessary string copy.
      let
        aStrVal = asCString(regs[rb])
        bStrVal = asCString(regs[rc])

      regs[ra].intVal =
        if aStrVal != nil and bStrVal != nil:
          ord(idents.cmpIgnoreStyle(aStrVal, bStrVal, high(int)) == 0)
        else:
          0
    of opcStrToIdent:
      decodeB(rkNimNode)
      checkHandle(regs[rb])
      assert regs[rb].handle.typ.kind == akString
      regs[ra].nimNode = newNodeI(nkIdent, c.debug[pc])
      # XXX: `getIdent` doesn't use openArray[char]
      regs[ra].nimNode.ident = getIdent(c.cache, $regs[rb].strVal)
    of opcSetType:
      let typ = c.types[instr.regBx - wordExcess]
      assert typ.kind == akPtr
      regs[ra].addrTyp = typ.targetType

    of opcNSetType:
      # Only used internally
      let typ = c.rtti[instr.regBx - wordExcess]
      # XXX: instead of (ab-)using the rtti for this, the type could also be
      #      passed via a `nkType` constant. The latter matches the type's
      #      usage here better
      regs[ra].nimNode.typ = typ.nimType

    of opcConv:
      let rb = instr.regB
      inc pc
      let desttyp = c.rtti[c.code[pc].regBx - wordExcess]
      inc pc
      let srctyp = c.rtti[c.code[pc].regBx - wordExcess]

      # No need to check the handle of regs[ra] since it's not accessed
      checkHandle(regs[rb])

      # TODO: `opConv` needs to accept `VmTypeInfo` directly
      let dt = (desttyp.nimType, desttyp.internal)
      let st = (srctyp.nimType, srctyp.internal)
      if opConv(c, regs[ra], regs[rb], dt, st):
        raiseVmError(reportStr(
          rsemVmIllegalConv,
          errIllegalConvFromXtoY % [
            typeToString(srctyp.nimType),
            typeToString(desttyp.nimType)
        ]))
    of opcCast:
      let rb = instr.regB
      inc pc
      let desttyp = c.types[c.code[pc].regBx - wordExcess]
      inc pc
      let srctyp = c.types[c.code[pc].regBx - wordExcess]

      # XXX: `fficast`, which is now removed, was used for here previously.
      # Since we're soon storing objects in a flat representation, doing the
      # cast in a safe manner becomes possible, but I'm unsure if this feature
      # is really needed, so an error is always reported here for now
      raiseVmError(reportSem(rsemVmCannotCast))

    of opcNSetIntVal:
      decodeB(rkNimNode)
      var dest = regs[ra].nimNode
      if dest.kind in {nkCharLit..nkUInt64Lit}:
        dest.intVal = regs[rb].intVal
      elif dest.kind == nkSym and dest.sym.kind == skEnumField:
        raiseVmError(reportStr(
          rsemVmErrInternal,
          "`intVal` cannot be changed for an enum symbol."))
      else:
        raiseVmError(reportStr(rsemVmFieldNotFound, "intVal"))
    of opcNSetFloatVal:
      decodeB(rkNimNode)
      var dest = regs[ra].nimNode
      if dest.kind in {nkFloatLit..nkFloat64Lit}:
        dest.floatVal = regs[rb].floatVal
      else:
        raiseVmError(reportStr(rsemVmFieldNotFound, "floatVal"))
    of opcNSetStrVal:
      decodeB(rkNimNode)
      checkHandle(regs[rb])
      var dest = regs[ra].nimNode
      assert regs[rb].handle.typ.kind == akString
      if dest.kind in {nkStrLit..nkTripleStrLit}:
        dest.strVal = $regs[rb].strVal
      elif dest.kind == nkCommentStmt:
        dest.comment = $regs[rb].strVal
      else:
        raiseVmError(reportStr(rsemVmFieldNotFound, "strVal"))
    of opcNNewNimNode:
      decodeBC(rkNimNode)
      var k = regs[rb].intVal
      guestValidate(k in 0..ord(high(TNodeKind)),
        "request to create a NimNode of invalid kind")

      let cc = regs[rc].nimNode

      let x = newNodeI(TNodeKind(int(k)),
        if cc.kind != nkNilLit:
          cc.info
        elif c.comesFromHeuristic.line != 0'u16:
          c.comesFromHeuristic
        elif c.callsite != nil and c.callsite.safeLen > 1:
          c.callsite[1].info
        else:
          c.debug[pc])
      # prevent crashes in the compiler resulting from wrong macros:
      if x.kind == nkIdent: x.ident = c.cache.emptyIdent
      regs[ra].nimNode = x
    of opcNCopyNimNode:
      decodeB(rkNimNode)
      regs[ra].nimNode = copyNode(regs[rb].nimNode)
    of opcNCopyNimTree:
      decodeB(rkNimNode)
      regs[ra].nimNode = copyTree(regs[rb].nimNode)
    of opcNDel:
      decodeBC(rkNimNode)
      let bb = regs[rb].intVal.int
      for i in 0..<regs[rc].intVal.int:
        delSon(regs[ra].nimNode, bb)
    of opcGenSym:
      decodeBC(rkNimNode)
      let k = regs[rb].intVal
      checkHandle(regs[rc])
      assert regs[rc].handle.typ.kind == akString
      # XXX: costly stringify of strVal... `getIdent` doesn't use openArray :(
      let name = if regs[rc].strVal.len == 0: ":tmp"
                 else: $regs[rc].strVal
      guestValidate(k in 0..ord(high(TSymKind)),
        "request to create symbol of invalid kind")
      var sym = newSym(k.TSymKind, getIdent(c.cache, name), nextSymId c.idgen, c.module.owner, c.debug[pc])
      incl(sym.flags, sfGenSym)
      regs[ra].nimNode = newSymNode(sym)
    of opcNccValue:
      decodeB(rkInt)
      checkHandle(regs[rb])
      # TODO: don't do a string copy here
      let destKey = $regs[rb].strVal
      regs[ra].intVal = getOrDefault(c.graph.cacheCounters, destKey)
    of opcNccInc:
      let g = c.graph
      declBC()
      checkHandle(regs[rb])
      # TODO: don't do a string copy here
      let destKey = $regs[rb].strVal
      let by = regs[rc].intVal
      let v = getOrDefault(g.cacheCounters, destKey)
      g.cacheCounters[destKey] = v+by
      recordInc(c, c.debug[pc], destKey, by)
    of opcNcsAdd:
      let g = c.graph
      declBC()
      checkHandle(regs[rb])
      # TODO: don't do a string copy here
      let destKey = $regs[rb].strVal
      let val = regs[rc].nimNode
      if not contains(g.cacheSeqs, destKey):
        g.cacheSeqs[destKey] = newTree(nkStmtList, val)
      else:
        g.cacheSeqs[destKey].add val
      recordAdd(c, c.debug[pc], destKey, val)
    of opcNcsIncl:
      let g = c.graph
      declBC()
      checkHandle(regs[rb])
      # TODO: don't do a string copy here
      let destKey = $regs[rb].strVal
      let val = regs[rc].nimNode
      if not contains(g.cacheSeqs, destKey):
        g.cacheSeqs[destKey] = newTree(nkStmtList, val)
      else:
        block search:
          for existing in g.cacheSeqs[destKey]:
            if exprStructuralEquivalent(existing, val, strictSymEquality=true):
              break search
          g.cacheSeqs[destKey].add val
      recordIncl(c, c.debug[pc], destKey, val)
    of opcNcsLen:
      let g = c.graph
      decodeB(rkInt)
      checkHandle(regs[rb])
      # TODO: don't do a string copy here
      let destKey = $regs[rb].strVal
      regs[ra].intVal =
        if contains(g.cacheSeqs, destKey): g.cacheSeqs[destKey].len else: 0
    of opcNcsAt:
      let g = c.graph
      decodeBC(rkNimNode)
      checkHandle(regs[rb])
      let idx = regs[rc].intVal
      # TODO: don't do a string copy here
      let destKey = $regs[rb].strVal
      if contains(g.cacheSeqs, destKey) and idx <% g.cacheSeqs[destKey].len:
        regs[ra].nimNode = g.cacheSeqs[destKey][idx.int]
      else:
        raiseVmError(reportVmIdx(idx, g.cacheSeqs[destKey].len-1))

    of opcNctPut:
      let g = c.graph
      checkHandle(regs[ra])
      checkHandle(regs[instr.regB])
      # TODO: don't do a string copy here
      let destKey = $regs[ra].strVal
      let key = $regs[instr.regB].strVal
      let val = regs[instr.regC].nimNode
      if not contains(g.cacheTables, destKey):
        g.cacheTables[destKey] = initBTree[string, PNode]()
      if not contains(g.cacheTables[destKey], key):
        g.cacheTables[destKey].add(key, val)
        recordPut(c, c.debug[pc], destKey, key, val)
      else:
        raiseVmError(reportStr(
          rsemVmCacheKeyAlreadyExists, destKey))

    of opcNctLen:
      let g = c.graph
      decodeB(rkInt)
      checkHandle(regs[rb])
      # TODO: don't do a string copy here
      let destKey = $regs[rb].strVal
      regs[ra].intVal =
        if contains(g.cacheTables, destKey): g.cacheTables[destKey].len else: 0
    of opcNctGet:
      let g = c.graph
      decodeBC(rkNimNode)
      checkHandle(regs[rb])
      checkHandle(regs[rc])
      let destKey = $regs[rb].strVal
      let key = $regs[rc].strVal
      if contains(g.cacheTables, destKey):
        if contains(g.cacheTables[destKey], key):
          regs[ra].nimNode = getOrDefault(g.cacheTables[destKey], key)
        else:
          raiseVmError(reportStr(rsemVmMissingCacheKey, destKey))
      else:
        raiseVmError(reportStr(rsemVmMissingCacheKey, destKey))
    of opcNctHasNext:
      let g = c.graph
      decodeBC(rkInt)
      checkHandle(regs[rb])
      # TODO: don't do a string copy here
      let destKey = $regs[rb].strVal
      regs[ra].intVal =
        if g.cacheTables.contains(destKey):
          ord(btrees.hasNext(g.cacheTables[destKey], regs[rc].intVal.int))
        else:
          0
    of opcNctNext:
      let g = c.graph
      let rb = instr.regB
      let rc = instr.regC
      checkHandle(regs[rb])
      let destKey = $regs[rb].strVal
      let index = regs[rc].intVal

      # vmgen makes sure that `regs[ra]` stores a location of the correct type
      let handle = regs[ra].handle
      if contains(g.cacheTables, destKey):
        let (k, v, nextIndex) = btrees.next(g.cacheTables[destKey], index.int)
        deref(handle.getFieldHandle(FieldIndex(0))).strVal.newVmString(k, c.allocator)
        deref(handle.getFieldHandle(FieldIndex(1))).nodeVal = v
        writeInt(handle.getFieldHandle(FieldIndex(2)).byteView(), nextIndex)
      else:
        raiseVmError(reportStr(rsemVmMissingCacheKey, destKey))

    of opcTypeTrait:
      # XXX only supports 'name' for now; we can use regC to encode the
      # type trait operation
      decodeB(akString)
      var typ = regs[rb].nimNode.typ
      if unlikely(typ != nil):
        raiseVmError(reportSem(rsemVmNoType))
      while typ.kind == tyTypeDesc and typ.len > 0: typ = typ[0]
      #createStr regs[ra]
      regs[ra].strVal = typ.typeToString(preferExported)

    c.profiler.leave(c)

    inc pc

type
  ExecErrorReport = object
    stackTrace: SemReport ## The report storing the stack-trace
    report: SemReport     ## The report detailing the error

  ExecutionResult = Result[PNode, ExecErrorReport]

# prevent a default `$` implmenentation from being generated
func `$`(e: ExecErrorReport): string {.error.}

# XXX: the execution procs together with the result value processing needs
#      some refactoring

template mkCallback(cn, rn, body): untyped =
  let p = proc(cn: TCtx, rn: TFullReg): PNode = body
  p

proc execute(c: var TCtx, start: int, frame: sink TStackFrame; cb: proc(c: TCtx, r: TFullReg): PNode): ExecutionResult {.inline.} =
  assert c.sframes.len == 0
  c.sframes.add frame

  var pc = start
  var sframe = c.sframes.high
  try:
    let r = rawExecute(c, pc, sframe)
    assert c.sframes.len == 1
    let n = cb(c, c.sframes[0].slots[r])
    result = ExecutionResult.ok(n)
  except VmError as e:
    # Execution failed, generate a stack-trace and wrap the error payload
    # into an `ExecutionResult`
    var err: ExecErrorReport
    err.stackTrace = createStackTrace(c, sframe, pc)
    err.report = move e.report

    if err.report.location.isNone():
      # Use the location info of the failing instruction if none is provided
      err.report.location = some(c.debug[pc])

    # Set stack-trace report information
    err.stackTrace.location = some(c.debug[pc])
    err.stackTrace.reportInst = toReportLineInfo(instLoc(-1))

    result = ExecutionResult.err(err)

  finally:
    # Clean up frames whenever leaving execution
    for f in c.sframes.mitems:
      c.memory.cleanUpLocations(f)
    c.sframes.setLen(0)

    # Free pending
    cleanUpPending(c.memory)

proc unpackResult(res: sink ExecutionResult; config: ConfigRef, node: PNode): PNode =
  ## Unpacks the execution result. If the result represents a failure, returns
  ## a new `nkError` wrapping `node`. Otherwise, returns the value/tree result,
  ## optionally filling in the node's `info` with that of `node`, if not
  ## present already.
  if res.isOk:
    result = res.take
    if node != nil and result.info.line < 0:
      result.info = node.info
  else:
    let
      err = res.takeErr
      errKind = err.report.kind

      stId = config.addReport(wrap(err.stackTrace))
      rId = config.addReport(wrap(err.report))

    result = config.newError(node, errKind, rId, instLoc(-1)):
                             [newIntNode(nkIntLit, ord(stId))]

proc execute(c: var TCtx, start: int): ExecutionResult =
  var tos = TStackFrame(prc: nil, comesFrom: 0, next: -1)
  tos.slots.newSeq(c.prc.regInfo.len)
  execute(c, start, tos, proc(c: TCtx, r: TFullReg): PNode = c.graph.emptyNode)

template returnOnErr(res: VmGenResult, config: ConfigRef, node: PNode): int =
  ## Unpacks the vmgen result. If the result represents an error, exits the
  ## calling function by returning a new `nkError` wrapping `node`
  let r = res
  if r.isOk:
    r.take
  else:
    let
      report = r.takeErr
      kind = report.kind
      rid = config.addReport(wrap(report))

    return config.newError(node, kind, rid, instLoc())

proc reportIfError(config: ConfigRef, n: PNode) =
  ## If `n` is a `nkError`, reports the error via `handleReport`. This is
  ## only meant for errors from vm/vmgen invocations and is also only a
  ## transition helper until all vm invocation functions properly propagate
  ## `nkError`
  if n.isError:
    # Errors from direct vmgen invocations don't have a stack-trace
    if n.len == 4 and n[3].kind == nkIntLit:
      # XXX: testing for the presence of a stack-trace report like this is
      #      not a good idea. Error node layout might change, making the test
      #      invalid. VM errors should get their own report object with the
      #      stack-trace embedded as part of it.
      config.handleReport(n[3].intVal.ReportId, instLoc(-1)) # stack-trace
    config.localReport(n)

proc execProc*(c: var TCtx; sym: PSym; args: openArray[PNode]): PNode =
  # XXX: `localReport` is still used here since execProc is only used by the
  # VM's compilerapi (`nimeval`) whose users don't know about nkError yet

  c.loopIterations = c.config.maxLoopIterationsVM
  if sym.kind in routineKinds:
    if sym.typ.len-1 != args.len:
      localReport(c.config, sym.info, SemReport(
        kind: rsemWrongNumberOfArguments,
        sym: sym,
        countMismatch: (
          expected: toInt128(sym.typ.len - 1),
          got: toInt128(args.len))))

    else:
      let start = block:
        # XXX: `returnOnErr` should be used here instead, but isn't for
        #      backwards compatiblity
        let r = genProc(c, sym)
        if unlikely(r.isErr):
          localReport(c.config, r.takeErr)
          return nil
        r.unsafeGet

      var tos = TStackFrame(prc: sym, comesFrom: 0, next: -1)
      let maxSlots = sym.offset
      tos.slots.newSeq(maxSlots)

      # setup parameters:
      if not isEmptyType(sym.typ[0]) or sym.kind == skMacro:
        let typ = c.getOrCreate(sym.typ[0])
        if not tos.slots[0].loadEmptyReg(typ, sym.info, c.memory):
          tos.slots[0].initLocReg(typ, c.memory)
      # XXX We could perform some type checking here.
      for i in 1..<sym.typ.len:
        putIntoReg(tos.slots[i], c, args[i-1], sym.typ[i])

      let cb =
        if not isEmptyType(sym.typ[0]):
          mkCallback(c, r): c.regToNode(r, sym.typ[0], sym.info)
        elif sym.kind == skMacro:
          # TODO: missing cyclic check
          mkCallback(c, r): r.nimNode
        else:
          mkCallback(c, r): newNodeI(nkEmpty, sym.info)

      let r = execute(c, start, tos, cb)
      result = r.unpackResult(c.config, c.graph.emptyNode)
      reportIfError(c.config, result)
      if result.isError:
        result = nil
  else:
    localReport(c.config, sym.info, reportSym(rsemVmCallingNonRoutine, sym))

proc evalStmt(c: var TCtx, n: PNode): PNode =
  let n = transformExpr(c.graph, c.idgen, c.module, n)
  let start = genStmt(c, n).returnOnErr(c.config, n)

  # execute new instructions; this redundant opcEof check saves us lots
  # of allocations in 'execute':
  if c.code[start].opcode != opcEof:
    result = execute(c, start).unpackResult(c.config, n)
  else:
    result = c.graph.emptyNode

proc evalExpr*(c: var TCtx, n: PNode): PNode =
  # deadcode
  # `nim --eval:"expr"` might've used it at some point for idetools; could
  # be revived for nimsuggest
  let n = transformExpr(c.graph, c.idgen, c.module, n)
  let start = genExpr(c, n).returnOnErr(c.config, n)

  assert c.code[start].opcode != opcEof
  result = execute(c, start).unpackResult(c.config, n)

# XXX: the compilerapi regarding globals (getGlobalValue/setGlobalValue)
#      doesn't work the same as before. Previously, the returned PNode
#      could be used to modify the actual global value, but this is not
#      possible anymore

proc getGlobalValue*(c: TCtx; s: PSym): PNode =
  ## Does not perform type checking, so ensure that `s.typ` matches the
  ## global's type
  internalAssert(c.config, s.kind in {skLet, skVar} and sfGlobal in s.flags)
  let slotIdx = c.globals[s.position-1]
  let slot = c.heap.slots[slotIdx]

  result = c.deserialize(slot.handle, s.typ, s.info)

proc setGlobalValue*(c: var TCtx; s: PSym, val: PNode) =
  ## Does not do type checking so ensure the `val` matches the `s.typ`
  internalAssert(c.config, s.kind in {skLet, skVar} and sfGlobal in s.flags)
  let slotIdx = c.globals[s.position-1]
  let slot = c.heap.slots[slotIdx]

  c.serialize(val, slot.handle)

include vmops

proc setupGlobalCtx*(module: PSym; graph: ModuleGraph; idgen: IdGenerator) =
  addInNimDebugUtils(graph.config, "setupGlobalCtx")
  if graph.vm.isNil:
    graph.vm = newCtx(module, graph.cache, graph, idgen)
    registerAdditionalOps(PCtx graph.vm)
  else:
    let c = PCtx(graph.vm)
    refresh(c[], module, idgen)

proc myOpen(graph: ModuleGraph; module: PSym; idgen: IdGenerator): PPassContext {.nosinks.} =
  #var c = newEvalContext(module, emRepl)
  #c.features = {allowCast, allowInfiniteLoops}
  #pushStackFrame(c, newStackFrame())

  # XXX produce a new 'globals' environment here:
  setupGlobalCtx(module, graph, idgen)
  result = PCtx graph.vm

proc myProcess(c: PPassContext, n: PNode): PNode =
  let c = PCtx(c)
  # don't eval errornous code:
  if c.oldErrorCount == c.config.errorCounter:
    let r = evalStmt(c[], n)
    reportIfError(c.config, r)
    # TODO: use the node returned by evalStmt as the result and don't report
    #       the error here
    result = newNodeI(nkEmpty, n.info)
  else:
    result = n
  c.oldErrorCount = c.config.errorCounter

proc myClose(graph: ModuleGraph; c: PPassContext, n: PNode): PNode =
  result = myProcess(c, n)

const evalPass* = makePass(myOpen, myProcess, myClose)

proc evalConstExprAux(module: PSym; idgen: IdGenerator;
                      g: ModuleGraph; prc: PSym, n: PNode,
                      mode: TEvalMode): PNode =
  addInNimDebugUtils(g.config, "evalConstExprAux")
  #if g.config.errorCounter > 0: return n
  let n = transformExpr(g, idgen, module, n)
  setupGlobalCtx(module, g, idgen)
  let c = PCtx g.vm
  let oldMode = c.mode
  c.mode = mode
  let requiresValue = mode!=emStaticStmt

  let start = genExpr(c[], n, requiresValue).returnOnErr(c.config, n)

  if c.code[start].opcode == opcEof: return newNodeI(nkEmpty, n.info)
  assert c.code[start].opcode != opcEof
  when debugEchoCode:
    c.codeListing(prc, n)

  var tos = TStackFrame(prc: prc, comesFrom: 0, next: -1)
  tos.slots.newSeq(c.prc.regInfo.len)
  #for i in 0..<c.prc.regInfo.len: tos.slots[i] = newNode(nkEmpty)
  let cb =
    if requiresValue:
      mkCallback(c, r): c.regToNode(r, n.typ, n.info)
    else:
      mkCallback(c, r): newNodeI(nkEmpty, n.info)

  result = execute(c[], start, tos, cb).unpackResult(c.config, n)

  c.mode = oldMode

proc evalConstExpr*(module: PSym; idgen: IdGenerator; g: ModuleGraph; e: PNode): PNode =
  result = evalConstExprAux(module, idgen, g, nil, e, emConst)

proc evalStaticExpr*(module: PSym; idgen: IdGenerator; g: ModuleGraph; e: PNode, prc: PSym): PNode =
  result = evalConstExprAux(module, idgen, g, prc, e, emStaticExpr)

proc evalStaticStmt*(module: PSym; idgen: IdGenerator; g: ModuleGraph; e: PNode, prc: PSym) =
  let r = evalConstExprAux(module, idgen, g, prc, e, emStaticStmt)
  # TODO: the node needs to be returned to the caller instead
  reportIfError(g.config, r)

proc setupCompileTimeVar*(module: PSym; idgen: IdGenerator; g: ModuleGraph; n: PNode) =
  let r = evalConstExprAux(module, idgen, g, nil, n, emStaticStmt)
  # TODO: the node needs to be returned to the caller instead
  reportIfError(g.config, r)

proc setupMacroParam(reg: var TFullReg, c: var TCtx, x: PNode, typ: PType) =
  case typ.kind
  of tyStatic:
    putIntoReg(reg, c, x, typ)
  else:
    var n = x
    if n.kind in {nkHiddenSubConv, nkHiddenStdConv}: n = n[1]
    # TODO: is anyone on the callsite dependent on this modifiction of `x`?
    n.typ = x.typ
    reg = TFullReg(kind: rkNimNode, nimNode: n)

iterator genericParamsInMacroCall*(macroSym: PSym, call: PNode): (PSym, PNode) =
  let gp = macroSym.ast[genericParamsPos]
  for i in 0..<gp.len:
    let genericParam = gp[i].sym
    let posInCall = macroSym.typ.len + i
    if posInCall < call.len:
      yield (genericParam, call[posInCall])

# to prevent endless recursion in macro instantiation
const evalMacroLimit = 1000

#proc errorNode(idgen: IdGenerator; owner: PSym, n: PNode): PNode =
#  result = newNodeI(nkEmpty, n.info)
#  result.typ = newType(tyError, nextTypeId idgen, owner)
#  result.typ.flags.incl tfCheckedForDestructor

proc evalMacroCall*(module: PSym; idgen: IdGenerator; g: ModuleGraph; templInstCounter: ref int;
                    n: PNode, sym: PSym): PNode =
  #if g.config.errorCounter > 0: return errorNode(idgen, module, n)

  # XXX globalReport() is ugly here, but I don't know a better solution for now
  inc(g.config.evalMacroCounter)
  if g.config.evalMacroCounter > evalMacroLimit:
    globalReport(g.config, n.info, reportAst(
      rsemMacroInstantiationTooNested, n))

  # immediate macros can bypass any type and arity checking so we check the
  # arity here too:
  if sym.typ.len > n.safeLen and sym.typ.len > 1:
    globalReport(g.config, n.info, SemReport(
      kind: rsemWrongNumberOfArguments,
      ast: n,
      countMismatch: (
        expected: toInt128(sym.typ.len - 1),
        got: toInt128(n.safeLen - 1))))

  setupGlobalCtx(module, g, idgen)
  let c = PCtx g.vm
  let oldMode = c.mode
  c.mode = emStaticStmt
  c.comesFromHeuristic.line = 0'u16
  c.callsite = n
  c.templInstCounter = templInstCounter

  let start = genProc(c[], sym).returnOnErr(c.config, n)

  var tos = TStackFrame(prc: sym, comesFrom: 0, next: -1)
  let maxSlots = sym.offset
  tos.slots.newSeq(maxSlots)
  # setup arguments:
  var L = n.safeLen
  if L == 0: L = 1
  # This is wrong for tests/reject/tind1.nim where the passed 'else' part
  # doesn't end up in the parameter:
  #InternalAssert tos.slots.len >= L

  # return value:
  tos.slots[0] = TFullReg(kind: rkNimNode, nimNode: newNodeI(nkEmpty, n.info))

  # put macro call arguments into registers
  for i in 1..<sym.typ.len:
    setupMacroParam(tos.slots[i], c[], n[i], sym.typ[i])

  # put macro generic parameters into registers
  let gp = sym.ast[genericParamsPos]
  for i in 0..<gp.len:
    let idx = sym.typ.len + i
    if idx < n.len:
      setupMacroParam(tos.slots[idx], c[], n[idx], gp[i].sym.typ)
    else:
      # TODO: the decrement here is wrong, but the else branch is likely
      #       currently not reached anyway
      dec(g.config.evalMacroCounter)
      c.callsite = nil
      localReport(c.config, n.info, SemReport(
        kind: rsemWrongNumberOfGenericParams,
        countMismatch: (
          expected: toInt128(gp.len),
          got: toInt128(idx))))

  # temporary storage:
  #for i in L..<maxSlots: tos.slots[i] = newNode(nkEmpty)

  # n.typ == nil is valid and means that resulting NimNode represents
  # a statement
  let cb = mkCallback(c, r): r.nimNode
  result = execute(c[], start, tos, cb).unpackResult(c.config, n)

  if result.kind != nkError and cyclicTree(result):
    globalReport(c.config, n.info, reportAst(rsemCyclicTree, n, sym = sym))

  dec(g.config.evalMacroCounter)
  c.callsite = nil
  c.mode = oldMode
