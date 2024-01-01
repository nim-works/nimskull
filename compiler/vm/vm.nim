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
## # TODO: update the above -- it doesn't reflect reality anymore
##
## To use the VM once has to first set up an execution environment, which is
## represented by ``TCtx``. In order to execute something, a ``VmThread``
## instance is required and can be created with ``initVmThread``. Only a single
## thread may exist for a given ``TCtx`` at a time -- creating multiple ones,
## even if the others are not used, is not allowed.

import
  std/[
    strutils,
    tables,
    parseutils
  ],
  compiler/ast/[
    ast,
    lineinfos,
    renderer, # toStrLit implementation
    trees,
    idents,
    typesrenderer,
    types,
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    idioms,
    int128,
    btrees,
    bitsets
  ],
  compiler/sem/[
    sighashes,
    macrocacheimpl,
    transf,
    evaltempl
  ],
  compiler/vm/[
    vmprofiler,
    vmchecks,
    vmcompilerserdes,
    vmdef,
    vmdeps,
    vmerrors,
    vmmemory,
    vmobjects,
    vmtypes,
  ],
  experimental/[
    results
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport
from compiler/ast/report_enums import ReportKind

# xxx: `Report` is faaaar too wide a type for what the VM needs, even with all
#      the ground that can cover.
from compiler/ast/reports import Report, wrap

# XXX: instead of `assert` a special `vmAssert` could be used for internal
#      checks. This would allow for also reporting things like the current
#      executed instruction

import std/options as stdoptions
from std/math import round, copySign

type
  VmThread* = object
    ## This is beginning of splitting up ``TCtx``. A ``VmThread`` is
    ## meant to encapsulate the state that makes up a single execution. This
    ## includes things like the program counter, stack frames, active
    ## exception, etc.
    pc: int ## the program counter. Points to the instruction to execute next
    sframes: seq[TStackFrame] ## stack frames
    loopIterations: int
      ## the number of remaining jumps backwards

    # exception state:
    currentException: HeapSlotHandle
      ## the exception ref that's returned when querying the current exception
    activeException: HeapSlotHandle
      ## the exception that is currently in-flight (i.e. being raised), or
      ## nil, if none is in-flight. Note that `activeException` is different
      ## from `currentException`
    activeExceptionTrace: VmRawStackTrace
      ## the stack-trace of where the exception was raised from

  YieldReasonKind* = enum
    yrkDone
      ## execution is done. There is no more code to execute
    yrkError
      ## an error occurred. No clear distinction between fatal (the thread
      ## must not be resumed) and non-fatal (the thread may be resumed) is
      ## made yet, so all errors should be treated as fatal
    yrkQuit
      ## a call to ``quit`` was executed. The thread can't be resumed
    yrkMissingProcedure
      ## a procedure stub was called. The stub has to be resolved before
      ## continuing execution
    yrkEcho
      ## "syscall" of echo, the VM expects the code executing it to handle the
      ## echo and then resume execution
      # xxx: once there are more syscalls, consider having a few general
      #      syscall variants and the data in embedded sub-objects. While on
      #      the subject it's possible that quit could be considered a syscall,
      #      one that doesn't expect resuming.

  YieldReason* = object
    ## The result of a single execution step (i.e. a call to ``execute``)
    case kind*: YieldReasonKind:
    of yrkDone:
      reg*: Option[TRegister] ## the register that holds the result, or
                              ## 'none', if there is no result
    of yrkError:
      error*: VmEvent
    of yrkQuit:
      exitCode*: int
    of yrkMissingProcedure:
      entry*: FunctionIndex   ## the entry of the procedure that is a stub
    of yrkEcho:
      strs*: seq[string]      ## strings to be echo'd, at least one item

const
  traceCode = defined(nimVMDebugExecute)

const
  errIllegalConvFromXtoY = "illegal conversion from '$1' to '$2'"

proc createStackTrace*(
    c:          TCtx,
    thread:     VmThread,
    recursionLimit: int = 100
  ): VmStackTrace =
  ## Generates a stack-trace report starting at frame `sframe` (inclusive).
  ## The amount of entries in the trace is limited to `recursionLimit`, further
  ## entries are skipped, though the entry function is always included in the
  ## trace
  assert thread.sframes.len > 0

  result = VmStackTrace()
  # Just leave the exceptions empty, they aren't used anyways
  result.currentExceptionA = nil
  result.currentExceptionB = nil

  block:
    var count = 0
    var pc = thread.pc
    # create the stacktrace entries:
    for i in countdown(thread.sframes.high, 0):
      let f {.cursor.} = thread.sframes[i]

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
  copyToLocation(mm, dest.byteView(), src.byteView(), src.typ, reset)

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
    reg = TFullReg(kind: rkAddress,
                   addrVal: atom.ptrVal, addrTyp: h.typ.targetType)
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
  result.event = VmEvent(
    kind: FailureCodeToEvent[c],
    instLoc: inst)

template toException(x: DerefFailureCode): untyped =
  ## `Result` -> exception translation
  toVmError(x, instLoc())

proc reportException(c: TCtx; trace: VmRawStackTrace, raised: LocHandle) =
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
  raiseVmError(VmEvent(kind: vmEvtUnhandledException, exc: ast, trace: trace))

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
                # here, as the underlying `HeapSlot` is only moved around

    assert not slot.handle.p.isNil

    resetLocation(mm, slot.handle.byteView(), slot.handle.typ)
    mm.allocator.dealloc(slot.handle)

    mm.heap.slots[idx].reset()

    inc i

  mm.heap.pending.setLen(0)

# XXX: ensureKind (register transition) will be moved into a dedicated
#      instruction

func ensureKind*(n: var TFullReg, k: TRegisterKind, mm: var VmMemoryManager) {.inline.} =
  if n.kind != k:
    cleanUpReg(n, mm)
    n = TFullReg(kind: k)

func initLocReg*(r: var TFullReg, typ: PVmType, mm: var VmMemoryManager) =
  ## Transitions `r` to a location register storing a location of type `typ`
  cleanUpReg(r, mm)

  r = TFullReg(kind: rkLocation)
  r.handle = mm.allocator.allocSingleLocation(typ)

func initIntReg(r: var TFullReg, i: BiggestInt) =
  r = TFullReg(kind: rkInt, intVal: i)

template ensureKind(k: untyped) {.dirty.} =
  ensureKind(regs[ra], k, c.memory)

template ensureAtomKind(k: AtomKind) {.dirty.} =
  assert regs[ra].kind in {rkLocation, rkHandle}
  assert regs[ra].handle.typ.kind == k

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

template decodeBC(k: AtomKind) {.dirty, used.} =
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

template decodeBImm(k: AtomKind) {.dirty, used.} =
  let rb = instr.regB
  let imm = instr.regC - byteExcess
  ensureAtomKind(k)


template decodeBImm() {.dirty.} =
  let rb = instr.regB
  let imm = instr.regC - byteExcess

template decodeBx(k: TRegisterKind) {.dirty.} =
  let rbx = instr.regBx - wordExcess
  ensureKind(k)

template decodeBx(k: AtomKind) {.dirty, used.} =
  let rbx = instr.regBx - wordExcess
  ensureAtomKind(k)

template decodeBx() {.dirty.} =
  let rbx = instr.regBx - wordExcess

proc asgnValue(mm: var VmMemoryManager, dest: var TFullReg, src: TFullReg) =
  ## Assigns the value in `src` to `dest`, performing a register transition if
  ## necessary. An assignment in user code (`a = b`) maps directly to this
  ## function

  # Since vmgen doesn't take care of register/location lifetime management, it
  # has to be done here

  case src.kind
  of rkNone, rkInt, rkFloat, rkAddress, rkNimNode:
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
  of rkInt, rkFloat, rkAddress, rkNimNode: x = y
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
    # FIXME assertion should only allow for the `akInt` type, but
    # tests/vm/taccess_checks.nim fails when executed in debug build if
    # `akPtr` is removed. This is a vmgen.nim bug acc. to @zerbina
    assert h.typ.kind in {akInt, akPtr}, $h.typ.kind
    # Narrow/NarrowU made sure that the integer is in the  correct range
    writeInt(h.byteView(), x.intVal)
  of rkFloat:
    assert h.typ.kind == akFloat
    writeFloat(h, x.floatVal)
  of rkAddress:
    assert h.typ.kind == akPtr # but not akRef
    deref(h).ptrVal = x.addrVal
  of rkHandle, rkLocation:
    assert x.handle.typ == h.typ
    # Prevent writing to self (this can happen via opcWrDeref)
    # XXX: this is mostly due to how vmgen handles var parameters
    if h.rawPointer != x.handle.rawPointer:
      copyToLocation(dest=h, src=x.handle, mm)
  of rkNimNode:
    assert h.typ.kind == akPNode
    setNodeValue(h, x.nimNode)


proc regToNode*(c: TCtx, x: TFullReg; typ: PType, info: TLineInfo): PNode =
  ## Deserializes the value stored by `x` to a `PNode` of type `typ`
  case x.kind
  of rkNone: unreachable()
  of rkInt:
    result = newNodeIT(nkIntLit, info, typ)
    result.intVal = x.intVal
  of rkFloat:
    result = newNodeIT(nkFloatLit, info, typ)
    result.floatVal = x.floatVal
  of rkAddress:
    if x.addrVal.isNil:
      result = newNodeI(nkNilLit, info)
      result.typ = typ
    else:
      # TODO: validate the address
      result = c.deserialize(c.allocator.makeLocHandle(x.addrVal, x.addrTyp), typ, info)
  of rkHandle, rkLocation: result = c.deserialize(x.handle, typ, info)
  of rkNimNode: result = x.nimNode

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

proc resumeRaise(c: var TCtx, t: var VmThread): PrgCtr =
  ## Resume raising the active exception and returns the program counter
  ## (adjusted by -1) of the instruction to execute next. The stack is unwound
  ## until either an exception handler matching the active exception's type or
  ## a finalizer is found.
  let
    raised = c.heap.tryDeref(t.activeException, noneType).value()
    excType = raised.typ

  var
    frame = t.sframes.len
    jumpTo = (why: ExceptionGotoUnhandled, where: 0)

  # search for the first enclosing matching handler or finalizer:
  while jumpTo.why == ExceptionGotoUnhandled and frame > 0:
    dec frame
    jumpTo = findExceptionHandler(c, t.sframes[frame], excType)

  case jumpTo.why:
  of ExceptionGotoHandler, ExceptionGotoFinally:
    # unwind till the frame of the handler or finalizer
    for i in (frame+1)..<t.sframes.len:
      cleanUpLocations(c.memory, t.sframes[i])

    t.sframes.setLen(frame + 1)

    if jumpTo.why == ExceptionGotoHandler:
      # jumping to the handler means that the exception was handled. Clear
      # out the *active* exception (but not the *current* exception)
      t.activeException.reset()
      t.activeExceptionTrace.setLen(0)

    result = jumpTo.where - 1 # -1 because of the increment at the end
  of ExceptionGotoUnhandled:
    # nobody handled this exception, error out.
    reportException(c, t.activeExceptionTrace, raised)

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
    let styp = srctyp.skipTypes(abstractRange)
    case styp.kind
    of tyEnum:
      # TODO: don't use ``opcConv`` for string conversions -- make ``vmgen``
      #       insert calls to the enum-to-string procedures instead
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
    of tyFloat..tyFloat64:
      dest.strVal = $src.floatVal
    of tyString:
      dest.strVal = src.strVal
    of tyCstring:
      dest.strVal = src.strVal
    of tyChar:
      dest.strVal = $chr(src.intVal)
    else:
      unreachable(styp.kind)
  else:
    unreachable(desttyp.kind)

proc opNumConv(dest: var TFullReg, src: TFullReg, info: uint16) =
  ## Perform a conversion between two numeric values. The source value is
  ## provided by `src`, and the result written to `dest` (note that the
  ## register is overwritten). `info` is the compressed operation
  ## description.

  template toFloat(val: typed, size: int): BiggestFloat =
    case size
    of 8: BiggestFloat(val)
    of 4: BiggestFloat(val.float32)
    else: unreachable()

  let (op, destbytes, srcbytes) = unpackedConvDesc(info)
  case op
  of nckFToI: # float-to-signed
    dest = TFullReg(kind: rkInt, intVal: int(src.floatVal))
  of nckFToU: # float-to-unsigned
    dest = TFullReg(kind: rkInt, intVal: int(src.floatVal))
    # truncate, if the source value is not wider than the destination:
    if destbytes <= srcbytes and destbytes.int < sizeof(dest.intVal):
      dest.intVal = dest.intVal and ((1'i64 shl (destbytes * 8)) - 1)
  of nckIToF: # signed-to-float
    dest = TFullReg(kind: rkFloat,
                    floatVal: toFloat(src.intVal, destbytes))
  of nckUToF: # unsigned-to-float
    dest = TFullReg(kind: rkFloat,
                    floatVal: toFloat(cast[BiggestUInt](src.intVal), destbytes))
  of nckFToF: # float-to-float
    dest = TFullReg(kind: rkFloat,
                    floatVal: toFloat(src.floatVal, destbytes))
  of nckToB:  # float or int to bool
    # note: for floats, only a *positive* zero converts to `false`
    dest = TFullReg(kind: rkInt)
    dest.intVal =
      case src.kind
      of rkInt:   ord(src.intVal   != 0)
      of rkFloat: ord(src.floatVal != 0 and src.floatVal != -0.0)
      else:       raiseVmError(VmEvent(kind: vmEvtErrInternal,
                                        msg: "illegal operand"))

template handleJmpBack() {.dirty.} =
  if t.loopIterations <= 0:
    if allowInfiniteLoops in c.features:
      t.loopIterations = c.config.maxLoopIterationsVM
    else:
      raiseVmError(VmEvent(kind: vmEvtTooManyIterations))

  dec(t.loopIterations)


func setAddress(r: var TFullReg, p: VmMemPointer, typ: PVmType) =
  assert r.kind == rkAddress
  r.addrVal = p.rawPointer
  r.addrTyp = typ

func setAddress(r: var TFullReg, handle: LocHandle) =
  assert r.kind == rkAddress
  r.addrVal = handle.rawPointer
  r.addrTyp = handle.typ

func setHandle(r: var TFullReg, handle: LocHandle) =
  assert r.kind == rkHandle # Not rkLocation
  assert not handle.p.isNil
  assert handle.typ.isValid
  r.handle = handle

func loadEmptyReg*(r: var TFullReg, typ: PVmType, info: TLineInfo, mm: var VmMemoryManager): bool =
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
    r.setAddress(nil, typ)
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
  let k: VmEventKindAccessError =
    case reason
    of avrNoError: unreachable()
    of avrOutOfBounds: vmEvtAccessOutOfBounds
    of avrTypeMismatch: vmEvtAccessTypeMismatch
    of avrNoLocation: vmEvtAccessNoLocation

  raiseVmError(VmEvent(kind: k), inst)

template checkHandle(a: VmAllocator, re: TFullReg) =
  ## Tests if the handle in `re` is valid and raises an access violation error
  ## if it's not valid
  let reg = addr re
  if reg.kind == rkHandle:
    let r = checkValid(a, reg.handle)
    if unlikely(r != avrNoError):
      const L = instLoc()
      {.line: L.}: raiseAccessViolation(r, L)

template checkHandle(a: VmAllocator, handle: LocHandle) =
  ## Tests if `handle` is valid and raises an access violation error if it's
  ## not valid
  let r = checkValid(a, handle)
  if unlikely(r != avrNoError):
    const L = instLoc()
    {.line: L.}: raiseAccessViolation(r, L)


when not defined(nimHasSinkInference):
  {.pragma: nosinks.}

proc rawExecute(c: var TCtx, t: var VmThread, pc: var int): YieldReason =
  ## Runs the execution loop, starting in frame `tos` at program counter `pc`.
  ## In the case of an error, raises an exception of type `VmError`. If no
  ## fatal error occurred, the reason for why the loop was left plus extra
  ## data related to it is returned.
  ##
  ## If the loop was exited due to an error, `pc` will point to the faulting
  ## instruction. If the loop exits without errors, `pc` points to the last
  ## executed instruction.

  when defined(gcArc) or defined(gcOrc):
    # Use {.cursor.} as a way to get a shallow copy of the seq. This is safe,
    # since `slots` is never changed in length (no add/delete)
    var regs {.cursor.}: seq[TFullReg]
    template updateRegsAlias =
      regs = t.sframes[^1].slots
    updateRegsAlias
  else:
    var regs: seq[TFullReg] # alias to tos.slots for performance
    template updateRegsAlias =
      shallowCopy(regs, t.sframes[^1].slots)
    updateRegsAlias

  # alias templates to shorten common expressions:
  template currFrame: untyped = t.sframes[^1]
  template tos: untyped =
    # tos = top-of-stack
    t.sframes.high

  template pushFrame(p: TStackFrame) =
    t.sframes.add(p)
    updateRegsAlias()

  template popFrame() =
    cleanUpLocations(c.memory, t.sframes[tos])
    t.sframes.setLen(t.sframes.len - 1)

    updateRegsAlias()

  template guestValidate(cond: bool, strMsg: string) =
    ## Ensures that a guest-input related condition holds true and raises
    ## a `VmError` if it doesn't
    if unlikely(not cond): # Treat input violations as unlikely
      # TODO: don't use a string message here; use proper reports
      raiseVmError(VmEvent(kind: vmEvtErrInternal, msg: strMsg))

  template checkHandle(re: TFullReg) =
    {.line.}:
      checkHandle(c.allocator, re)

  proc reportVmIdx(usedIdx, maxIdx: SomeInteger): VmEvent =
    VmEvent(
      kind: vmEvtIndexError,
      indexSpec: (
        usedIdx: toInt128(usedIdx),
        minIdx: toInt128(0),
        maxIdx: toInt128(maxIdx)))

  #echo "NEW RUN ------------------------"
  while true:
    #{.computedGoto.}
    let instr = c.code[pc]
    let ra = instr.regA

    when traceCode:
      template regDescr(r): TRegisterKind =
        if r < regs.len: regs[r].kind else: rkNone

      c.vmTraceHandler(c, VmExecTrace(
        kind: vmTraceFull,
        pc: pc,
        ra: regDesc(instr.regA),
        rb: regDesc(instr.regB),
        rc: regDesc(instr.regC)))

    if c.config.active.isVmTrace:
      # unlike nimVMDebug, this doesn't require re-compiling nim and is
      # controlled by user code
      c.vmTraceHandler(c, VmExecTrace(
          kind: vmTraceMin,
          pc: pc))

    c.profiler.enter()
    case instr.opcode
    of opcEof:
      # XXX: eof shouldn't be used to return a register
      return YieldReason(kind: yrkDone, reg: none[TRegister]())
    of opcRet:
      let newPc = c.cleanUpOnReturn(t.sframes[tos])
      # Perform any cleanup action before returning
      if newPc < 0:
        pc = t.sframes[tos].comesFrom
        if tos == 0:
          # opcRet returns its value as indicated in the first operand
          return YieldReason(kind: yrkDone, reg: some(instr.regA))

        assert c.code[pc].opcode in {opcIndCall, opcIndCallAsgn}
        if c.code[pc].opcode == opcIndCallAsgn:
          # move the return register's content (that stores either the simple
          # value or the destination handle) to the destination register on the
          # caller's frame
          let i = c.code[pc].regA
          t.sframes[tos - 1].slots[i] = move regs[0]

        popFrame()
      else:
        currFrame.savedPC = pc
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
          intVal = cast[int](regs[rb].addrVal)
        else:
          raiseVmError(VmEvent(
            kind: vmEvtErrInternal,
            msg: "opcCastPtrToInt: got " & $regs[rb].kind))

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
        ptrVal = regs[rb].addrVal
      else:
        raiseVmError(VmEvent(
          kind: vmEvtErrInternal,
          msg: "opcCastIntToPtr: regs[rb].kind: " & $regs[rb].kind))

      regs[ra].setAddress(cast[VmMemPointer](ptrVal), nil)
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
          unreachable() # vmgen issue
      else:
        unreachable(regs[rb].kind)

    of opcLdArr:
      # a = b[c]
      decodeBC(rkHandle)
      checkHandle(regs[rb])

      let
        idx = regs[rc].intVal.int
        srcTyp = regs[rb].handle.typ

      case srcTyp.kind
      of akString:
        # XXX: `vmgen` uses `opcLdArr` for openArrays. Since strings are
        #       directly passed into openArray parameters, we have to handle
        #       akString here too
        # TODO: Remove this case once openArray handling is reworked
        regs[ra].setHandle:
          getItemHandle(regs[rb].strVal.VmSeq, srcTyp, idx, c.allocator)
      of akSeq, akArray:
        regs[ra].setHandle(getItemHandle(regs[rb].handle, idx, c.allocator))
      else:
        unreachable(srcTyp.kind)

    of opcLdArrAddr:
      # a = addr(b[c])
      decodeBC(rkAddress)
      let idx = regs[rc].intVal.int

      checkHandle(regs[rb])
      let src = regs[rb].handle

      case src.typ.kind
      of akString:
        # XXX: see todo and comment in `opcLdArr` for the reasons why
        #      `akString` has to be handled here
        let t = c.typeInfoCache.charType
        regs[ra].setAddress(
          regs[rb].strVal.data.applyOffset(idx.uint * t.sizeInBytes),
          t)
      of akSeq, akArray:
        let h = getItemHandle(src, idx, c.allocator)
        regs[ra].setAddress(h.p, h.typ)
      else:
        unreachable(src.typ.kind) # vmgen issue

    of opcLdStrIdx:
      decodeBC(rkInt)
      let idx = regs[rc].intVal.int
      checkHandle(regs[rb])
      let s = regs[rb].atomVal.strVal
      if idx <% s.len:
        regs[ra].intVal = s[idx].ord
      else:
        raiseVmError(reportVmIdx(idx, s.len-1))

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

      let slice =
        case dest.typ.kind
        of akString:
          # XXX: see todo and comment in `opcLdArr` for the reasons why
          #      `akString` is also allowed here
          toSlice(deref(dest).strVal.VmSeq, dTyp.seqElemType, c.allocator)
        of akSeq:
          toSlice(deref(dest).seqVal, dTyp.seqElemType, c.allocator)
        of akArray:
          toSlice(dest)
        of akInt, akFloat, akSet, akPtr, akRef, akObject, akPNode, akCallable,
           akDiscriminator:
          unreachable(dTyp.kind)

      if idx >= 0 and idx <% slice.len:
        checkHandle(regs[rc])
        writeLoc(slice[idx], regs[rc], c.memory)
      else:
        raiseVmError(reportVmIdx(idx, slice.len - 1))

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

      checkHandle(regs[rc])
      writeLoc(h, regs[rc], c.memory)

    of opcWrLoc:
      # a = b
      let rb = instr.regB
      checkHandle(regs[ra])
      checkHandle(regs[rb])
      writeLoc(regs[ra].handle, regs[rb], c.memory)
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
        discr = regs[ra].handle.subLocation(0, t).getFieldHandle(idx)
        combined = regs[rc].intVal

      assert discr.typ.kind == akDiscriminator
      let (v, branch) = unpackDiscr(combined.int, discr.typ.numBits)

      if instr.opcode == opcSetDisc:
        let oldBranch = readDiscrBranch(discr, t, idx)
        if oldBranch != branch:
          let h = regs[ra].handle.subLocation(0, t)
          resetBranch(c.memory, h, idx, oldBranch)

      writeDiscrField(discr, t, idx, v, branch)

    of opcWrProc:
      # TODO: once all atoms go into registers, rename to `opcLdProc`
      let fncIdx = FunctionIndex(instr.regBx - wordExcess)

      # No need to validate the handle. Due to the usage of `opcWrProc` in
      # vmgen, it's always valid here

      let handle = regs[ra].handle
      assert handle.typ.kind == akCallable
      assert handle.typ.routineSig == c.functions[fncIdx.int].sig

      deref(handle).callableVal = toFuncPtr(fncIdx)
    of opcAddr:
      # the operation expects a handle as input and turns it into an address
      decodeB(rkAddress)
      regs[ra].setAddress(regs[rb].handle)

    of opcLdDeref:
      # a = b[]
      decodeB(rkHandle)

      case regs[rb].kind
      of rkAddress:
        let r = addr regs[rb]
        assert r.addrTyp != nil, "Deref of untyped pointer"
        if not r.addrVal.isNil:
          # it's not an error to create an invalid handle from a pointer;
          # using the resulting handle is
          regs[ra].setHandle(c.allocator.makeLocHandle(r.addrVal, r.addrTyp))
        else:
          raiseVmError(VmEvent(kind: vmEvtNilAccess))

      of rkHandle, rkLocation:
        checkHandle(regs[rb])
        assert regs[rb].handle.typ.kind == akRef

        let slot = deref(regs[rb].handle).refVal
        let h = c.heap.tryDeref(slot, regs[rb].handle.typ.targetType).value()

        regs[ra].setHandle(h)
      else:
        unreachable(regs[rb].kind)

    of opcWrDeref:
      # a[] = c; b unused
      let rc = instr.regC

      # XXX: vmgen could use `opcLdDeref` and then mutate the target through
      #      the resulting handle (via opcWrLoc). This would make `opcWrDeref`
      #      obsolete

      # Copies c into the location pointed to by a

      checkHandle(regs[rc])

      let r = addr regs[ra]
      case r.kind
      of rkAddress:
        assert r.addrTyp != nil, "Deref of untyped pointer" # vmgen issue
        if not r.addrVal.isNil:
          let
            loc = c.allocator.makeLocHandle(r.addrVal, r.addrTyp)
            cr = c.allocator.checkValid(loc)

          if cr == avrNoError:
            writeLoc(loc, regs[rc], c.memory)
          else:
            raiseAccessViolation(cr, instLoc(0))

        elif r.addrVal.isNil:
          raiseVmError(VmEvent(kind: vmEvtNilAccess))

      of rkHandle, rkLocation:
        checkHandle(regs[ra])
        assert r.handle.typ.kind == akRef

        let slot = deref(r.handle).refVal
        let h = c.heap.tryDeref(slot, r.handle.typ.targetType).value()

        writeLoc(h, regs[rc], c.memory)

      else:
        raiseVmError(VmEvent(
          kind: vmEvtErrInternal,
          msg:  "opcWrDeref: regs[ra].kind: " & $r.kind))

    of opcAddInt:
      decodeBC(rkInt)
      let
        bVal = regs[rb].intVal
        cVal = regs[rc].intVal
        sum = bVal +% cVal
      if (sum xor bVal) >= 0 or (sum xor cVal) >= 0:
        regs[ra].intVal = sum
      else:
        raiseVmError(VmEvent(kind: vmEvtOverOrUnderflow))
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
        raiseVmError(VmEvent(kind: vmEvtOverOrUnderflow))
    of opcSubInt:
      decodeBC(rkInt)
      let
        bVal = regs[rb].intVal
        cVal = regs[rc].intVal
        diff = bVal -% cVal
      if (diff xor bVal) >= 0 or (diff xor not cVal) >= 0:
        regs[ra].intVal = diff
      else:
        raiseVmError(VmEvent(kind: vmEvtOverOrUnderflow))
    of opcSubImmInt:
      decodeBImm(rkInt)
      let
        bVal = regs[rb].intVal
        cVal = imm
        diff = bVal -% cVal
      if (diff xor bVal) >= 0 or (diff xor not cVal) >= 0:
        regs[ra].intVal = diff
      else:
        raiseVmError(VmEvent(kind: vmEvtOverOrUnderflow))
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
        raiseVmError(VmEvent(kind: vmEvtOverOrUnderflow))
    of opcDivInt:
      decodeBC(rkInt)
      if regs[rc].intVal == 0:
        raiseVmError(VmEvent(kind: vmEvtDivisionByConstZero))
      else: regs[ra].intVal = regs[rb].intVal div regs[rc].intVal
    of opcModInt:
      decodeBC(rkInt)
      if regs[rc].intVal == 0:
        raiseVmError(VmEvent(kind: vmEvtDivisionByConstZero))
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
          a.addrVal == b.addrVal
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
          else: unreachable() # vmgen issue
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
        raiseVmError(VmEvent(kind: vmEvtOverOrUnderflow))
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

      safeCopyMemSrc(mbitSet(regs[ra].handle), bitSet(regs[rb].handle))
      bitSetIntersect(mbitSet(regs[ra].handle), bitSet(regs[rc].handle))

    of opcPlusSet:
      # a = union(b, c)
      let rb = instr.regB
      let rc = instr.regC

      checkHandle(regs[rb])
      checkHandle(regs[rc])

      safeCopyMemSrc(mbitSet(regs[ra].handle), bitSet(regs[rb].handle))
      bitSetUnion(mbitSet(regs[ra].handle), bitSet(regs[rc].handle))

    of opcMinusSet:
      # a = diff(b, c)
      let rb = instr.regB
      let rc = instr.regC

      checkHandle(regs[rb])
      checkHandle(regs[rc])

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
      getItemHandle(s[], typ, s.length - 1, c.allocator).writeLoc(regs[rb], c.memory)
    of opcGetImpl:
      decodeB(rkNimNode)
      var a = regs[rb].nimNode
      if a.kind == nkVarTy: a = a[0]
      if a.kind == nkSym:
        regs[ra].nimNode = if a.sym.ast.isNil: newNode(nkNilLit)
                          else: copyTree(a.sym.ast)
      else:
        raiseVmError(VmEvent(kind: vmEvtNodeNotASymbol))

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
        raiseVmError(VmEvent(kind: vmEvtNodeNotASymbol))

    of opcDataToAst:
      decodeBC(rkNimNode)
      # the data to be deserialized is passed via register 'b' (either as a
      # value directly or as a handle), and the ``PType`` to use is provided
      # by the ``NimNode`` in register 'c'
      checkHandle(regs[rb])
      regs[ra].nimNode = c.regToNode(regs[rb], regs[rc].nimNode.typ, c.debug[pc])
    of opcExpandToAst:
      decodeBC(rkNimNode)
      # evaluate (i.e. expand) the template and store the resulting AST in
      # register 'a'
      let
        templ =     regs[rb+0].nimNode.sym
        prevFrame = tos - 1

      assert templ.kind == skTemplate

      let genSymOwner = if prevFrame > 0 and t.sframes[prevFrame].prc != nil:
                          t.sframes[prevFrame].prc
                        else:
                          c.module
      var templCall = newNodeI(nkCall, c.debug[pc], rc)
      templCall[0] = newSymNode(templ)
      for i in 1..<rc:
        # the code generator is responsible for making sure that we're getting
        # ``NimNode``s as the arguments
        let arg = regs[rb + i].nimNode
        # XXX: we're modifying the line information of a possibly user-created
        #      ``NimNode`` here!
        arg.info = c.debug[pc]
        templCall[i] = arg

      var a = evalTemplate(templCall, templ, genSymOwner, c.config, c.cache, c.templInstCounter, c.idgen)
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
        raiseVmError(VmEvent(kind: vmEvtNodeNotASymbol))
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
        raiseVmError(VmEvent(kind: vmEvtNodeNotAProcSymbol))

    of opcEcho:
      let rb = instr.regB

      result = YieldReason(kind: yrkEcho,
                           strs: newSeqOfCap[string](rb))

      case rb
      of 1:
        checkHandle(regs[ra])
        result.strs.add $regs[ra].strVal
      elif rb <= 0:
        raiseVmError(VmEvent(kind: vmEvtErrInternal,
                             msg: "echo with $1 args" % $rb))
      else:
        for i in ra..(ra+rb-1):
          checkHandle(regs[i])
          result.strs.add $regs[i].strVal

      inc pc # resume at the next instruction
      return # yield control for the echo syscall

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
      # c[] = parseFloat(rb, rd)
      decodeBC(rkInt)
      inc pc
      assert c.code[pc].opcode == opcParseFloat
      let rd = c.code[pc].regA

      checkHandle(regs[rb])
      checkHandle(regs[rc])
      assert regs[rc].handle.typ.kind == akFloat

      # because the ``number`` parameter of ``parseBiggestFloat`` is an out
      # parameter, no valid input value needs to be provided
      var number: BiggestFloat
      # TODO: don't do a string copy here
      let r = parseBiggestFloat($regs[rb].strVal, number, regs[rd].intVal.int)
      if r != 0:
        # only write back the number if parsing succeeded (matching the
        # behaviour of ``parseBiggestFloat``)
        writeFloat(regs[rc].handle, number)

      regs[ra].intVal = r
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
        else: unreachable(x.kind)

      let a = addr regs[ra]
      let l = addr regs[rb]
      let h = addr regs[rc]

      if not (isLessEqual(l, a) and
              isLessEqual(a, h)):
        # TODO: are range errors `Exception`s or `Defect`s in normal Nim? If
        #       they are `Exception`s we should raise a user catchable error
        #       here
        raiseVmError(VmEvent(
          kind: vmEvtIllegalConv,
          msg: errIllegalConvFromXtoY % [
            regs[ra].toStr,
            "[" & regs[rb].toStr & ".." & regs[rc].toStr & "]"
          ]))
    of opcIndexChck:
      # raise an error if c is not within b's bounds
      let
        rb = instr.regB
        idx = regs[instr.regC].intVal
      checkHandle(regs[rb])

      let len = arrayLen(regs[rb].handle)
      if idx < 0 or idx >=% len:
        raiseVmError(reportVmIdx(idx, len-1))
    of opcArrCopy:
      let rb = instr.regB
      let rc = instr.regC

      proc toSlice(h: LocHandle, a: VmAllocator): VmSlice =
        let typ = h.typ
        case typ.kind
        of akString: toSlice(deref(h).strVal.VmSeq, typ.seqElemType, a)
        of akSeq:    toSlice(deref(h).seqVal,       typ.seqElemType, a)
        of akArray:  toSlice(h)
        else:        unreachable(typ.kind)

      checkHandle(regs[ra])
      checkHandle(regs[rb])

      let
        dest = toSlice(regs[ra].handle, c.allocator)
        src = toSlice(regs[rb].handle, c.allocator)
        L = regs[rc].intVal.int

      # TODO: validate that both slices reference valid memory and are
      #       correctly typed

      assert dest.typ == src.typ

      if L > dest.len:
        # XXX: since opcArrCopy is only used internally be vmgen, this should
        #      probably be an assert
        raiseVmError(reportVmIdx(L, dest.len - 1))

      if L > src.len:
        # XXX: same comment as aboves
        raiseVmError(reportVmIdx(L, src.len - 1))

      c.memory.arrayCopy(byteView(dest), byteView(src), L, src.typ, true)
    of opcIndCall, opcIndCallAsgn:
      # dest = call regStart, n; where regStart = fn, arg1, ...
      let rb = instr.regB

      checkHandle(regs[rb])
      let fPtr = deref(regs[rb].handle).callableVal

      if unlikely(fPtr.isNil):
        raiseVmError(VmEvent(kind: vmEvtNilAccess))

      let entry {.cursor.} = c.functions[int toFuncIndex(fPtr)]
      assert entry.sig == regs[rb].handle.typ.routineSig
      let
        retType = entry.retValDesc
        # if the called procedure uses the .closure calling convention, there
        # must an additional hidden argument slot
        rc = instr.regC + ord(entry.isClosure)

      let prc = entry.sym
      case entry.kind:
      of ckCallback:
        # it's a callback:
        if instr.opcode == opcIndCallAsgn:
          # XXX: the callback should be responsible for initializing the
          #      result register (we wouldn't need the setup logic here then),
          #      but in order to be able to do that, owning handles need (i.e.
          #      ``rkLocation``) to be removed first
          # setup register that will store the result
          if not loadEmptyReg(regs[ra], retType, c.debug[pc], c.memory):
            # allocating the destination location is the responsibility of
            # ``vmgen``
            discard

        # We have to assume that the callback makes use of its parameters and
        # thus need to validate them here
        for i in (rb+1)..<(rb+rc):
          checkHandle(regs[i])

        c.callbacks[entry.cbOffset](
          VmArgs(ra: ra, rb: rb, rc: rc, slots: cast[ptr UncheckedArray[TFullReg]](addr regs[0]),
                 currentExceptionPtr: addr t.currentException,
                 currentLineInfo: c.debug[pc],
                 typeCache: addr c.typeInfoCache,
                 mem: addr c.memory,
                 heap: addr c.heap,
                 graph: c.graph,
                 config: c.config,
                 currentModule: c.module,
                 cache: c.cache,
                 idgen: c.idgen))
      of ckDefault:
        # the instruction may be executed a second time, so everything leading
        # up to the yield (i.e. ``return``) must not modify any VM state
        if entry.start < 0:
          # the procedure entry is a stub. Yield back control to the VM's
          # callsite, so that it can decide what do to
          return YieldReason(kind: yrkMissingProcedure,
                             entry: toFuncIndex(fPtr))

        let (newPc, regCount) = (entry.start, entry.regCount.int)

        # tricky: a recursion is also a jump back, so we use the same
        # logic as for loops:
        if newPc < pc: handleJmpBack()
        #echo "new pc ", newPc, " calling: ", prc.name.s
        var newFrame = TStackFrame(prc: prc, comesFrom: pc, savedPC: -1)
        newFrame.slots.newSeq(regCount)
        if instr.opcode == opcIndCallAsgn:
          checkHandle(regs[ra])
          # the destination might be a temporary complex location (`ra` is an
          # ``rkLocation`` register then). While we could use
          # ``fastAsgnComplex`` like we do with the arguments, it would mean
          # that each result access is subjected to access checks. That's
          # inefficient, so we *move* (destructive) the register's content for
          # the duration of the call and move it back when the call returns
          newFrame.slots[0] = move regs[ra]

        for i in 1..<rc:
          newFrame.slots[i].fastAsgnComplex(regs[rb+i])

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

      func cmp(a: string, b: VmString): int =
        let minLen = min(a.len, b.len)
        if minLen > 0:
          result = cmpMem(unsafeAddr a[0], b.data.rawPointer, minLen)
        if result == 0:
          result = a.len - b.len

      var cond = false
      case value.kind
      of cnstInt:      cond = regs[ra].intVal == value.intVal
      of cnstString:   cond = regs[ra].strVal == value.strVal
      of cnstFloat:    cond = regs[ra].floatVal == value.floatVal
      of cnstSliceListInt:   cond = regs[ra].intVal in value.intSlices
      of cnstSliceListFloat: cond = regs[ra].floatVal in value.floatSlices
      of cnstSliceListStr:
        # string slice-lists don't store the strings directly, but the ID of
        # a constant instead
        let str = regs[ra].strVal
        for s in value.strSlices.items:
          let a = c.constants[s.a].strVal
          let r = cmp(a, str)
          if s.a == s.b:
            # no need to compare the string with both slice elements if
            # they're the same
            if r == 0:
              cond = true
              break
          else:
            let b = c.constants[s.b].strVal
            if r <= 0 and cmp(b, str) >= 0:
              cond = true
              break

      else:
        unreachable(value.kind)

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
      t.sframes[tos].pushSafePoint(pc + rbx)
      assert c.code[pc+rbx].opcode in {opcExcept, opcFinally}
    of opcExcept:
      # This opcode is never executed, it only holds information for the
      # exception handling routines.
      doAssert(false)
    of opcFinally:
      # Pop the last safepoint introduced by a opcTry. This opcode is only
      # executed _iff_ no exception was raised in the body of the `try`
      # statement hence the need to pop the safepoint here.
      doAssert(currFrame.savedPC < 0)
      t.sframes[tos].popSafePoint()
    of opcFinallyEnd:
      # The control flow may not resume at the next instruction since we may be
      # raising an exception or performing a cleanup.
      # XXX: the handling here is wrong in many scenarios, but it works okay
      #      enough until ``finally`` handling is reworked
      if currFrame.savedPC >= 0:
        # resume clean-up
        pc = currFrame.savedPC - 1
        currFrame.savedPC = -1
      elif t.activeException.isNotNil:
        # the finally was entered through a raise -> resume. A return can abort
        # unwinding, thus an active exception is only considered when there's
        # no cleanup action in progress
        pc = resumeRaise(c, t)
        updateRegsAlias()
      else:
        discard "fall through"
    of opcRaise:
      decodeBImm()
      checkHandle(regs[ra])

      # `imm == 0` -> raise; `imm == 1` -> reraise current exception
      let isReraise = imm == 1

      let raisedRef =
        if isReraise:
          # TODO: must raise a defect when there's no current exception
          t.currentException
        else:
          assert regs[ra].handle.typ.kind == akRef
          regs[ra].atomVal.refVal

      let raised = c.heap.tryDeref(raisedRef, noneType).value()

      # XXX: the exception is never freed right now

      # Keep the exception alive during exception handling
      c.heap.heapIncRef(raisedRef)
      if not t.currentException.isNil:
        c.heap.heapDecRef(c.allocator, t.currentException)

      t.currentException = raisedRef
      t.activeException = raisedRef

      # gather the stack-trace for the exception:
      block:
        var pc = pc
        t.activeExceptionTrace.setLen(t.sframes.len)

        for i, it in t.sframes.pairs:
          let p =
            if i + 1 < t.sframes.len:
              t.sframes[i+1].comesFrom
            else:
              pc

          t.activeExceptionTrace[i] = (it.prc, p)

      let name = deref(raised.getFieldHandle(1.fpos))
      if not isReraise and name.strVal.len == 0:
        # XXX: the VM doesn't distinguish between a `nil` cstring and an empty
        #      `cstring`, leading to the name erroneously being overridden if
        #      it was explicitly initialized with `""`
        # Set the `name` field of the exception. No need to valdiate the
        # handle in `regs[rb]`, since it's a constant loaded prior to the
        # raise
        name.strVal.asgnVmString(regs[rb].strVal, c.allocator)

      pc = resumeRaise(c, t)
      updateRegsAlias()
    of opcNew:
      let typ = c.types[instr.regBx - wordExcess]
      assert typ.kind == akRef

      # typ is the ref type, not the target type
      let slot = c.heap.heapNew(c.allocator, typ.targetType)
      # XXX: making sure that the previous ref value was destroyed will become
      #      the responsibility of the code generator, in the future
      asgnRef(regs[ra].atomVal.refVal, slot, c.memory, reset=true)
    of opcNewSeq:
      let typ = c.types[instr.regBx - wordExcess]
      inc pc
      let instr2 = c.code[pc]
      let count = regs[instr2.regA].intVal.int

      assert typ.kind == akSeq
      newVmSeq(regs[ra].atomVal.seqVal, typ, count, c.memory)
    of opcNewStr:
      let rb = instr.regB
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
        regs[ra] = TFullReg(kind: rkLocation)
        regs[ra].handle =
          c.allocator.allocSingleLocation(c.typeInfoCache.stringType)
        # TODO: once implemented, assign the string as a literal instead of
        #       via deep copying
        deref(regs[ra].handle).strVal.newVmString(cnst.strVal, c.allocator)
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
      let rb = instr.regBx - wordExcess
      let slot = c.globals[rb]
      ensureKind(rkHandle)
      regs[ra].setHandle(c.heap.slots[slot].handle)

    of opcLdCmplxConst:
      decodeBx(rkHandle)

      regs[ra].setHandle(c.complexConsts[rbx])

    of opcRepr:
      # Turn the provided value into its string representation. Used for:
      # - implementing the general ``repr`` when not using the ``repr`` v2
      # - rendering an AST to its text representation (``repr`` for
      #   ``NimNode``)
      # - rendering the discriminant value for a ``FieldDefect``'s message

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

      regs[ra].strVal.newVmString(str, c.allocator)
    of opcQuit:
      return YieldReason(kind: yrkQuit, exitCode: regs[ra].intVal.int)
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
      raiseVmError(VmEvent(kind: vmEvtFieldUnavailable, msg: msg2))
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
        raiseVmError(VmEvent(kind: vmEvtOutOfRange))
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
        of akInt, akFloat, akSet, akObject, akArray, akPNode, akDiscriminator:
          unreachable(regs[rb].kind)
      of rkNimNode:
        res = regs[rb].nimNode.kind == nkNilLit
      of rkAddress:
        res = regs[rb].addrVal.isNil
      of rkNone, rkInt, rkFloat:
        unreachable()

      regs[ra].intVal = ord(res)
    of opcNChild:
      decodeBC(rkNimNode)
      let idx = regs[rc].intVal.int
      let src = regs[rb].nimNode
      # TODO: This if-else block should be reordered so as to match the
      #       expectation of occurence
      if src.kind in {nkEmpty..nkNilLit, nkError}:
        raiseVmError(VmEvent(kind: vmEvtCannotGetChild, ast: src))
      elif idx >=% src.len:
        raiseVmError(reportVmIdx(idx, src.len - 1))
      else:
        regs[ra].nimNode = src[idx]
    of opcNSetChild:
      decodeBC(rkNimNode)
      let idx = regs[rb].intVal.int
      var dest = regs[ra].nimNode
      if nfSem in dest.flags:
        raiseVmError(VmEvent(kind: vmEvtCannotModifyTypechecked))
      elif dest.kind in {nkEmpty..nkNilLit, nkError}:
        raiseVmError(VmEvent(kind: vmEvtCannotSetChild, ast: dest))
      elif idx >=% dest.len:
        raiseVmError(reportVmIdx(idx, dest.len - 1))
      else:
        dest[idx] = regs[rc].nimNode
    of opcNAdd:
      decodeBC(rkNimNode)
      var u = regs[rb].nimNode
      if nfSem in u.flags:
        raiseVmError(VmEvent(kind: vmEvtCannotModifyTypechecked))
      elif u.kind in {nkEmpty..nkNilLit, nkError}:
        raiseVmError(VmEvent(kind: vmEvtCannotAddChild, ast: u))
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
      if nfSem in u.flags:
        raiseVmError(VmEvent(kind: vmEvtCannotModifyTypechecked))
      elif u.kind in {nkEmpty..nkNilLit, nkError}:
        raiseVmError(VmEvent(kind: vmEvtCannotAddChild, ast: u))
      else:
        let L = arrayLen(x)
        for i in 0..<L:
          u.add(deref(getItemHandle(x, i, c.allocator)).nodeVal)
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
        raiseVmError(VmEvent(kind: vmEvtNodeNotASymbol))
      c.comesFromHeuristic = a.info

    of opcNIntVal:
      decodeB(rkInt)
      let a = regs[rb].nimNode
      if a.kind in {nkCharLit..nkUInt64Lit}:
        regs[ra].intVal = a.intVal
      elif a.kind == nkSym and a.sym.kind == skEnumField:
        regs[ra].intVal = a.sym.position
      else:
        raiseVmError(VmEvent(kind: vmEvtFieldNotFound, msg: "intVal"))
    of opcNFloatVal:
      decodeB(rkFloat)
      let a = regs[rb].nimNode
      case a.kind
      of nkFloatLit..nkFloat64Lit: regs[ra].floatVal = a.floatVal
      else: raiseVmError(VmEvent(kind: vmEvtFieldNotFound, msg: "floatVal"))
    of opcNodeId:
      decodeB(rkInt)
      regs[ra].intVal = regs[rb].nimNode.id.int32
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
          raiseVmError(VmEvent(kind: vmEvtNoType))
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
          raiseVmError(VmEvent(kind: vmEvtNoType))
      else:
        # getTypeImpl opcode:
        ensureKind(rkNimNode)
        if b.typ != nil:
          regs[ra].nimNode = opMapTypeImplToAst(c.cache, b.typ, c.debug[pc], c.idgen)
        elif b.kind == nkSym and b.sym.typ != nil:
          regs[ra].nimNode = opMapTypeImplToAst(c.cache, b.sym.typ, c.debug[pc], c.idgen)
        else:
          raiseVmError(VmEvent(kind: vmEvtNoType))
    of opcNGetSize:
      decodeBImm(rkInt)
      let n = regs[rb].nimNode
      case imm
      of 0: # size
        if n.typ == nil:
          raiseVmError(VmEvent(kind: vmEvtNoType, ast: n))
        else:
          regs[ra].intVal = getSize(c.config, n.typ)
      of 1: # align
        if n.typ == nil:
          raiseVmError(VmEvent(kind: vmEvtNoType, ast: n))
        else:
          regs[ra].intVal = getAlign(c.config, n.typ)
      else: # offset
        if n.kind != nkSym:
          raiseVmError(VmEvent(kind: vmEvtNodeNotASymbol, ast: n))
        elif n.sym.kind != skField:
          raiseVmError(VmEvent(kind: vmEvtNotAField, sym: n.sym))
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
        raiseVmError(VmEvent(kind: vmEvtFieldNotFound, msg: "strVal"))

    of opcNSigHash:
      decodeB(akString)
      if regs[rb].nimNode.kind == nkSym:
        regs[ra].strVal = $sigHash(regs[rb].nimNode.sym)
      else:
        raiseVmError(VmEvent(
          kind: vmEvtNodeNotASymbol, ast: regs[rb].nimNode))

    of opcParseExprToAst, opcParseStmtToAst:
      decodeBC(rkNimNode)

      checkHandle(regs[rb])
      checkHandle(regs[rc])

      type Res = Result[PNode, Report]

      var parsed =
        parseCode($regs[rb].strVal, c.cache, c.config, c.debug[pc])

      if instr.opcode == opcParseExprToAst:
        # make sure that the parsed code is that of an expression:
        parsed = parsed.flatMap proc(ast: PNode): Res =
          if ast.len == 1:
            assert ast.kind == nkStmtList
            Res.ok:  ast[0]
          else:
            Res.err: SemReport(kind: rvmOpcParseExpectedExpression).wrap()

      if parsed.isOk:
        # success! Write the parsed AST to the result register and return an
        # empty error message
        regs[ra].nimNode = parsed.unsafeGet
        regs[rc].strVal = ""
      else:
        # failure. Write out the error
        regs[rc].strVal = errorReportToString(c.config, parsed.error)

    of opcCallSite:
      ensureKind(rkNimNode)
      if c.callsite != nil:
        regs[ra].nimNode = c.callsite
      else:
        raiseVmError(VmEvent(kind: vmEvtFieldNotFound, msg: "callsite"))
    of opcNGetLineInfo:
      decodeBImm()
      let n = regs[rb].nimNode
      case imm
      of 0: # getFile
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
          unreachable(a.kind) # vmgen issue

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
      # a = conv(b); a complex conversion where the destination is a memory
      # location. Takes up two instruction words
      let
        desttyp = c.rtti[instr.regBx - wordExcess]
        next    = c.code[pc+1]
        rb      = next.regA
        srctyp  = c.rtti[next.regBx - wordExcess]
      inc pc

      checkHandle(regs[ra])
      checkHandle(regs[rb])

      let dt = (desttyp.nimType, desttyp.internal)
      let st = (srctyp.nimType, srctyp.internal)
      if opConv(c, regs[ra], regs[rb], dt, st):
        raiseVmError(VmEvent(
          kind: vmEvtIllegalConvFromXToY,
          typeMismatch: VmTypeMismatch(
            actualType: srctyp.nimType,
            formalType: desttyp.nimType)))

    of opcNumConv:
      # a = conv(b); a conversion between two numeric values
      let
        rb = instr.regB
        rc = instr.regC
      # the register is overwritten, so make sure to first clean it up
      cleanUpReg(regs[ra], c.memory)
      opNumConv(regs[ra], regs[rb], rc.uint16)

    of opcObjConv:
      # a = conv(b)
      ensureKind(rkHandle)
      let src = regs[instr.regB].handle
      inc pc
      let desttyp = c.types[c.code[pc].regBx - wordExcess]

      assert src.typ.kind == desttyp.kind
      # perform a conversion check, if necessary
      case desttyp.kind
      of akRef:
        # a ref up- or down conversion
        assert src.typ.kind == akRef # vmgen issue
        checkHandle(regs[instr.regB])

        let refVal = deref(src).refVal

        if isNotNil(refVal):
          let loc = c.heap.tryDeref(refVal, noneType).value()
          if getTypeRel(loc.typ, desttyp.targetType) notin {vtrSame, vtrSub}:
            # an illegal up-conversion (assuming both types are related)
            raiseVmError(VmEvent(
              kind: vmEvtIllegalConv,
              msg: "illegal up-conversion"))

      of akObject:
        # an object-to-object lvalue conversion
        discard "no check necessary"
      else:
        unreachable(desttyp.kind)

      regs[ra].handle = src
      regs[ra].handle.typ = desttyp
    of opcCast:
      # XXX: `fficast`, which is now removed, was used for here previously.
      # Since we're soon storing objects in a flat representation, doing the
      # cast in a safe manner becomes possible, but I'm unsure if this feature
      # is really needed, so an error is always reported here for now
      raiseVmError(VmEvent(kind: vmEvtCannotCast))

    of opcNSetIntVal:
      decodeB(rkNimNode)
      var dest = regs[ra].nimNode
      if dest.kind in {nkCharLit..nkUInt64Lit}:
        dest.intVal = regs[rb].intVal
      elif dest.kind == nkSym and dest.sym.kind == skEnumField:
        raiseVmError(VmEvent(
          kind: vmEvtErrInternal,
          msg: "`intVal` cannot be changed for an enum symbol."))
      else:
        raiseVmError(VmEvent(kind: vmEvtFieldNotFound, msg: "intVal"))
    of opcNSetFloatVal:
      decodeB(rkNimNode)
      var dest = regs[ra].nimNode
      if dest.kind in {nkFloatLit..nkFloat64Lit}:
        dest.floatVal = regs[rb].floatVal
      else:
        raiseVmError(VmEvent(kind: vmEvtFieldNotFound, msg: "floatVal"))
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
        raiseVmError(VmEvent(kind: vmEvtFieldNotFound, msg: "strVal"))
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
        raiseVmError(VmEvent(
          kind: vmEvtCacheKeyAlreadyExists,
          msg:  destKey))
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
          raiseVmError(VmEvent(kind: vmEvtMissingCacheKey, msg: destKey))
      else:
        raiseVmError(VmEvent(kind: vmEvtMissingCacheKey, msg: destKey))
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
        raiseVmError(VmEvent(kind: vmEvtMissingCacheKey, msg: destKey))

    of opcTypeTrait:
      # XXX only supports 'name' for now; we can use regC to encode the
      # type trait operation
      decodeB(akString)
      var typ = regs[rb].nimNode.typ
      if unlikely(typ != nil):
        raiseVmError(VmEvent(kind: vmEvtNoType))
      while typ.kind == tyTypeDesc and typ.len > 0: typ = typ[0]
      #createStr regs[ra]
      regs[ra].strVal = typ.typeToString(preferExported)

    c.profiler.leave(t.sframes)

    inc pc

proc `=copy`*(x: var VmThread, y: VmThread) {.error.}

proc initVmThread*(c: var TCtx, pc: int, frame: sink TStackFrame): VmThread =
  ## Sets up a ``VmThread`` instance that will start execution at `pc`.
  ## `frame` provides the initial stack frame.
  frame.savedPC = -1 # initialize the field here
  VmThread(pc: pc,
           loopIterations: c.config.maxLoopIterationsVM,
           sframes: @[frame])

proc dispose*(c: var TCtx, t: sink VmThread) =
  ## Cleans up and frees all VM data owned by `t`.
  for f in t.sframes.mitems:
    c.memory.cleanUpLocations(f)

  if t.currentException.isNotNil:
    c.heap.heapDecRef(c.allocator, t.currentException)

  # free heap slots that are pending cleanup
  cleanUpPending(c.memory)

proc execute*(c: var TCtx, thread: var VmThread): YieldReason {.inline.} =
  ## Executes the VM thread until it yields. The reason for yielding together
  ## with associated data is returned as a ``YieldReason`` object

  # both `pc` and `sframe` are modified and queried *very* often, so we move
  # them to close stack locations for better spatial memory locality (the
  # `thread` instance could be located in a far away memory location)
  var
    pc = thread.pc

  try:
    result = rawExecute(c, thread, pc)
  except VmError as e:
    # an error occurred during execution
    result = YieldReason(kind: yrkError, error: move e.event)
  finally:
    thread.pc = pc

template source*(c: TCtx, t: VmThread): TLineInfo =
  ## Gets the source-code information for the instruction the program counter
  ## of `t` currently points to
  c.debug[t.pc]

template `[]`*(t: VmThread, i: Natural): TStackFrame =
  ## Returns `t`'s stack frame at index `i`.
  t.sframes[i]

func vmEventToAstDiagVmError*(evt: VmEvent): AstDiagVmError {.inline.} =
  let kind =
    case evt.kind
    of vmEvtOpcParseExpectedExpression: adVmOpcParseExpectedExpression
    of vmEvtUserError: adVmUserError
    of vmEvtUnhandledException: adVmUnhandledException
    of vmEvtCannotCast: adVmCannotCast
    of vmEvtCallingNonRoutine: adVmCallingNonRoutine
    of vmEvtCannotModifyTypechecked: adVmCannotModifyTypechecked
    of vmEvtNilAccess: adVmNilAccess
    of vmEvtAccessOutOfBounds: adVmAccessOutOfBounds
    of vmEvtAccessTypeMismatch: adVmAccessTypeMismatch
    of vmEvtAccessNoLocation: adVmAccessNoLocation
    of vmEvtErrInternal: adVmErrInternal
    of vmEvtIndexError: adVmIndexError
    of vmEvtOutOfRange: adVmOutOfRange
    of vmEvtOverOrUnderflow: adVmOverOrUnderflow
    of vmEvtDivisionByConstZero: adVmDivisionByConstZero
    of vmEvtArgNodeNotASymbol: adVmArgNodeNotASymbol
    of vmEvtNodeNotASymbol: adVmNodeNotASymbol
    of vmEvtNodeNotAProcSymbol: adVmNodeNotAProcSymbol
    of vmEvtIllegalConv: adVmIllegalConv
    of vmEvtIllegalConvFromXToY: adVmIllegalConvFromXToY
    of vmEvtMissingCacheKey: adVmMissingCacheKey
    of vmEvtCacheKeyAlreadyExists: adVmCacheKeyAlreadyExists
    of vmEvtFieldNotFound: adVmFieldNotFound
    of vmEvtNotAField: adVmNotAField
    of vmEvtFieldUnavailable: adVmFieldUnavailable
    of vmEvtCannotSetChild: adVmCannotSetChild
    of vmEvtCannotAddChild: adVmCannotAddChild
    of vmEvtCannotGetChild: adVmCannotGetChild
    of vmEvtNoType: adVmNoType
    of vmEvtTooManyIterations: adVmTooManyIterations

  {.cast(uncheckedAssign).}: # discriminants on both sides lead to saddness
    result =
      case kind:
      of adVmUserError:
        AstDiagVmError(
          kind: kind,
          errLoc: evt.errLoc,
          errMsg: evt.errMsg)
      of adVmArgNodeNotASymbol:
        AstDiagVmError(
          kind: kind,
          callName: evt.callName,
          argAst: evt.argAst,
          argPos: evt.argPos)
      of adVmCannotCast, adVmIllegalConvFromXToY:
        AstDiagVmError(
          kind: kind,
          formalType: evt.typeMismatch.formalType,
          actualType: evt.typeMismatch.actualType)
      of adVmIndexError:
        AstDiagVmError(
          kind: kind,
          indexSpec: evt.indexSpec)
      of adVmErrInternal, adVmNilAccess, adVmIllegalConv,
          adVmFieldUnavailable, adVmFieldNotFound,
          adVmCacheKeyAlreadyExists, adVmMissingCacheKey:
        AstDiagVmError(
          kind: kind,
          msg: evt.msg)
      of adVmCannotSetChild, adVmCannotAddChild, adVmCannotGetChild,
          adVmNoType, adVmNodeNotASymbol:
        AstDiagVmError(
          kind: kind,
          ast: evt.ast)
      of adVmUnhandledException:
        AstDiagVmError(
          kind: kind,
          ast: evt.exc)
      of adVmNotAField:
        AstDiagVmError(
          kind: kind,
          sym: evt.sym)
      of adVmOpcParseExpectedExpression,
          adVmCallingNonRoutine,
          adVmCannotModifyTypechecked,
          adVmAccessOutOfBounds,
          adVmAccessTypeMismatch,
          adVmAccessNoLocation,
          adVmOutOfRange,
          adVmOverOrUnderflow,
          adVmDivisionByConstZero,
          adVmNodeNotAProcSymbol,
          adVmTooManyIterations:
        AstDiagVmError(kind: kind)
