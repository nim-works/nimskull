## This module implements a standalone runner for the VM. The runner is
## currently only meant for use by the compiler and thus performs only
## little to no input validation.

import
  std/[
    os,
    strutils
  ],

  compiler/ast/[
    ast_types,
    ast,
    lineinfos
  ],
  compiler/front/[
    cli_reporter,
    msgs,
    options
  ],
  compiler/vm/[
    packed_env,
    vm,
    vmdef,
    vmhooks,
    vmlegacy,
    vmmemory,
    vmobjects,
    vmops,
    vmtypes
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/utils/[
    idioms,
    pathutils,
    bitsets
  ],
  experimental/[
    results
  ]

import std/options as std_options

# TODO: legacy report cruft remove from here
from compiler/ast/reports_vm import VMReport
from compiler/ast/reports import ReportKind, toReportLineInfo

proc loadDiscrConst(s: PackedEnv, constIdx: int, dst: LocHandle,
                    objTyp: PVmType, fieldPos: FieldPosition): int =
  let
    n =            s.nodes[constIdx]
    packed =       s.numbers[n.pos.LitId]
    (owner, idx) = getFieldAndOwner(objTyp, fieldPos) # `owner` might be != `objTyp`

  # During const packing, `packDiscr` is used with `numBits` = 32
  let (v, i) = unpackDiscr(packed, numBits = 32)
  writeDiscrField(dst, owner, idx, v, i)

  result = 1

proc loadConst(s: PackedEnv, idx: int, dst: LocHandle,
               mem: var VmMemoryManager): int =
  ## Loads the constant data represented by the ``PackedDataNode`` sub-tree at
  ## `idx` into the given VM memory location. Returns the total number of
  ## nodes that were part of the processed sub-tree
  let n = s.nodes[idx]
  case dst.typ.kind
  of akInt:
    # this works for both signed and unsigned integers
    dst.writeUInt(s.getIntVal(n))
  of akFloat:
    dst.writeFloat(s.getFloatVal(n))
  of akPtr:
    assert n.kind == pdkPtr
    case n.pos
    of 0: discard "nil ptr"
    else: unreachable()
  of akRef:
    assert n.kind == pdkPtr
    case n.pos
    of 0: discard "nil ref"
    else: unreachable()
  of akSeq, akArray:
    assert n.kind == pdkArray
    let L = n.pos.int

    if dst.typ.kind == akSeq:
      deref(dst).seqVal.newVmSeq(dst.typ, L, mem)

    var i = 0
    while i < L:
      result += loadConst(s, idx+1+result, getItemHandle(dst, i), mem)
      inc i

  of akString:
    assert n.kind == pdkString
    deref(dst).strVal.newVmString(s.strings[n.pos.LitId], mem.allocator)

  of akSet:
    assert n.kind == pdkSet
    const ValueRange = 0'u32..uint32(uint16.high)
    let L = n.pos.int
    for i in countup(0, L-1, 2):
      let
        a = s.nodes[idx+1+i+0].pos
        b = s.nodes[idx+1+i+1].pos
      assert a in ValueRange
      assert b in ValueRange
      bitSetInclRange(mbitSet(dst), BiggestInt(a)..BiggestInt(b))

    result = n.pos.int

  of akCallable:
    case n.kind
    of pdkPtr:
      # must be nil:
      assert n.pos == 0
    else:#of pckProc:
      deref(dst).callableVal = toFuncPtr(n.pos.FunctionIndex)

  of akClosure:
    case n.kind
    of pdkPtr:
      # must be nil
      assert n.pos == 0
    else: # of pckProc:
      let fncPtr = toFuncPtr(n.pos.FunctionIndex)
      deref(dst).closureVal = VmClosure(fnc: fncPtr, env: 0)

  of akObject:
    assert n.kind == pdkObj
    let L = n.pos.int
    var npos = idx+1
    for i in 0..<L:
      assert s.nodes[npos].kind == pdkField
      let
        fieldPos = s.nodes[npos].pos.int.fpos
        subLoc = getFieldHandle(dst, fieldPos)

      inc npos
      if subLoc.typ.kind != akDiscriminator:
        npos += loadConst(s, npos, subLoc, mem)
      else:
        npos += loadDiscrConst(s, npos, subLoc, dst.typ, fieldPos)

    result = npos - idx - 1 # -1 because of the `inc` below

  of akPNode, akDiscriminator:
    # `akPNode` is not possible since it's already rejected during executable
    # generation
    # `akDiscriminator` is already handled in the `akObject` of-branch
    unreachable()

  # increment again for the node itself
  inc result


proc loadIntoContext(c: var TCtx, p: PackedEnv) =
  loadEnv(c, p)

  c.typeInfoCache.emptyType = c.types[1]
  c.typeInfoCache.boolType = c.types[2]
  c.typeInfoCache.charType = c.types[3]
  c.typeInfoCache.stringType = c.types[4]
  c.typeInfoCache.pointerType = c.types[5]
  c.typeInfoCache.nodeType = c.types[6]

  c.allocator.byteType = c.typeInfoCache.charType

  mapList(c.globals, p.globals, x):
    c.heap.heapNew(c.allocator, c.types[x])

  mapList(c.complexConsts, p.cconsts, x):
    let
      t = c.types[x.typ]
      handle = c.allocator.allocConstantLocation(t)

    discard loadConst(p, x.packedId.int, handle, c.memory)
    handle

  mapList(c.callbackKeys, p.callbacks, it):
    # the actual callback procs are setup separately via ``registerCallbacks``
    IdentPattern(it)

proc loadFromFile(c: var TCtx, file: AbsoluteFile): Result[FunctionIndex, RodFileError] =
  var p: PackedEnv
  let err = readFromFile(p, file)
  if err != RodFileError.ok:
    result.initFailure(err)
    return

  loadIntoContext(c, p)
  c.code = move p.code

  result.initSuccess(p.entryPoint)

proc registerCallbacks(c: var TCtx): bool =
  ## Registers the callbacks and makes sure that they match with the ones the
  ## executable expects. Returns 'true' on success and 'false' otherwise
  var other: seq[IdentPattern]
  swap(other, c.callbackKeys) # `c.callbackKeys` is now empty

  # first, register all ops that the runner knows of:
  registerBasicOps(c)
  registerDebugOps(c)
  registerIoReadOps(c)
  registerIoWriteOps(c)
  registerOsOps(c)
  registerOs2Ops(c)

  registerCallback c, "stdlib.system.getOccupiedMem", proc (a: VmArgs) =
    setResult(a, a.mem.allocator.getUsedMem().int)

  if c.callbackKeys.len != other.len:
    # this means one of the following:
    # - the runner and the executable's compiler were built from different
    #  compiler sources
    # - the runner uses a stdlib different from the one the executable uses
    # In both cases, execution errors are very likely to happen, so we abort
    return false

  # then make sure that the callbacks are at the indices the function table
  # entries expect them to be:
  result = true
  swap(other, c.callbackKeys)
  for i, p in c.callbackKeys.pairs:
    if other[i].string != p.string:
      echo "expected '$#' callback but got '$#'" % [p.string, other[i].string]
      result = false

proc createLegacyStackTrace(
    c: TCtx, 
    thread: VmThread, 
    instLoc: InstantiationInfo = instLoc(-1)
  ): VMReport =
  # TODO: duplicated from `compilerbridge`, consolidate after all the reports
  #       bits are untangled.
  let st = createStackTrace(c, thread)
  result = VMReport(kind: rvmStackTrace,
                    currentExceptionA: st.currentExceptionA,
                    currentExceptionB: st.currentExceptionB,
                    traceReason: vmEventToLegacyReportKind(st.traceReason),
                    stacktrace: st.stacktrace,
                    skipped: st.skipped,
                    location: some source(c, thread),
                    reportInst: toReportLineInfo(instLoc))

proc main*(args: seq[string]): int =
  let config = newConfigRef(cli_reporter.reportHook)
  config.writeHook =
    proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
      msgs.msgWrite(conf, msg, flags)
  config.writelnHook =
    proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
      conf.writeHook(conf, msg & "\n", flags)

  # setup the execution context
  var c = TCtx(config: config, mode: emStandalone)
  c.features.incl(allowInfiniteLoops)
  c.heap.slots.newSeq(1) # setup the heap

  let lr = loadFromFile(c, toAbsolute(args[0], getCurrentDir().AbsoluteDir))
  if lr.isErr:
    echo "failed to load file: ", lr.takeErr
    return 1

  if not registerCallbacks(c):
    # callback setup failed -> abort
    echo "failed to register callbacks"
    return 1

  let
    entryPoint = c.functions[lr.unsafeGet.int]
    cb = proc (c: TCtx, r: TFullReg): PNode =
      c.config.internalAssert(r.kind == rkInt):
        "expected int return value" # either the executable is malformed or
                                    # there's an issue with the code-generator
      newIntNode(nkIntLit, r.intVal)

  # setup the starting frame:
  var frame = TStackFrame(prc: entryPoint.sym, next: -1)
  frame.slots.newSeq(entryPoint.regCount)

  # the execution part. Set up a thread and run it until it either exits
  # normally or abnormally
  var thread = initVmThread(c, entryPoint.start, frame)
  block:
    let r = execute(c, thread)
    case r.kind
    of yrkDone:
      # on successful execution, the executable's main function returns the
      # value of ``programResult``, which we use as the runner's exit code
      let reg = c.sframes[0].slots[r.reg.get]
      result = regToNode(c, reg, nil, TLineInfo()).intVal.int
      break
    of yrkError:
      # an uncaught error occurred
      c.config.localReport(createLegacyStackTrace(c, thread))
      let location = 
        case r.error.kind
        of vmEvtUserError:
          r.error.errLoc
        of vmEvtArgNodeNotASymbol:
          r.error.argAst.info
        else:
          source(c, thread)
      c.config.localReport(vmEventToLegacyVmReport(r.error, some location))
      result = 1
    of yrkQuit:
      # ``quit`` was called
      result = r.exitCode
    of yrkMissingProcedure:
      # a procedure stub was encountered -- this shouldn't happen. We print
      # the current stack-trace, write out an error message, and then quit
      c.config.localReport(createLegacyStackTrace(c, thread))
      c.config.msgWrite("trying to call a procedure that is a stub: $1" %
                        [c.functions[r.entry.int].sym.name.s])
      result = 1

  dispose(c, thread)

when isMainModule:
  programResult = main(commandLineParams())