## This module implements the interface between the VM and the rest of the
## compiler. The VM is only interacted with through this interface. Do note
## that right now, the compiler still indirectly interacts with the VM through
## the ``vmdef.TCtx`` object.
##
## The interface includes:
## - the procedure that sets up a VM instance for use during compilation
##   (``setupGlobalCtx``)
## - the routines for executing expressions, statements, and macros with the VM
## - an implementation of the ``passes`` interface that executes processed
##   code with the VM (``evalPass``)
## - the VM-related compilerapi

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_types,
    ast,
    errorhandling,
    errorreporting,
    lineinfos,
    trees
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/mir/[
    mirenv,
    mirtrees
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    passes,
    semcomptime,
    transf
  ],
  compiler/utils/[
    debugutils,
    idioms
  ],
  compiler/vm/[
    vmcompilerserdes,
    vmdef,
    vmhooks,
    vmjit,
    vmlegacy,
    vmops,
    vmprofiler,
    vmserialize,
    vmtypegen,
    vmutils,
    vm
  ],
  experimental/[
    results
  ]

import std/options as std_options

from std/strutils import join
from std/times import cpuTime

from compiler/vm/vmgen import vmGenDiagToAstDiagVmGenError

# TODO: legacy report cruft remove from here
from compiler/ast/reports import wrap, toReportLineInfo
from compiler/ast/reports_vm import VMReport
from compiler/ast/reports_sem import SemReport
from compiler/ast/reports_internal import InternalReport
from compiler/ast/report_enums import ReportKind

type
  ExecErrorKind* = enum
    execErrorVm
    execErrorVmGen
    execErrorQuit

  ExecErrorReport* = object
    stackTrace*: VmStackTrace   ## The VM stack-trace
    location*: TLineInfo        ## Source location of the trace
    instLoc*: InstantiationInfo ## report instantiation location
    case kind*: ExecErrorKind   ## kind of error execution of vm code gen
      of execErrorVm:
        vmErr*: VmEvent
      of execErrorVmGen:
        genErr*: VmGenDiag
      of execErrorQuit:
        exitCode*: int

  ExecutionResult* = Result[PNode, ExecErrorReport]

  PEvalContext* = ref EvalContext
  EvalContext* {.final.} = object of RootObj
    ## All state required to on-demand translate AST to VM bytecode and execute
    ## it. An ``EvalContext`` instance makes up everything that is required
    ## for running code at compile-time.
    vm*: TCtx
    jit*: JitState

  PVmCtx* = ref object of RootObj
    ## Wrapper type intended for storing only a VM instance (without a JIT
    ## environment) in the module graph.
    context*: TCtx

  PEvalPassContext = ref object of PPassContext
    ## Pass context for the evaluation pass.
    graph: ModuleGraph
    module: PSym
    oldErrorCount: int

# prevent a default `$` implementation from being generated
func `$`(e: ExecErrorReport): string {.error.}

proc logBytecode(c: TCtx, owner: PSym, start: int) =
  ## If enabled, renders the bytecode ranging from `start` to the current end
  ## into text that is then written to the standard output.
  const Symbol = "expandVmListing"
  if owner != nil and c.config.isDefined(Symbol):
    let name = c.config.getDefined(Symbol)
    # if no value is specified for the conditional sym (i.e.,
    # ``--define:expandVmListing``), `name` is 'true', which we interpret
    # as "log everything"
    if name == "true" or name == owner.name.s:
      let listing = codeListing(c, start)
      c.config.msgWrite: renderCodeListing(c.config, owner, listing)

proc putIntoReg(dest: var TFullReg; jit: var JitState, c: var TCtx, n: PNode,
                formal: PType) =
  ## Treats `n` as a constant expression and loads the value it represents
  ## into `dest`.
  let
    typ = c.getOrCreate(formal.skipTypes({tySink, tyStatic}))
    data = constDataToMir(c, jit, n)
  case typ.kind
  of akInt:
    dest.ensureKind(rkInt, c.memory)
    dest.intVal = jit.env.getInt(data[0].number)
  of akFloat:
    dest.ensureKind(rkFloat, c.memory)
    dest.floatVal = jit.env.getFloat(data[0].number)
  of akPtr:
    dest.ensureKind(rkAddress, c.memory)
    # non-nil values should have already been reported as an error
    assert data[0].kind == mnkNilLit
  of akPNode:
    dest.ensureKind(rkNimNode, c.memory)
    dest.nimNode = jit.env[data[0].ast]
  else:
    dest.initLocReg(typ, c.memory)
    initFromExpr(dest.handle, data, jit.env, c)

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
      errKind = err.kind
      astDiagTrace = AstDiagVmTrace(
        currentExceptionA: err.stackTrace.currentExceptionA,
        currentExceptionB: err.stackTrace.currentExceptionB,
        stacktrace: err.stackTrace.stacktrace,
        skipped: err.stackTrace.skipped,
        location: err.location,
        instLoc: err.instLoc)
      astDiag =
        case errKind
        of execErrorVm:
          let location =
            case err.vmErr.kind
            of vmEvtUserError:         err.vmErr.errLoc
            of vmEvtArgNodeNotASymbol: err.vmErr.argAst.info
            else:                      err.location

          PAstDiag(
            kind: adVmError,
            location: location,
            instLoc: err.vmErr.instLoc,
            vmErr: vmEventToAstDiagVmError(err.vmErr),
            vmTrace: astDiagTrace)
        of execErrorVmGen:
          PAstDiag(
            kind: adVmGenError,
            location: err.genErr.location,
            instLoc: err.genErr.instLoc,
            vmGenErr: vmGenDiagToAstDiagVmGenError(err.genErr),
            duringJit: true,
            vmGenTrace: astDiagTrace)
        of execErrorQuit:
          PAstDiag(
            kind: adVmQuit,
            location: err.location,
            instLoc: err.instLoc,
            vmExitCode: err.exitCode,
            vmExitTrace: astDiagTrace)

    result = config.newError(node, astDiag, instLoc(-1))

proc createStackTrace(c: TCtx, raw: VmRawStackTrace;
                      recursionLimit: int = 100): VmStackTrace =
  ## Creates a compiler-facing stacktrace from the internal stacktrace `raw`.
  result = VmStackTrace(currentExceptionA: nil, currentExceptionB: nil)

  var count = 0
  # create the stacktrace entries:
  for i in countdown(raw.high, 0):
    let f {.cursor.} = raw[i]

    if count < recursionLimit - 1 or i == 0:
      # The `i == 0` is to make sure that we're always including the bottom of
      # the stack (the entry procedure) in the trace

      # Since we're walking the stack from top to bottom, the elements are
      # added to the trace in reverse order (the most recent procedure is
      # first in the list, not last). This needs to be accounted for by the
      # actual reporting logic
      result.stacktrace.add((sym: f.sym, location: c.debug[f.pc]))

    inc count

  if count > recursionLimit:
    result.skipped = count - recursionLimit

  assert result.stacktrace.len() <= recursionLimit # post condition check

proc buildError(c: TCtx, thread: VmThread, event: sink VmEvent): ExecErrorReport  =
  ## Creates an `ExecErrorReport` with the `event` and a stack-trace for
  ## `thread`
  let stackTrace =
    if event.kind == vmEvtUnhandledException and event.trace.len > 0:
      # HACK: an unhandled exception can be reported without providing a trace.
      #       Ideally, that shouldn't happen
      createStackTrace(c, event.trace)
    else:
      createStackTrace(c, thread)

  ExecErrorReport(
    stackTrace: stackTrace,
    instLoc: instLoc(-1),
    location: source(c, thread),
    kind: execErrorVm,
    vmErr: event)

proc buildError(c: TCtx, thread: VmThread, diag: sink VmGenDiag): ExecErrorReport  =
  ## Creates an `ExecErrorReport` with the `diag` and a stack-trace for
  ## `thread`
  ExecErrorReport(
    stackTrace: createStackTrace(c, thread),
    instLoc: instLoc(-1),
    location: source(c, thread),
    kind: execErrorVmGen,
    genErr: diag)

proc buildQuit(c: TCtx, thread: VmThread, exitCode: int): ExecErrorReport =
  ## Creates an `ExecErrorReport` with the `exitCode` and a stack-trace for
  ## `thread`
  ExecErrorReport(
    stackTrace: createStackTrace(c, thread),
    instLoc: instLoc(-1),
    location: source(c, thread),
    kind: execErrorQuit,
    exitCode: exitCode)

proc createLegacyStackTrace(
    c: TCtx,
    thread: VmThread,
    instLoc: InstantiationInfo = instLoc(-1)
  ): VMReport =
  let st = createStackTrace(c, thread)
  result = VMReport(kind: rvmStackTrace,
                    currentExceptionA: st.currentExceptionA,
                    currentExceptionB: st.currentExceptionB,
                    stacktrace: st.stacktrace,
                    skipped: st.skipped,
                    location: some source(c, thread),
                    reportInst: toReportLineInfo(instLoc))

proc execute(jit: var JitState, c: var TCtx, thread: sink VmThread,
             cb: proc(c: TCtx, r: TFullReg): PNode
            ): ExecutionResult {.inline.} =
  ## This is the entry point for invoking the VM to execute code at
  ## compile-time. The `cb` callback is used to deserialize the result stored
  ## as VM data into ``PNode`` AST, and is invoked with the register that
  ## holds the result

  # run the VM until either no code is left to execute or an event implying
  # execution can't go on occurs
  while true:
    var r = execute(c, thread)
    case r.kind
    of yrkDone:
      # execution is finished
      doAssert r.reg.isSome() or c.mode in {emStaticStmt, emRepl},
        "non-static stmt evaluation must produce a value, mode: " & $c.mode
      let reg =
        if r.reg.isSome:
          thread.regs[r.reg.get]
        else:
          TFullReg(kind: rkNone)
      result.initSuccess cb(c, reg)
      break
    of yrkError:
      result.initFailure buildError(c, thread, r.error)
      break
    of yrkQuit:
      case c.mode
      of emRepl, emStaticExpr, emStaticStmt:
        # XXX: should code run at compile time really be able to force-quit
        #      the compiler? It currently can.
        localReport(c.config, createLegacyStackTrace(c, thread))
        localReport(c.config, InternalReport(kind: rintQuitCalled))
        # FIXME: this will crash the compiler (RangeDefect) if `quit` is
        #        called with a value outside of int8 range!
        msgQuit(int8(r.exitCode))
      of emConst, emOptimize:
        result.initFailure buildQuit(c, thread, r.exitCode)
        break
      of emStandalone:
        unreachable("not valid at compile-time")
    of yrkMissingProcedure:
      # a stub entry was encountered -> generate the code for the
      # corresponding procedure
      let res = compile(jit, c, r.entry)
      if res.isErr:
        # code-generation failed
        result.initFailure:
          buildError(c, thread, res.takeErr)
        break
      # success! ``compile`` updated the procedure's entry, so we can
      # continue execution
      logBytecode(c, c.functions[r.entry.int].sym, res.get.start)
    of yrkEcho:
      # vm yielded with an echo
      # xxx: `localReport` and report anything needs to be replaced, this is
      #      just output and it's ridiculous that it all funnles through
      #      `cli_reporter`. Using it here only because I'm sure there is some
      #      spooky action at a distance breakage without. at least it's pushed
      #      out to the edge.
      localReport(c.config, InternalReport(msg: r.strs.join(""),
                                           kind: rintEchoMessage))
      # after echo continue executing, hence no `break`

  dispose(c, thread)

proc execute(jit: var JitState, c: var TCtx, info: CodeInfo): ExecutionResult =
  let thread = initVmThread(c, info.start, info.regCount, nil)
  execute(jit, c, thread,
          proc(c: TCtx, r: TFullReg): PNode = c.graph.emptyNode)

template returnOnErr(res: VmGenResult, config: ConfigRef, node: PNode): CodeInfo =
  ## Unpacks the vmgen result. If the result represents an error, exits the
  ## calling function by returning a new `nkError` wrapping `node`
  let r = res
  if r.isOk:
    r.take
  else:
    let
      vmGenDiag = r.takeErr
      diag = PAstDiag(
              kind: adVmGenError,
              location: vmGenDiag.location,
              instLoc: vmGenDiag.instLoc,
              vmGenErr: vmGenDiagToAstDiagVmGenError(vmGenDiag),
              duringJit: false)

    return config.newError(node, diag, instLoc())

proc reportIfError(config: ConfigRef, n: PNode) =
  ## If `n` is a `nkError`, reports the error via `handleReport`. This is
  ## only meant for errors from vm/vmgen invocations and is also only a
  ## transition helper until all vm invocation functions properly propagate
  ## `nkError`
  if n.isError:
    # Errors from direct vmgen invocations don't have a stack-trace
    if n.diag.kind == adVmGenError and n.diag.duringJit or
        n.diag.kind == adVmError:
      let st =
        case n.diag.kind
        of adVmGenError: n.diag.vmGenTrace
        of adVmError:    n.diag.vmTrace
        else:            unreachable()

      config.handleReport(
                wrap(VMReport(kind: rvmStackTrace,
                        currentExceptionA: st.currentExceptionA,
                        currentExceptionB: st.currentExceptionB,
                        stacktrace: st.stacktrace,
                        skipped: st.skipped,
                        location: some st.location,
                        reportInst: toReportLineInfo(st.instLoc))),
                instLoc(-1))

    config.localReport(n)


template mkCallback(cn, rn, body): untyped =
  let p = proc(cn: TCtx, rn: TFullReg): PNode = body
  p

proc evalStmt(jit: var JitState, c: var TCtx, n: PNode): PNode =
  let n = transformExpr(c.graph, c.idgen, c.module, n)
  let info = genStmt(jit, c, n).returnOnErr(c.config, n)

  # execute new instructions; this redundant opcEof check saves us lots
  # of allocations in 'execute':
  if c.code[info.start].opcode != opcEof:
    result = execute(jit, c, info).unpackResult(c.config, n)
  else:
    result = c.graph.emptyNode

proc registerAdditionalOps*(c: var TCtx, disallowDangerous: bool) =
  ## Convenience proc used for setting up the overrides relevant during
  ## compile-time execution. If `disallowDangerous` is set to 'true', all
  ## operations that are able to modify the host's environment are replaced
  ## with no-ops
  template register(list: untyped) =
    for it in list:
      registerCallback(c, it.pattern, it.prc)

  register(): basicOps()
  register(): macroOps()
  register(): debugOps()
  register(): compileTimeOps()
  register(): ioReadOps()
  register(): osOps()

  let cbStart = c.callbacks.len # remember where the callbacks for dangerous
                                # ops start
  register(): gorgeOps()
  register(): ioWriteOps()
  register(): os2Ops()

  if disallowDangerous:
    # note: replacing the callbacks like this only works because
    # ``registerCallback`` always appends them to the list
    for i in cbStart..<c.callbacks.len:
      # replace with a no-op
      c.callbacks[i] = proc(a: VmArgs) {.nimcall.} = discard

  # the `cpuTime` callback doesn't fit any other category so it's registered
  # here
  if optBenchmarkVM in c.config.globalOptions or not disallowDangerous:
    registerCallback c, "stdlib.times.cpuTime", proc(a: VmArgs) {.nimcall.} =
      setResult(a, cpuTime())
  else:
    registerCallback c, "stdlib.times.cpuTime", proc(a: VmArgs) {.nimcall.} =
      setResult(a, 5.391245e-44)  # Randomly chosen

proc setupGlobalCtx*(module: PSym; graph: ModuleGraph; idgen: IdGenerator) =
  addInNimDebugUtils(graph.config, "setupGlobalCtx")
  if graph.vm.isNil:
    let
      disallowDangerous =
        defined(nimsuggest) or graph.config.cmd == cmdCheck or
        vmopsDanger notin graph.config.features

    var ctx = initCtx(module, graph.cache, graph, idgen, legacyReportsVmTracer)
    ctx.flags = {cgfAllowMeta}
    registerAdditionalOps(ctx, disallowDangerous)

    graph.vm = PEvalContext(vm: ctx, jit: initJit(graph))
  elif graph.vm of PVmCtx:
    # take the VM instance provided by the wrapper and create a proper
    # evaluation context from it
    let ctx = move PVmCtx(graph.vm).context
    graph.vm = PEvalContext(vm: ctx, jit: initJit(graph))
  else:
    let c = PEvalContext(graph.vm)
    refresh(c.vm, module, idgen)

proc eval(jit: var JitState, c: var TCtx; prc: PSym, n: PNode): PNode =
  let
    n = transformExpr(c.graph, c.idgen, c.module, n)
    requiresValue = c.mode != emStaticStmt
    r =
      if requiresValue: genExpr(jit, c, n)
      else:             genStmt(jit, c, n)

  let (start, regCount) = r.returnOnErr(c.config, n)

  if c.code[start].opcode == opcEof: return newNodeI(nkEmpty, n.info)
  assert c.code[start].opcode != opcEof

  logBytecode(c, prc, start)

  let cb =
    if requiresValue:
      mkCallback(c, r): c.regToNode(r, n.typ, n.info)
    else:
      mkCallback(c, r): newNodeI(nkEmpty, n.info)

  let thread = initVmThread(c, start, regCount, prc)
  result = execute(jit, c, thread, cb).unpackResult(c.config, n)

proc evalConstExprAux(module: PSym, idgen: IdGenerator, g: ModuleGraph,
                      prc: PSym, n: PNode,
                      mode: TEvalMode): PNode =
  addInNimDebugUtils(g.config, "evalConstExprAux", prc, n, result)
  setupGlobalCtx(module, g, idgen)

  # check whether the code accesses unavailable locations:
  if (let r = check(n); r != nil):
    # it does
    return g.config.newError(r, PAstDiag(kind: adSemUnavailableLocation))

  let
    c = PEvalContext(g.vm)
    oldMode = c.vm.mode

  # update the mode, and restore it once we're done
  c.vm.mode = mode
  result = eval(c.jit, c.vm, prc, n)
  c.vm.mode = oldMode

proc evalConstExpr*(module: PSym; idgen: IdGenerator; g: ModuleGraph; e: PNode): PNode {.inline.} =
  result = evalConstExprAux(module, idgen, g, nil, e, emConst)

proc evalStaticExpr*(module: PSym; idgen: IdGenerator; g: ModuleGraph; e: PNode, prc: PSym): PNode {.inline.} =
  result = evalConstExprAux(module, idgen, g, prc, e, emStaticExpr)

proc evalStaticStmt*(module: PSym; idgen: IdGenerator; g: ModuleGraph; e: PNode, prc: PSym): PNode {.inline.} =
  result = evalConstExprAux(module, idgen, g, prc, e, emStaticStmt)

proc setupCompileTimeVar*(module: PSym; idgen: IdGenerator; g: ModuleGraph; n: PNode) {.inline.} =
  let r = evalConstExprAux(module, idgen, g, nil, n, emStaticStmt)
  # TODO: the node needs to be returned to the caller instead
  reportIfError(g.config, r)

proc setupMacroParam(reg: var TFullReg, jit: var JitState, c: var TCtx, x: PNode, typ: PType) =
  case typ.kind
  of tyStatic:
    putIntoReg(reg, jit, c, x, typ)
  else:
    var n = x
    if n.kind in {nkHiddenSubConv, nkHiddenStdConv}: n = n[1]
    # TODO: is anyone on the callsite dependent on this modifiction of `x`?
    n.typ = x.typ
    reg = TFullReg(kind: rkNimNode, nimNode: n)

proc evalMacroCall*(jit: var JitState, c: var TCtx, call, args: PNode,
                    sym: PSym): PNode =
  ## Evaluates a call to the macro `sym` with arguments `arg` with the VM.
  ##
  ## `call` is the original call expression, which is used as the ``wrongNode``
  ## in case of an error, as the node returned by the ``callsite`` macro API
  ## procedure, and for providing line information.
  let oldMode = c.mode
  c.mode = emStaticStmt
  c.comesFromHeuristic.line = 0'u16
  c.callsite = call

  defer:
    # restore the previous state when exiting this procedure
    # TODO: neither ``mode`` nor ``callsite`` should be stored as part of the
    #       global execution environment (i.e. ``TCtx``). ``callsite`` is part
    #       of the state that makes up a single VM invocation, and ``mode`` is
    #       only needed for ``vmgen``
    c.mode = oldMode
    c.callsite = nil

  let wasAvailable = isAvailable(jit, c, sym)
  let (start, regCount) = loadProc(jit, c, sym).returnOnErr(c.config, call)

  # make sure to only output the code listing once:
  if not wasAvailable:
    logBytecode(c, sym, start)

  var thread = initVmThread(c, start, regCount, sym)
  # return value:
  thread.regs[0] = TFullReg(kind: rkNimNode, nimNode: newNodeI(nkEmpty, call.info))

  # put the normal arguments into registers
  for i in 1..<sym.typ.len:
    setupMacroParam(thread.regs[i], jit, c, args[i - 1], sym.typ[i])

  # put the generic arguments into registers
  let gp = sym.ast[genericParamsPos]
  for i in 0..<gp.safeLen:
    # skip implicit type parameters -- they're not part of the internal
    # signature
    if tfImplicitTypeParam notin gp[i].sym.typ.flags:
      let idx = sym.typ.len + i
      setupMacroParam(thread.regs[idx], jit, c, args[idx - 1], gp[i].sym.typ)

  let cb = mkCallback(c, r): r.nimNode
  result = execute(jit, c, thread, cb).unpackResult(c.config, call)

  if result.kind != nkError and cyclicTree(result):
    result = c.config.newError(call, PAstDiag(kind: adCyclicTree))

proc evalMacroCall*(module: PSym; idgen: IdGenerator; g: ModuleGraph;
                    templInstCounter: ref int;
                    call, args: PNode, sym: PSym): PNode =
  ## Similar to the other ``evalMacroCall`` overload, but also updates the
  ## compile-time execution context with the provided `module`, `idgen`, `g`,
  ## and `templInstCounter`.
  setupGlobalCtx(module, g, idgen)
  let c = PEvalContext(g.vm)
  c.vm.templInstCounter = templInstCounter

  result = evalMacroCall(c.jit, c.vm, call, args, sym)

proc dumpVmProfilerData*(graph: ModuleGraph): string =
  ## Dumps the profiler data collected by the profiler of the VM instance
  ## associated with `graph` to a string.
  let c = PEvalContext(graph.vm)
  result =
    if c != nil: dump(graph.config, c.vm.profiler)
    else:        ""

# ----------- the VM-related compilerapi -----------

# NOTE: it might make sense to move the VM-related compilerapi into
#       ``nimeval.nim`` -- the compiler itself doesn't depend on or uses it

proc execProc*(jit: var JitState, c: var TCtx; sym: PSym;
               args: openArray[PNode]): PNode =
  # XXX: `localReport` is still used here since execProc is only used by the
  # VM's compilerapi (`nimeval`) whose users don't know about nkError yet
  if sym.kind in routineKinds:
    if sym.typ.len-1 != args.len:
      localReport(c.config, sym.info, SemReport(
        kind: rsemWrongNumberOfArguments,
        sym: sym,
        countMismatch: (
          expected: sym.typ.len - 1,
          got: args.len)))

    else:
      let (start, maxSlots) = block:
        # XXX: `returnOnErr` should be used here instead, but isn't for
        #      backwards compatiblity
        let r = loadProc(jit, c, sym)
        if unlikely(r.isErr):
          localReport(c.config, vmGenDiagToLegacyVmReport(r.takeErr))
          return nil
        r.unsafeGet

      var thread = initVmThread(c, start, maxSlots, sym)

      # setup parameters:
      if not isEmptyType(sym.typ[0]) or sym.kind == skMacro:
        let typ = c.getOrCreate(sym.typ[0])
        if not thread.regs[0].loadEmptyReg(typ, sym.info, c.memory):
          thread.regs[0].initLocReg(typ, c.memory)
      # XXX We could perform some type checking here.
      for i in 1..<sym.typ.len:
        putIntoReg(thread.regs[i], jit, c, args[i-1], sym.typ[i])

      let cb =
        if not isEmptyType(sym.typ[0]):
          mkCallback(c, r): c.regToNode(r, sym.typ[0], sym.info)
        elif sym.kind == skMacro:
          # TODO: missing cyclic check
          mkCallback(c, r): r.nimNode
        else:
          mkCallback(c, r): newNodeI(nkEmpty, sym.info)

      let r = execute(jit, c, thread, cb)
      result = r.unpackResult(c.config, c.graph.emptyNode)
      reportIfError(c.config, result)
      if result.isError:
        result = nil
  else:
    c.config.internalError(sym.info, "symbol doesn't represent a routine")

# XXX: the compilerapi regarding globals (getGlobalValue/setGlobalValue)
#      doesn't work the same as before. Previously, the returned PNode
#      could be used to modify the actual global value, but this is not
#      possible anymore

proc getGlobalValue*(c: EvalContext, s: PSym): PNode =
  ## Does not perform type checking, so ensure that `s.typ` matches the
  ## global's type
  internalAssert(c.vm.config, s.kind in {skLet, skVar} and sfGlobal in s.flags)
  let
    slotIdx = c.vm.globals[c.jit.getGlobal(s)]
    slot = c.vm.heap.slots[slotIdx]

  result = c.vm.deserialize(slot.handle, s.typ, s.info)

proc setGlobalValue*(c: var EvalContext; s: PSym, val: PNode) =
  ## Does not do type checking so ensure the `val` matches the `s.typ`
  internalAssert(c.vm.config, s.kind in {skLet, skVar} and sfGlobal in s.flags)
  let
    slotIdx = c.vm.globals[c.jit.getGlobal(s)]
    slot = c.vm.heap.slots[slotIdx]
    data = constDataToMir(c.vm, c.jit, val)

  initFromExpr(slot.handle, data, c.jit.env, c.vm)

## what follows is an implementation of the ``passes`` interface that evaluates
## the code directly inside the VM. It is used for NimScript execution and by
## the ``nimeval`` interface

proc myOpen(graph: ModuleGraph; module: PSym; idgen: IdGenerator): PPassContext {.nosinks.} =
  result = PEvalPassContext(idgen: idgen, graph: graph, module: module)

proc isDecl(n: PNode): bool =
  case n.kind
  of nkStmtList:
    # if one sub-node is not declarative, neither is `n`
    for it in n.items:
      if not isDecl(it):
        return false
    result = true
  of nkEmpty, nkTypeSection, nkConstSection, nkImportStmt, nkImportAs,
     nkImportExceptStmt, nkFromStmt, nkCommentStmt, routineDefs:
    result = true
  else:
    result = false

proc myProcess(c: PPassContext, n: PNode): PNode =
  let c = PEvalPassContext(c)
  # don't eval errornous code. Also skip declarative nodes, as those represent
  # type definitions required for bootstrapping the basic type environment
  if c.oldErrorCount == c.graph.config.errorCounter and not n.isError and
     not isDecl(n):
    setupGlobalCtx(c.module, c.graph, c.idgen)
    let eval = PEvalContext(c.graph.vm)

    let r = evalStmt(eval.jit, eval.vm, n)
    reportIfError(c.graph.config, r)
    # TODO: use the node returned by evalStmt as the result and don't report
    #       the error here
    result = newNodeI(nkEmpty, n.info)
  else:
    result = n
  c.oldErrorCount = c.graph.config.errorCounter

proc myClose(graph: ModuleGraph; c: PPassContext, n: PNode): PNode =
  result = myProcess(c, n)

const evalPass* = makePass(myOpen, myProcess, myClose)
