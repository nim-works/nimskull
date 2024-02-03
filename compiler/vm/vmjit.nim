## Implements the framework for just-in-time (JIT) code-generation
## for the VM. Both procedures and standalone statements/expressions are
## supported as input.
##
## When a procedure is requested that hasn't been processed by the JIT
## compiler, it is transformed, pre-processed (MIR passes applied, etc.), and
## then the bytecode for it is generated. If code generation succeeds, all not-
## already-seen dependencies (like globals, constants, etc.) of the procedure
## are collected, registered with the JIT state, and loaded into the VM's
## execution environment, meaning that the requested procedure can be
## immediately invoked after.
##
## Both compile-time code execution and running NimScript files make use of
## the JIT compiler.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_types,
    ast_query,
  ],
  compiler/backend/[
    backends,
    cgir
  ],
  compiler/mir/[
    datatables,
    mirbodies,
    mirbridge,
    mirconstr,
    mirenv,
    mirgen,
    mirpasses,
    mirtrees,
  ],
  compiler/modules/[
    magicsys
  ],
  compiler/sem/[
    transf
  ],
  compiler/vm/[
    vmaux,
    vmserialize,
    vmdef,
    vmgen,
    vmjit_checks,
    vmlinker,
    vmmemory,
    vmtypegen
  ],
  experimental/[
    results
  ]

export VmGenResult

type
  JitState* = object
    ## State of the VM's just-in-time compiler that is kept across invocations.
    gen: CodeGenCtx
      ## code generator state

func selectOptions(c: TCtx): TranslationConfig =
  result = TranslationConfig(options: {goIsNimvm}, magicsToKeep: MagicsToKeep)
  # include additional options based on the JIT's configuration
  if cgfAllowMeta in c.flags:
    result.options.incl goGenTypeExpr

  if c.mode in {emConst, emOptimize, emStaticExpr, emStaticStmt}:
    result.options.incl goIsCompileTime

func swapState(c: var TCtx, gen: var CodeGenCtx) =
  ## Swaps the values of the fields shared between ``TCtx`` and ``CodeGenCtx``.
  ## This achieves reasonably fast mutable-borrow-like semantics without
  ## resorting to pointers.
  template swap(field: untyped) =
    swap(c.field, gen.field)

  # input parameters:
  swap(graph)
  swap(config)
  swap(mode)
  swap(features)
  swap(module)
  swap(callbackKeys)

  # input-output parameters:
  swap(code)
  swap(debug)
  swap(constants)
  swap(typeInfoCache)
  swap(rtti)

proc updateEnvironment(c: var TCtx, env: var MirEnv, cp: EnvCheckpoint) =
  ## Needs to be called after a `vmgen` invocation and prior to resuming
  ## execution. Allocates and sets up the execution resources required for the
  ## newly gathered dependencies (those added since `cp` was created).
  ##
  ## This "commits" to the new dependencies.

  # procedures
  for id, sym in since(env.procedures, cp.procs):
    c.functions.add initProcEntry(c, sym)

  # globals (which includes threadvars)
  for id, sym in since(env.globals, cp.globals):
    let typ = c.getOrCreate(sym.typ)
    c.globals.add c.heap.heapNew(c.allocator, typ)

  # constants
  for id, data in since(env.data, cp.data):
    let
      typ = c.getOrCreate(data[0].typ)
      handle = c.allocator.allocConstantLocation(typ)

    initFromExpr(handle, data, c)

    c.complexConsts.add handle

template preCheck(env: MirEnv, n: PNode) =
  ## Verifies that `n` can be built and run in JIT mode. If not, aborts the
  ## surrounding routine by returning a ``VmGenResult``.
  block:
    let r = validate(env, n)
    if r.isErr:
      return VmGenResult.err(r.takeErr)

func removeLastEof(c: var TCtx) =
  let last = c.code.len-1
  if last >= 0 and c.code[last].opcode == opcEof:
    # overwrite last EOF:
    assert c.code.len == c.debug.len
    c.code.setLen(last)
    c.debug.setLen(last)

proc generateMirCode(c: var TCtx, env: var MirEnv, n: PNode;
                     isStmt = false): MirBody =
  ## Generates the initial MIR code for a standalone statement/expression.
  if isStmt:
    # we want statements wrapped in a scope, hence generating a proper
    # fragment
    result = generateCode(c.graph, env, c.module, selectOptions(c), n)
  else:
    var bu: MirBuilder
    generateCode(c.graph, env, selectOptions(c), n, bu, result.source)
    result.code = finish(bu)

proc generateIR(c: var TCtx, env: MirEnv, body: sink MirBody): Body =
  backends.generateIR(c.graph, c.idgen, env, c.module, body)

proc setupRootRef(c: var TCtx) =
  ## Sets up if the ``RootRef`` type for the type info cache. This
  ## is a temporary workaround, refer to the documentation of the
  ## ``rootRef`` field.
  if c.typeInfoCache.rootRef == nil:
    let t = c.graph.getCompilerProc("RootObj")
    # the``RootObj`` type may not be available yet
    if t != nil:
      c.typeInfoCache.initRootRef(c.graph.config, t.typ)

template runCodeGen(c: var TCtx, cg: var CodeGenCtx, b: Body,
                    body: untyped): untyped =
  ## Prepares the code generator's context and then executes `body`. A
  ## delimiting 'eof' instruction is emitted at the end.
  setupRootRef(c)
  swapState(c, cg)
  let info = b.code.info
  let r = body
  cg.gABC(info, opcEof)
  swapState(c, cg)
  r

proc genStmt*(jit: var JitState, c: var TCtx; n: PNode): VmGenResult =
  ## Generates and emits code for the standalone top-level statement `n`.
  preCheck(jit.gen.env, n)
  c.removeLastEof()

  let cp = checkpoint(jit.gen.env)

  # `n` is expected to have been put through ``transf`` already
  var mirBody = generateMirCode(c, jit.gen.env, n, isStmt = true)
  applyPasses(mirBody, c.module, jit.gen.env, c.config, targetVm)
  for _ in discover(jit.gen.env, cp):
    discard "nothing to register"

  let
    body = generateIR(c, jit.gen.env, mirBody)
    start = c.code.len

  # generate the bytecode:
  let r = runCodeGen(c, jit.gen, body): genStmt(jit.gen, body)

  if unlikely(r.isErr):
    rewind(jit.gen.env, cp)
    return VmGenResult.err(r.takeErr)

  updateEnvironment(c, jit.gen.env, cp)

  result = VmGenResult.ok: (start: start, regCount: r.get)

proc genExpr*(jit: var JitState, c: var TCtx, n: PNode): VmGenResult =
  ## Generates and emits code for the standalone expression `n`
  preCheck(jit.gen.env, n)
  c.removeLastEof()

  # XXX: the way standalone expressions are currently handled is going to
  #      be a problem as soon as proper MIR passes need to be run (which
  #      all expect statements). Ideally, dedicated support for
  #      expressions would be removed from the JIT.

  let cp = checkpoint(jit.gen.env)

  var mirBody = generateMirCode(c, jit.gen.env, n)
  applyPasses(mirBody, c.module, jit.gen.env, c.config, targetVm)
  for _ in discover(jit.gen.env, cp):
    discard "nothing to register"

  let
    body = generateIR(c, jit.gen.env, mirBody)
    start = c.code.len

  # generate the bytecode:
  let r = runCodeGen(c, jit.gen, body): genExpr(jit.gen, body)

  if unlikely(r.isErr):
    rewind(jit.gen.env, cp)
    return VmGenResult.err(r.takeErr)

  updateEnvironment(c, jit.gen.env, cp)

  result = VmGenResult.ok: (start: start, regCount: r.get)

proc genProc(jit: var JitState, c: var TCtx, s: PSym): VmGenResult =
  let body =
    if isCompileTimeProc(s) and not defined(nimsuggest):
      # no need to go through the transformation cache
      transformBody(c.graph, c.idgen, s, s.ast[bodyPos])
    else:
      # watch out! Since transforming a procedure body permanently alters
      # the state of inner procedures, we need to both cache and later
      # retrieve the transformed body for non-compile-only routines or
      # when in suggest mode
      transformBody(c.graph, c.idgen, s, cache = true)

  preCheck(jit.gen.env, body)
  c.removeLastEof()

  let cp = checkpoint(jit.gen.env)

  echoInput(c.config, s, body)
  var mirBody = generateCode(c.graph, jit.gen.env, s, selectOptions(c), body)
  echoMir(c.config, s, mirBody)
  applyPasses(mirBody, s, jit.gen.env, c.config, targetVm)
  for _ in discover(jit.gen.env, cp):
    discard "nothing to register"

  let outBody = generateIR(c.graph, c.idgen, jit.gen.env, s, mirBody)
  echoOutput(c.config, s, outBody)

  try:
    # generate the bytecode:
    result = runCodeGen(c, jit.gen, outBody): genProc(jit.gen, s, outBody)
  except:
    # echo render(tree)
    raise

  if unlikely(result.isErr):
    rewind(jit.gen.env, cp)
    return

  updateEnvironment(c, jit.gen.env, cp)

func getGlobal*(jit: JitState, g: PSym): LinkIndex =
  ## Returns the link index for the symbol `g`. `g` must be known to `jit`.
  LinkIndex jit.gen.env.globals[g]

func isAvailable*(jit: JitState, c: TCtx, prc: PSym): bool =
  ## Returns whether the bytecode for `prc` is already available.
  prc in jit.gen.env.procedures and
    c.functions[jit.gen.env.procedures[prc].int].start >= 0

proc registerProcedure*(jit: var JitState, c: var TCtx, prc: PSym): FunctionIndex =
  ## If it hasn't been already, adds `prc` to the set of procedures the JIT
  ## code-generator knowns about and sets up a function-table entry. `jit` is
  ## required to not be in the process of generating code.
  if prc notin jit.gen.env.procedures:
    let id = jit.gen.env.procedures.add(prc)
    c.functions.add initProcEntry(c, prc)
    assert int(id) == c.functions.high, "tables are out of sync"

  result = FunctionIndex jit.gen.env.procedures[prc]

proc compile*(jit: var JitState, c: var TCtx, fnc: FunctionIndex): VmGenResult =
  ## Generates code for the the given function and updates the execution
  ## environment. In addition, the function's table entry is updated with the
  ## bytecode position and execution requirements (i.e. register count). Make
  ## sure to only use `compile` when you're sure the function wasn't generated
  ## yet
  let prc = c.functions[fnc.int]
  assert prc.start == -1, "proc already generated: " & $prc.start

  result = genProc(jit, c, prc.sym)
  if unlikely(result.isErr):
    return

  fillProcEntry(c.functions[fnc.int], result.unsafeGet)

proc loadProc*(jit: var JitState, c: var TCtx, sym: PSym): VmGenResult =
  ## The main entry point into the JIT code-generator. Retrieves the
  ## information required for executing `sym`. A function table entry is
  ## created first if it doesn't exist yet, and the procedure is also
  ## generated via `compile` if it wasn't already
  let
    idx = jit.registerProcedure(c, sym)
    prc = c.functions[idx.int]

  if prc.start >= 0:
    VmGenResult.ok: (start: prc.start, regCount: prc.regCount.int)
  else:
    compile(jit, c, idx)

proc registerCallback*(c: var TCtx; pattern: string; callback: VmCallback) =
  ## Registers the `callback` with `c`. After the ``registerCallback`` call,
  ## when a procedures of which the fully qualified identifier matches
  ## `pattern` is added to the VM's function table, all invocations of the
  ## procedure at run-time will invoke the callback instead.
  # XXX: consider renaming this procedure to ``registerOverride``
  c.callbacks.add(callback) # some consumers rely on preserving registration order
  c.callbackKeys.add(IdentPattern(pattern))
  assert c.callbacks.len == c.callbackKeys.len

proc constDataToMir*(c: var TCtx, jit: var JitState, e: PNode): MirTree =
  ## Translates the constant expression `e` to a MIR constant expression and
  ## returns it. Entities referenced by the constant expression (e.g.,
  ## procedures), are direclty registered with the environment.
  let cp = checkpoint(jit.gen.env)
  result = constDataToMir(jit.gen.env, e)

  # run the discovery pass:
  for _ in discover(jit.gen.env, cp):
    discard "nothing to register"

  # populate the VM environment with the discovered entities:
  updateEnvironment(c, jit.gen.env, cp)
