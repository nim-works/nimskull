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
    cgir,
    cgirgen
  ],
  compiler/mir/[
    mirbridge,
    mirgen,
    mirpasses,
    mirtrees,
    sourcemaps,
  ],
  compiler/sem/[
    transf
  ],
  compiler/vm/[
    vmaux,
    vmcompilerserdes,
    vmdef,
    vmgen,
    vmmemory,
    vmtypegen
  ],
  experimental/[
    results
  ]

when defined(nimVMDebugGenerate):
  import
    compiler/front/msgs,
    compiler/vm/vmutils

export VmGenResult

type
  JitState* = object
    ## State of the VM's just-in-time compiler that is kept across invocations.
    discovery: DiscoveryData
      ## acts as the source-of-truth regarding what entities exists. All
      ## entities not registered with `discovery` also don't exist in
      ## ``TCtx``

func selectOptions(c: TCtx): set[GenOption] =
  result = {goIsNimvm}
  if cgfAllowMeta in c.flags:
    result.incl goGenTypeExpr

  if c.mode in {emConst, emOptimize, emStaticExpr, emStaticStmt}:
    result.incl goIsCompileTime

proc updateEnvironment(c: var TCtx, data: var DiscoveryData) =
  ## Needs to be called after a `vmgen` invocation and prior to resuming
  ## execution. Allocates and sets up the execution resources required for the
  ## newly gathered dependencies.
  ##
  ## This "commits" to the new dependencies.

  # procedures
  c.functions.setLen(data.procedures.len)
  for i, sym in visit(data.procedures):
    c.functions[i] = initProcEntry(c, sym)

  block: # globals and threadvars
    # threadvars are currently treated the same as normal globals
    var i = c.globals.len
    c.globals.setLen(data.globals.len + data.threadvars.len)

    template alloc(q: Queue[PSym]) =
      for _, sym in visit(q):
        let typ = c.getOrCreate(sym.typ)
        c.globals[i] = c.heap.heapNew(c.allocator, typ)
        inc i

    # order is important here!
    alloc(data.globals)
    alloc(data.threadvars)

  # constants
  c.complexConsts.setLen(data.constants.len)
  for i, sym in visit(data.constants):
    assert sym.ast.kind notin nkLiterals

    let
      typ = c.getOrCreate(sym.typ)
      handle = c.allocator.allocConstantLocation(typ)

    # TODO: strings, seqs and other values using allocation also need to be
    #       allocated with `allocConstantLocation` inside `serialize` here
    c.serialize(sym.ast, handle)

    c.complexConsts[i] = handle


func removeLastEof(c: var TCtx) =
  let last = c.code.len-1
  if last >= 0 and c.code[last].opcode == opcEof:
    # overwrite last EOF:
    assert c.code.len == c.debug.len
    c.code.setLen(last)
    c.debug.setLen(last)

func discoverGlobalsAndRewrite(data: var DiscoveryData, tree: var MirTree,
                               source: var SourceMap) =
  ## Scans `tree` for definitions of globals, registers them with the `data`,
  ## and rewrites their definitions into assignments.

  # scan the body for definitions of globals:
  var i = NodePosition 0
  while i.int < tree.len:
    case tree[i].kind
    of DefNodes:
      if tree[i + 1].kind == mnkGlobal and
         (let g = tree[i+1].sym; sfImportc notin g.flags):
        # found a global definition; register it. Imported ones are
        # ignored -- ``vmgen`` will report an error when the global is
        # accessed
        let s =
          if g.owner.kind in {skVar, skLet, skForVar}:
            g.owner # account for duplicated symbols (see ``transf.freshVar``)
          else:
            g
        data.registerGlobal(s)

      i = findEnd(tree, i) + 1 # skip the def's body
    else:
      inc i

  # at the moment, nothing depends on non-outermost defs of globals, so we
  # can rewrite all defs in one go:
  rewriteGlobalDefs(tree, source, outermost = false)

func register(c: var TCtx, data: DiscoveryData) =
  ## Registers the newly discovered entities in the link table, but doesn't
  ## commit to them yet.
  for i, it in peek(data.procedures):
    c.symToIndexTbl[it.id] = LinkIndex(i)

  for i, it in peek(data.constants):
    c.symToIndexTbl[it.id] = LinkIndex(i)

  # first register globals, then threadvars. This order must be the same as
  # the one they're later committed to in
  for i, it in peek(data.globals):
    c.symToIndexTbl[it.id] = LinkIndex(i)

  for i, it in peek(data.threadvars):
    c.symToIndexTbl[it.id] = LinkIndex(i)

proc generateMirCode(c: var TCtx, n: PNode;
                     isStmt = false): (MirTree, SourceMap) =
  ## Generates the initial MIR code for a standalone statement/expression.
  if isStmt:
    # we want statements wrapped in a scope, hence generating a proper
    # fragment
    result = generateCode(c.graph, c.module, selectOptions(c), n)
  else:
    generateCode(c.graph, selectOptions(c), n, result[0], result[1])

proc generateIR(c: var TCtx, tree: sink MirTree,
                source: sink SourceMap): CgNode {.inline.} =
  if tree.len > 0: generateIR(c.graph, c.idgen, c.module, tree, source)
  else:            newNode(cnkEmpty)

proc genStmt*(jit: var JitState, c: var TCtx; n: PNode): VmGenResult =
  ## Generates and emits code for the standalone top-level statement `n`.
  c.removeLastEof()

  # `n` is expected to have been put through ``transf`` already
  var (tree, sourceMap) = generateMirCode(c, n, isStmt = true)
  discoverGlobalsAndRewrite(jit.discovery, tree, sourceMap)
  applyPasses(tree, sourceMap, c.module, c.config, targetVm)
  discoverFrom(jit.discovery, MagicsToKeep, tree)
  register(c, jit.discovery)

  let
    n = generateIR(c, tree, sourceMap)
    start = c.code.len
    r = vmgen.genStmt(c, n)

  if unlikely(r.isErr):
    rewind(jit.discovery)
    return VmGenResult.err(r.takeErr)

  c.gABC(n, opcEof)
  updateEnvironment(c, jit.discovery)

  result = VmGenResult.ok: (start: start, regCount: c.prc.regInfo.len)

proc genExpr*(jit: var JitState, c: var TCtx, n: PNode): VmGenResult =
  ## Generates and emits code for the standalone expression `n`
  c.removeLastEof()

  # XXX: the way standalone expressions are currently handled is going to
  #      be a problem as soon as proper MIR passes need to be run (which
  #      all expect statements). Ideally, dedicated support for
  #      expressions would be removed from the JIT.

  var (tree, sourceMap) = generateMirCode(c, n)
  # constant expression outside of routines can currently also contain
  # definitions of globals...
  # XXX: they really should not, but that's up to sem. Example:
  #
  #        const c = block: (var x = 0; x)
  #
  #     If `c` is defined at the top-level, then `x` is a "global" variable
  discoverGlobalsAndRewrite(jit.discovery, tree, sourceMap)
  applyPasses(tree, sourceMap, c.module, c.config, targetVm)
  discoverFrom(jit.discovery, MagicsToKeep, tree)
  register(c, jit.discovery)

  let
    n = generateIR(c, tree, sourceMap)
    start = c.code.len
    r = vmgen.genExpr(c, n)

  if unlikely(r.isErr):
    rewind(jit.discovery)
    return VmGenResult.err(r.takeErr)

  c.gABC(n, opcRet, r.unsafeGet)
  updateEnvironment(c, jit.discovery)

  result = VmGenResult.ok: (start: start, regCount: c.prc.regInfo.len)

proc genProc(jit: var JitState, c: var TCtx, s: PSym): VmGenResult =
  c.removeLastEof()

  let body =
    if s.kind == skMacro:
      transformBody(c.graph, c.idgen, s, s.ast[bodyPos])
    else:
      # watch out! While compile-time only procedures don't need to be cached
      # here, we still need to retrieve their already cached body (if one
      # exists). Lifted inner procedures would otherwise not work.
      transformBody(c.graph, c.idgen, s, cache = not isCompileTimeProc(s))

  echoInput(c.config, s, body)
  var (tree, sourceMap) = generateCode(c.graph, s, selectOptions(c), body)
  echoMir(c.config, s, tree)
  # XXX: lifted globals are currently not extracted from the procedure and,
  #      for the most part, behave like normal locals. The call to
  #      ``discoverGlobalsAndRewrite`` makes sure that at least ``vmgen``
  #      doesn't have to be concerned with that, but eventually it needs
  #      to be decided how lifted globals should work in compile-time and
  #      interpreted contexts
  discoverGlobalsAndRewrite(jit.discovery, tree, sourceMap)
  applyPasses(tree, sourceMap, s, c.config, targetVm)
  discoverFrom(jit.discovery, MagicsToKeep, tree)
  register(c, jit.discovery)

  let outBody = generateIR(c.graph, c.idgen, s, tree, sourceMap)
  echoOutput(c.config, s, outBody)

  result = genProc(c, s, outBody)
  if unlikely(result.isErr):
    rewind(jit.discovery)
    return

  c.gABC(outBody, opcEof)
  updateEnvironment(c, jit.discovery)

proc registerProcedure*(jit: var JitState, c: var TCtx, prc: PSym): FunctionIndex =
  ## If it hasn't been already, adds `prc` to the set of procedures the JIT
  ## code-generator knowns about and sets up a function-table entry. `jit` is
  ## required to not be in the process of generating code.
  assert jit.discovery.procedures.isProcessed, "code generation in progress?"
  var index = -1

  register(jit.discovery, prc)
  # if one was added, commit to the new entry now and create a function-table
  # entry it
  c.functions.setLen(jit.discovery.procedures.len)
  for i, it in visit(jit.discovery.procedures):
    assert it == prc
    c.symToIndexTbl[it.id] = LinkIndex(i)
    c.functions[i] = initProcEntry(c, it)
    index = i

  if index == -1:
    # no entry was added -> one must exist already
    result = FunctionIndex(c.symToIndexTbl[prc.id])
  else:
    result = FunctionIndex(index)

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

  when defined(nimVMDebugGenerate):
    # XXX: ``compile`` shouldn't be responsible for neither the generating nor
    #      the reporting of a code-listing
    c.config.localReport():
      initVmCodeListingReport(c, prc.sym, nil, start = result.unsafeGet.start)

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
  c.callbacks.add(callback)
  c.callbackKeys.add(IdentPattern(pattern))
  assert c.callbacks.len == c.callbackKeys.len