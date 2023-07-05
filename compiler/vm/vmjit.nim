## This module implements the just-in-time (JIT) code-generation logic. When
## running in JIT-mode, procedures are only ran through code-generation just
## before they're exectued. The resulting bytecode for each procedure is
## cached and re-used for subsequent procedure calls
##
## A high-level description:
## - a not yet generated function is encountered -> invoke `vmgen`
## - `vmgen` generates the code and collects new dependcies
## - the collected new dependencies are loaded into the execution environment
## - the function table entry is updated with the bytecode offset and the
##  required register count
## - execution resumes inside the procedure's body
##
## Compile-time code execution makes use of the JIT-mode

import
  compiler/ast/[
    ast_types,
    ast_query,
  ],
  compiler/mir/[
    mirbridge
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

# TODO: use `stdlib.pairs` instead once it uses `lent`
iterator lpairs[T](x: seq[T]): tuple[key: int, value: lent T] =
  var i = 0
  let L = x.len
  while i < L:
    yield (i, x[i])
    inc i

func selectOptions(c: TCtx): set[GenOption] =
  result = {goIsNimvm}
  if cgfAllowMeta in c.flags:
    result.incl goGenTypeExpr

  if c.mode in {emConst, emOptimize, emStaticExpr, emStaticStmt}:
    result.incl goIsCompileTime

func setupLinkState(c: var TCtx) =
  c.linkState.newProcs.setLen(0)
  c.linkState.newGlobals.setLen(0)
  c.linkState.newConsts.setLen(0)
  c.linkState.nextGlobal = c.globals.len.uint32
  c.linkState.nextConst = c.complexConsts.len.uint32
  c.linkState.nextProc = c.functions.len.uint32

proc updateEnvironment(c: var TCtx) =
  ## Needs to be called after a `vmgen` invocation and prior to resuming
  ## execution. Allocates and sets up the execution resources required for the
  ## newly gathered dependencies
  template grow(list, nextName) =
    assert c.list.len <= c.linkState.nextName.int
    c.list.setLen(c.linkState.nextName)

  let
    ps = c.functions.len
    gs = c.globals.len
    cs = c.complexConsts.len

  # allocate the required global/const/function slots
  grow(globals, nextGlobal)
  grow(complexConsts, nextConst)
  grow(functions, nextProc)

  for i, sym in c.linkState.newProcs.lpairs:
    c.functions[ps + i] = initProcEntry(c, sym)

  for i, sym in c.linkState.newGlobals.lpairs:
    let typ = c.getOrCreate(sym.typ)
    c.globals[gs + i] = c.heap.heapNew(c.allocator, typ)

  for i, sym in c.linkState.newConsts.lpairs:
    assert sym.ast.kind notin nkLiterals

    let
      typ = c.getOrCreate(sym.typ)
      handle = c.allocator.allocConstantLocation(typ)

    # TODO: strings, seqs and other values using allocation also need to be
    #       allocated with `allocConstantLocation` inside `serialize` here
    c.serialize(sym.ast, handle)

    c.complexConsts[cs + i] = handle


func removeLastEof(c: var TCtx) =
  let last = c.code.len-1
  if last >= 0 and c.code[last].opcode == opcEof:
    # overwrite last EOF:
    assert c.code.len == c.debug.len
    c.code.setLen(last)
    c.debug.setLen(last)

proc genStmt*(c: var TCtx; n: PNode): VmGenResult =
  ## Generates and emits code for the standalone statement `n`
  c.removeLastEof()
  c.setupLinkState()

  let n = canonicalizeSingle(c.graph, c.idgen, c.module, n, selectOptions(c))
  gatherDependencies(c, n)

  let
    start = c.code.len
    r = vmgen.genStmt(c, n)

  if unlikely(r.isErr):
    return VmGenResult.err(r.takeErr)

  c.gABC(n, opcEof)
  updateEnvironment(c)

  result = VmGenResult.ok: (start: start, regCount: c.prc.regInfo.len)

proc genExpr*(c: var TCtx; n: PNode): VmGenResult =
  ## Generates and emits code for the standalone expression `n`
  c.removeLastEof()
  c.setupLinkState()

  let n = canonicalizeSingle(c.graph, c.idgen, c.module, n, selectOptions(c))
  gatherDependencies(c, n)

  let
    start = c.code.len
    r = vmgen.genExpr(c, n)

  if unlikely(r.isErr):
    return VmGenResult.err(r.takeErr)

  c.gABC(n, opcRet, r.unsafeGet)
  updateEnvironment(c)

  result = VmGenResult.ok: (start: start, regCount: c.prc.regInfo.len)

proc genProc(c: var TCtx, s: PSym): VmGenResult =
  c.removeLastEof()
  c.setupLinkState()

  var body =
    if s.kind == skMacro:
      transformBody(c.graph, c.idgen, s, s.ast[bodyPos])
    else:
      # watch out! While compile-time only procedures don't need to be cached
      # here, we still need to retrieve their already cached body (if one
      # exists). Lifted inner procedures would otherwise not work.
      transformBody(c.graph, c.idgen, s, cache = not isCompileTimeProc(s))

  body = canonicalize(c.graph, c.idgen, s, body, selectOptions(c))
  c.gatherDependencies(body)

  result = genProc(c, s, body)
  if unlikely(result.isErr):
    return

  c.gABC(body, opcEof)
  updateEnvironment(c)


proc compile*(c: var TCtx, fnc: FunctionIndex): VmGenResult =
  ## Generates code for the the given function and updates the execution
  ## environment. In addition, the function's table entry is updated with the
  ## bytecode position and execution requirements (i.e. register count). Make
  ## sure to only use `compile` when you're sure the function wasn't generated
  ## yet
  let prc = c.functions[fnc.int]
  assert prc.start == -1, "proc already generated: " & $prc.start

  result = genProc(c, prc.sym)
  if unlikely(result.isErr):
    return

  fillProcEntry(c.functions[fnc.int], result.unsafeGet)

  when defined(nimVMDebugGenerate):
    # XXX: ``compile`` shouldn't be responsible for neither the generating nor
    #      the reporting of a code-listing
    c.config.localReport():
      initVmCodeListingReport(c, prc.sym, nil, start = result.unsafeGet.start)

proc loadProc*(c: var TCtx, sym: PSym): VmGenResult =
  ## The main entry point into the JIT code-generator. Retrieves the
  ## information required for executing `sym`. A function table entry is
  ## created first if it doesn't exist yet, and the procedure is also
  ## generated via `compile` if it wasn't already
  let
    idx = c.registerProc(sym)
    prc = c.functions[idx.int]

  if prc.start >= 0:
    VmGenResult.ok: (start: prc.start, regCount: prc.regCount.int)
  else:
    compile(c, idx)