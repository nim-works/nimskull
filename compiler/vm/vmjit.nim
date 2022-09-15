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
    ast_query,
    reports
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

func setupLinkState(c: var TCtx) =
  c.codegenInOut.newProcs.setLen(0)
  c.codegenInOut.newGlobals.setLen(0)
  c.codegenInOut.newConsts.setLen(0)
  c.codegenInOut.nextGlobal = c.globals.len.uint32
  c.codegenInOut.nextConst = c.complexConsts.len.uint32
  c.codegenInOut.nextProc = c.functions.len.uint32

proc updateEnvironment(c: var TCtx) =
  ## Needs to be called after a `vmgen` invocation and prior to resuming
  ## execution. Allocates and sets up the execution resources required for the
  ## newly gathered dependencies
  template grow(list, nextName) =
    assert c.list.len <= c.codegenInOut.nextName.int
    c.list.setLen(c.codegenInOut.nextName)

  let
    ps = c.functions.len
    gs = c.globals.len
    cs = c.complexConsts.len

  # allocate the required global/const/function slots
  grow(globals, nextGlobal)
  grow(complexConsts, nextConst)
  grow(functions, nextProc)

  for i, sym in c.codegenInOut.newProcs.lpairs:
    c.functions[ps + i] = initProcEntry(c, sym)

  for i, sym in c.codegenInOut.newGlobals.lpairs:
    let typ = c.getOrCreate(sym.typ)
    c.globals[gs + i] = c.heap.heapNew(c.allocator, typ)

  for i, sym in c.codegenInOut.newConsts.lpairs:
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

  let
    start = c.code.len
    r = vmgen.genStmt(c, n)

  if unlikely(r.isErr):
    return VmGenResult.err(r.takeErr)

  c.gABC(n, opcEof)
  updateEnvironment(c)

  result = VmGenResult.ok: (start: start, regCount: c.prc.regInfo.len)

proc genExpr*(c: var TCtx; n: PNode, requiresValue = true): VmGenResult =
  ## Generates and emits code for the standalone expression `n`
  c.removeLastEof()
  c.setupLinkState()

  result = vmgen.genExpr(c, n, requiresValue)
  if unlikely(result.isErr):
    return

  updateEnvironment(c)

proc genProc(c: var TCtx, s: PSym): VmGenResult =
  # remember the previous 'eof' (if there's one) for later retrieval of it's
  # operand
  let last = c.code.len-1
  var eofInstr: TInstr
  if last >= 0 and c.code[last].opcode == opcEof:
    eofInstr = c.code[last]
    c.code.setLen(last)
    c.debug.setLen(last)

  let
    body = transformBody(c.graph, c.idgen, s, cache = not isCompileTimeProc(s))

  c.setupLinkState()

  result = genProc(c, s, body)
  if unlikely(result.isErr):
    return

  # insert an 'eof' using the previous one's operand. If no there was no
  # previous 'eof', the operand is '0', which is correct
  let newEof = opcEof.TInstrType or (eofInstr.regA.TInstrType shl regAShift)
  c.code.add(newEof.TInstr)
  c.debug.add(body.info)

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