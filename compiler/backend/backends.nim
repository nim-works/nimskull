## Shared processing logic used by all backends.

import
  compiler/ast/[
    ast,
    idents,
    lineinfos
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/sem/[
    collectors
  ],
  compiler/utils/[
    containers
  ]

iterator ritems[T](x: openArray[T]): lent T =
  ## Iterates and yields the items from the container `x` in reverse.
  var i = x.high
  while i >= 0:
    yield x[i]
    dec i

proc emitOpCall(graph: ModuleGraph, op: PSym, dest: PNode) =
  ## Emits a call to the provided operator `op`, but only if the operator is
  ## a non-empty procedure.
  if getBody(graph, op).kind != nkEmpty:
    dest.add newTree(nkCall, newSymNode(op))

proc generateMain*(graph: ModuleGraph, modules: ModuleList, result: PNode) =
  ## Generates the program initialization logic and emits it to `result`. This
  ## is the code invoking each module's init procedures.

  # XXX: why not fully initialize the ``system`` module first?
  # first initialize the additional data associated with each module:
  for it in closed(modules):
    emitOpCall(graph, it.dataInit, result)
    # the system module is special cased: its fully during the data-init phase
    if sfSystemModule in it.sym.flags:
      emitOpCall(graph, it.init, result)

  # then the modules are initialized and their module-level code executed
  for it in closed(modules):
    if sfSystemModule notin it.sym.flags:
      emitOpCall(graph, it.init, result)

proc generateTeardown*(graph: ModuleGraph, modules: ModuleList, result: PNode) =
  ## Generates the code for de-initializing the program and emits it to `result`.
  # tearing down the modules has to happen in the reverse order they were
  # initialized in, but with ``system`` always coming last
  for it in rclosed(modules):
    if sfSystemModule notin it.sym.flags:
      emitOpCall(graph, it.destructor, result)

  emitOpCall(graph, systemModule(modules).destructor, result)

proc finishDeinit*(graph: ModuleGraph, modules: var ModuleList) =
  ## Appends destructor calls for procedure-level globals to the bodies of
  ## the modules' de-init procedures. Called at the very end of code generation
  ## once all alive procedure-level globals have been discovered.

  # XXX: if the called destructors access other globals, or introduce new
  #      ones, behaviour is undefined. In the best but most unlikely case,
  #      everything will work as expected, but in the worst case, the code
  #      is going to misbehave at run-time

  proc prepare(prc: PSym): PNode =
    # XXX: not pretty, but it's going to removed again anyway
    case prc.ast[bodyPos].kind
    of nkEmpty:
      result = newNodeI(nkStmtList, prc.ast[bodyPos].info)
      prc.ast[bodyPos] = result
    of nkStmtList:
      # already supports appending
      result = prc.ast[bodyPos]
    else:
      result = newTree(nkStmtList, prc.ast[bodyPos])
      prc.ast[bodyPos] = result

  # XXX: the whole ``globalDestructors`` mechanism is wrong. Moving dead-code
  #      elimination into the orchestrators will, among other things, allow
  #      for removing it
  for (module, call) in ritems(graph.globalDestructors):
    prepare(modules[module.FileIndex].destructor).add call

proc generateMainProcedure*(graph: ModuleGraph, idgen: IdGenerator, modules: ModuleList): PSym =
  ## Generates the procedure for initializing, running, and de-initializing
  ## the full program (`modules`).
  let
    owner = mainModule(modules).sym
    programRes = getCompilerProc(graph, "programResult")

  # setup the symbol:
  result = newSym(skProc, getIdent(graph.cache, "NimMain"), nextSymId idgen,
                  owner, unknownLineInfo, {})
  result.typ = newProcType(unknownLineInfo, nextTypeId idgen, owner)

  let resSym = newSym(skResult, getIdent(graph.cache, "result"),
                      nextSymId idgen, result, unknownLineInfo, {})
  resSym.typ = programRes.typ
  result.typ[0] = resSym.typ # set the correct return type

  var body = newNode(nkStmtList)
  generateMain(graph, modules, body)
  generateTeardown(graph, modules, body)
  # generate the result assignment:
  body.add newTree(nkAsgn, [newSymNode(resSym), newSymNode(programRes)])

  result.ast = newProcNode(nkProcDef, owner.info, body,
                           params        = newTree(nkFormalParams, [newNodeIT(nkType, owner.info, resSym.typ)]),
                           name          = newSymNode(result),
                           pattern       = graph.emptyNode,
                           genericParams = graph.emptyNode,
                           pragmas       = graph.emptyNode,
                           exceptions    = graph.emptyNode)
  # attach the result symbol:
  result.ast.sons.setLen(resultPos + 1)
  result.ast[resultPos] = newSymNode(resSym)