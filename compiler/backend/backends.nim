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
    modulelowering
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
  ## Generates the program initialization code and emits it to `result`. The
  ## initialization logic is the code that invokes each module's init
  ## procedures.

  # XXX: why not fully initialize the ``system`` module first?
  # first initialize the additional data associated with each module:
  for it in closed(modules):
    emitOpCall(graph, it.dataInit, result)
    # the system module is special cased: its fully initialized during the
    # data-init phase
    if sfSystemModule in it.sym.flags:
      emitOpCall(graph, it.preInit, result)
      emitOpCall(graph, it.init, result)

  # then the modules are initialized and their module-level code executed
  for it in closed(modules):
    if sfSystemModule notin it.sym.flags:
      emitOpCall(graph, it.preInit, result)
      emitOpCall(graph, it.init, result)

proc generateTeardown*(graph: ModuleGraph, modules: ModuleList, result: PNode) =
  ## Generates the code for de-initializing the program, and emits it to
  ## `result`.
  # tearing down the modules has to happen in the reverse order they were
  # initialized in, but with ``system`` always coming last
  for it in rclosed(modules):
    if sfSystemModule notin it.sym.flags:
      emitOpCall(graph, it.destructor, result)
      emitOpCall(graph, it.postDestructor, result)

  emitOpCall(graph, systemModule(modules).destructor, result)
  emitOpCall(graph, systemModule(modules).postDestructor, result)

proc generateMainProcedure*(graph: ModuleGraph, idgen: IdGenerator,
                            modules: ModuleList): PSym =
  ## Generates the procedure for initializing, running, and de-initializing
  ## the full program (`modules`). The procedure returns the value of the
  ## internal ``programResult`` global.
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