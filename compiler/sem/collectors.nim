## Implements the lowering of modules into procedures and objects. The AST
## of a module is separated in *declarative* and *imperative* nodes:
## *declarative* nodes are gathered into a single list, while *imperative*
## nodes are turned into a statement list, transformed, and then wrapped in a
## procedure (the module's ``init`` procedure).
##
## For integration with the ``passes`` interface, a simple adapter pass is
## provided. It sets up and populates a ``ModuleList`` instance, which can
## then be retrieved from the ``ModuleGraph`` after all passes have run via
## ``takeModuleList``.
##
## The collection part is somewhat similar to the rodfile-based IC backend, but
## instead of reading the modules' content from the rodfiles, it's collected
## via the pass interface.

# TODO: the module needs a better name

import
  compiler/ast/[
    ast,
    ast_idgen,
    ast_types,
    ast_query,
    lineinfos,
    idents
  ],
  compiler/front/[
    options
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    lowerings,
    passes
  ],
  compiler/utils/[
    containers,
    idioms
  ]

from compiler/sem/injectdestructors import genDestroy

type
  ModuleStructs* = object
    globals*: seq[PSym]
      ## all top-level globals part of the module

    threadvars*: seq[PSym]
      ## all thread-local variables part of the module

  Module* = object
    ## Represents the contents of a fully analysed module, intended for use by
    ## the compiler backend.
    sym*: PSym          ## module symbol
    idgen*: IdGenerator ## the ID generator associated with the module

    decls*: PNode
      ## all declarative statements (type, routine, and constant
      ## definitions)
    structs*: ModuleStructs
      ## stores the contents of the module's structs

    # the module-bound procedures. Conceptually, these operate on the module
    # structs
    dataInit*: PSym
      ## the procedure responsible for initializing data associated with the
      ## module. This is data that must be initialized at run-time but does
      ## not depend on user-defined globals
    init*: PSym
      ## the procedure responsible for initializing the module's globals
    destructor*: PSym
      ## the prodcedure responsible for de-initializing the module's
      ## globals

    # XXX: the design around the pre-init procedure is likely not final yet.
    #      At the moment, we set it up here so that the code generators /
    #      orchestrators have a symbol to attach code to
    preInit*: PSym
      ## the procedure for initializing the module's pure globals

  ModuleList* = object
    modules*: SeqMap[FileIndex, Module]
    modulesClosed*: seq[FileIndex]
      ## stores the modules in the order they were closed. The first closed
      ## module comes first, then the next, etc.

  ModuleListBackend = ref object of RootObj
    ## Adapter type required for storing a ``ModuleList`` in the
    ## ``ModuleGraph.backend`` field.
    modules: ModuleList

  CollectPassCtx = ref object of TPassContext
    ## Represents a module during the "collect" pass, and is populated as part
    ## of it. Turned into a ``Module`` instance once the module is closed.
    module: PSym

    decls: seq[PNode]      ## all declarative statements
    imperative: seq[PNode] ## all imperative statements

func isFilled*(m: Module): bool =
  # required so that ``Module`` is usable as the item type of a ``SeqMap``
  m.sym != nil

proc takeModuleList*(graph: ModuleGraph): ModuleList =
  ## Moves the ``ModuleList`` set up by the collector pass out of the
  ## `graph.backend` field and returns it.
  result = move ModuleListBackend(graph.backend).modules
  graph.backend = nil

proc distribute(n: PNode, decl, imperative: var seq[PNode]) =
  case n.kind
  of nkCommentStmt, nkIncludeStmt, nkImportStmt, nkImportExceptStmt,
     nkExportStmt, nkExportExceptStmt, nkFromStmt, nkMixinStmt, nkBindStmt,
     nkStaticStmt:
    # declarative statements not relevant to the backends -- they're dropped
    discard
  of nkTemplateDef, nkMacroDef:
    # not relevant to the backend
    discard
  of nkEmpty:
    discard "drop empty node"
  of nkNone, nkError:
    unreachable()
  of nkStmtList:
    # flatten statement lists
    for it in n.items:
      distribute(it, decl, imperative)
  of nkTypeSection, nkConstSection, declarativeDefs:
    decl.add(n)
  else:
    # everything else is treated as imperative statements. Until it's clearer
    # what to do with them, this also includes declarative statements part
    # of nested scopes (those inside ``if``, ``block``, etc. statements)
    imperative.add(n)

proc createModuleOp(graph: ModuleGraph, idgen: IdGenerator, name: string, module: PSym, body: PNode, options: TOptions): PSym =
  ## Creates the symbol for a module-bound operator. Note that this attachment
  ## is purely at the conceptional level at the moment.
  result = newSym(skProc, getIdent(graph.cache, name), nextSymId idgen,
                  module, module.info, options)
  # the procedure doesn't return anything and doesn't have parameters:
  result.typ = newProcType(module.info, nextTypeId idgen, module)

  # also set up a proper definition AST:
  result.ast = newProcNode(nkProcDef, module.info, body,
                           params        = newTree(nkFormalParams, [graph.emptyNode]),
                           name          = newSymNode(result),
                           pattern       = graph.emptyNode,
                           genericParams = graph.emptyNode,
                           pragmas       = graph.emptyNode,
                           exceptions    = graph.emptyNode)

proc extractGlobals(stmts: seq[PNode], graph: ModuleGraph, idgen: IdGenerator,
                    owner: PSym,
                    struct: var ModuleStructs): PNode =
  ## Extracts the definitions of top-level globals and threadvars, and
  ## extract. The AST with the relevant ``var``/``let`` sections rewritten as
  ## assignments is returned.
  ##
  ## `graph`, `idgen`, and `owner` are currently required for lowering var
  ## tuples.

  proc transform(n: PNode, graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                 struct: var ModuleStructs, result: PNode) =
    # XXX: ``transform`` being an iterator would be more flexibile, but also
    #      more complex
    case n.kind
    of nkVarSection, nkLetSection:
      for it in n.items:
        case it.kind
        of nkCommentStmt:
          discard "ignore"
        of nkVarTuple:
          # XXX: things would be much simpler if var tuples would have be
          #      lowered already. However, even if ``transf`` did that (which
          #      is likely should), it would be too late for our case here
          let r = lowerTupleUnpacking(graph, it, idgen, owner)
          for x in r.items:
            transform(x, graph, idgen, owner, struct, result)
        of nkIdentDefs:
          assert it.len == 3
          let s = it[0].sym
          if sfCompileTime in s.flags:
            # don't lift compile-time globals into a module struct, and also
            # don't generate an assignment for them
            # XXX: how they work exactly is currently left to the code
            #      generator, but this is going to change
            discard
          elif sfThread in s.flags:
            struct.threadvars.add s
          elif sfGlobal in s.flags:
            struct.globals.add s
            # replace with an assignment:
            if it[2].kind != nkEmpty:
              result.add newTreeI(nkAsgn, it.info, [it[0], it[2]])
          else:
            # keep the node as an identdefs
            result.add newTreeI(n.kind, n.info, it)
        else:
          unreachable()

    of nkStmtList:
      unreachable("top-level statement lists must not exist at this point")
    else:
      result.add(n)

  result = newNode(nkStmtList)
  result.sons = newSeqOfCap[PNode](stmts.len)

  for it in stmts.items:
    transform(it, graph, idgen, owner, struct, result)

proc generateModuleDestructor(graph: ModuleGraph, m: Module): PNode =
  ## Generates the body for the destructor procedure of module `m` (also
  ## referred to as the 'de-init' procedure).
  result = newNode(nkStmtList)
  for s in m.structs.globals.items:
    if hasDestructor(s.typ):
      result.add genDestroy(graph, m.idgen, m.sym, newSymNode(s))

  # note: the generated body is not yet final -- we're still missing
  # destructor calls for globals defined inside procedures (i.e., pure
  # globals). These calls are inserted once we know all alive pure globals
  # (which is after the main part of code generation has finished)

  if result.len == 0:
    # collapse to an empty node (dead-code elimination looks for them in
    # order to detect empty procedures)
    result = newNode(nkEmpty)

proc setupModule*(graph: ModuleGraph, idgen: IdGenerator, m: PSym,
                  decls, imperative: seq[PNode]): Module =
  ## From the declaratives and imperative statements gathered through the pass
  ## interface for a single module, creates a ``Module`` instance. The module
  ## structs are populated with the initial items (top-level globals defined
  ## in the outermost scope, and threadvars), and the module-bound
  ## operators created and initialized.
  result = Module(sym: m, idgen: idgen)

  result.decls =
    if decls.len == 0: newNodeI(nkEmpty, m.info)
    else:              newTreeI(nkStmtList, m.info, decls)

  # extract the top-level globals and threadvars:
  var imperative = extractGlobals(imperative, graph, idgen, m, result.structs)
  if imperative.len == 0:
    # there's no imperative code
    imperative = newNodeI(nkEmpty, imperative.info)
  else:
    imperative.info = m.info

  # TODO: the symbols defined by the AST we're moving into the init procedure
  #       also need to be re-targeted (i.e., have their owner adjusted), but
  #       that would break too many assumptions right now

  var options = graph.config.options
  if sfSystemModule in m.flags:
    # don't generate stack traces for code in the ``system`` module
    options.excl optStackTrace

  # now that we have the code that makes up the user-defined module
  # initialization, we wrap it into a procedure:
  result.init = createModuleOp(graph, idgen, m.name.s & "Init", m, imperative, options)
  result.init.flags = m.flags * {sfInjectDestructors} # inherit the flag

  # create a procedure for the data-init operator already. The selected backend
  # is then responsible for filling it with content
  result.dataInit = createModuleOp(graph, idgen, m.name.s & "DatInit", m, newNode(nkEmpty), options)

  # setup the module struct clean-up operator:
  let destructorBody = generateModuleDestructor(graph, result)
  result.destructor = createModuleOp(graph, idgen, m.name.s & "Deinit", m, destructorBody, options)

  result.preInit = createModuleOp(graph, idgen, m.name.s & "PreInit", m, newNode(nkEmpty), options)

# Below is the `passes` interface implementation

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  result = CollectPassCtx(idgen: idgen, module: module)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n

  let c = CollectPassCtx(b)
  distribute(n, c.decls, c.imperative)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)

  if graph.backend == nil:
    graph.backend = ModuleListBackend()

  template list: ModuleList = ModuleListBackend(graph.backend).modules

  let
    c = CollectPassCtx(b)
    pos = c.module.position.FileIndex

  list.modules[pos] = setupModule(graph, c.idgen, c.module, c.decls,
                                  c.imperative)
  list.modulesClosed.add(pos)

const collectPass* = makePass(myOpen, myProcess, myClose)
