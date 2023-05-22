## Implements the "collect" pass. This pass gathers the full AST for each
## module into a single structure, which is then meant to be consumed by the
## code-generation orchestrators (``cbackend``, ``jsbackend``, etc.).
##
## This is somewhat similar to the rodfile-based IC backend, but instead of
## reading the modules' content from the rodfiles, it's collected via the pass
## interface.

import
  compiler/ast/[
    ast,
    ast_idgen,
    ast_types,
    lineinfos
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    passes
  ],
  compiler/utils/[
    containers
  ]

type
  Module* = object
    ## Represents the contents of a fully analysed module, intended for use by
    ## the compiler backend.
    stmts*: seq[PNode] ## top level statements in the order they were parsed
    sym*: PSym         ## module symbol
    idgen*: IdGenerator

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
    stmts: seq[PNode]

func isFilled*(m: Module): bool =
  # required so that ``Module`` is usable as the item type of a ``SeqMap``
  m.sym != nil

proc takeModuleList*(graph: ModuleGraph): ModuleList =
  ## Moves the ``ModuleList`` set up by the collector pass out of the
  ## `graph.backend` field and returns it.
  result = move ModuleListBackend(graph.backend).modules
  graph.backend = nil

# Below is the `passes` interface implementation

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  result = CollectPassCtx(idgen: idgen, module: module)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n

  let c = CollectPassCtx(b)
  c.stmts.add(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)

  if graph.backend == nil:
    graph.backend = ModuleListBackend()

  template list: ModuleList = ModuleListBackend(graph.backend).modules

  let
    c = CollectPassCtx(b)
    pos = c.module.position.FileIndex

  list.modules[pos] = Module(stmts: move c.stmts,
                             sym: c.module,
                             idgen: c.idgen)
  list.modulesClosed.add(pos)

const collectPass* = makePass(myOpen, myProcess, myClose)
