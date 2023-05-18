## Implements the "collect" pass. This pass gathers the full AST for each
## module into a single structure, which is then meant to be consumed by the
## code-generation orchestrators.
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
  FullModule* = object
    stmts*: seq[PNode] ## top level statements in the order they were parsed
    sym*: PSym         ## module symbol
    idgen*: IdGenerator

  ModuleListRef* = ref ModuleList
  ModuleList* = object of RootObj
    modules*: SeqMap[FileIndex, FullModule]
    modulesClosed*: seq[FileIndex]
      ## stores the modules in the order they were closed. The first closed
      ## module comes first, then the next, etc.

  ModuleRef = ref object of TPassContext
    ## The pass context for the VM backend. Represents a reference to a
    ## module in the module list
    list: ModuleListRef
    index: FileIndex

func isFilled*(m: FullModule): bool =
  # required so that ``FullModule`` is usable as the item type of a ``SeqMap``
  m.sym != nil

# Below is the `passes` interface implementation

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  if graph.backend == nil:
    graph.backend = ModuleListRef()

  let
    mlist = ModuleListRef(graph.backend)
    pos = module.position.FileIndex

  # add an empty entry for the module:
  mlist.modules[pos] = FullModule(sym: module, idgen: idgen)

  result = ModuleRef(list: mlist, index: pos)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n
  let m = ModuleRef(b)

  m.list.modules[m.index].stmts.add(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)

  let m = ModuleRef(b)
  m.list.modulesClosed.add(m.index)

const collectPass* = makePass(myOpen, myProcess, myClose)
