## Implements the "collect" pass. This pass gathers the full AST for each
## module into a single structure, which is then meant to be consumed by the
## code-generation orchestrators.
##
## This is somewhat similar to the rodfile-based IC backend, but instead of
## reading the modules' content from the rodfiles, it's collected via the pass
## interface.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast,
    ast_idgen,
    ast_types
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    passes
  ]

type
  FullModule* = object
    stmts*: seq[PNode] ## top level statements in the order they were parsed
    sym*: PSym         ## module symbol

  ModuleListRef* = ref ModuleList
  ModuleList* = object of RootObj
    modules*: seq[FullModule]
    modulesClosed*: seq[int]   ## indices into `modules` in the order the
                               ## modules were closed. The first closed module
                               ## comes first, then the next, etc.
    moduleMap*: Table[int, int] ## module sym-id -> index into `modules`

  ModuleRef = ref object of TPassContext
    ## The pass context for the VM backend. Represents a reference to a
    ## module in the module list
    list: ModuleListRef
    index: int

# Below is the `passes` interface implementation

func growBy[T](x: var seq[T], n: Natural) {.inline.} =
  x.setLen(x.len + n)

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  if graph.backend == nil:
    graph.backend = ModuleListRef()

  let
    mlist = ModuleListRef(graph.backend)
    next = mlist.modules.len

  # append an empty module to the list
  mlist.modules.growBy(1)
  mlist.modules[next] = FullModule(sym: module)
  mlist.moduleMap[module.id] = next

  result = ModuleRef(list: mlist, index: next)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n
  let m = ModuleRef(b)

  const declarativeKinds = routineDefs + {nkTypeSection, nkPragma,
    nkExportStmt, nkExportExceptStmt, nkFromStmt, nkImportStmt,
    nkImportExceptStmt}

  if n.kind notin declarativeKinds:
    m.list.modules[m.index].stmts.add(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)

  let m = ModuleRef(b)
  m.list.modulesClosed.add(m.index)

const collectPass* = makePass(myOpen, myProcess, myClose)
